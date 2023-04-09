module Dahdit.Run
  ( GetError (..)
  , prettyGetError
  , runGetInternal
  , runCount
  , runPutInternal
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), onException)
import Control.Monad (replicateM_, unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Dahdit.Counts (ByteCount (..), ElemCount (..))
import Dahdit.Free
  ( Get (..)
  , GetF (..)
  , GetLookAheadF (..)
  , GetScopeF (..)
  , GetStaticArrayF (..)
  , GetStaticSeqF (..)
  , Put
  , PutF (..)
  , PutM (..)
  , PutStaticArrayF (..)
  , PutStaticHintF (..)
  , PutStaticSeqF (..)
  , ScopeMode (..)
  )
import Dahdit.LiftedPrim (LiftedPrimArray (..), sizeofLiftedPrimArray)
import Dahdit.Mem (ReadMem (..), WriteMem (..), readSBSMem, writeSBSMem)
import Dahdit.Nums
  ( FloatBE
  , FloatLE
  , Int16BE
  , Int16LE (..)
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Word16BE
  , Word16LE (..)
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  )
import Dahdit.Proxy (proxyForF)
import Dahdit.Sizes (staticByteSize)
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromJust)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import Data.Word (Word8)

-- Sizes:

getStaticSeqSize :: GetStaticSeqF a -> ByteCount
getStaticSeqSize (GetStaticSeqF ec g _) = staticByteSize (proxyForF g) * coerce ec

getStaticArraySize :: GetStaticArrayF a -> ByteCount
getStaticArraySize (GetStaticArrayF n prox _) = staticByteSize prox * coerce n

putStaticSeqSize :: PutStaticSeqF a -> ByteCount
putStaticSeqSize (PutStaticSeqF n _ _ s _) = staticByteSize (proxyForF s) * coerce n

putStaticArrayElemSize :: PutStaticArrayF a -> ByteCount
putStaticArrayElemSize (PutStaticArrayF _ _ a _) = staticByteSize (proxyForF a)

putStaticArraySize :: PutStaticArrayF a -> ByteCount
putStaticArraySize (PutStaticArrayF n _ a _) = staticByteSize (proxyForF a) * coerce n

-- Get:

data GetError
  = GetErrorParseNeed !String !ByteCount !ByteCount
  | GetErrorScopedMismatch !ByteCount !ByteCount
  | GetErrorFail !String
  deriving stock (Eq, Show)

instance Exception GetError where
  displayException = prettyGetError

prettyGetError :: GetError -> String
prettyGetError = \case
  GetErrorParseNeed nm ac bc -> "End of input parsing " ++ nm ++ " (have " ++ show (unByteCount ac) ++ " bytes, need " ++ show (unByteCount bc) ++ ")"
  GetErrorScopedMismatch ac bc -> "Did not parse enough scoped input (read " ++ show (unByteCount ac) ++ " bytes, expected " ++ show (unByteCount bc) ++ ")"
  GetErrorFail msg -> "User error: " ++ msg

data GetEnv s r = GetEnv
  { geLen :: !ByteCount
  -- ^ Remaining length of buffer segment
  , geOff :: !(STRef s ByteCount)
  -- ^ Offset from buffer start (in bytes)
  , geArray :: !r
  -- ^ Source buffer
  }

newGetEnv :: ByteCount -> r -> ST s (GetEnv s r)
newGetEnv len mem = do
  off <- newSTRef 0
  pure (GetEnv len off mem)

newtype GetEff s r a = GetEff {unGetEff :: ReaderT (GetEnv s r) (ExceptT GetError (ST s)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (GetEnv s r), MonadError GetError)

runGetEff :: GetEff s r a -> GetEnv s r -> ST s (Either GetError a)
runGetEff act env = runExceptT (runReaderT (unGetEff act) env)

instance MonadFail (GetEff s r) where
  fail = GetEff . throwError . GetErrorFail

stGetEff :: ST s a -> GetEff s r a
stGetEff = GetEff . lift . lift

newtype GetRun s r a = GetRun {unGetRun :: FreeT GetF (GetEff s r) a}
  deriving newtype (Functor, Applicative, Monad)

guardReadBytes :: String -> ByteCount -> GetEff s r ByteCount
guardReadBytes nm bc = do
  GetEnv len offRef _ <- ask
  off <- stGetEff (readSTRef offRef)
  let ac = len - off
  if bc > ac
    then throwError (GetErrorParseNeed nm ac bc)
    else pure off

readBytes :: String -> ByteCount -> (r -> ByteCount -> a) -> GetEff s r a
readBytes nm bc f = do
  off <- guardReadBytes nm bc
  GetEnv _ offRef mem <- ask
  stGetEff $ do
    let a = f mem off
        newOff = off + bc
    writeSTRef offRef newOff
    pure a

readScope :: ReadMem r => GetScopeF (GetEff s r a) -> GetEff s r a
readScope (GetScopeF sm bc g k) = do
  GetEnv oldLen offRef _ <- ask
  oldOff <- stGetEff (readSTRef offRef)
  let oldAvail = oldLen - oldOff
  if bc > oldAvail
    then throwError (GetErrorParseNeed "scope" oldAvail bc)
    else do
      let newLen = oldOff + bc
      a <- local (\ge -> ge {geLen = newLen}) (mkGetEff g)
      case sm of
        ScopeModeWithin -> k a
        ScopeModeExact -> do
          newOff <- stGetEff (readSTRef offRef)
          let actualBc = newOff - oldOff
          if actualBc == bc
            then k a
            else throwError (GetErrorScopedMismatch actualBc bc)

readStaticSeq :: ReadMem r => GetStaticSeqF (GetEff s r a) -> GetEff s r a
readStaticSeq gss@(GetStaticSeqF ec g k) = do
  let bc = getStaticSeqSize gss
  _ <- guardReadBytes "static sequence" bc
  ss <- Seq.replicateA (coerce ec) (mkGetEff g)
  k ss

readStaticArray :: ReadMem r => GetStaticArrayF (GetEff s r a) -> GetEff s r a
readStaticArray gsa@(GetStaticArrayF _ _ k) = do
  let bc = getStaticArraySize gsa
  sa <- readBytes "static vector" bc (\mem off -> cloneArrayMemInBytes mem off bc)
  k (LiftedPrimArray sa)

readLookAhead :: ReadMem r => GetLookAheadF (GetEff s r a) -> GetEff s r a
readLookAhead (GetLookAheadF g k) = do
  offRef <- asks geOff
  startOff <- stGetEff (readSTRef offRef)
  a <- mkGetEff g
  stGetEff (writeSTRef offRef startOff)
  k a

execGetRun :: ReadMem r => GetF (GetEff s r a) -> GetEff s r a
execGetRun = \case
  GetFWord8 k -> readBytes "Word8" 1 (indexMemInBytes @_ @Word8) >>= k
  GetFInt8 k -> readBytes "Int8" 1 (indexMemInBytes @_ @Int8) >>= k
  GetFWord16LE k -> readBytes "Word16LE" 2 (indexMemInBytes @_ @Word16LE) >>= k
  GetFInt16LE k -> readBytes "Int16LE" 2 (indexMemInBytes @_ @Int16LE) >>= k
  GetFWord24LE k -> readBytes "Word24LE" 3 (indexMemInBytes @_ @Word24LE) >>= k
  GetFInt24LE k -> readBytes "Int24LE" 3 (indexMemInBytes @_ @Int24LE) >>= k
  GetFWord32LE k -> readBytes "Word32LE" 4 (indexMemInBytes @_ @Word32LE) >>= k
  GetFInt32LE k -> readBytes "Int32LE" 4 (indexMemInBytes @_ @Int32LE) >>= k
  GetFFloatLE k -> readBytes "FloatLE" 4 (indexMemInBytes @_ @FloatLE) >>= k
  GetFWord16BE k -> readBytes "Word16BE" 2 (indexMemInBytes @_ @Word16BE) >>= k
  GetFInt16BE k -> readBytes "Int16BE" 2 (indexMemInBytes @_ @Int16BE) >>= k
  GetFWord24BE k -> readBytes "Word24BE" 3 (indexMemInBytes @_ @Word24BE) >>= k
  GetFInt24BE k -> readBytes "Int24BE" 3 (indexMemInBytes @_ @Int24BE) >>= k
  GetFWord32BE k -> readBytes "Word32BE" 4 (indexMemInBytes @_ @Word32BE) >>= k
  GetFInt32BE k -> readBytes "Int32BE" 4 (indexMemInBytes @_ @Int32BE) >>= k
  GetFFloatBE k -> readBytes "FloatBE" 4 (indexMemInBytes @_ @FloatBE) >>= k
  GetFShortByteString bc k ->
    readBytes "ShortByteString" bc (\mem off -> readSBSMem mem off bc) >>= k
  GetFStaticSeq gss -> readStaticSeq gss
  GetFStaticArray gsa -> readStaticArray gsa
  GetFByteArray bc k ->
    readBytes "ByteArray" bc (\mem off -> cloneArrayMemInBytes mem off bc) >>= k
  GetFScope gs -> readScope gs
  GetFSkip bc k -> readBytes "skip" bc (\_ _ -> ()) *> k
  GetFLookAhead gla -> readLookAhead gla
  GetFRemainingSize k -> do
    GetEnv len offRef _ <- ask
    off <- stGetEff (readSTRef offRef)
    k (len - off)
  GetFFail msg -> fail msg

runGetRun :: ReadMem r => GetRun s r a -> GetEnv s r -> ST s (Either GetError a)
runGetRun = runGetEff . iterGetRun

iterGetRun :: ReadMem r => GetRun s r a -> GetEff s r a
iterGetRun act = iterT execGetRun (unGetRun act)

mkGetRun :: Get a -> GetRun s r a
mkGetRun (Get (F w)) = GetRun (w pure wrap)

mkGetEff :: ReadMem r => Get a -> GetEff s r a
mkGetEff = iterGetRun . mkGetRun

runGetInternal :: ReadMem r => Get a -> ByteCount -> r -> (Either GetError a, ByteCount)
runGetInternal act len mem = runST $ do
  let eff = mkGetEff act
  env <- newGetEnv len mem
  ea <- runGetEff eff env
  bc <- readSTRef (geOff env)
  pure (ea, bc)

-- Put unsafe:

data PutEnv s q = PutEnv
  { peLen :: !ByteCount
  -- ^ Remaining capacity in buffer segment
  , peOff :: !(STRef s ByteCount)
  -- ^ Offset in bytes from start of buffer
  , peArray :: !(q s)
  -- ^ Destination buffer
  }

newPutEnv :: ByteCount -> q s -> ST s (PutEnv s q)
newPutEnv len mem = do
  offRef <- newSTRef 0
  pure (PutEnv len offRef mem)

newtype PutEff s q a = PutEff {unPutEff :: ReaderT (PutEnv s q) (ST s) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (PutEnv s q))

runPutEff :: PutEff s q a -> PutEnv s q -> ST s a
runPutEff act = runReaderT (unPutEff act)

stPutEff :: ST s a -> PutEff s q a
stPutEff = PutEff . lift

newtype PutRun s q a = PutRun {unPutRun :: FreeT PutF (PutEff s q) a}
  deriving newtype (Functor, Applicative, Monad)

writeBytes :: ByteCount -> (q s -> ByteCount -> ST s ()) -> PutEff s q ()
writeBytes bc f = do
  PutEnv _ offRef mem <- ask
  stPutEff $ do
    off <- readSTRef offRef
    f mem off
    let newOff = off + bc
    writeSTRef offRef newOff

writeStaticSeq :: WriteMem q => PutStaticSeqF (PutEff s q a) -> PutEff s q a
writeStaticSeq (PutStaticSeqF n mz p s k) = do
  for_ (take (coerce n) (toList s)) $ \a -> do
    mkPutEff (p a)
  let e = Seq.length s
  unless (coerce n <= e) $ do
    let q = mkPutEff (p (fromJust mz))
    replicateM_ (coerce n - e) q
  k

writeStaticArray :: WriteMem q => PutStaticArrayF (PutEff s q a) -> PutEff s q a
writeStaticArray psa@(PutStaticArrayF needElems mz a@(LiftedPrimArray ba) k) = do
  let elemSize = putStaticArrayElemSize psa
      haveElems = sizeofLiftedPrimArray a
      useElems = min haveElems (coerce needElems)
      useBc = elemSize * coerce useElems
  writeBytes useBc (copyArrayMemInBytes ba 0 useBc)
  let needBc = putStaticArraySize psa
  unless (useBc == needBc) $ do
    let extraBc = needBc - useBc
    case mz of
      Nothing -> error "no default element for undersized static array"
      Just z -> writeBytes extraBc (setMemInBytes extraBc z)
  k

execPutRun :: WriteMem q => PutF (PutEff s q a) -> PutEff s q a
execPutRun = \case
  PutFWord8 x k -> writeBytes 1 (writeMemInBytes x) *> k
  PutFInt8 x k -> writeBytes 1 (writeMemInBytes x) *> k
  PutFWord16LE x k -> writeBytes 2 (writeMemInBytes x) *> k
  PutFInt16LE x k -> writeBytes 2 (writeMemInBytes x) *> k
  PutFWord24LE x k -> writeBytes 3 (writeMemInBytes x) *> k
  PutFInt24LE x k -> writeBytes 3 (writeMemInBytes x) *> k
  PutFWord32LE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFInt32LE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFFloatLE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFWord16BE x k -> writeBytes 2 (writeMemInBytes x) *> k
  PutFInt16BE x k -> writeBytes 2 (writeMemInBytes x) *> k
  PutFWord24BE x k -> writeBytes 3 (writeMemInBytes x) *> k
  PutFInt24BE x k -> writeBytes 3 (writeMemInBytes x) *> k
  PutFWord32BE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFInt32BE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFFloatBE x k -> writeBytes 4 (writeMemInBytes x) *> k
  PutFShortByteString bc sbs k ->
    writeBytes bc (writeSBSMem sbs bc) *> k
  PutFStaticSeq pss -> writeStaticSeq pss
  PutFStaticArray psa -> writeStaticArray psa
  PutFByteArray bc barr k ->
    writeBytes bc (copyArrayMemInBytes barr 0 bc) *> k
  PutFStaticHint (PutStaticHintF _ p k) -> mkPutEff p *> k

runPutRun :: WriteMem q => PutRun s q a -> PutEnv s q -> ST s a
runPutRun = runPutEff . iterPutRun

iterPutRun :: WriteMem q => PutRun s q a -> PutEff s q a
iterPutRun act = iterT execPutRun (unPutRun act)

mkPutRun :: PutM a -> PutRun s q a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: WriteMem q => PutM a -> PutEff s q a
mkPutEff = iterPutRun . mkPutRun

runPutUnsafe :: WriteMem q => Put -> ByteCount -> q s -> ST s ByteCount
runPutUnsafe act len mem = do
  let eff = mkPutRun act
  st@(PutEnv _ offRef _) <- newPutEnv len mem
  runPutRun eff st
  readSTRef offRef

-- Count:

newtype CountEff a = CountEff {unCountEff :: MaybeT (State ByteCount) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadState ByteCount)

runCountEff :: CountEff a -> ByteCount -> (Maybe a, ByteCount)
runCountEff act = runState (runMaybeT (unCountEff act))

newtype CountRun a = CountRun {unCountRun :: FreeT PutF CountEff a}
  deriving newtype (Functor, Applicative, Monad)

execCountRun :: PutF (CountEff a) -> CountEff a
execCountRun = \case
  PutFWord8 _ k -> State.modify' (1 +) *> k
  PutFInt8 _ k -> State.modify' (1 +) *> k
  PutFWord16LE _ k -> State.modify' (2 +) *> k
  PutFInt16LE _ k -> State.modify' (2 +) *> k
  PutFWord24LE _ k -> State.modify' (3 +) *> k
  PutFInt24LE _ k -> State.modify' (3 +) *> k
  PutFWord32LE _ k -> State.modify' (4 +) *> k
  PutFInt32LE _ k -> State.modify' (4 +) *> k
  PutFFloatLE _ k -> State.modify' (4 +) *> k
  PutFWord16BE _ k -> State.modify' (2 +) *> k
  PutFInt16BE _ k -> State.modify' (2 +) *> k
  PutFWord24BE _ k -> State.modify' (3 +) *> k
  PutFInt24BE _ k -> State.modify' (3 +) *> k
  PutFWord32BE _ k -> State.modify' (4 +) *> k
  PutFInt32BE _ k -> State.modify' (4 +) *> k
  PutFFloatBE _ k -> State.modify' (4 +) *> k
  PutFShortByteString bc _ k -> State.modify' (bc +) *> k
  PutFStaticSeq pss@(PutStaticSeqF _ _ _ _ k) ->
    let bc = putStaticSeqSize pss
    in  State.modify' (bc +) *> k
  PutFStaticArray psv@(PutStaticArrayF _ _ _ k) ->
    let bc = putStaticArraySize psv
    in  State.modify' (bc +) *> k
  PutFByteArray bc _ k -> State.modify' (bc +) *> k
  PutFStaticHint (PutStaticHintF bc _ k) -> State.modify' (bc +) *> k

runCountRun :: CountRun a -> ByteCount -> (Maybe a, ByteCount)
runCountRun = runCountEff . iterCountRun

iterCountRun :: CountRun a -> CountEff a
iterCountRun act = iterT execCountRun (unCountRun act)

mkCountRun :: PutM a -> CountRun a
mkCountRun (PutM (F w)) = CountRun (w pure wrap)

mkCountEff :: PutM a -> CountEff a
mkCountEff = iterCountRun . mkCountRun

runCount :: Put -> ByteCount
runCount act =
  let eff = mkCountRun act
      (_, bc) = runCountRun eff 0
  in  bc

-- Put safe:

runPutInternal :: WriteMem q => Put -> (forall s. ByteCount -> ST s (q s)) -> (forall s. q s -> ByteCount -> ByteCount -> ST s z) -> z
runPutInternal act mkMem useMem = runST $ do
  let cap = runCount act
  mem <- mkMem cap
  case releaseMem mem of
    Nothing -> runPutUnsafe act cap mem >>= useMem mem cap
    Just rel -> unsafeIOToST (onException (unsafeSTToIO (runPutUnsafe act cap mem >>= useMem mem cap)) rel)
