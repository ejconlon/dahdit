module Dahdit.Run
  ( GetError (..)
  , prettyGetError
  , runGet
  , runGetIO
  , runGetFile
  , runCount
  , runPut
  , runPutFile
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..), throwIO)
import Control.Monad (replicateM_, unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Free.Church (F (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (FreeT (..), iterT, wrap)
import Control.Monad.Trans.Maybe (MaybeT (..))
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
import Dahdit.Mem (ReadMem (..), WriteMem (..), freezeSBSMem, readSBSMem, viewSBSMem, writeSBSMem)
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
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromJust)
import Data.Primitive.ByteArray (newByteArray)
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import Data.Word (Word8)

-- Sizes:

getStaticSeqSize :: GetStaticSeqF a -> Int
getStaticSeqSize (GetStaticSeqF ec g _) =
  let !z = fromIntegral (staticByteSize (proxyForF g))
  in  z * fromIntegral ec

getStaticArraySize :: GetStaticArrayF a -> Int
getStaticArraySize (GetStaticArrayF n prox _) =
  let !z = fromIntegral (staticByteSize prox)
  in  z * fromIntegral n

putStaticSeqSize :: PutStaticSeqF a -> Int
putStaticSeqSize (PutStaticSeqF n _ _ s _) =
  let !z = fromIntegral (staticByteSize (proxyForF s))
  in  z * fromIntegral n

putStaticArrayElemSize :: PutStaticArrayF a -> Int
putStaticArrayElemSize (PutStaticArrayF _ _ a _) =
  fromIntegral (staticByteSize (proxyForF a))

putStaticArraySize :: PutStaticArrayF a -> Int
putStaticArraySize (PutStaticArrayF n _ a _) =
  let !z = fromIntegral (staticByteSize (proxyForF a))
  in  z * fromIntegral n

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
  { geLen :: !Int
  -- ^ Remaining length of buffer segment
  , geOff :: !(STRef s Int)
  -- ^ Offset from buffer start (in bytes)
  , geArray :: !r
  -- ^ Source buffer
  }

newGetEnv :: ReadMem r => r -> ST s (GetEnv s r)
newGetEnv mem = do
  let !len = totalReadMemInBytes mem
  off <- newSTRef 0
  pure $! GetEnv len off mem

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

guardReadBytes :: String -> Int -> GetEff s r Int
guardReadBytes nm bc = do
  GetEnv l offRef _ <- ask
  off <- stGetEff (readSTRef offRef)
  let !ac = l - off
  if bc > ac
    then throwError (GetErrorParseNeed nm (fromIntegral ac) (fromIntegral bc))
    else pure off

readBytes :: String -> Int -> (r -> Int -> a) -> GetEff s r a
readBytes nm bc f = do
  off <- guardReadBytes nm bc
  GetEnv _ offRef mem <- ask
  stGetEff $ do
    let !a = f mem off
        !newOff = off + bc
    writeSTRef offRef newOff
    pure a

readScope :: ReadMem r => GetScopeF (GetEff s r a) -> GetEff s r a
readScope (GetScopeF sm bc g k) = do
  let intBc = fromIntegral bc
  GetEnv oldLen offRef _ <- ask
  oldOff <- stGetEff (readSTRef offRef)
  let !oldAvail = oldLen - oldOff
  if intBc > oldAvail
    then throwError (GetErrorParseNeed "scope" (fromIntegral oldAvail) bc)
    else do
      let !newLen = oldOff + intBc
      a <- local (\ge -> ge {geLen = newLen}) (mkGetEff g)
      case sm of
        ScopeModeWithin -> k a
        ScopeModeExact -> do
          newOff <- stGetEff (readSTRef offRef)
          let !actualBc = newOff - oldOff
          if actualBc == intBc
            then k a
            else throwError (GetErrorScopedMismatch (fromIntegral actualBc) bc)

readStaticSeq :: ReadMem r => GetStaticSeqF (GetEff s r a) -> GetEff s r a
readStaticSeq gss@(GetStaticSeqF ec g k) = do
  let !bc = getStaticSeqSize gss
  _ <- guardReadBytes "static sequence" bc
  ss <- Seq.replicateA (fromIntegral ec) (mkGetEff g)
  k ss

readStaticArray :: ReadMem r => GetStaticArrayF (GetEff s r a) -> GetEff s r a
readStaticArray gsa@(GetStaticArrayF _ _ k) = do
  let !bc = getStaticArraySize gsa
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
    let !len = fromIntegral bc
    in  readBytes "ShortByteString" len (\mem off -> readSBSMem mem off len) >>= k
  GetFStaticSeq gss -> readStaticSeq gss
  GetFStaticArray gsa -> readStaticArray gsa
  GetFByteArray bc k ->
    let !len = fromIntegral bc
    in  readBytes "ByteArray" len (\mem off -> cloneArrayMemInBytes mem off len) >>= k
  GetFScope gs -> readScope gs
  GetFSkip bc k -> readBytes "skip" (fromIntegral bc) (\_ _ -> ()) *> k
  GetFLookAhead gla -> readLookAhead gla
  GetFRemainingSize k -> do
    GetEnv len offRef _ <- ask
    off <- stGetEff (readSTRef offRef)
    let !bc = fromIntegral (len - off)
    k bc
  GetFFail msg -> fail msg

runGetRun :: ReadMem r => GetRun s r a -> GetEnv s r -> ST s (Either GetError a)
runGetRun = runGetEff . iterGetRun

iterGetRun :: ReadMem r => GetRun s r a -> GetEff s r a
iterGetRun act = iterT execGetRun (unGetRun act)

mkGetRun :: Get a -> GetRun s r a
mkGetRun (Get (F w)) = GetRun (w pure wrap)

mkGetEff :: ReadMem r => Get a -> GetEff s r a
mkGetEff = iterGetRun . mkGetRun

runGetST :: ReadMem r => Get a -> r -> (Either GetError a, ByteCount)
runGetST act mem = runST $ do
  let eff = mkGetEff act
  env <- newGetEnv mem
  ea <- runGetEff eff env
  bc <- readSTRef (geOff env)
  pure (ea, fromIntegral bc)

runGet :: Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGet act = runGetST act . viewSBSMem

runGetIO :: Get a -> ShortByteString -> IO (a, ByteCount)
runGetIO act sbs =
  let (ea, bc) = runGet act sbs
  in  case ea of
        Left e -> throwIO e
        Right a -> pure (a, bc)

runGetFile :: Get a -> FilePath -> IO (a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  let sbs = BSS.toShort bs
  runGetIO act sbs

-- Put unsafe:

data PutEnv s q = PutEnv
  { peLen :: !Int
  -- ^ Remaining capacity in buffer segment
  , peOff :: !(STRef s Int)
  -- ^ Offset in bytes from start of buffer
  , peArray :: !(q s)
  -- ^ Destination buffer
  }

newPutEnv :: WriteMem q => q s -> ST s (PutEnv s q)
newPutEnv mem = do
  let len = totalWriteMemInBytes mem
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

writeBytes :: Int -> (q s -> Int -> ST s ()) -> PutEff s q ()
writeBytes bc f = do
  PutEnv _ offRef mem <- ask
  stPutEff $ do
    off <- readSTRef offRef
    f mem off
    let !newOff = off + bc
    writeSTRef offRef newOff

writeStaticSeq :: WriteMem q => PutStaticSeqF (PutEff s q a) -> PutEff s q a
writeStaticSeq (PutStaticSeqF n mz p s k) = do
  let n' = fromIntegral n
  for_ (take n' (toList s)) $ \a -> do
    let !x = p a
    mkPutEff x
  let !e = Seq.length s
  unless (n' <= e) $ do
    let !q = mkPutEff (p (fromJust mz))
    replicateM_ (n' - e) q
  k

writeStaticArray :: WriteMem q => PutStaticArrayF (PutEff s q a) -> PutEff s q a
writeStaticArray psa@(PutStaticArrayF needElems mz a@(LiftedPrimArray ba) k) = do
  let !elemSize = putStaticArrayElemSize psa
      !haveElems = sizeofLiftedPrimArray a
      !useElems = min haveElems (fromIntegral needElems)
      !useBc = elemSize * useElems
  writeBytes useBc (copyArrayMemInBytes ba 0 useBc)
  let !needBc = putStaticArraySize psa
  unless (useBc == needBc) $ do
    let !extraBc = needBc - useBc
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
    let !len = fromIntegral bc
    in  writeBytes len (writeSBSMem sbs len) *> k
  PutFStaticSeq pss -> writeStaticSeq pss
  PutFStaticArray psa -> writeStaticArray psa
  PutFByteArray bc barr k ->
    let !len = fromIntegral bc
    in  writeBytes len (copyArrayMemInBytes barr 0 len) *> k
  PutFStaticHint (PutStaticHintF _ p k) -> mkPutEff p *> k

runPutRun :: WriteMem q => PutRun s q a -> PutEnv s q -> ST s a
runPutRun = runPutEff . iterPutRun

iterPutRun :: WriteMem q => PutRun s q a -> PutEff s q a
iterPutRun act = iterT execPutRun (unPutRun act)

mkPutRun :: PutM a -> PutRun s q a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: WriteMem q => PutM a -> PutEff s q a
mkPutEff = iterPutRun . mkPutRun

runPutUnsafe :: WriteMem q => Put -> ByteCount -> q s -> ST s (q s)
runPutUnsafe act bc mem = do
  let !len = fromIntegral bc
      !eff = mkPutRun act
  st@(PutEnv _ offRef _) <- newPutEnv mem
  runPutRun eff st
  off <- readSTRef offRef
  -- This is just a sanity check - if it goes wrong then there's a bug in the library
  unless (off == len) (error ("Invalid put length: (given " ++ show len ++ ", used " ++ show off ++ ")"))
  pure mem

-- Count:

newtype CountEff a = CountEff {unCountEff :: MaybeT (State Int) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadState Int)

runCountEff :: CountEff a -> Int -> (Maybe a, Int)
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
  PutFShortByteString bc _ k ->
    let !len = fromIntegral bc
    in  State.modify' (len +) *> k
  PutFStaticSeq pss@(PutStaticSeqF _ _ _ _ k) ->
    let !len = putStaticSeqSize pss
    in  State.modify' (len +) *> k
  PutFStaticArray psv@(PutStaticArrayF _ _ _ k) ->
    let !len = putStaticArraySize psv
    in  State.modify' (len +) *> k
  PutFByteArray bc _ k ->
    let !len = fromIntegral bc
    in  State.modify' (len +) *> k
  PutFStaticHint (PutStaticHintF bc _ k) ->
    let !len = fromIntegral bc
    in  State.modify' (len +) *> k

runCountRun :: CountRun a -> Int -> (Maybe a, Int)
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
  in  fromIntegral bc

-- Put safe:

runPutST :: WriteMem q => Put -> (forall s. ByteCount -> ST s (q s)) -> (forall s. q s -> ST s z) -> z
runPutST act mkMem useMem = do
  let bc = runCount act
  runST (mkMem bc >>= runPutUnsafe act bc >>= useMem)

runPut :: Put -> ShortByteString
runPut act = runPutST act (newByteArray . fromIntegral) freezeSBSMem

-- Put file:

runPutFile :: FilePath -> Put -> IO ()
runPutFile fp act =
  let !bs = runPut act
      !bs' = BSS.fromShort bs
  in  BS.writeFile fp bs'
