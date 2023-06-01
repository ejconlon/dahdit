{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Control.Monad.Free.Church (F (..), iterM)
import Control.Monad.Free.Class (MonadFree (..))
import Control.Monad.Primitive (MonadPrim, PrimBase, PrimMonad (..), unsafeIOToPrim, unsafePrimToIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.ST.Strict (runST)
import Control.Monad.State.Strict (MonadState, State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (FT (..), iterT)
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
import Dahdit.LiftedPrimArray (LiftedPrimArray (..), sizeofLiftedPrimArray)
import Dahdit.Mem (ReadMem (..), WriteMem (..), readSBSMem, writeSBSMem)
import Dahdit.Nums
  ( DoubleBE
  , DoubleLE
  , FloatBE
  , FloatLE
  , Int16BE
  , Int16LE (..)
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , Word16BE
  , Word16LE (..)
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  , Word64BE
  , Word64LE
  )
import Dahdit.Proxy (proxyForF)
import Dahdit.Sizes (ByteCount (..), ElemCount (..), staticByteSize)
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromJust)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
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
  = GetErrorLocalCap !Text !ByteCount !ByteCount
  | GetErrorScopedMismatch !ScopeMode !ByteCount !ByteCount
  | GetErrorFail !Text
  | GetErrorGlobalCap !Text !ByteCount !ByteCount
  | GetErrorRemaining !ByteCount
  deriving stock (Eq, Show)

instance Exception GetError where
  displayException = T.unpack . prettyGetError

-- TODO better error message for scoped mismatch
prettyGetError :: GetError -> Text
prettyGetError = \case
  GetErrorLocalCap nm ac bc -> "End of chunk parsing " <> nm <> " (have " <> T.pack (show (unByteCount ac)) <> " bytes, need " <> T.pack (show (unByteCount bc)) <> ")"
  GetErrorScopedMismatch _ ac bc -> "Did not parse enough scoped input (read " <> T.pack (show (unByteCount ac)) <> " bytes, expected " <> T.pack (show (unByteCount bc)) <> ")"
  GetErrorFail msg -> "User error: " <> msg
  GetErrorGlobalCap nm ac bc -> "Hit limit parsing " <> nm <> " (allowed " <> T.pack (show (unByteCount ac)) <> " bytes, need " <> T.pack (show (unByteCount bc)) <> ")"
  GetErrorRemaining ac -> "Cannot read remaining length in stream context (read " <> T.pack (show (unByteCount ac)) <> ")"

-- | Get from a single buffer
runGetInternal :: ReadMem r => ByteCount -> Get a -> ByteCount -> r -> (Either GetError a, ByteCount)
runGetInternal off act cap mem = runST $ do
  let chunk = GetIncChunk off cap mem
  env <- newGetIncEnv (Just (cap - off)) chunk
  (ea, _, off') <- runGetIncInternal act env (const (pure Nothing))
  pure (ea, off')

-- Get inc

data GetIncChunk r = GetIncChunk
  { gicLocalOff :: !ByteCount
  -- ^ Offset from start of local buffer to data
  , gicLocalCap :: !ByteCount
  -- ^ Capacity of local buffer
  , gicArray :: !r
  -- ^ Source buffer
  }

data GetIncEnv s r = GetIncEnv
  { gieGlobalAbs :: !(MutVar s ByteCount)
  -- ^ Offset from start of parsing (0) to current position
  , gieGlobalRel :: !(MutVar s ByteCount)
  -- ^ Offset from start of parsing (0) to start of buffer
  -- It will always be the case that gloRel <= gloAbs
  , gieGlobalCap :: !(Maybe ByteCount)
  -- ^ Maximum capacity across all chunks
  , gieCurChunk :: !(MutVar s (GetIncChunk r))
  -- ^ Current chunk
  , gieLookAhead :: !(Seq ByteCount)
  -- ^ Stack of look ahead points (in absolute position)
  -- Top of stack is end of sequence
  }

newGetIncEnv :: MonadPrim s m => Maybe ByteCount -> GetIncChunk r -> m (GetIncEnv s r)
newGetIncEnv mayCap chunk = do
  gloAbsVar <- newMutVar 0
  gloRelVar <- newMutVar 0
  chunkVar <- newMutVar chunk
  pure (GetIncEnv gloAbsVar gloRelVar mayCap chunkVar Empty)

data GetIncState r = GetIncState
  { gisBaseOff :: !ByteCount
  , gisNeedLength :: !ByteCount
  , gisCurChunk :: !(GetIncChunk r)
  }

-- | See 'GetIncCb' - this is the functor-ized version so we can suspend execution.
data GetIncDemand s r x = GetIncDemand !(GetIncState r) !(Maybe (GetIncChunk r) -> x)
  deriving stock (Functor)

newtype GetIncM s r m a = GetIncM {unGetIncM :: ReaderT (GetIncEnv s r) (ExceptT GetError (FT (GetIncDemand s r) m)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (GetIncEnv s r)
    , MonadError GetError
    , MonadFree (GetIncDemand s r)
    )

instance MonadTrans (GetIncM s r) where
  lift = GetIncM . lift . lift . lift

-- | Return new chunk containing enough data (or nothing).
type GetIncCb r m = GetIncState r -> m (Maybe (GetIncChunk r))

runGetIncM :: MonadPrim s m => GetIncM s r m a -> GetIncEnv s r -> GetIncCb r m -> m (Either GetError a)
runGetIncM m env cb =
  runFT
    (runExceptT (runReaderT (unGetIncM m) env))
    pure
    (\k2 (GetIncDemand st k1) -> cb st >>= k2 . k1)

guardReadBytes :: MonadPrim s m => Text -> ByteCount -> GetIncM s r m (ByteCount, GetIncChunk r, ByteCount)
guardReadBytes nm bc = do
  GetIncEnv gloAbsRef gloRelRef mayGloCap chunkRef looks <- ask
  -- First check if we're in cap
  gloAbsStart <- lift (readMutVar gloAbsRef)
  let gloAbsEnd = gloAbsStart + bc
  case mayGloCap of
    Just gloCap
      | gloAbsEnd > gloCap ->
          throwError (GetErrorGlobalCap nm gloCap gloAbsEnd)
    _ -> pure ()
  -- Now check that we have enough in the local buf
  gloRel <- lift (readMutVar gloRelRef)
  oldChunk@(GetIncChunk oldOff oldCap _) <- lift (readMutVar chunkRef)
  let gloBaseStart = case looks of Empty -> gloAbsStart; x :<| _ -> x
      gloBaseOffStart = gloBaseStart - gloRel + oldOff
      gloBaseOffEnd = gloAbsStart - gloRel + oldOff + bc
  (newChunk, newLocOffStart) <-
    if gloBaseOffEnd <= oldCap
      then pure (oldChunk, gloBaseOffStart)
      else do
        -- TODO actually get more
        throwError (GetErrorLocalCap nm oldCap gloBaseOffEnd)
  pure (gloAbsEnd, newChunk, newLocOffStart)

-- let needLength = gloAbsEnd -
-- oldLocOffStart <- lift (readMutVar (gicLocalOff oldChunk))
-- let oldLocCap = gicLocalCap oldChunk
--     oldLocOffEnd = oldLocOffStart + bc
-- -- If we do, use the current buf, otherwise refresh the buffer
-- (newChunk, newLocOffStart, newLocOffEnd) <-
--   if oldLocOffEnd <= oldLocCap
--     then pure (oldChunk, oldLocOffStart, oldLocOffEnd)
--     else wrap $ GetIncDemand bc $ \case
--       Nothing -> throwError (GetErrorLocalCap nm oldLocCap oldLocOffEnd)
--       Just newChunk -> do
--         lift (writeMutVar chunkRef newChunk)
--         -- Check that the new one has enough
--         newLocOffStart <- lift (readMutVar (gicLocalOff newChunk))
--         let newLocCap = gicLocalCap newChunk
--             newLocOffEnd = newLocOffStart + bc
--         unless
--           (newLocOffEnd <= newLocCap)
--           (throwError (GetErrorLocalCap nm newLocCap newLocOffEnd))
--         pure (newChunk, newLocOffStart, newLocOffEnd)

-- Memory read function takes local start offset
readBytes :: MonadPrim s m => Text -> ByteCount -> (r -> ByteCount -> a) -> GetIncM s r m a
readBytes nm bc f = do
  (gloAbsEnd, newChunk, newLocOffStart) <- guardReadBytes nm bc
  let mem = gicArray newChunk
      a = f mem newLocOffStart
  gloAbsRef <- asks gieGlobalAbs
  lift (writeMutVar gloAbsRef gloAbsEnd)
  pure a

readScope :: (MonadPrim s m, ReadMem r) => GetScopeF (GetIncM s r m a) -> GetIncM s r m a
readScope (GetScopeF sm bc g k) = do
  GetIncEnv gloAbsRef _ mayGloCap _ _ <- ask
  gloAbsStart <- lift (readMutVar gloAbsRef)
  let gloAbsMax = gloAbsStart + bc
  case mayGloCap of
    Just gloCap
      | sm == ScopeModeExact && gloAbsMax > gloCap ->
          throwError (GetErrorGlobalCap "scope" gloCap gloAbsMax)
    _ -> pure ()
  a <- case mayGloCap of
    Nothing -> interpGetInc g
    Just gloCap -> local (\env -> env {gieGlobalCap = Just (gloCap + bc)}) (interpGetInc g)
  gloAbsEnd <- lift (readMutVar gloAbsRef)
  let actualBc = gloAbsEnd - gloAbsStart
  if (sm == ScopeModeExact && actualBc == bc) || (sm == ScopeModeWithin && actualBc <= bc)
    then k a
    else throwError (GetErrorScopedMismatch sm actualBc bc)

readStaticSeq :: (MonadPrim s m, ReadMem r) => GetStaticSeqF (GetIncM s r m a) -> GetIncM s r m a
readStaticSeq gss@(GetStaticSeqF ec g k) = do
  let bc = getStaticSeqSize gss
  _ <- guardReadBytes "static sequence" bc
  ss <- Seq.replicateA (coerce ec) (interpGetInc g)
  k ss

readStaticArray :: (MonadPrim s m, ReadMem r) => GetStaticArrayF (GetIncM s r m a) -> GetIncM s r m a
readStaticArray gsa@(GetStaticArrayF _ _ k) = do
  let bc = getStaticArraySize gsa
  sa <- readBytes "static vector" bc (\mem off -> cloneArrayMemInBytes mem off bc)
  k (LiftedPrimArray sa)

readLookAhead :: (MonadPrim s m, ReadMem r) => GetLookAheadF (GetIncM s r m a) -> GetIncM s r m a
readLookAhead (GetLookAheadF g k) = do
  error "TODO"

-- offRef <- asks geOff
-- startOff <- stGetEff (readSTRef offRef)
-- a <- mkGetEff g
-- stGetEff (writeSTRef offRef startOff)
-- k a

interpGetInc :: (MonadPrim s m, ReadMem r) => Get a -> GetIncM s r m a
interpGetInc (Get g) = flip iterM g $ \case
  GetFWord8 k -> readBytes "Word8" 1 (indexMemInBytes @_ @Word8) >>= k
  GetFInt8 k -> readBytes "Int8" 1 (indexMemInBytes @_ @Int8) >>= k
  GetFWord16LE k -> readBytes "Word16LE" 2 (indexMemInBytes @_ @Word16LE) >>= k
  GetFInt16LE k -> readBytes "Int16LE" 2 (indexMemInBytes @_ @Int16LE) >>= k
  GetFWord24LE k -> readBytes "Word24LE" 3 (indexMemInBytes @_ @Word24LE) >>= k
  GetFInt24LE k -> readBytes "Int24LE" 3 (indexMemInBytes @_ @Int24LE) >>= k
  GetFWord32LE k -> readBytes "Word32LE" 4 (indexMemInBytes @_ @Word32LE) >>= k
  GetFInt32LE k -> readBytes "Int32LE" 4 (indexMemInBytes @_ @Int32LE) >>= k
  GetFWord64LE k -> readBytes "Word64LE" 8 (indexMemInBytes @_ @Word64LE) >>= k
  GetFInt64LE k -> readBytes "Int64LE" 8 (indexMemInBytes @_ @Int64LE) >>= k
  GetFFloatLE k -> readBytes "FloatLE" 4 (indexMemInBytes @_ @FloatLE) >>= k
  GetFDoubleLE k -> readBytes "DoubleLE" 8 (indexMemInBytes @_ @DoubleLE) >>= k
  GetFWord16BE k -> readBytes "Word16BE" 2 (indexMemInBytes @_ @Word16BE) >>= k
  GetFInt16BE k -> readBytes "Int16BE" 2 (indexMemInBytes @_ @Int16BE) >>= k
  GetFWord24BE k -> readBytes "Word24BE" 3 (indexMemInBytes @_ @Word24BE) >>= k
  GetFInt24BE k -> readBytes "Int24BE" 3 (indexMemInBytes @_ @Int24BE) >>= k
  GetFWord32BE k -> readBytes "Word32BE" 4 (indexMemInBytes @_ @Word32BE) >>= k
  GetFInt32BE k -> readBytes "Int32BE" 4 (indexMemInBytes @_ @Int32BE) >>= k
  GetFWord64BE k -> readBytes "Word64BE" 8 (indexMemInBytes @_ @Word64BE) >>= k
  GetFInt64BE k -> readBytes "Int64BE" 8 (indexMemInBytes @_ @Int64BE) >>= k
  GetFFloatBE k -> readBytes "FloatBE" 4 (indexMemInBytes @_ @FloatBE) >>= k
  GetFDoubleBE k -> readBytes "DoubleBE" 8 (indexMemInBytes @_ @DoubleBE) >>= k
  GetFShortByteString bc k ->
    readBytes "ShortByteString" bc (\mem off -> readSBSMem mem off bc) >>= k
  GetFStaticSeq gss -> readStaticSeq gss
  GetFStaticArray gsa -> readStaticArray gsa
  GetFByteArray bc k ->
    readBytes "ByteArray" bc (\mem off -> cloneArrayMemInBytes mem off bc) >>= k
  GetFScope gs -> readScope gs
  GetFSkip bc k -> readBytes "skip" bc (\_ _ -> ()) >> k
  GetFLookAhead gla -> readLookAhead gla
  GetFRemainingSize k -> do
    GetIncEnv gloAbsRef _ mayGloCap _ _ <- ask
    gloAbs <- lift (readMutVar gloAbsRef)
    case mayGloCap of
      Nothing -> throwError (GetErrorRemaining gloAbs)
      Just gloCap -> k (gloCap - gloAbs)
  GetFFail msg -> throwError (GetErrorFail msg)

runGetIncInternal :: (ReadMem r, s ~ PrimState m, PrimMonad m) => Get a -> GetIncEnv s r -> GetIncCb r m -> m (Either GetError a, ByteCount, ByteCount)
runGetIncInternal getter env cb = do
  let m = interpGetInc getter
  res <- runGetIncM m env cb
  gloAbs <- readMutVar (gieGlobalAbs env)
  gloRel <- readMutVar (gieGlobalRel env)
  curChunk <- readMutVar (gieCurChunk env)
  let baseOff = gloAbs - gloRel + gicLocalOff curChunk
  pure (res, gloAbs, baseOff)

-- Put unsafe:

data PutEnv s q = PutEnv
  { peOff :: !(MutVar s ByteCount)
  -- ^ Offset in bytes from start of buffer
  , peCap :: !ByteCount
  -- ^ Capacity of buffer segment
  , peArray :: !(q s)
  -- ^ Destination buffer
  }

newPutEnv :: PrimMonad m => ByteCount -> ByteCount -> q (PrimState m) -> m (PutEnv (PrimState m) q)
newPutEnv off cap mem = do
  offRef <- newMutVar off
  pure (PutEnv offRef cap mem)

newtype PutEff q m a = PutEff {unPutEff :: ReaderT (PutEnv (PrimState m) q) m a}
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance (Monad m, s ~ PrimState m) => MonadReader (PutEnv s q) (PutEff q m)

runPutEff :: PutEff q m a -> PutEnv (PrimState m) q -> m a
runPutEff act = runReaderT (unPutEff act)

stPutEff :: Monad m => m a -> PutEff q m a
stPutEff = PutEff . lift

newtype PutRun q m a = PutRun {unPutRun :: FT PutF (PutEff q m) a}
  deriving newtype (Functor, Applicative, Monad)

writeBytes :: PrimMonad m => ByteCount -> (q (PrimState m) -> ByteCount -> m ()) -> PutEff q m ()
writeBytes bc f = do
  PutEnv offRef _ mem <- ask
  stPutEff $ do
    off <- readMutVar offRef
    f mem off
    let newOff = off + bc
    writeMutVar offRef newOff

writeStaticSeq :: WriteMem q m => PutStaticSeqF (PutEff q m a) -> PutEff q m a
writeStaticSeq (PutStaticSeqF n mz p s k) = do
  for_ (take (coerce n) (toList s)) $ \a -> do
    mkPutEff (p a)
  let e = Seq.length s
  unless (coerce n <= e) $ do
    let q = mkPutEff (p (fromJust mz))
    replicateM_ (coerce n - e) q
  k

writeStaticArray :: WriteMem q m => PutStaticArrayF (PutEff q m a) -> PutEff q m a
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

execPutRun :: WriteMem q m => PutF (PutEff q m a) -> PutEff q m a
execPutRun = \case
  PutFWord8 x k -> writeBytes 1 (writeMemInBytes x) >> k
  PutFInt8 x k -> writeBytes 1 (writeMemInBytes x) >> k
  PutFWord16LE x k -> writeBytes 2 (writeMemInBytes x) >> k
  PutFInt16LE x k -> writeBytes 2 (writeMemInBytes x) >> k
  PutFWord24LE x k -> writeBytes 3 (writeMemInBytes x) >> k
  PutFInt24LE x k -> writeBytes 3 (writeMemInBytes x) >> k
  PutFWord32LE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFInt32LE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFWord64LE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFInt64LE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFFloatLE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFDoubleLE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFWord16BE x k -> writeBytes 2 (writeMemInBytes x) >> k
  PutFInt16BE x k -> writeBytes 2 (writeMemInBytes x) >> k
  PutFWord24BE x k -> writeBytes 3 (writeMemInBytes x) >> k
  PutFInt24BE x k -> writeBytes 3 (writeMemInBytes x) >> k
  PutFWord32BE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFInt32BE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFWord64BE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFInt64BE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFFloatBE x k -> writeBytes 4 (writeMemInBytes x) >> k
  PutFDoubleBE x k -> writeBytes 8 (writeMemInBytes x) >> k
  PutFShortByteString bc sbs k ->
    writeBytes bc (writeSBSMem sbs bc) >> k
  PutFStaticSeq pss -> writeStaticSeq pss
  PutFStaticArray psa -> writeStaticArray psa
  PutFByteArray bc barr k ->
    writeBytes bc (copyArrayMemInBytes barr 0 bc) >> k
  PutFStaticHint (PutStaticHintF _ p k) -> mkPutEff p >> k

runPutRun :: WriteMem q m => PutRun q m a -> PutEnv (PrimState m) q -> m a
runPutRun = runPutEff . iterPutRun

iterPutRun :: WriteMem q m => PutRun q m a -> PutEff q m a
iterPutRun act = iterT execPutRun (unPutRun act)

mkPutRun :: PutM a -> PutRun q m a
mkPutRun (PutM (F w)) = PutRun (w pure wrap)

mkPutEff :: WriteMem q m => PutM a -> PutEff q m a
mkPutEff = iterPutRun . mkPutRun

runPutUnsafe :: WriteMem q m => ByteCount -> Put -> ByteCount -> q (PrimState m) -> m ByteCount
runPutUnsafe off act len mem = do
  let eff = mkPutRun act
      cap = off + len
  st@(PutEnv offRef _ _) <- newPutEnv off cap mem
  runPutRun eff st
  readMutVar offRef

-- Count:

newtype CountEff a = CountEff {unCountEff :: MaybeT (State ByteCount) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadState ByteCount)

runCountEff :: CountEff a -> ByteCount -> (Maybe a, ByteCount)
runCountEff act = runState (runMaybeT (unCountEff act))

newtype CountRun a = CountRun {unCountRun :: FT PutF CountEff a}
  deriving newtype (Functor, Applicative, Monad)

execCountRun :: PutF (CountEff a) -> CountEff a
execCountRun = \case
  PutFWord8 _ k -> State.modify' (1 +) >> k
  PutFInt8 _ k -> State.modify' (1 +) >> k
  PutFWord16LE _ k -> State.modify' (2 +) >> k
  PutFInt16LE _ k -> State.modify' (2 +) >> k
  PutFWord24LE _ k -> State.modify' (3 +) >> k
  PutFInt24LE _ k -> State.modify' (3 +) >> k
  PutFWord32LE _ k -> State.modify' (4 +) >> k
  PutFInt32LE _ k -> State.modify' (4 +) >> k
  PutFWord64LE _ k -> State.modify' (8 +) >> k
  PutFInt64LE _ k -> State.modify' (8 +) >> k
  PutFFloatLE _ k -> State.modify' (4 +) >> k
  PutFDoubleLE _ k -> State.modify' (8 +) >> k
  PutFWord16BE _ k -> State.modify' (2 +) >> k
  PutFInt16BE _ k -> State.modify' (2 +) >> k
  PutFWord24BE _ k -> State.modify' (3 +) >> k
  PutFInt24BE _ k -> State.modify' (3 +) >> k
  PutFWord32BE _ k -> State.modify' (4 +) >> k
  PutFInt32BE _ k -> State.modify' (4 +) >> k
  PutFWord64BE _ k -> State.modify' (8 +) >> k
  PutFInt64BE _ k -> State.modify' (8 +) >> k
  PutFFloatBE _ k -> State.modify' (4 +) >> k
  PutFDoubleBE _ k -> State.modify' (8 +) >> k
  PutFShortByteString bc _ k -> State.modify' (bc +) >> k
  PutFStaticSeq pss@(PutStaticSeqF _ _ _ _ k) ->
    let bc = putStaticSeqSize pss
    in  State.modify' (bc +) >> k
  PutFStaticArray psv@(PutStaticArrayF _ _ _ k) ->
    let bc = putStaticArraySize psv
    in  State.modify' (bc +) >> k
  PutFByteArray bc _ k -> State.modify' (bc +) >> k
  PutFStaticHint (PutStaticHintF bc _ k) -> State.modify' (bc +) >> k

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

primOnExc :: PrimBase m => m a -> Maybe (IO ()) -> m a
primOnExc prim = maybe prim $ \onExc ->
  unsafeIOToPrim (onException (unsafePrimToIO prim) onExc)

runPutInternal :: (PrimBase m, WriteMem q m) => ByteCount -> Put -> ByteCount -> (ByteCount -> ByteCount -> m (q (PrimState m), Maybe (IO ()))) -> (q (PrimState m) -> ByteCount -> ByteCount -> m z) -> m z
runPutInternal off act len mkMem useMem = do
  (mem, rel) <- mkMem off len
  primOnExc (runPutUnsafe off act len mem >>= useMem mem off) rel
