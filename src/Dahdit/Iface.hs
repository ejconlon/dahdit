module Dahdit.Iface
  ( BinaryTarget (..)
  , getTarget
  , putTarget
  , MutBinaryTarget (..)
  , mutPutTargetOffset
  , mutPutTarget
  , getEnd
  , decode
  , decodeInc
  , decodeEnd
  , decodeFile
  , decodeFileEnd
  , encode
  , encodeFile
  , mutEncode
  )
where

import Control.Monad (unless)
import Control.Monad.Primitive (MonadPrim, PrimMonad (..))
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (getRemainingSize)
import Dahdit.Mem (mutViewVecMem, viewBSMem, viewSBSMem, viewVecMem, withBAMem, withBSMem, withSBSMem, withVecMem)
import Dahdit.Run (GetError, GetIncCb, GetIncChunk (..), newGetIncEnv, runCount, runGetIncInternal, runGetInternal, runPutInternal)
import Dahdit.Sizes (ByteCount (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, emptyByteArray, sizeofByteArray)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import Data.Word (Word8)

-- | Abstracts over the sources we can read from / sinks we can render to.
class PrimMonad m => BinaryTarget z m where
  -- | Put an action to the sink with the given length.
  -- Prefer 'putTarget' to safely count capacity, or use 'encode' to use byte size.
  putTargetUnsafe :: Put -> ByteCount -> m z

  -- | Get a value from the source given a starting offset, returning a result and final offset.
  -- On error, the offset will indicate where in the source the error occurred.
  getTargetOffset :: ByteCount -> Get a -> z -> m (Either GetError a, ByteCount)

  -- | Get a value incrementally from sources yielded by the given callback, returning a
  -- result, final offset in the whole stream, and final offset in the current chunk.
  getTargetInc :: Get a -> GetIncCb z m -> m (Either GetError a, ByteCount, ByteCount)

-- | Get a value from the source, returning a result and final offset.
getTarget :: BinaryTarget z m => Get a -> z -> m (Either GetError a, ByteCount)
getTarget = getTargetOffset 0

-- | Put an action to the sink with calculated capacity.
putTarget :: BinaryTarget z m => Put -> m z
putTarget p = putTargetUnsafe p (runCount p)

class PrimMonad m => MutBinaryTarget z m where
  mutPutTargetOffsetUnsafe :: ByteCount -> Put -> ByteCount -> z -> m ByteCount

mutPutTargetOffset :: MutBinaryTarget z m => ByteCount -> Put -> z -> m ByteCount
mutPutTargetOffset off p = mutPutTargetOffsetUnsafe off p (runCount p)

mutPutTarget :: MutBinaryTarget z m => Put -> z -> m ByteCount
mutPutTarget = mutPutTargetOffset 0

instance BinaryTarget String IO where
  getTargetOffset = runGetString
  putTargetUnsafe = runPutString
  getTargetInc = runGetIncString

instance BinaryTarget Text IO where
  getTargetOffset = runGetText
  putTargetUnsafe = runPutText
  getTargetInc = runGetIncText

instance PrimMonad m => BinaryTarget ShortByteString m where
  getTargetOffset = runGetSBS
  putTargetUnsafe = runPutSBS
  getTargetInc = runGetIncSBS

instance BinaryTarget ByteString IO where
  getTargetOffset = runGetBS
  putTargetUnsafe = runPutBS
  getTargetInc = runGetIncBS

instance PrimMonad m => BinaryTarget ByteArray m where
  getTargetOffset = runGetBA
  putTargetUnsafe = runPutBA
  getTargetInc = runGetIncBA

instance BinaryTarget (Vector Word8) IO where
  getTargetOffset = runGetVec
  putTargetUnsafe = runPutVec
  getTargetInc = runGetIncVec

instance MonadPrim s m => MutBinaryTarget (MutableByteArray s) m where
  mutPutTargetOffsetUnsafe = runMutPutBA

instance MutBinaryTarget (IOVector Word8) IO where
  mutPutTargetOffsetUnsafe = runMutPutVec

-- | Wrapper that asserts a get action has consumed to the end.
getEnd :: Get a -> Get a
getEnd getter = do
  a <- getter
  b <- getRemainingSize
  unless (b == 0) (fail ("Expected end of input but had bytes remaining: " ++ show (unByteCount b)))
  pure a

-- | Decode a value from a source returning a result and consumed byte count.
decode :: (Binary a, BinaryTarget z m) => z -> m (Either GetError a, ByteCount)
decode = getTarget get

-- | Decode a value incrementally from sources yielded by a callback.
decodeInc :: (Binary a, BinaryTarget z m) => GetIncCb z m -> m (Either GetError a, ByteCount, ByteCount)
decodeInc = getTargetInc get

-- | 'decode' but expect the end of input.
decodeEnd :: (Binary a, BinaryTarget z m) => z -> m (Either GetError a, ByteCount)
decodeEnd = getTarget (getEnd get)

-- | Decode a value from a file.
decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

-- | 'decodeFile' but expect the end of file.
decodeFileEnd :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFileEnd = runGetFile (getEnd get)

-- | Encode a value to a sink.
encode :: (Binary a, BinaryTarget z m) => a -> m z
encode a = putTargetUnsafe (put a) (byteSize a)

-- | Encode a value to a file.
encodeFile :: Binary a => a -> FilePath -> IO ()
encodeFile a = runPutFile (put a) (byteSize a)

-- | Encode a value to a mutable buffer, returning number of bytes filled.
mutEncode :: (Binary a, MutBinaryTarget z m) => a -> z -> m ByteCount
mutEncode a = mutPutTargetOffsetUnsafe 0 (put a) (byteSize a)

runGetString :: ByteCount -> Get a -> String -> IO (Either GetError a, ByteCount)
runGetString off act = runGetBS off act . BSC.pack

runGetText :: ByteCount -> Get a -> Text -> IO (Either GetError a, ByteCount)
runGetText off act = runGetBS off act . TE.encodeUtf8

runGetBA :: PrimMonad m => ByteCount -> Get a -> ByteArray -> m (Either GetError a, ByteCount)
runGetBA off act ba = runGetInternal off act (coerce (sizeofByteArray ba)) ba

runGetSBS :: PrimMonad m => ByteCount -> Get a -> ShortByteString -> m (Either GetError a, ByteCount)
runGetSBS off act sbs = runGetInternal off act (coerce (BSS.length sbs)) (viewSBSMem sbs)

runGetBS :: ByteCount -> Get a -> ByteString -> IO (Either GetError a, ByteCount)
runGetBS off act bs = runGetInternal off act (coerce (BS.length bs)) (viewBSMem bs)

runGetVec :: ByteCount -> Get a -> Vector Word8 -> IO (Either GetError a, ByteCount)
runGetVec off act vec = runGetInternal off act (coerce (VS.length vec)) (viewVecMem vec)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  runGetBS 0 act bs

runGetIncString :: Get a -> GetIncCb String IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncString g cb = runGetIncBS g (fmap (fmap BSC.pack) . cb)

runGetIncText :: Get a -> GetIncCb Text IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncText g cb = runGetIncBS g (fmap (fmap TE.encodeUtf8) . cb)

runGetIncBA :: PrimMonad m => Get a -> GetIncCb ByteArray m -> m (Either GetError a, ByteCount, ByteCount)
runGetIncBA = error "TODO"

runGetIncSBS :: PrimMonad m => Get a -> GetIncCb ShortByteString m -> m (Either GetError a, ByteCount, ByteCount)
runGetIncSBS act cb = do
  env <- newGetIncEnv Nothing (GetIncChunk 0 0 emptyByteArray)
  let view s = GetIncChunk 0 (coerce (BSS.length s)) (viewSBSMem s)
  let cb' = fmap (fmap view) . cb
  runGetIncInternal act env cb'

runGetIncBS :: Get a -> GetIncCb ByteString IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncBS = error "TODO"

runGetIncVec :: Get a -> GetIncCb (Vector Word8) IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncVec = error "TODO"

runPutString :: Put -> ByteCount -> IO String
runPutString act len = fmap BSC.unpack (runPutBS act len)

runPutText :: Put -> ByteCount -> IO Text
runPutText act len = fmap TE.decodeUtf8 (runPutBS act len)

runPutBA :: PrimMonad m => Put -> ByteCount -> m ByteArray
runPutBA act len = withBAMem len (runPutInternal 0 act len)

runPutSBS :: PrimMonad m => Put -> ByteCount -> m ShortByteString
runPutSBS act len = withSBSMem len (runPutInternal 0 act len)

runPutBS :: Put -> ByteCount -> IO ByteString
runPutBS act len = withBSMem len (runPutInternal 0 act len)

runPutVec :: Put -> ByteCount -> IO (Vector Word8)
runPutVec act len = withVecMem len (runPutInternal 0 act len)

runPutFile :: Put -> ByteCount -> FilePath -> IO ()
runPutFile act cap fp = do
  bs <- runPutBS act cap
  BS.writeFile fp bs

runMutPutBA :: MonadPrim s m => ByteCount -> Put -> ByteCount -> MutableByteArray s -> m ByteCount
runMutPutBA = runPutInternal

runMutPutVec :: ByteCount -> Put -> ByteCount -> IOVector Word8 -> IO ByteCount
runMutPutVec off act len mvec = runPutInternal off act len (mutViewVecMem mvec)
