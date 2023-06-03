module Dahdit.Iface
  ( BinaryGetTarget (..)
  , getTarget
  , BinaryPutTarget (..)
  , putTarget
  , MutBinaryPutTarget (..)
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
import Control.Monad.Primitive (MonadPrim, PrimMonad (..), RealWorld)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (getRemainingSize)
import Dahdit.Mem (MemPtr (..), emptyMemPtr, mutViewVecMem, viewBSMem, viewSBSMem, viewVecMem, withBAMem, withBSMem, withSBSMem, withVecMem)
import Dahdit.Run (GetError, GetIncCb, GetIncChunk (..), newGetIncEnv, runCount, runGetIncInternal, runGetInternal, runPutInternal)
import Dahdit.Sizes (ByteCount (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, emptyByteArray, sizeofByteArray)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import Data.Word (Word8)

-- | Abstracts over the sources we can read from.
class PrimMonad m => BinaryGetTarget z m where
  -- | Get a value from the source given a starting offset, returning a result and final offset.
  -- On error, the offset will indicate where in the source the error occurred.
  getTargetOffset :: ByteCount -> Get a -> z -> m (Either GetError a, ByteCount)

  -- | Get a value incrementally from sources yielded by the given callback, returning a
  -- result, final offset in the whole stream, and final offset in the current chunk.
  -- Takes an optional maximum capacity.
  getTargetInc :: Maybe ByteCount -> Get a -> GetIncCb z m -> m (Either GetError a, ByteCount, ByteCount)

-- | Get a value from the source, returning a result and final offset.
getTarget :: BinaryGetTarget z m => Get a -> z -> m (Either GetError a, ByteCount)
getTarget = getTargetOffset 0

-- | Abstracts over the immutable sinks we can render to.
class BinaryGetTarget z m => BinaryPutTarget z m where
  -- | Put an action to the sink with the given length.
  -- Prefer 'putTarget' to safely count capacity, or use 'encode' to use byte size.
  putTargetUnsafe :: Put -> ByteCount -> m z

-- | Put an action to the sink with calculated capacity.
putTarget :: BinaryPutTarget z m => Put -> m z
putTarget p = putTargetUnsafe p (runCount p)

-- TODO require implement get
class PrimMonad m => MutBinaryPutTarget z m where
  mutPutTargetOffsetUnsafe :: ByteCount -> Put -> ByteCount -> z -> m ByteCount

mutPutTargetOffset :: MutBinaryPutTarget z m => ByteCount -> Put -> z -> m ByteCount
mutPutTargetOffset off p = mutPutTargetOffsetUnsafe off p (runCount p)

mutPutTarget :: MutBinaryPutTarget z m => Put -> z -> m ByteCount
mutPutTarget = mutPutTargetOffset 0

instance BinaryGetTarget String IO where
  getTargetOffset = runGetString
  getTargetInc = runGetIncString

instance BinaryPutTarget String IO where
  putTargetUnsafe = runPutString

instance BinaryGetTarget Text IO where
  getTargetOffset = runGetText
  getTargetInc = runGetIncText

instance BinaryPutTarget Text IO where
  putTargetUnsafe = runPutText

instance PrimMonad m => BinaryGetTarget ShortByteString m where
  getTargetOffset = runGetSBS
  getTargetInc = runGetIncSBS

instance PrimMonad m => BinaryPutTarget ShortByteString m where
  putTargetUnsafe = runPutSBS

instance BinaryGetTarget ByteString IO where
  getTargetOffset = runGetBS
  getTargetInc = runGetIncBS

instance BinaryPutTarget ByteString IO where
  putTargetUnsafe = runPutBS

instance PrimMonad m => BinaryGetTarget ByteArray m where
  getTargetOffset = runGetBA
  getTargetInc = runGetIncBA

instance PrimMonad m => BinaryPutTarget ByteArray m where
  putTargetUnsafe = runPutBA

instance BinaryGetTarget (Vector Word8) IO where
  getTargetOffset = runGetVec
  getTargetInc = runGetIncVec

instance BinaryPutTarget (Vector Word8) IO where
  putTargetUnsafe = runPutVec

instance MonadPrim s m => MutBinaryPutTarget (MutableByteArray s) m where
  mutPutTargetOffsetUnsafe = runMutPutBA

instance MutBinaryPutTarget (IOVector Word8) IO where
  mutPutTargetOffsetUnsafe = runMutPutVec

-- | Wrapper that asserts a get action has consumed to the end.
getEnd :: Get a -> Get a
getEnd getter = do
  a <- getter
  b <- getRemainingSize
  unless (b == 0) (fail ("Expected end of input but had bytes remaining: " ++ show (unByteCount b)))
  pure a

-- | Decode a value from a source returning a result and consumed byte count.
decode :: (Binary a, BinaryGetTarget z m) => z -> m (Either GetError a, ByteCount)
decode = getTarget get

-- | Decode a value incrementally from sources yielded by a callback.
decodeInc :: (Binary a, BinaryGetTarget z m) => Maybe ByteCount -> GetIncCb z m -> m (Either GetError a, ByteCount, ByteCount)
decodeInc mayCap = getTargetInc mayCap get

-- | 'decode' but expect the end of input.
decodeEnd :: (Binary a, BinaryGetTarget z m) => z -> m (Either GetError a, ByteCount)
decodeEnd = getTarget (getEnd get)

-- | Decode a value from a file.
decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

-- | 'decodeFile' but expect the end of file.
decodeFileEnd :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFileEnd = runGetFile (getEnd get)

-- | Encode a value to a sink.
encode :: (Binary a, BinaryPutTarget z m) => a -> m z
encode a = putTargetUnsafe (put a) (byteSize a)

-- | Encode a value to a file.
encodeFile :: Binary a => a -> FilePath -> IO ()
encodeFile a = runPutFile (put a) (byteSize a)

-- | Encode a value to a mutable buffer, returning number of bytes filled.
mutEncode :: (Binary a, MutBinaryPutTarget z m) => a -> z -> m ByteCount
mutEncode a = mutPutTargetOffsetUnsafe 0 (put a) (byteSize a)

runGetString :: ByteCount -> Get a -> String -> IO (Either GetError a, ByteCount)
runGetString off act = runGetBS off act . BSC.pack

runGetText :: ByteCount -> Get a -> Text -> IO (Either GetError a, ByteCount)
runGetText off act = runGetBS off act . TE.encodeUtf8

runGetBA :: PrimMonad m => ByteCount -> Get a -> ByteArray -> m (Either GetError a, ByteCount)
runGetBA off act ba = runGetInternal off act (coerce (sizeofByteArray ba)) ba

runGetSBS :: PrimMonad m => ByteCount -> Get a -> ShortByteString -> m (Either GetError a, ByteCount)
runGetSBS off act = runGetBA off act . viewSBSMem

runGetBS :: ByteCount -> Get a -> ByteString -> IO (Either GetError a, ByteCount)
runGetBS off act bs = runGetInternal off act (coerce (BS.length bs)) (viewBSMem bs)

runGetVec :: ByteCount -> Get a -> Vector Word8 -> IO (Either GetError a, ByteCount)
runGetVec off act vec = runGetInternal off act (coerce (VS.length vec)) (viewVecMem vec)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  runGetBS 0 act bs

runGetIncString :: Maybe ByteCount -> Get a -> GetIncCb String IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncString mayCap g cb = runGetIncBS mayCap g (fmap (fmap BSC.pack) . cb)

runGetIncText :: Maybe ByteCount -> Get a -> GetIncCb Text IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncText mayCap g cb = runGetIncBS mayCap g (fmap (fmap TE.encodeUtf8) . cb)

runGetIncBA :: PrimMonad m => Maybe ByteCount -> Get a -> GetIncCb ByteArray m -> m (Either GetError a, ByteCount, ByteCount)
runGetIncBA mayCap act cb = do
  env <- newGetIncEnv mayCap (GetIncChunk 0 0 emptyByteArray)
  let view s = GetIncChunk 0 (coerce (sizeofByteArray s)) s
  let cb' = fmap (fmap view) . cb
  runGetIncInternal act env cb'

runGetIncSBS :: PrimMonad m => Maybe ByteCount -> Get a -> GetIncCb ShortByteString m -> m (Either GetError a, ByteCount, ByteCount)
runGetIncSBS mayCap act cb = runGetIncBA mayCap act (fmap (fmap viewSBSMem) . cb)

runGetIncMemPtr :: Maybe ByteCount -> Get a -> GetIncCb (MemPtr RealWorld) IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncMemPtr mayCap act cb = do
  mem <- emptyMemPtr
  env <- newGetIncEnv mayCap (GetIncChunk 0 0 mem)
  let view mem'@(MemPtr _ off len) = GetIncChunk 0 (len - off) mem'
  let cb' = fmap (fmap view) . cb
  runGetIncInternal act env cb'

runGetIncBS :: Maybe ByteCount -> Get a -> GetIncCb ByteString IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncBS mayCap act cb = runGetIncMemPtr mayCap act (fmap (fmap viewBSMem) . cb)

runGetIncVec :: Maybe ByteCount -> Get a -> GetIncCb (Vector Word8) IO -> IO (Either GetError a, ByteCount, ByteCount)
runGetIncVec mayCap act cb = runGetIncMemPtr mayCap act (fmap (fmap viewVecMem) . cb)

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
