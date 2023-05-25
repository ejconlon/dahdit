module Dahdit.Iface
  ( BinaryTarget (..)
  , getTarget
  , putTarget
  , MutBinaryTarget (..)
  , mutPutTargetOffset
  , mutPutTarget
  , decode
  , decodeEnd
  , decodeFile
  , decodeFileEnd
  , encode
  , encodeFile
  , mutEncode
  )
where

import Control.Monad (unless)
import Control.Monad.Primitive (PrimBase, PrimMonad (..))
import Control.Monad.ST (runST)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (getRemainingSize)
import Dahdit.Mem (allocBAMem, allocPtrMem, freezeBAMem, freezeBSMem, freezeSBSMem, freezeVecMem, mutAllocBAMem, mutAllocVecMem, mutFreezeBAMem, mutFreezeVecMem, viewBSMem, viewSBSMem, viewVecMem)
import Dahdit.Run (GetError, runCount, runGetInternal, runPutInternal)
import Dahdit.Sizes (ByteCount (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, sizeofByteArray)
import Data.Text (Text)
import Data.Text.Encoding as TE
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (MVector)
import Data.Word (Word8)

-- | Abstracts over the sources we can read from / sinks we can render to.
class BinaryTarget z where
  -- | Put an action to the sink with the given length.
  -- Prefer 'putTarget' to safely count capacity, or use 'encode' to use byte size.
  putTargetUnsafe :: Put -> ByteCount -> z

  -- | Get a value from the source given a starting offset, returning a result and final offset.
  -- On error, the offset will indicate where in the source the error occurred.
  getTargetOffset :: ByteCount -> Get a -> z -> (Either GetError a, ByteCount)

-- | Get a value from the source, returning a result and final offset.
getTarget :: BinaryTarget z => Get a -> z -> (Either GetError a, ByteCount)
getTarget = getTargetOffset 0

-- | Put an action to the sink with calculated capacity.
putTarget :: BinaryTarget z => Put -> z
putTarget p = putTargetUnsafe p (runCount p)

class MutBinaryTarget m z where
  mutPutTargetOffsetUnsafe :: ByteCount -> Put -> ByteCount -> z -> m ByteCount

mutPutTargetOffset :: MutBinaryTarget m z => ByteCount -> Put -> z -> m ByteCount
mutPutTargetOffset off p = mutPutTargetOffsetUnsafe off p (runCount p)

mutPutTarget :: MutBinaryTarget m z => Put -> z -> m ByteCount
mutPutTarget = mutPutTargetOffset 0

instance BinaryTarget String where
  getTargetOffset = runGetString
  putTargetUnsafe = runPutString

instance BinaryTarget Text where
  getTargetOffset = runGetText
  putTargetUnsafe = runPutText

instance BinaryTarget ShortByteString where
  getTargetOffset = runGetSBS
  putTargetUnsafe = runPutSBS

instance BinaryTarget ByteString where
  getTargetOffset = runGetBS
  putTargetUnsafe = runPutBS

instance BinaryTarget ByteArray where
  getTargetOffset = runGetBA
  putTargetUnsafe = runPutBA

instance BinaryTarget (Vector Word8) where
  getTargetOffset = runGetVec
  putTargetUnsafe = runPutVec

instance (PrimBase m, s ~ PrimState m) => MutBinaryTarget m (MutableByteArray s) where
  mutPutTargetOffsetUnsafe = runMutPutBA

instance (PrimBase m, s ~ PrimState m) => MutBinaryTarget m (MVector s Word8) where
  mutPutTargetOffsetUnsafe = runMutPutVec

getEnd :: Get a -> Get a
getEnd getter = do
  a <- getter
  b <- getRemainingSize
  unless (b == 0) (fail ("Expected end of input but had bytes remaining: " ++ show (unByteCount b)))
  pure a

-- | Decode a value from a source returning a result and consumed byte count.
decode :: (Binary a, BinaryTarget z) => z -> (Either GetError a, ByteCount)
decode = getTarget get

-- | 'decode' but expect the end of input.
decodeEnd :: (BinaryTarget z, Binary a) => z -> (Either GetError a, ByteCount)
decodeEnd = getTarget (getEnd get)

-- | Decode a value from a file.
decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

-- | 'decodeFile' but expect the end of file.
decodeFileEnd :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFileEnd = runGetFile (getEnd get)

-- | Encode a value to a sink.
encode :: (Binary a, BinaryTarget z) => a -> z
encode a = putTargetUnsafe (put a) (byteSize a)

-- | Encode a value to a file.
encodeFile :: (Binary a) => a -> FilePath -> IO ()
encodeFile a = runPutFile (put a) (byteSize a)

-- | Encode a value to a mutable buffer, returning number of bytes filled.
mutEncode :: (Binary a, MutBinaryTarget m z) => a -> z -> m ByteCount
mutEncode a = mutPutTargetOffsetUnsafe 0 (put a) (byteSize a)

runGetString :: ByteCount -> Get a -> String -> (Either GetError a, ByteCount)
runGetString off act = runGetBS off act . BSC.pack

runGetText :: ByteCount -> Get a -> Text -> (Either GetError a, ByteCount)
runGetText off act = runGetBS off act . TE.encodeUtf8

runGetBA :: ByteCount -> Get a -> ByteArray -> (Either GetError a, ByteCount)
runGetBA off act ba = runGetInternal off act (coerce (sizeofByteArray ba)) ba

runGetSBS :: ByteCount -> Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGetSBS off act sbs = runGetInternal off act (coerce (BSS.length sbs)) (viewSBSMem sbs)

runGetBS :: ByteCount -> Get a -> ByteString -> (Either GetError a, ByteCount)
runGetBS off act bs = runGetInternal off act (coerce (BS.length bs)) (viewBSMem bs)

runGetVec :: ByteCount -> Get a -> Vector Word8 -> (Either GetError a, ByteCount)
runGetVec off act vec = runGetInternal off act (coerce (VS.length vec)) (viewVecMem vec)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  pure (runGetBS 0 act bs)

runPutString :: Put -> ByteCount -> String
runPutString act len = BSC.unpack (runPutBS act len)

runPutText :: Put -> ByteCount -> Text
runPutText act len = TE.decodeUtf8 (runPutBS act len)

runPutBA :: Put -> ByteCount -> ByteArray
runPutBA act len = runST (runPutInternal 0 act len allocBAMem freezeBAMem)

runPutSBS :: Put -> ByteCount -> ShortByteString
runPutSBS act len = runST (runPutInternal 0 act len allocBAMem freezeSBSMem)

runPutBS :: Put -> ByteCount -> ByteString
runPutBS act len = runST (runPutInternal 0 act len allocPtrMem freezeBSMem)

runPutVec :: Put -> ByteCount -> Vector Word8
runPutVec act len = runST (runPutInternal 0 act len allocPtrMem freezeVecMem)

runPutFile :: Put -> ByteCount -> FilePath -> IO ()
runPutFile act cap fp =
  let bs = runPutBS act cap
  in  BS.writeFile fp bs

runMutPutBA :: PrimBase m => ByteCount -> Put -> ByteCount -> MutableByteArray (PrimState m) -> m ByteCount
runMutPutBA off act len marr = runPutInternal off act len (mutAllocBAMem marr) mutFreezeBAMem

runMutPutVec :: PrimBase m => ByteCount -> Put -> ByteCount -> MVector (PrimState m) Word8 -> m ByteCount
runMutPutVec off act len mvec = runPutInternal off act len (mutAllocVecMem mvec) mutFreezeVecMem
