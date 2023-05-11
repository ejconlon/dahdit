module Dahdit.Iface
  ( BinaryTarget (..)
  , getTarget
  , putTarget
  , decode
  , decodeFile
  , encode
  , encodeFile
  )
where

import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Mem (allocArrayMem, allocPtrMem, freezeBSMem, freezeSBSMem, freezeVecMem, viewBSMem, viewSBSMem, viewVecMem)
import Dahdit.Run (GetError, runCount, runGetInternal, runPutInternal)
import Dahdit.Sizes (ByteCount (..), ByteSized (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

-- | Abstracts over the sources we can read from / sinks we can render to.
class BinaryTarget z where
  -- | Put an action to the sink with the given capacity.
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

instance BinaryTarget ShortByteString where
  getTargetOffset = runGetSBS
  putTargetUnsafe = runPutSBS

instance BinaryTarget ByteString where
  getTargetOffset = runGetBS
  putTargetUnsafe = runPutBS

instance BinaryTarget (Vector Word8) where
  getTargetOffset = runGetVec
  putTargetUnsafe = runPutVec

-- | Decode a value from a source returning a result and consumed byte count.
decode :: (Binary a, BinaryTarget z) => z -> (Either GetError a, ByteCount)
decode = getTarget get

-- | Decode a value from a file.
decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

-- | Encode a value to a sink.
encode :: (Binary a, ByteSized a, BinaryTarget z) => a -> z
encode a = putTargetUnsafe (put a) (byteSize a)

-- | Encode a value to a file.
encodeFile :: (Binary a, ByteSized a) => a -> FilePath -> IO ()
encodeFile a = runPutFile (put a) (byteSize a)

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

runPutSBS :: Put -> ByteCount -> ShortByteString
runPutSBS act cap = runPutInternal act cap allocArrayMem freezeSBSMem

runPutBS :: Put -> ByteCount -> ByteString
runPutBS act cap = runPutInternal act cap allocPtrMem freezeBSMem

runPutVec :: Put -> ByteCount -> Vector Word8
runPutVec act cap = runPutInternal act cap allocPtrMem freezeVecMem

runPutFile :: Put -> ByteCount -> FilePath -> IO ()
runPutFile act cap fp =
  let bs = runPutBS act cap
  in  BS.writeFile fp bs
