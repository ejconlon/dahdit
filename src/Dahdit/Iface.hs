module Dahdit.Iface
  ( BinaryTarget (..)
  , decode
  , decodeFile
  , encode
  , encodeFile
  , runGetSBS
  , runGetBS
  , runGetVec
  , runGetFile
  , runPutSBS
  , runPutBS
  , runPutVec
  , runPutFile
  )
where

import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Mem (allocArrayMem, allocPtrMem, freezeBSMem, freezeSBSMem, freezeVecMem, viewBSMem, viewSBSMem, viewVecMem)
import Dahdit.Run (GetError, runGetInternal, runPutInternal)
import Dahdit.Sizes (ByteCount (..), ByteSized (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

-- TODO support get/put with offset
class BinaryTarget z where
  getTarget :: Get a -> z -> (Either GetError a, ByteCount)
  putTarget :: Put -> ByteCount -> z

instance BinaryTarget ShortByteString where
  getTarget = runGetSBS
  putTarget = runPutSBS

instance BinaryTarget ByteString where
  getTarget = runGetBS
  putTarget = runPutBS

instance BinaryTarget (Vector Word8) where
  getTarget = runGetVec
  putTarget = runPutVec

decode :: (Binary a, BinaryTarget z) => z -> (Either GetError a, ByteCount)
decode = getTarget get

decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

encode :: (Binary a, ByteSized a, BinaryTarget z) => a -> z
encode a = putTarget (put a) (byteSize a)

encodeFile :: (Binary a, ByteSized a) => a -> FilePath -> IO ()
encodeFile a = runPutFile (put a) (byteSize a)

runGetSBS :: Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGetSBS act sbs = runGetInternal act (coerce (BSS.length sbs)) (viewSBSMem sbs)

runGetBS :: Get a -> ByteString -> (Either GetError a, ByteCount)
runGetBS act bs = runGetInternal act (coerce (BS.length bs)) (viewBSMem bs)

runGetVec :: Get a -> Vector Word8 -> (Either GetError a, ByteCount)
runGetVec act vec = runGetInternal act (coerce (VS.length vec)) (viewVecMem vec)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  pure (runGetBS act bs)

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
