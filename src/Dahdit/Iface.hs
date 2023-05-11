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

class BinaryTarget z where
  getTargetOffset :: ByteCount -> Get a -> z -> (Either GetError a, ByteCount)
  putTargetOffset :: ByteCount -> Put -> ByteCount -> z

getTarget :: BinaryTarget z => Get a -> z -> (Either GetError a, ByteCount)
getTarget = getTargetOffset 0

putTarget :: BinaryTarget z => Put -> ByteCount -> z
putTarget = putTargetOffset 0

instance BinaryTarget ShortByteString where
  getTargetOffset = runGetSBS
  putTargetOffset = runPutSBS

instance BinaryTarget ByteString where
  getTargetOffset = runGetBS
  putTargetOffset = runPutBS

instance BinaryTarget (Vector Word8) where
  getTargetOffset = runGetVec
  putTargetOffset = runPutVec

decode :: (Binary a, BinaryTarget z) => z -> (Either GetError a, ByteCount)
decode = getTarget get

decodeFile :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFile = runGetFile get

encode :: (Binary a, ByteSized a, BinaryTarget z) => a -> z
encode a = putTarget (put a) (byteSize a)

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

runPutSBS :: ByteCount -> Put -> ByteCount -> ShortByteString
runPutSBS off act cap = runPutInternal off act cap allocArrayMem freezeSBSMem

runPutBS :: ByteCount -> Put -> ByteCount -> ByteString
runPutBS off act cap = runPutInternal off act cap allocPtrMem freezeBSMem

runPutVec :: ByteCount -> Put -> ByteCount -> Vector Word8
runPutVec off act cap = runPutInternal off act cap allocPtrMem freezeVecMem

runPutFile :: Put -> ByteCount -> FilePath -> IO ()
runPutFile act cap fp =
  let bs = runPutBS 0 act cap
  in  BS.writeFile fp bs
