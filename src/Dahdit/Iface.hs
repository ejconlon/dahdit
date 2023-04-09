module Dahdit.Iface
  ( GetSource (..)
  , PutSink (..)
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

import Dahdit.Counts (ByteCount (..))
import Dahdit.Free (Get, Put)
import Dahdit.Mem (allocArrayMem, allocPtrMem, freezeBSMem, freezeSBSMem, freezeVecMem, viewBSMem, viewSBSMem, viewVecMem)
import Dahdit.Run (GetError, runGetInternal, runPutInternal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

-- TODO support getting with offset
class GetSource z where
  getFromSource :: Get a -> z -> (Either GetError a, ByteCount)

instance GetSource ShortByteString where
  getFromSource = runGetSBS

instance GetSource ByteString where
  getFromSource = runGetBS

instance GetSource (Vector Word8) where
  getFromSource = runGetVec

-- TODO support putting with offset
class PutSink z where
  putToSink :: Put -> z

instance PutSink ShortByteString where
  putToSink = runPutSBS

instance PutSink ByteString where
  putToSink = runPutBS

instance PutSink (Vector Word8) where
  putToSink = runPutVec

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

runPutSBS :: Put -> ShortByteString
runPutSBS act = runPutInternal act allocArrayMem freezeSBSMem

runPutBS :: Put -> ByteString
runPutBS act = runPutInternal act allocPtrMem freezeBSMem

runPutVec :: Put -> Vector Word8
runPutVec act = runPutInternal act allocPtrMem freezeVecMem

runPutFile :: FilePath -> Put -> IO ()
runPutFile fp act =
  let bs = runPutBS act
  in  BS.writeFile fp bs
