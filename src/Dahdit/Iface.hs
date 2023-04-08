module Dahdit.Iface
  ( GetSource (..)
  , PutSink (..)
  , runGetSBS
  , runGetBS
  , runGetFile
  , runPutSBS
  , runPutBS
  , runPutFile
  )
where

import Control.Monad.ST (ST)
import Dahdit.Counts (ByteCount (..))
import Dahdit.Free (Get, Put)
import Dahdit.Mem (freezeSBSMem, viewBSMem, viewSBSMem)
import Dahdit.Run (GetError, runGetInternal, runPutInternal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (newByteArray)

-- TODO support getting with offset
class GetSource z where
  getFromSource :: Get a -> z -> (Either GetError a, ByteCount)

instance GetSource ShortByteString where
  getFromSource = runGetSBS

instance GetSource ByteString where
  getFromSource = runGetBS

-- TODO support putting with offset
class PutSink z where
  putToSink :: Put -> z

instance PutSink ShortByteString where
  putToSink = runPutSBS

instance PutSink ByteString where
  putToSink = runPutBS

runGetSBS :: Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGetSBS act sbs = runGetInternal act (coerce (BSS.length sbs)) (viewSBSMem sbs)

runGetBS :: Get a -> ByteString -> (Either GetError a, ByteCount)
runGetBS act bs = runGetInternal act (coerce (BS.length bs)) (viewBSMem bs)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  pure (runGetBS act bs)

guardedFreeze :: (q s -> ByteCount -> ST s z) -> q s -> ByteCount -> ByteCount -> ST s z
guardedFreeze freeze arr len off =
  -- This is a sanity check - if it goes wrong then there's a bug in the library
  if off /= len
    then error ("Invalid put length: (given " ++ show len ++ ", used " ++ show off ++ ")")
    else freeze arr len

runPutSBS :: Put -> ShortByteString
runPutSBS act = runPutInternal act (newByteArray . coerce) (guardedFreeze freezeSBSMem)

runPutBS :: Put -> ByteString
runPutBS _act = error "TODO"

runPutFile :: FilePath -> Put -> IO ()
runPutFile fp act =
  let bs = runPutBS act
  in  BS.writeFile fp bs
