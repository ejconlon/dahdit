module Dahdit.Iface
  ( runGetSBS
  , runGetFile
  , runPutSBS
  , runPutFile
  )
where

import Control.Monad.ST (ST)
import Dahdit.Counts (ByteCount (..))
import Dahdit.Free (Get, Put)
import Dahdit.Mem (freezeSBSMem, viewSBSMem)
import Dahdit.Run (GetError, runGet, runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (MutableByteArray, newByteArray)

runGetSBS :: Get a -> ShortByteString -> (Either GetError a, ByteCount)
runGetSBS act sbs = runGet act (coerce (BSS.length sbs)) (viewSBSMem sbs)

runGetFile :: Get a -> FilePath -> IO (Either GetError a, ByteCount)
runGetFile act fp = do
  bs <- BS.readFile fp
  let sbs = BSS.toShort bs
  pure (runGetSBS act sbs)

guardedFreezeSBS :: MutableByteArray s -> ByteCount -> ByteCount -> ST s ShortByteString
guardedFreezeSBS marr len off =
  -- This is a sanity check - if it goes wrong then there's a bug in the library
  if off /= len
    then error ("Invalid put length: (given " ++ show len ++ ", used " ++ show off ++ ")")
    else freezeSBSMem marr len

runPutSBS :: Put -> ShortByteString
runPutSBS act = runPut act (newByteArray . coerce) guardedFreezeSBS

runPutFile :: FilePath -> Put -> IO ()
runPutFile fp act =
  let bs = runPutSBS act
      bs' = BSS.fromShort bs
  in  BS.writeFile fp bs'
