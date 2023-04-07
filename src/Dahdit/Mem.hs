module Dahdit.Mem
  ( ReadPtr (..)
  , WritePtr (..)
  , ReadMem (..)
  , readSBSMem
  , viewSBSMem
  , WriteMem (..)
  , writeSBSMem
  , freezeSBSMem
  )
where

import Control.Monad.ST (ST)
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, freezeByteArray, sizeofByteArray, sizeofMutableByteArray)
import Foreign.Ptr (Ptr)

data ReadPtr x = ReadPtr
  { rpPtr :: !(Ptr x)
  , rpLen :: !Int
  }
  deriving stock (Eq, Ord, Show)

data WritePtr x s = WritePtr
  { wpPtr :: !(Ptr x)
  , wpLen :: !Int
  }
  deriving stock (Eq, Ord, Show)

class ReadMem r where
  totalReadMemInBytes :: r -> Int
  indexMemInBytes :: LiftedPrim a => r -> Int -> a
  cloneArrayMemInBytes :: r -> Int -> Int -> ByteArray

instance ReadMem ByteArray where
  indexMemInBytes = indexArrayLiftedInBytes
  totalReadMemInBytes = sizeofByteArray
  cloneArrayMemInBytes = cloneByteArray

instance ReadMem (ReadPtr x) where
  indexMemInBytes = indexPtrLiftedInBytes . rpPtr
  totalReadMemInBytes = rpLen
  cloneArrayMemInBytes = error "TODO"

readSBSMem :: ReadMem r => r -> Int -> Int -> ShortByteString
readSBSMem mem off len = let !(ByteArray frozArr) = cloneArrayMemInBytes mem off len in SBS frozArr

viewSBSMem :: ShortByteString -> ByteArray
viewSBSMem (SBS harr) = ByteArray harr

class WriteMem q where
  writeMemInBytes :: LiftedPrim a => a -> q s -> Int -> ST s ()
  totalWriteMemInBytes :: q s -> Int
  copyArrayMemInBytes :: ByteArray -> Int -> Int -> q s -> Int -> ST s ()
  setMemInBytes :: LiftedPrim a => Int -> a -> q s -> Int -> ST s ()

instance WriteMem MutableByteArray where
  writeMemInBytes val mem off = writeArrayLiftedInBytes mem off val
  totalWriteMemInBytes = sizeofMutableByteArray
  copyArrayMemInBytes arr arrOff arrLen mem off = copyByteArray mem off arr arrOff arrLen
  setMemInBytes len val mem off = setByteArrayLifted mem off len val

instance WriteMem (WritePtr x) where
  writeMemInBytes val mem off = writePtrLiftedInBytes (wpPtr mem) off val
  totalWriteMemInBytes = wpLen
  copyArrayMemInBytes = error "TODO"
  setMemInBytes = error "TODO"

writeSBSMem :: WriteMem q => ShortByteString -> Int -> q s -> Int -> ST s ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

freezeSBSMem :: MutableByteArray s -> ST s ShortByteString
freezeSBSMem marr = do
  ByteArray harr <- freezeByteArray marr 0 (sizeofMutableByteArray marr)
  pure (SBS harr)
