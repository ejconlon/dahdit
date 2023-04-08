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
import Dahdit.Counts (ByteCount (..))
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, freezeByteArray, sizeofMutableByteArray)
import Foreign.Ptr (Ptr)

data ReadPtr x = ReadPtr
  { rpPtr :: !(Ptr x)
  , rpLen :: !ByteCount
  }
  deriving stock (Eq, Ord, Show)

data WritePtr x s = WritePtr
  { wpPtr :: !(Ptr x)
  , wpLen :: !ByteCount
  }
  deriving stock (Eq, Ord, Show)

class ReadMem r where
  indexMemInBytes :: LiftedPrim a => r -> ByteCount -> a
  cloneArrayMemInBytes :: r -> ByteCount -> ByteCount -> ByteArray

instance ReadMem ByteArray where
  indexMemInBytes = indexArrayLiftedInBytes
  cloneArrayMemInBytes arr off len = cloneByteArray arr (coerce off) (coerce len)

instance ReadMem (ReadPtr x) where
  indexMemInBytes = indexPtrLiftedInBytes . rpPtr
  cloneArrayMemInBytes = error "TODO"

readSBSMem :: ReadMem r => r -> ByteCount -> ByteCount -> ShortByteString
readSBSMem mem off len = let !(ByteArray frozArr) = cloneArrayMemInBytes mem off len in SBS frozArr

viewSBSMem :: ShortByteString -> ByteArray
viewSBSMem (SBS harr) = ByteArray harr

class WriteMem q where
  writeMemInBytes :: LiftedPrim a => a -> q s -> ByteCount -> ST s ()
  copyArrayMemInBytes :: ByteArray -> ByteCount -> ByteCount -> q s -> ByteCount -> ST s ()
  setMemInBytes :: LiftedPrim a => ByteCount -> a -> q s -> ByteCount -> ST s ()

instance WriteMem MutableByteArray where
  writeMemInBytes val mem off = writeArrayLiftedInBytes mem off val
  copyArrayMemInBytes arr arrOff arrLen mem off = copyByteArray mem (coerce off) arr (coerce arrOff) (coerce arrLen)
  setMemInBytes len val mem off = setByteArrayLifted mem off len val

instance WriteMem (WritePtr x) where
  writeMemInBytes val mem off = writePtrLiftedInBytes (wpPtr mem) off val
  copyArrayMemInBytes = error "TODO"
  setMemInBytes = error "TODO"

writeSBSMem :: WriteMem q => ShortByteString -> ByteCount -> q s -> ByteCount -> ST s ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

freezeSBSMem :: MutableByteArray s -> ST s ShortByteString
freezeSBSMem marr = do
  ByteArray harr <- freezeByteArray marr 0 (sizeofMutableByteArray marr)
  pure (SBS harr)
