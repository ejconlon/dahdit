module Dahdit.Mem
  ( PtrLen (..)
  , IxPtrLen (..)
  , ReadMem (..)
  , readSBSMem
  , viewSBSMem
  , WriteMem (..)
  , writeSBSMem
  , freezeSBSMem
  )
where

import Control.Monad.ST (ST, runST)
import Dahdit.Counts (ByteCount (..))
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Dahdit.Proxy (proxyFor)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, copyByteArrayToPtr, freezeByteArray, newByteArray, sizeofMutableByteArray, unsafeFreezeByteArray)
import Data.Primitive.Ptr (copyPtrToMutableByteArray)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)

-- | Pair of pointer to chunk of memory and usable length.
data PtrLen x = PtrLen
  { plPtr :: !(Ptr x)
  , plLen :: !ByteCount
  }
  deriving stock (Eq, Ord, Show)

-- | A wrapper over 'PtrLen' with an additional free type index to align with 'ST' state.
newtype IxPtrLen x s = IxPtrLen {unIxPtrLen :: PtrLen x}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

class ReadMem r where
  indexMemInBytes :: LiftedPrim a => r -> ByteCount -> a
  cloneArrayMemInBytes :: r -> ByteCount -> ByteCount -> ByteArray

instance ReadMem ByteArray where
  indexMemInBytes = indexArrayLiftedInBytes
  cloneArrayMemInBytes arr off len = cloneByteArray arr (coerce off) (coerce len)

clonePtr :: Ptr x -> ByteCount -> ByteCount -> ByteArray
clonePtr ptr off len = runST $ do
  let wptr = coerce (plusPtr ptr (coerce off)) :: Ptr Word8
  marr <- newByteArray (coerce len)
  copyPtrToMutableByteArray marr 0 wptr (coerce len)
  unsafeFreezeByteArray marr

instance ReadMem (PtrLen x) where
  indexMemInBytes = indexPtrLiftedInBytes . plPtr
  cloneArrayMemInBytes = clonePtr . plPtr

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

copyPtr :: ByteArray -> ByteCount -> ByteCount -> Ptr x -> ByteCount -> ST s ()
copyPtr arr arrOff arrLen ptr off =
  let wptr = coerce (plusPtr ptr (coerce off)) :: Ptr Word8
  in  copyByteArrayToPtr wptr arr (coerce arrOff) (coerce arrLen)

setPtr :: LiftedPrim a => ByteCount -> a -> Ptr x -> ByteCount -> ST s ()
setPtr len val ptr off = do
  let elemSize = elemSizeLifted (proxyFor val)
      elemLen = div (coerce len) elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writePtrLiftedInBytes ptr (off + pos * elemSize) val

instance WriteMem (IxPtrLen x) where
  writeMemInBytes val mem off = writePtrLiftedInBytes (plPtr (unIxPtrLen mem)) off val
  copyArrayMemInBytes arr arrOff arrLen = copyPtr arr arrOff arrLen . plPtr . unIxPtrLen
  setMemInBytes len val = setPtr len val . plPtr . unIxPtrLen

writeSBSMem :: WriteMem q => ShortByteString -> ByteCount -> q s -> ByteCount -> ST s ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

freezeSBSMem :: MutableByteArray s -> ST s ShortByteString
freezeSBSMem marr = do
  ByteArray harr <- freezeByteArray marr 0 (sizeofMutableByteArray marr)
  pure (SBS harr)
