module Dahdit.Mem
  ( IxPtr (..)
  , ReadMem (..)
  , readSBSMem
  , viewSBSMem
  , viewBSMem
  , viewVecMem
  , WriteMem (..)
  , writeSBSMem
  , allocArrayMem
  , allocPtrMem
  , freezeSBSMem
  , freezeBSMem
  , freezeVecMem
  )
where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Dahdit.Proxy (proxyFor)
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString (..))
import qualified Data.ByteString.Unsafe as BSU
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, copyByteArrayToPtr, freezeByteArray, newByteArray, unsafeFreezeByteArray)
import Data.Primitive.Ptr (copyPtrToMutableByteArray)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (callocBytes, finalizerFree, free)
import Foreign.Ptr (Ptr, plusPtr)

-- | A wrapper over 'Ptr' with an additional phantom type index to align with 'ST' state.
newtype IxPtr s = IxPtr {unIxPtr :: Ptr Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

class ReadMem r where
  indexMemInBytes :: LiftedPrim a => r -> ByteCount -> a
  cloneArrayMemInBytes :: r -> ByteCount -> ByteCount -> ByteArray

instance ReadMem ByteArray where
  indexMemInBytes = indexArrayLiftedInBytes
  cloneArrayMemInBytes arr off len = cloneByteArray arr (coerce off) (coerce len)

clonePtr :: Ptr Word8 -> ByteCount -> ByteCount -> ByteArray
clonePtr ptr off len = runST $ do
  let wptr = coerce (plusPtr ptr (coerce off)) :: Ptr Word8
  marr <- newByteArray (coerce len)
  copyPtrToMutableByteArray marr 0 wptr (coerce len)
  unsafeFreezeByteArray marr

instance ReadMem (Ptr Word8) where
  indexMemInBytes = indexPtrLiftedInBytes
  cloneArrayMemInBytes = clonePtr

readSBSMem :: ReadMem r => r -> ByteCount -> ByteCount -> ShortByteString
readSBSMem mem off len = let !(ByteArray frozArr) = cloneArrayMemInBytes mem off len in SBS frozArr

viewSBSMem :: ShortByteString -> ByteArray
viewSBSMem (SBS harr) = ByteArray harr

viewBSMem :: ByteString -> Ptr Word8
viewBSMem bs =
  let (fp, _) = BSI.toForeignPtr0 bs
  in  unsafeForeignPtrToPtr fp

viewVecMem :: Vector Word8 -> Ptr Word8
viewVecMem vec =
  let (fp, _) = VS.unsafeToForeignPtr0 vec
  in  unsafeForeignPtrToPtr fp

class WriteMem q where
  writeMemInBytes :: LiftedPrim a => a -> q s -> ByteCount -> ST s ()
  copyArrayMemInBytes :: ByteArray -> ByteCount -> ByteCount -> q s -> ByteCount -> ST s ()
  setMemInBytes :: LiftedPrim a => ByteCount -> a -> q s -> ByteCount -> ST s ()
  releaseMem :: q s -> Maybe (IO ())

instance WriteMem MutableByteArray where
  writeMemInBytes val mem off = writeArrayLiftedInBytes mem off val
  copyArrayMemInBytes arr arrOff arrLen mem off = copyByteArray mem (coerce off) arr (coerce arrOff) (coerce arrLen)
  setMemInBytes len val mem off = setByteArrayLifted mem off len val
  releaseMem = const Nothing

copyPtr :: ByteArray -> ByteCount -> ByteCount -> Ptr Word8 -> ByteCount -> ST s ()
copyPtr arr arrOff arrLen ptr off =
  let wptr = coerce (plusPtr ptr (coerce off)) :: Ptr Word8
  in  copyByteArrayToPtr wptr arr (coerce arrOff) (coerce arrLen)

setPtr :: LiftedPrim a => ByteCount -> a -> Ptr Word8 -> ByteCount -> ST s ()
setPtr len val ptr off = do
  let elemSize = staticByteSize (proxyFor val)
      elemLen = div (coerce len) elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writePtrLiftedInBytes ptr (off + pos * elemSize) val

instance WriteMem IxPtr where
  writeMemInBytes val mem off = writePtrLiftedInBytes (unIxPtr mem) off val
  copyArrayMemInBytes arr arrOff arrLen = copyPtr arr arrOff arrLen . unIxPtr
  setMemInBytes len val = setPtr len val . unIxPtr
  releaseMem = Just . free . unIxPtr

writeSBSMem :: WriteMem q => ShortByteString -> ByteCount -> q s -> ByteCount -> ST s ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

guardedFreeze :: (q s -> ByteCount -> ST s z) -> q s -> ByteCount -> ByteCount -> ST s z
guardedFreeze freeze arr len off =
  -- This is a sanity check - if it goes wrong then there's a bug in the library
  if off /= len
    then error ("Invalid put length: (given " ++ show len ++ ", used " ++ show off ++ ")")
    else freeze arr len

freezeSBSMem :: MutableByteArray s -> ByteCount -> ByteCount -> ST s ShortByteString
freezeSBSMem marr cap len = fmap (\(ByteArray harr) -> SBS harr) (if cap == len then unsafeFreezeByteArray marr else freezeByteArray marr 0 (coerce len))

freezeBSMem :: IxPtr s -> ByteCount -> ByteCount -> ST s ByteString
freezeBSMem (IxPtr ptr) _ len =
  unsafeIOToST (BSU.unsafePackCStringFinalizer ptr (coerce len) (free ptr))

freezeVecMem :: IxPtr s -> ByteCount -> ByteCount -> ST s (Vector Word8)
freezeVecMem (IxPtr ptr) _ len = unsafeIOToST (fmap (\fp -> VS.unsafeFromForeignPtr0 fp (coerce len)) (newForeignPtr finalizerFree ptr))

allocPtrMem :: ByteCount -> ST s (IxPtr s)
allocPtrMem = fmap IxPtr . unsafeIOToST . callocBytes . coerce

allocArrayMem :: ByteCount -> ST s (MutableByteArray s)
allocArrayMem = newByteArray . coerce
