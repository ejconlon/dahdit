module Dahdit.Mem
  ( IxPtr (..)
  , ReadMem (..)
  , readSBSMem
  , viewSBSMem
  , viewBSMem
  , viewVecMem
  , WriteMem (..)
  , writeSBSMem
  , allocBAMem
  , allocPtrMem
  , freezeBAMem
  , freezeSBSMem
  , freezeBSMem
  , freezeVecMem
  , mutAllocBAMem
  , mutFreezeBAMem
  , mutAllocVecMem
  , mutFreezeVecMem
  )
where

import Control.Monad.Primitive (PrimMonad (..), unsafeIOToPrim)
import Control.Monad.ST (runST)
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Dahdit.Proxy (proxyFor)
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString (..))
import qualified Data.ByteString.Unsafe as BSU
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, copyByteArrayToPtr, freezeByteArray, newByteArray, sizeofMutableByteArray, unsafeFreezeByteArray)
import Data.Primitive.Ptr (copyPtrToMutableByteArray)
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
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

mutViewVecMem :: MVector s Word8 -> Ptr Word8
mutViewVecMem mvec =
  let (fp, _) = VSM.unsafeToForeignPtr0 mvec
  in  unsafeForeignPtrToPtr fp

class PrimMonad m => WriteMem q m where
  writeMemInBytes :: LiftedPrim a => a -> q (PrimState m) -> ByteCount -> m ()
  copyArrayMemInBytes :: ByteArray -> ByteCount -> ByteCount -> q (PrimState m) -> ByteCount -> m ()
  setMemInBytes :: LiftedPrim a => ByteCount -> a -> q (PrimState m) -> ByteCount -> m ()

instance PrimMonad m => WriteMem MutableByteArray m where
  writeMemInBytes val mem off = writeArrayLiftedInBytes mem off val
  copyArrayMemInBytes arr arrOff arrLen mem off = copyByteArray mem (coerce off) arr (coerce arrOff) (coerce arrLen)
  setMemInBytes len val mem off = setByteArrayLifted mem off len val

copyPtr :: PrimMonad m => ByteArray -> ByteCount -> ByteCount -> Ptr Word8 -> ByteCount -> m ()
copyPtr arr arrOff arrLen ptr off =
  let wptr = coerce (plusPtr ptr (coerce off)) :: Ptr Word8
  in  copyByteArrayToPtr wptr arr (coerce arrOff) (coerce arrLen)

setPtr :: (PrimMonad m, LiftedPrim a) => ByteCount -> a -> Ptr Word8 -> ByteCount -> m ()
setPtr len val ptr off = do
  let elemSize = staticByteSize (proxyFor val)
      elemLen = div (coerce len) elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writePtrLiftedInBytes ptr (off + pos * elemSize) val

instance PrimMonad m => WriteMem IxPtr m where
  writeMemInBytes val mem off = writePtrLiftedInBytes (unIxPtr mem) off val
  copyArrayMemInBytes arr arrOff arrLen = copyPtr arr arrOff arrLen . unIxPtr
  setMemInBytes len val = setPtr len val . unIxPtr

writeSBSMem :: WriteMem q m => ShortByteString -> ByteCount -> q (PrimState m) -> ByteCount -> m ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

freezeBAMem :: PrimMonad m => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> m ByteArray
freezeBAMem marr (ByteCount startOff) (ByteCount endOff) =
  if startOff == 0 && endOff == sizeofMutableByteArray marr
    then unsafeFreezeByteArray marr
    else freezeByteArray marr startOff (endOff - startOff)

freezeSBSMem :: PrimMonad m => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> m ShortByteString
freezeSBSMem marr startOff endOff = fmap (\(ByteArray harr) -> SBS harr) (freezeBAMem marr startOff endOff)

freezeBSMem :: PrimMonad m => IxPtr (PrimState m) -> ByteCount -> ByteCount -> m ByteString
freezeBSMem (IxPtr ptr) startOff endOff =
  unsafeIOToPrim $
    BSU.unsafePackCStringFinalizer
      (plusPtr ptr (unByteCount startOff))
      (unByteCount (endOff - startOff))
      (free ptr)

freezeVecMem :: PrimMonad m => IxPtr (PrimState m) -> ByteCount -> ByteCount -> m (Vector Word8)
freezeVecMem (IxPtr ptr) _ len = unsafeIOToPrim (fmap (\fp -> VS.unsafeFromForeignPtr0 fp (coerce len)) (newForeignPtr finalizerFree ptr))

allocPtrMem :: PrimMonad m => ByteCount -> ByteCount -> m (IxPtr (PrimState m), Maybe (IO ()))
allocPtrMem off len = do
  let cap = off + len
  ptr <- unsafeIOToPrim (callocBytes (unByteCount cap))
  pure (IxPtr ptr, Just (free ptr))

allocBAMem :: PrimMonad m => ByteCount -> ByteCount -> m (MutableByteArray (PrimState m), Maybe (IO ()))
allocBAMem off len = do
  let cap = off + len
  arr <- newByteArray (unByteCount cap)
  pure (arr, Nothing)

mutAllocBAMem :: PrimMonad m => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> m (MutableByteArray (PrimState m), Maybe (IO ()))
mutAllocBAMem u _ _ = pure (u, Nothing)

mutFreezeBAMem :: PrimMonad m => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> m ByteCount
mutFreezeBAMem _ _ = pure

mutAllocVecMem :: PrimMonad m => MVector (PrimState m) Word8 -> ByteCount -> ByteCount -> m (IxPtr (PrimState m), Maybe (IO ()))
mutAllocVecMem u _ _ = pure (IxPtr (mutViewVecMem u), Nothing)

mutFreezeVecMem :: PrimMonad m => IxPtr (PrimState m) -> ByteCount -> ByteCount -> m ByteCount
mutFreezeVecMem _ _ = pure
