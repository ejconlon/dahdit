module Dahdit.LiftedPrim
  ( LiftedPrim (..)
  , LiftedPrimArray (..)
  , MutableLiftedPrimArray (..)
  , emptyLiftedPrimArray
  , indexLiftedPrimArray
  , writeLiftedPrimArray
  , freezeLiftedPrimArray
  , thawLiftedPrimArray
  , unsafeFreezeLiftedPrimArray
  , unsafeThawLiftedPrimArray
  , liftedPrimArrayFromListN
  , liftedPrimArrayFromList
  , generateLiftedPrimArray
  , sizeofLiftedPrimArray
  , lengthLiftedPrimArray
  , cloneLiftedPrimArray
  , replicateLiftedPrimArray
  , setByteArrayLifted
  )
where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Counts (ByteCount (..), ElemCount (..))
import Dahdit.Internal (ViaFromIntegral (..))
import Dahdit.Proxy (proxyFor, proxyForF)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , cloneByteArray
  , emptyByteArray
  , freezeByteArray
  , indexByteArray
  , newByteArray
  , runByteArray
  , sizeofByteArray
  , thawByteArray
  , unsafeFreezeByteArray
  , unsafeThawByteArray
  , writeByteArray
  )
import Data.Primitive.Ptr (indexOffPtr, writeOffPtr)
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

-- | This is a stripped-down version of 'Prim' that is possible for a human to implement.
-- It's all about reading and writing structures from lifted byte arrays and pointers.
class LiftedPrim a where
  elemSizeLifted :: Proxy a -> ByteCount
  indexArrayLiftedInBytes :: ByteArray -> ByteCount -> a
  writeArrayLiftedInBytes :: PrimMonad m => MutableByteArray (PrimState m) -> ByteCount -> a -> m ()
  indexPtrLiftedInBytes :: Ptr x -> ByteCount -> a
  writePtrLiftedInBytes :: PrimMonad m => Ptr x -> ByteCount -> a -> m ()

indexArrayLiftedInElems :: LiftedPrim a => Proxy a -> ByteArray -> ElemCount -> a
indexArrayLiftedInElems prox arr pos =
  indexArrayLiftedInBytes arr (coerce pos * elemSizeLifted prox)

writeArrayLiftedInElems :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> ElemCount -> a -> m ()
writeArrayLiftedInElems arr pos val =
  writeArrayLiftedInBytes arr (coerce pos * elemSizeLifted (proxyFor val)) val

indexPtrLiftedInElems :: LiftedPrim a => Proxy a -> Ptr x -> ElemCount -> a
indexPtrLiftedInElems prox ptr pos =
  indexPtrLiftedInBytes ptr (coerce pos * elemSizeLifted prox)

writePtrLiftedInElems :: (PrimMonad m, LiftedPrim a) => Ptr x -> ElemCount -> a -> m ()
writePtrLiftedInElems ptr pos val =
  writePtrLiftedInBytes ptr (coerce pos * elemSizeLifted (proxyFor val)) val

instance LiftedPrim Word8 where
  elemSizeLifted _ = 1
  indexArrayLiftedInBytes arr = indexByteArray arr . coerce
  writeArrayLiftedInBytes marr = writeByteArray marr . coerce
  indexPtrLiftedInBytes ptr = indexOffPtr (coerce ptr) . coerce
  writePtrLiftedInBytes ptr = writeOffPtr (coerce ptr) . coerce

instance LiftedPrim Int8 where
  elemSizeLifted _ = 1
  indexArrayLiftedInBytes arr = indexByteArray arr . coerce
  writeArrayLiftedInBytes marr = writeByteArray marr . coerce
  indexPtrLiftedInBytes ptr = indexOffPtr (coerce ptr) . coerce
  writePtrLiftedInBytes ptr = writeOffPtr (coerce ptr) . coerce

-- | NOTE: Relies on same byte width of both types!
instance (Integral x, LiftedPrim x, Integral y) => LiftedPrim (ViaFromIntegral x y) where
  elemSizeLifted _ = elemSizeLifted (Proxy :: Proxy x)
  indexArrayLiftedInBytes arr off = ViaFromIntegral (fromIntegral (indexArrayLiftedInBytes arr off :: x))
  writeArrayLiftedInBytes arr off val = let x = fromIntegral (unViaFromIntegral val) :: x in writeArrayLiftedInBytes arr off x
  indexPtrLiftedInBytes ptr = ViaFromIntegral . fromIntegral @x @y . indexPtrLiftedInBytes ptr
  writePtrLiftedInBytes ptr off (ViaFromIntegral y) = writePtrLiftedInBytes ptr off (fromIntegral y :: x)

newtype LiftedPrimArray a = LiftedPrimArray {unLiftedPrimArray :: ByteArray}
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)

instance Default (LiftedPrimArray a) where
  def = emptyLiftedPrimArray

newtype MutableLiftedPrimArray m a = MutableLiftedPrimArray {unMutableLiftedPrimArray :: MutableByteArray m}
  deriving newtype (Eq)

emptyLiftedPrimArray :: LiftedPrimArray a
emptyLiftedPrimArray = LiftedPrimArray emptyByteArray

indexLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> ElemCount -> a
indexLiftedPrimArray (LiftedPrimArray arr) = indexArrayLiftedInElems Proxy arr

writeLiftedPrimArray :: (LiftedPrim a, PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> ElemCount -> a -> m ()
writeLiftedPrimArray (MutableLiftedPrimArray arr) = writeArrayLiftedInElems arr

freezeLiftedPrimArray :: PrimMonad m => MutableLiftedPrimArray (PrimState m) a -> ElemCount -> ElemCount -> m (LiftedPrimArray a)
freezeLiftedPrimArray (MutableLiftedPrimArray arr) off len = fmap LiftedPrimArray (freezeByteArray arr (coerce off) (coerce len))

unsafeFreezeLiftedPrimArray :: PrimMonad m => MutableLiftedPrimArray (PrimState m) a -> m (LiftedPrimArray a)
unsafeFreezeLiftedPrimArray (MutableLiftedPrimArray arr) = fmap LiftedPrimArray (unsafeFreezeByteArray arr)

thawLiftedPrimArray :: PrimMonad m => LiftedPrimArray a -> ElemCount -> ElemCount -> m (MutableLiftedPrimArray (PrimState m) a)
thawLiftedPrimArray (LiftedPrimArray arr) off len = fmap MutableLiftedPrimArray (thawByteArray arr (coerce off) (coerce len))

unsafeThawLiftedPrimArray :: PrimMonad m => LiftedPrimArray a -> m (MutableLiftedPrimArray (PrimState m) a)
unsafeThawLiftedPrimArray (LiftedPrimArray arr) = fmap MutableLiftedPrimArray (unsafeThawByteArray arr)

liftedPrimArrayFromListN :: LiftedPrim a => ElemCount -> [a] -> LiftedPrimArray a
liftedPrimArrayFromListN n xs = LiftedPrimArray $ runByteArray $ do
  let elemSize = elemSizeLifted (proxyForF xs)
      len = coerce n * coerce elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeArrayLiftedInBytes arr off x
    modifySTRef' offRef (elemSize +)
  pure arr

liftedPrimArrayFromList :: LiftedPrim a => [a] -> LiftedPrimArray a
liftedPrimArrayFromList xs = liftedPrimArrayFromListN (coerce (length xs)) xs

generateLiftedPrimArray :: LiftedPrim a => ElemCount -> (ElemCount -> a) -> LiftedPrimArray a
generateLiftedPrimArray n f = liftedPrimArrayFromListN n (fmap f [0 .. n - 1])

sizeofLiftedPrimArray :: LiftedPrimArray a -> ByteCount
sizeofLiftedPrimArray (LiftedPrimArray arr) = coerce (sizeofByteArray arr)

lengthLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> ElemCount
lengthLiftedPrimArray pa@(LiftedPrimArray arr) =
  let elemSize = coerce (elemSizeLifted (proxyForF pa))
      arrSize = sizeofByteArray arr
  in  coerce (div arrSize elemSize)

cloneLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> ElemCount -> ElemCount -> LiftedPrimArray a
cloneLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let elemSize = elemSizeLifted (proxyForF pa)
      byteOff = coerce off * elemSize
      byteLen = coerce len * elemSize
      arr' = cloneByteArray arr (coerce byteOff) (coerce byteLen)
  in  LiftedPrimArray arr'

replicateLiftedPrimArray :: LiftedPrim a => ElemCount -> a -> LiftedPrimArray a
replicateLiftedPrimArray len val = LiftedPrimArray $ runByteArray $ do
  let elemSize = elemSizeLifted (proxyFor val)
      byteLen = coerce len * elemSize
  arr <- newByteArray (coerce byteLen)
  for_ [0 .. len - 1] $ \pos ->
    writeArrayLiftedInBytes arr (coerce pos * elemSize) val
  pure arr

-- | Fill a byte array with the given value
setByteArrayLifted :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> a -> m ()
setByteArrayLifted arr off len val = do
  let elemSize = elemSizeLifted (proxyFor val)
      elemLen = div (coerce len) elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writeArrayLiftedInBytes arr (off + pos * elemSize) val
