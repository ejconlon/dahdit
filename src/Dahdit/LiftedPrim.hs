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
  , cloneLiftedPrimArray
  , replicateLiftedPrimArray
  , setByteArrayLifted
  )
where

import Control.Monad.Primitive (PrimMonad (..))
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
  elemSizeLifted :: Proxy a -> Int

  -- |The offset here is in bytes
  indexArrayLiftedInBytes :: ByteArray -> Int -> a

  -- | The offset here is in bytes
  writeArrayLiftedInBytes :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> a -> m ()

  -- | The offset here is in bytes
  indexPtrLiftedInBytes :: Ptr x -> Int -> a

  -- | The offset here is in bytes
  writePtrLiftedInBytes :: PrimMonad m => Ptr x -> Int -> a -> m ()

-- | The position here is in elems
indexArrayLiftedInElems :: LiftedPrim a => Proxy a -> ByteArray -> Int -> a
indexArrayLiftedInElems prox arr pos =
  let !sz = elemSizeLifted prox
  in  indexArrayLiftedInBytes arr (pos * sz)

-- | The position here is in elems
writeArrayLiftedInElems :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> Int -> a -> m ()
writeArrayLiftedInElems arr pos val =
  let !sz = elemSizeLifted (proxyFor val)
  in  writeArrayLiftedInBytes arr (pos * sz) val

-- | The position here is in elems
indexPtrLiftedInElems :: LiftedPrim a => Proxy a -> Ptr x -> Int -> a
indexPtrLiftedInElems prox ptr pos =
  let !sz = elemSizeLifted prox
  in  indexPtrLiftedInBytes ptr (pos * sz)

-- | The position here is in elems
writePtrLiftedInElems :: (PrimMonad m, LiftedPrim a) => Ptr x -> Int -> a -> m ()
writePtrLiftedInElems ptr pos val =
  let !sz = elemSizeLifted (proxyFor val)
  in  writePtrLiftedInBytes ptr (pos * sz) val

instance LiftedPrim Word8 where
  elemSizeLifted _ = 1
  indexArrayLiftedInBytes = indexByteArray
  writeArrayLiftedInBytes = writeByteArray
  indexPtrLiftedInBytes = indexOffPtr . coerce
  writePtrLiftedInBytes = writeOffPtr . coerce

instance LiftedPrim Int8 where
  elemSizeLifted _ = 1
  indexArrayLiftedInBytes = indexByteArray
  writeArrayLiftedInBytes = writeByteArray
  indexPtrLiftedInBytes = indexOffPtr . coerce
  writePtrLiftedInBytes = writeOffPtr . coerce

-- | NOTE: Relies on same byte width of both types!
instance (Integral x, LiftedPrim x, Integral y) => LiftedPrim (ViaFromIntegral x y) where
  elemSizeLifted _ = elemSizeLifted (Proxy :: Proxy x)
  indexArrayLiftedInBytes arr off = ViaFromIntegral (fromIntegral (indexArrayLiftedInBytes arr off :: x))
  writeArrayLiftedInBytes arr off val = let !x = fromIntegral (unViaFromIntegral val) :: x in writeArrayLiftedInBytes arr off x
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

indexLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int -> a
indexLiftedPrimArray (LiftedPrimArray arr) = indexArrayLiftedInElems Proxy arr

writeLiftedPrimArray :: (LiftedPrim a, PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> Int -> a -> m ()
writeLiftedPrimArray (MutableLiftedPrimArray arr) = writeArrayLiftedInElems arr

freezeLiftedPrimArray :: PrimMonad m => MutableLiftedPrimArray (PrimState m) a -> Int -> Int -> m (LiftedPrimArray a)
freezeLiftedPrimArray (MutableLiftedPrimArray arr) off len = fmap LiftedPrimArray (freezeByteArray arr off len)

unsafeFreezeLiftedPrimArray :: PrimMonad m => MutableLiftedPrimArray (PrimState m) a -> m (LiftedPrimArray a)
unsafeFreezeLiftedPrimArray (MutableLiftedPrimArray arr) = fmap LiftedPrimArray (unsafeFreezeByteArray arr)

thawLiftedPrimArray :: PrimMonad m => LiftedPrimArray a -> Int -> Int -> m (MutableLiftedPrimArray (PrimState m) a)
thawLiftedPrimArray (LiftedPrimArray arr) off len = fmap MutableLiftedPrimArray (thawByteArray arr off len)

unsafeThawLiftedPrimArray :: PrimMonad m => LiftedPrimArray a -> m (MutableLiftedPrimArray (PrimState m) a)
unsafeThawLiftedPrimArray (LiftedPrimArray arr) = fmap MutableLiftedPrimArray (unsafeThawByteArray arr)

liftedPrimArrayFromListN :: LiftedPrim a => Int -> [a] -> LiftedPrimArray a
liftedPrimArrayFromListN n xs = LiftedPrimArray $ runByteArray $ do
  let !elemSize = elemSizeLifted (proxyForF xs)
      !len = n * elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeArrayLiftedInBytes arr off x
    modifySTRef' offRef (elemSize +)
  pure arr

liftedPrimArrayFromList :: LiftedPrim a => [a] -> LiftedPrimArray a
liftedPrimArrayFromList xs = liftedPrimArrayFromListN (length xs) xs

generateLiftedPrimArray :: LiftedPrim a => Int -> (Int -> a) -> LiftedPrimArray a
generateLiftedPrimArray n f = liftedPrimArrayFromListN n (fmap f [0 .. n - 1])

sizeofLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int
sizeofLiftedPrimArray pa@(LiftedPrimArray arr) =
  let !elemSize = elemSizeLifted (proxyForF pa)
      !arrSize = sizeofByteArray arr
  in  div arrSize elemSize

cloneLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int -> Int -> LiftedPrimArray a
cloneLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let !elemSize = elemSizeLifted (proxyForF pa)
      !byteOff = off * elemSize
      !byteLen = len * elemSize
      !arr' = cloneByteArray arr byteOff byteLen
  in  LiftedPrimArray arr'

replicateLiftedPrimArray :: LiftedPrim a => Int -> a -> LiftedPrimArray a
replicateLiftedPrimArray len val = LiftedPrimArray $ runByteArray $ do
  let !elemSize = elemSizeLifted (proxyFor val)
      !byteLen = len * elemSize
  arr <- newByteArray byteLen
  for_ [0 .. len - 1] $ \pos ->
    writeArrayLiftedInBytes arr (pos * elemSize) val
  pure arr

-- | Fill a byte array with the given value
setByteArrayLifted :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> Int -> Int -> a -> m ()
setByteArrayLifted arr off len val = do
  let !elemSize = elemSizeLifted (proxyFor val)
      !elemLen = div len elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writeArrayLiftedInBytes arr (off + pos * elemSize) val
