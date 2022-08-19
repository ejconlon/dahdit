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
  ) where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Internal (ViaFromIntegral (..))
import Dahdit.Proxy (proxyForF)
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, cloneByteArray, emptyByteArray, freezeByteArray,
                                 indexByteArray, newByteArray, runByteArray, sizeofByteArray, thawByteArray,
                                 unsafeFreezeByteArray, unsafeThawByteArray, writeByteArray)
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Word (Word8)

-- | This is a stripped-down version of 'Prim' that is possible for a human to implement.
-- It's all about reading and writing structures from byte arrays.
class LiftedPrim a where
  elemSizeLifted :: Proxy a -> Int
  indexByteArrayLiftedInBytes :: ByteArray -> Int -> a
  indexByteArrayLiftedInElems :: ByteArray -> Int -> a
  indexByteArrayLiftedInElems arr pos =
    let !sz = elemSizeLifted (Proxy :: Proxy a)
    in indexByteArrayLiftedInBytes arr (pos * sz)
  writeByteArrayLiftedInBytes :: PrimMonad m => a -> MutableByteArray (PrimState m) -> Int -> m ()
  writeByteArrayLiftedInElems :: PrimMonad m => a -> MutableByteArray (PrimState m) -> Int -> m ()
  writeByteArrayLiftedInElems val arr pos =
    let !sz = elemSizeLifted (Proxy :: Proxy a)
    in writeByteArrayLiftedInBytes val arr (pos * sz)

instance LiftedPrim Word8 where
  elemSizeLifted _ = 1
  indexByteArrayLiftedInBytes = indexByteArray
  writeByteArrayLiftedInBytes val arr pos = writeByteArray arr pos val

instance LiftedPrim Int8 where
  elemSizeLifted _ = 1
  indexByteArrayLiftedInBytes = indexByteArray
  indexByteArrayLiftedInElems = indexByteArray
  writeByteArrayLiftedInBytes val arr pos = writeByteArray arr pos val
  writeByteArrayLiftedInElems val arr pos = writeByteArray arr pos val

-- | NOTE: Relies on same byte width of both types!
instance (Num x, Integral x, LiftedPrim x, Num y, Integral y) => LiftedPrim (ViaFromIntegral x y) where
  elemSizeLifted _ = elemSizeLifted (Proxy :: Proxy x)
  indexByteArrayLiftedInBytes arr pos = ViaFromIntegral (fromIntegral (indexByteArrayLiftedInBytes arr pos :: x))
  writeByteArrayLiftedInBytes val arr pos = let !x = fromIntegral (unViaFromIntegral val) :: x in writeByteArrayLiftedInBytes x arr pos

newtype LiftedPrimArray a = LiftedPrimArray { unLiftedPrimArray :: ByteArray }
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)

instance Default (LiftedPrimArray a) where
  def = emptyLiftedPrimArray

newtype MutableLiftedPrimArray m a = MutableLiftedPrimArray { unMutableLiftedPrimArray :: MutableByteArray m }
  deriving newtype (Eq)

emptyLiftedPrimArray :: LiftedPrimArray a
emptyLiftedPrimArray = LiftedPrimArray emptyByteArray

indexLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int -> a
indexLiftedPrimArray (LiftedPrimArray arr) = indexByteArrayLiftedInElems arr

writeLiftedPrimArray :: (LiftedPrim a, PrimMonad m) => a -> MutableLiftedPrimArray (PrimState m) a -> Int -> m ()
writeLiftedPrimArray val (MutableLiftedPrimArray arr) = writeByteArrayLiftedInElems val arr

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
    writeByteArrayLiftedInBytes x arr off
    modifySTRef' offRef (elemSize+)
  pure arr

liftedPrimArrayFromList :: LiftedPrim a => [a] -> LiftedPrimArray a
liftedPrimArrayFromList xs = liftedPrimArrayFromListN (length xs) xs

generateLiftedPrimArray :: LiftedPrim a => Int -> (Int -> a) -> LiftedPrimArray a
generateLiftedPrimArray n f = liftedPrimArrayFromListN n (fmap f [0 .. n - 1])

sizeofLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int
sizeofLiftedPrimArray pa@(LiftedPrimArray arr) =
  let !elemSize = elemSizeLifted (proxyForF pa)
      !arrSize = sizeofByteArray arr
  in div arrSize elemSize

cloneLiftedPrimArray :: LiftedPrim a => LiftedPrimArray a -> Int -> Int -> LiftedPrimArray a
cloneLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let !elemSize = elemSizeLifted (proxyForF pa)
      !byteOff = off * elemSize
      !byteLen = len * elemSize
      !arr' = cloneByteArray arr byteOff byteLen
  in LiftedPrimArray arr'
