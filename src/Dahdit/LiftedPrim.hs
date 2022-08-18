module Dahdit.LiftedPrim
  ( LiftedPrim (..)
  , PrimArrayLifted (..)
  , MutablePrimArrayLifted (..)
  , indexPrimArrayLifted
  , writePrimArrayLifted
  , freezePrimArrayLifted
  , thawPrimArrayLifted
  , unsafeFreezePrimArrayLifted
  , unsafeThawPrimArrayLifted
  , primArrayLiftedFromListN
  , primArrayLiftedFromList
  , generatePrimArrayLifted
  , sizeofPrimArrayLifted
  ) where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Internal (ViaFromIntegral (..))
import Dahdit.Proxy (proxyForF)
import Data.Foldable (for_)
import Data.Int (Int8)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, freezeByteArray, indexByteArray, newByteArray,
                                 runByteArray, sizeofByteArray, thawByteArray, unsafeFreezeByteArray,
                                 unsafeThawByteArray, writeByteArray)
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Word (Word8)

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

newtype PrimArrayLifted a = PrimArrayLifted { unPrimArrayLifted :: ByteArray }
  deriving stock (Show)
  deriving newtype (Eq)

newtype MutablePrimArrayLifted m a = MutablePrimArrayLifted { unMutablePrimArrayLifted :: MutableByteArray m }
  deriving newtype (Eq)

indexPrimArrayLifted :: LiftedPrim a => PrimArrayLifted a -> Int -> a
indexPrimArrayLifted (PrimArrayLifted arr) = indexByteArrayLiftedInElems arr

writePrimArrayLifted :: (LiftedPrim a, PrimMonad m) => a -> MutablePrimArrayLifted (PrimState m) a -> Int -> m ()
writePrimArrayLifted val (MutablePrimArrayLifted arr) = writeByteArrayLiftedInElems val arr

freezePrimArrayLifted :: PrimMonad m => MutablePrimArrayLifted (PrimState m) a -> Int -> Int -> m (PrimArrayLifted a)
freezePrimArrayLifted (MutablePrimArrayLifted arr) off len = fmap PrimArrayLifted (freezeByteArray arr off len)

unsafeFreezePrimArrayLifted :: PrimMonad m => MutablePrimArrayLifted (PrimState m) a -> m (PrimArrayLifted a)
unsafeFreezePrimArrayLifted (MutablePrimArrayLifted arr) = fmap PrimArrayLifted (unsafeFreezeByteArray arr)

thawPrimArrayLifted :: PrimMonad m => PrimArrayLifted a -> Int -> Int -> m (MutablePrimArrayLifted (PrimState m) a)
thawPrimArrayLifted (PrimArrayLifted arr) off len = fmap MutablePrimArrayLifted (thawByteArray arr off len)

unsafeThawPrimArrayLifted :: PrimMonad m => PrimArrayLifted a -> m (MutablePrimArrayLifted (PrimState m) a)
unsafeThawPrimArrayLifted (PrimArrayLifted arr) = fmap MutablePrimArrayLifted (unsafeThawByteArray arr)

primArrayLiftedFromListN :: LiftedPrim a => Int -> [a] -> PrimArrayLifted a
primArrayLiftedFromListN n xs = PrimArrayLifted $ runByteArray $ do
  let !elemSize = elemSizeLifted (proxyForF xs)
      !len = n * elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeByteArrayLiftedInBytes x arr off
    modifySTRef' offRef (elemSize+)
  pure arr

primArrayLiftedFromList :: LiftedPrim a => [a] -> PrimArrayLifted a
primArrayLiftedFromList xs = primArrayLiftedFromListN (length xs) xs

generatePrimArrayLifted :: LiftedPrim a => Int -> (Int -> a) -> PrimArrayLifted a
generatePrimArrayLifted n f = primArrayLiftedFromListN n (fmap f [0 .. n - 1])

sizeofPrimArrayLifted :: LiftedPrim a => PrimArrayLifted a -> Int
sizeofPrimArrayLifted pa@(PrimArrayLifted arr) =
  let !elemSize = elemSizeLifted (proxyForF pa)
      !arrSize = sizeofByteArray arr
  in div arrSize elemSize
