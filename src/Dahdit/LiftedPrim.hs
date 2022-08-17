module Dahdit.LiftedPrim
  ( ViaFromIntegral (..)
  , LiftedPrim (..)
  , PrimArrayLifted (..)
  , MutablePrimArrayLifted (..)
  , indexPrimArrayLifted
  , writePrimArrayLifted
  , freezePrimArrayLifted
  , thawPrimArrayLifted
  , unsafeFreezePrimArrayLifted
  , unsafeThawPrimArrayLifted
  , primArrayLiftedFromList
  ) where

import Data.Primitive.ByteArray (MutableByteArray, ByteArray, indexByteArray, writeByteArray, freezeByteArray, thawByteArray, unsafeFreezeByteArray, unsafeThawByteArray, newByteArray, runByteArray)
import Control.Monad.Primitive (PrimMonad(..))
import Data.Word (Word8)
import Data.Int (Int8)
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Foldable (for_)
import Dahdit.Proxy (proxyForF)

newtype ViaFromIntegral x y = ViaFromIntegral { unViaFromIntegral :: y }

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

primArrayLiftedFromList :: LiftedPrim a => [a] -> PrimArrayLifted a
primArrayLiftedFromList xs = PrimArrayLifted $ runByteArray $ do
  let !elemSize = elemSizeLifted (proxyForF xs)
      !len = length xs * elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeByteArrayLiftedInBytes x arr off
    modifySTRef' offRef (elemSize+)
  pure arr
