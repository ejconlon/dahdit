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
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Word (Word8)

-- import Foreign.Ptr (Ptr)

-- | This is a stripped-down version of 'Prim' that is possible for a human to implement.
-- It's all about reading and writing structures from lifted byte arrays and pointers.
class LiftedPrim a where
  elemSizeLifted :: Proxy a -> Int
  indexByteArrayLifted :: ByteArray -> Int -> a
  writeByteArrayLifted :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> a -> m ()

  -- readOffAddrLifted :: PrimMonad m => Ptr Word8 -> Int -> m a
  -- writeOffAddrLifted :: PrimMonad m => Ptr Word8 -> Int -> a -> m ()

  indexByteArrayLiftedInElems :: ByteArray -> Int -> a
  indexByteArrayLiftedInElems arr pos =
    let !sz = elemSizeLifted (Proxy :: Proxy a)
    in  indexByteArrayLifted arr (pos * sz)
  writeByteArrayLiftedInElems :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> a -> m ()
  writeByteArrayLiftedInElems arr pos =
    let !sz = elemSizeLifted (Proxy :: Proxy a)
    in  writeByteArrayLifted arr (pos * sz)

-- readOffAddrLiftedInElems :: PrimMonad m => Ptr Word8 -> Int -> m a
-- readOffAddrLiftedInElems ptr pos =
--   let !sz = elemSizeLifted (Proxy :: Proxy a)
--   in  readOffAddrLifted ptr (pos * sz)
-- writeOffAddrLiftedInElems :: PrimMonad m => Ptr Word8 -> Int -> a -> m ()
-- writeOffAddrLiftedInElems ptr pos =
--   let !sz = elemSizeLifted (Proxy :: Proxy a)
--   in  writeOffAddrLifted ptr (pos * sz)

instance LiftedPrim Word8 where
  elemSizeLifted _ = 1
  indexByteArrayLifted = indexByteArray
  writeByteArrayLifted = writeByteArray

instance LiftedPrim Int8 where
  elemSizeLifted _ = 1
  indexByteArrayLifted = indexByteArray
  writeByteArrayLifted = writeByteArray

-- | NOTE: Relies on same byte width of both types!
instance (Integral x, LiftedPrim x, Integral y) => LiftedPrim (ViaFromIntegral x y) where
  elemSizeLifted _ = elemSizeLifted (Proxy :: Proxy x)
  indexByteArrayLifted arr pos = ViaFromIntegral (fromIntegral (indexByteArrayLifted arr pos :: x))
  writeByteArrayLifted arr pos val = let !x = fromIntegral (unViaFromIntegral val) :: x in writeByteArrayLifted arr pos x

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
indexLiftedPrimArray (LiftedPrimArray arr) = indexByteArrayLiftedInElems arr

writeLiftedPrimArray :: (LiftedPrim a, PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> Int -> a -> m ()
writeLiftedPrimArray (MutableLiftedPrimArray arr) = writeByteArrayLiftedInElems arr

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
    writeByteArrayLifted arr off x
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
    writeByteArrayLifted arr (pos * elemSize) val
  pure arr

-- | Fill a byte array with the given value
setByteArrayLifted :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> Int -> Int -> a -> m ()
setByteArrayLifted arr off len val = do
  let !elemSize = elemSizeLifted (proxyFor val)
      !elemLen = div len elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writeByteArrayLifted arr (off + pos * elemSize) val
