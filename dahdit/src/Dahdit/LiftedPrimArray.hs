module Dahdit.LiftedPrimArray
  ( LiftedPrimArray (..)
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
  , newLiftedPrimArray
  )
where

import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.LiftedPrim
  ( LiftedPrim (..)
  , indexArrayLiftedInElems
  , writeArrayLiftedInElems
  )
import Dahdit.Proxy (proxyFor, proxyForF)
import Dahdit.Sizes (ByteCount (..), ElemCount (..), StaticByteSized (..))
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , cloneByteArray
  , emptyByteArray
  , freezeByteArray
  , newByteArray
  , runByteArray
  , sizeofByteArray
  , thawByteArray
  , unsafeFreezeByteArray
  , unsafeThawByteArray
  )
import Data.Proxy (Proxy (..))
import Data.STRef (modifySTRef', newSTRef, readSTRef)

newtype LiftedPrimArray a = LiftedPrimArray {unLiftedPrimArray :: ByteArray}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Default (LiftedPrimArray a) where
  def = emptyLiftedPrimArray

newtype MutableLiftedPrimArray m a = MutableLiftedPrimArray {unMutableLiftedPrimArray :: MutableByteArray m}
  deriving newtype (Eq)

emptyLiftedPrimArray :: LiftedPrimArray a
emptyLiftedPrimArray = LiftedPrimArray emptyByteArray

indexLiftedPrimArray :: (LiftedPrim a) => LiftedPrimArray a -> ElemCount -> a
indexLiftedPrimArray (LiftedPrimArray arr) = indexArrayLiftedInElems Proxy arr

writeLiftedPrimArray :: (LiftedPrim a, PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> ElemCount -> a -> m ()
writeLiftedPrimArray (MutableLiftedPrimArray arr) = writeArrayLiftedInElems arr

freezeLiftedPrimArray
  :: (PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> ElemCount -> ElemCount -> m (LiftedPrimArray a)
freezeLiftedPrimArray (MutableLiftedPrimArray arr) off len = fmap LiftedPrimArray (freezeByteArray arr (coerce off) (coerce len))

unsafeFreezeLiftedPrimArray :: (PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> m (LiftedPrimArray a)
unsafeFreezeLiftedPrimArray (MutableLiftedPrimArray arr) = fmap LiftedPrimArray (unsafeFreezeByteArray arr)

thawLiftedPrimArray
  :: (PrimMonad m) => LiftedPrimArray a -> ElemCount -> ElemCount -> m (MutableLiftedPrimArray (PrimState m) a)
thawLiftedPrimArray (LiftedPrimArray arr) off len = fmap MutableLiftedPrimArray (thawByteArray arr (coerce off) (coerce len))

unsafeThawLiftedPrimArray :: (PrimMonad m) => LiftedPrimArray a -> m (MutableLiftedPrimArray (PrimState m) a)
unsafeThawLiftedPrimArray (LiftedPrimArray arr) = fmap MutableLiftedPrimArray (unsafeThawByteArray arr)

liftedPrimArrayFromListN :: (LiftedPrim a) => ElemCount -> [a] -> LiftedPrimArray a
liftedPrimArrayFromListN n xs = LiftedPrimArray $ runByteArray $ do
  let elemSize = staticByteSize (proxyForF xs)
      len = coerce n * coerce elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeArrayLiftedInBytes arr off x
    modifySTRef' offRef (elemSize +)
  pure arr

liftedPrimArrayFromList :: (LiftedPrim a) => [a] -> LiftedPrimArray a
liftedPrimArrayFromList xs = liftedPrimArrayFromListN (coerce (length xs)) xs

generateLiftedPrimArray :: (LiftedPrim a) => ElemCount -> (ElemCount -> a) -> LiftedPrimArray a
generateLiftedPrimArray n f = liftedPrimArrayFromListN n (fmap f [0 .. n - 1])

sizeofLiftedPrimArray :: LiftedPrimArray a -> ByteCount
sizeofLiftedPrimArray (LiftedPrimArray arr) = coerce (sizeofByteArray arr)

lengthLiftedPrimArray :: (LiftedPrim a) => LiftedPrimArray a -> ElemCount
lengthLiftedPrimArray pa@(LiftedPrimArray arr) =
  let elemSize = coerce (staticByteSize (proxyForF pa))
      arrSize = sizeofByteArray arr
  in  coerce (div arrSize elemSize)

cloneLiftedPrimArray :: (LiftedPrim a) => LiftedPrimArray a -> ElemCount -> ElemCount -> LiftedPrimArray a
cloneLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let elemSize = staticByteSize (proxyForF pa)
      byteOff = coerce off * elemSize
      byteLen = coerce len * elemSize
      arr' = cloneByteArray arr (coerce byteOff) (coerce byteLen)
  in  LiftedPrimArray arr'

replicateLiftedPrimArray :: (LiftedPrim a) => ElemCount -> a -> LiftedPrimArray a
replicateLiftedPrimArray len val = LiftedPrimArray $ runByteArray $ do
  let elemSize = staticByteSize (proxyFor val)
      byteLen = coerce len * elemSize
  arr <- newByteArray (coerce byteLen)
  for_ [0 .. len - 1] $ \pos ->
    writeArrayLiftedInBytes arr (coerce pos * elemSize) val
  pure arr

newLiftedPrimArray
  :: (PrimMonad m, StaticByteSized a) => ElemCount -> Proxy a -> m (MutableLiftedPrimArray (PrimState m) a)
newLiftedPrimArray len prox =
  let elemSize = staticByteSize prox
      byteLen = coerce len * elemSize
  in  fmap MutableLiftedPrimArray (newByteArray (coerce byteLen))
