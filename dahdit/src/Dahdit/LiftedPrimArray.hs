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
  , copyLiftedPrimArray
  , setLiftedPrimArray
  , readLiftedPrimArray
  , mapLiftedPrimArray
  , concatLiftedPrimArray
  , mergeLiftedPrimArray
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.ST.Strict (runST)
import Dahdit.LiftedPrim
  ( LiftedPrim (..)
  , indexArrayLiftedInElems
  , readArrayLiftedInElems
  , writeArrayLiftedInElems
  )
import Dahdit.Proxy (proxyFor, proxyForF)
import Dahdit.Sizes (ByteCount (..), ElemCount (..), StaticByteSized (..))
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , cloneByteArray
  , copyByteArray
  , emptyByteArray
  , freezeByteArray
  , getSizeofMutableByteArray
  , newByteArray
  , runByteArray
  , sizeofByteArray
  , thawByteArray
  , unsafeFreezeByteArray
  , unsafeThawByteArray
  )
import Data.Proxy (Proxy (..))
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Max (..), Sum (..))

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

writeLiftedPrimArray :: (PrimMonad m, LiftedPrim a) => MutableLiftedPrimArray (PrimState m) a -> ElemCount -> a -> m ()
writeLiftedPrimArray (MutableLiftedPrimArray arr) = writeArrayLiftedInElems arr

freezeLiftedPrimArray
  :: (PrimMonad m, StaticByteSized a)
  => MutableLiftedPrimArray (PrimState m) a
  -> ElemCount
  -> ElemCount
  -> m (LiftedPrimArray a)
freezeLiftedPrimArray ma@(MutableLiftedPrimArray arr) off len =
  let elemSize = staticByteSize (proxyForF ma)
      off' = unElemCount off * unByteCount elemSize
      len' = unElemCount len * unByteCount elemSize
  in  fmap LiftedPrimArray (freezeByteArray arr off' len')

unsafeFreezeLiftedPrimArray :: (PrimMonad m) => MutableLiftedPrimArray (PrimState m) a -> m (LiftedPrimArray a)
unsafeFreezeLiftedPrimArray (MutableLiftedPrimArray arr) = fmap LiftedPrimArray (unsafeFreezeByteArray arr)

thawLiftedPrimArray
  :: (PrimMonad m, StaticByteSized a)
  => LiftedPrimArray a
  -> ElemCount
  -> ElemCount
  -> m (MutableLiftedPrimArray (PrimState m) a)
thawLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let elemSize = staticByteSize (proxyForF pa)
      off' = unElemCount off * unByteCount elemSize
      len' = unElemCount len * unByteCount elemSize
  in  fmap MutableLiftedPrimArray (thawByteArray arr off' len')

unsafeThawLiftedPrimArray :: (PrimMonad m) => LiftedPrimArray a -> m (MutableLiftedPrimArray (PrimState m) a)
unsafeThawLiftedPrimArray (LiftedPrimArray arr) = fmap MutableLiftedPrimArray (unsafeThawByteArray arr)

liftedPrimArrayFromListN :: (LiftedPrim a) => ElemCount -> [a] -> LiftedPrimArray a
liftedPrimArrayFromListN n xs = LiftedPrimArray $ runByteArray $ do
  let elemSize = staticByteSize (proxyForF xs)
      len = unElemCount n * unByteCount elemSize
  arr <- newByteArray len
  offRef <- newSTRef 0
  for_ xs $ \x -> do
    off <- readSTRef offRef
    writeArrayLiftedInBytes arr off x
    writeSTRef offRef (off + elemSize)
  pure arr

liftedPrimArrayFromList :: (LiftedPrim a) => [a] -> LiftedPrimArray a
liftedPrimArrayFromList xs = liftedPrimArrayFromListN (ElemCount (length xs)) xs

generateLiftedPrimArray :: (LiftedPrim a) => ElemCount -> (ElemCount -> a) -> LiftedPrimArray a
generateLiftedPrimArray n f = liftedPrimArrayFromListN n (fmap f [0 .. n - 1])

sizeofLiftedPrimArray :: LiftedPrimArray a -> ByteCount
sizeofLiftedPrimArray (LiftedPrimArray arr) = ByteCount (sizeofByteArray arr)

lengthLiftedPrimArray :: (LiftedPrim a) => LiftedPrimArray a -> ElemCount
lengthLiftedPrimArray pa@(LiftedPrimArray arr) =
  let elemSize = staticByteSize (proxyForF pa)
      arrSize = sizeofByteArray arr
  in  ElemCount (div arrSize (unByteCount elemSize))

cloneLiftedPrimArray :: (LiftedPrim a) => LiftedPrimArray a -> ElemCount -> ElemCount -> LiftedPrimArray a
cloneLiftedPrimArray pa@(LiftedPrimArray arr) off len =
  let elemSize = staticByteSize (proxyForF pa)
      off' = unElemCount off * unByteCount elemSize
      len' = unElemCount len * unByteCount elemSize
  in  LiftedPrimArray (cloneByteArray arr off' len')

replicateLiftedPrimArray :: (LiftedPrim a) => ElemCount -> a -> LiftedPrimArray a
replicateLiftedPrimArray len val = LiftedPrimArray $ runByteArray $ do
  let elemSize = staticByteSize (proxyFor val)
      len' = unElemCount len * unByteCount elemSize
  arr <- newByteArray len'
  for_ [0 .. len - 1] $ \pos -> do
    let pos' = ByteCount (unElemCount pos * unByteCount elemSize)
    writeArrayLiftedInBytes arr pos' val
  pure arr

newLiftedPrimArray
  :: (PrimMonad m, StaticByteSized a) => ElemCount -> Proxy a -> m (MutableLiftedPrimArray (PrimState m) a)
newLiftedPrimArray len prox =
  let elemSize = staticByteSize prox
      len' = unElemCount len * unByteCount elemSize
  in  fmap MutableLiftedPrimArray (newByteArray len')

copyLiftedPrimArray
  :: (PrimMonad m, StaticByteSized a)
  => MutableLiftedPrimArray (PrimState m) a
  -> ElemCount
  -> LiftedPrimArray a
  -> ElemCount
  -> ElemCount
  -> m ()
copyLiftedPrimArray darr@(MutableLiftedPrimArray dbarr) doff (LiftedPrimArray sbarr) soff slen = do
  let elemSize = unByteCount (staticByteSize (proxyForF darr))
      byteDoff = unElemCount doff * elemSize
      byteSoff = unElemCount soff * elemSize
      byteSmax = unElemCount slen * elemSize
      byteSlen = sizeofByteArray sbarr
  byteDmax <- getSizeofMutableByteArray dbarr
  let byteDext = min byteDmax (byteDoff + byteSlen)
      byteSext = min byteSmax (byteSoff + byteSlen)
      byteLen = min byteDext byteSext - byteSoff
  when (byteLen > 0) (copyByteArray dbarr byteDoff sbarr byteSoff byteLen)

-- Can't use setByteArray because don't necessarily a have Prim instance,
-- so instead we have to loop.
setLiftedPrimArray
  :: (PrimMonad m, LiftedPrim a)
  => MutableLiftedPrimArray (PrimState m) a
  -> ElemCount
  -> ElemCount
  -> a
  -> m ()
setLiftedPrimArray (MutableLiftedPrimArray dbarr) doff slen sval = do
  let elemSize = staticByteSize (proxyFor sval)
  arrSize <- getSizeofMutableByteArray dbarr
  let arrLen = ElemCount (div arrSize (unByteCount elemSize))
      setExt = min arrLen (doff + slen)
      setLen = setExt - doff
  when (setLen > 0) $ do
    for_ [0 .. setLen - 1] $ \pos -> do
      let pos' = ByteCount (unByteCount elemSize * unElemCount (doff + pos))
      writeArrayLiftedInBytes dbarr pos' sval

readLiftedPrimArray
  :: (PrimMonad m, LiftedPrim a)
  => MutableLiftedPrimArray (PrimState m) a
  -> ElemCount
  -> m a
readLiftedPrimArray = readArrayLiftedInElems Proxy . unMutableLiftedPrimArray

mapLiftedPrimArray
  :: forall a b
   . (LiftedPrim a, LiftedPrim b)
  => (a -> b)
  -> LiftedPrimArray a
  -> LiftedPrimArray b
mapLiftedPrimArray f arrA = runST $ do
  -- TODO use byte-wise ops
  let len = lengthLiftedPrimArray arrA
  arrB <- newLiftedPrimArray len (Proxy @b)
  for_ [0 .. len - 1] $ \pos -> do
    let valA = indexLiftedPrimArray arrA pos
    writeLiftedPrimArray arrB pos (f valA)
  unsafeFreezeLiftedPrimArray arrB

concatLiftedPrimArray :: (LiftedPrim a) => [LiftedPrimArray a] -> LiftedPrimArray a
concatLiftedPrimArray = \case
  [] -> emptyLiftedPrimArray
  [s0] -> s0
  ss@(s0 : _) -> runST $ do
    -- TODO use byte-wise ops
    let totLen = getSum (foldMap (Sum . lengthLiftedPrimArray) ss)
    marr <- newLiftedPrimArray totLen (proxyForF s0)
    offRef <- newSTRef 0
    for_ ss $ \s -> do
      let len = lengthLiftedPrimArray s
      off <- readSTRef offRef
      copyLiftedPrimArray marr off s 0 len
      writeSTRef offRef (off + len)
    unsafeFreezeLiftedPrimArray marr

-- | Combine several arrays pointwise. The first two arguments are "zero" and "plus", with
-- appropriate laws.
mergeLiftedPrimArray :: (LiftedPrim a) => a -> (a -> a -> a) -> [LiftedPrimArray a] -> LiftedPrimArray a
mergeLiftedPrimArray val f = \case
  [] -> emptyLiftedPrimArray
  [s0] -> s0
  ss -> runST $ do
    -- TODO use byte-wise ops
    let totLen = getMax (foldMap (Max . lengthLiftedPrimArray) ss)
    marr <- newLiftedPrimArray totLen (proxyFor val)
    setLiftedPrimArray marr 0 totLen val
    for_ ss $ \s -> do
      let len = lengthLiftedPrimArray s
      for_ [0 .. len - 1] $ \pos -> do
        val0 <- readLiftedPrimArray marr pos
        let val1 = indexLiftedPrimArray s pos
        writeLiftedPrimArray marr pos (f val0 val1)
    unsafeFreezeLiftedPrimArray marr
