module Dahdit.Mem
  ( MemPtr (..)
  , emptyMemPtr
  , MutableMem (..)
  , ReadMem (..)
  , readSBSMem
  , viewSBSMem
  , viewBSMem
  , viewVecMem
  , mutViewVecMem
  , WriteMem (..)
  , writeSBSMem
  , withBAMem
  , withSBSMem
  , withVecMem
  , withBSMem
  )
where

import Control.Monad.Primitive (MonadPrim, PrimMonad (..), RealWorld)
import Dahdit.LiftedPrim (LiftedPrim (..), setByteArrayLifted)
import Dahdit.Proxy (proxyFor)
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, cloneByteArray, copyByteArray, copyByteArrayToPtr, freezeByteArray, newByteArray, unsafeFreezeByteArray, unsafeThawByteArray)
import Data.Primitive.Ptr (copyPtrToMutableByteArray)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

data MemPtr s = MemPtr
  { mpForeign :: !(ForeignPtr Word8)
  , mpOffset :: !ByteCount
  , mpLength :: !ByteCount
  }
  deriving stock (Eq, Ord, Show)

emptyMemPtr :: IO (MemPtr RealWorld)
emptyMemPtr = allocPtrMem 0

withMemPtr :: MemPtr RealWorld -> (Ptr Word8 -> IO a) -> IO a
withMemPtr (MemPtr fp off _) f = withForeignPtr fp (\ptr -> f (plusPtr ptr (coerce off)))

class PrimMonad m => MutableMem r w m | w m -> r where
  unsafeThawMem :: r -> m w
  unsafeUseThawedMem :: r -> (w -> m a) -> m a
  unsafeUseThawedMem r f = unsafeThawMem r >>= f
  unsafeFreezeMem :: w -> m r
  unsafeUseFrozenMem :: w -> (r -> m a) -> m a
  unsafeUseFrozenMem w f = unsafeFreezeMem w >>= f

instance MonadPrim s m => MutableMem ByteArray (MutableByteArray s) m where
  unsafeThawMem = unsafeThawByteArray
  unsafeFreezeMem = unsafeFreezeByteArray

instance MutableMem (VS.Vector Word8) (IOVector Word8) IO where
  unsafeThawMem = VS.unsafeThaw
  unsafeFreezeMem = VS.unsafeFreeze

class PrimMonad m => ReadMem r m where
  indexMemInBytes :: LiftedPrim a => r -> ByteCount -> m a
  cloneArrayMemInBytes :: r -> ByteCount -> ByteCount -> m ByteArray

instance PrimMonad m => ReadMem ByteArray m where
  indexMemInBytes arr off = pure (indexArrayLiftedInBytes arr off)
  cloneArrayMemInBytes arr off len = pure (cloneByteArray arr (coerce off) (coerce len))

cloneMemPtr :: MemPtr RealWorld -> ByteCount -> ByteCount -> IO ByteArray
cloneMemPtr mem off len = do
  marr <- newByteArray (coerce len)
  withMemPtr mem $ \ptr -> do
    let wptr = plusPtr ptr (coerce off) :: Ptr Word8
    copyPtrToMutableByteArray marr 0 wptr (coerce len)
  unsafeFreezeByteArray marr

instance ReadMem (MemPtr RealWorld) IO where
  indexMemInBytes mem off = withMemPtr mem (\ptr -> pure (indexPtrLiftedInBytes ptr off))
  cloneArrayMemInBytes = cloneMemPtr

readSBSMem :: ReadMem r m => r -> ByteCount -> ByteCount -> m ShortByteString
readSBSMem mem off len = do
  ByteArray frozArr <- cloneArrayMemInBytes mem off len
  pure (SBS frozArr)

viewSBSMem :: ShortByteString -> ByteArray
viewSBSMem (SBS harr) = ByteArray harr

viewBSMem :: ByteString -> MemPtr RealWorld
viewBSMem bs = let (fp, off, len) = BSI.toForeignPtr bs in MemPtr fp (coerce off) (coerce len)

viewVecMem :: Vector Word8 -> MemPtr RealWorld
viewVecMem vec = let (fp, off, len) = VS.unsafeToForeignPtr vec in MemPtr fp (coerce off) (coerce len)

mutViewVecMem :: IOVector Word8 -> MemPtr RealWorld
mutViewVecMem mvec = let (fp, off, len) = VSM.unsafeToForeignPtr mvec in MemPtr fp (coerce off) (coerce len)

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

instance WriteMem MemPtr IO where
  writeMemInBytes val mem off = withMemPtr mem (\ptr -> writePtrLiftedInBytes ptr off val)
  copyArrayMemInBytes arr arrOff arrLen mem off = withMemPtr mem (\ptr -> copyPtr arr arrOff arrLen ptr off)
  setMemInBytes len val mem off = withMemPtr mem (\ptr -> setPtr len val ptr off)

writeSBSMem :: WriteMem q m => ShortByteString -> ByteCount -> q (PrimState m) -> ByteCount -> m ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

withBAMem :: MonadPrim s m => ByteCount -> (MutableByteArray s -> m ByteCount) -> m ByteArray
withBAMem len use = do
  marr <- newByteArray (coerce len)
  len' <- use marr
  if len' == len
    then unsafeFreezeByteArray marr
    else freezeByteArray marr 0 (coerce len')

withSBSMem :: MonadPrim s m => ByteCount -> (MutableByteArray s -> m ByteCount) -> m ShortByteString
withSBSMem len use = fmap (\(ByteArray arr) -> SBS arr) (withBAMem len use)

allocPtrMem :: ByteCount -> IO (MemPtr RealWorld)
allocPtrMem len = do
  fp <- mallocForeignPtrBytes (coerce len)
  pure (MemPtr fp 0 len)

freezeVecMem :: MemPtr RealWorld -> ByteCount -> Vector Word8
freezeVecMem (MemPtr fp off _) len =
  VS.unsafeFromForeignPtr fp (coerce off) (coerce (off + len))

freezeBSMem :: MemPtr RealWorld -> ByteCount -> ByteString
freezeBSMem (MemPtr fp off _) len =
  BSI.fromForeignPtr fp (coerce off) (coerce (off + len))

withVecMem :: ByteCount -> (MemPtr RealWorld -> IO ByteCount) -> IO (Vector Word8)
withVecMem len use = do
  mem <- allocPtrMem len
  len' <- use mem
  pure (freezeVecMem mem len')

withBSMem :: ByteCount -> (MemPtr RealWorld -> IO ByteCount) -> IO ByteString
withBSMem len use = do
  mem <- allocPtrMem len
  len' <- use mem
  pure (freezeBSMem mem len')
