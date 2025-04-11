{-# LANGUAGE InstanceSigs #-}

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
import Dahdit.Proxy (proxyFor)
import Dahdit.Sizes (ByteCount (..), StaticByteSized, staticByteSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Foldable (for_)
import Data.Primitive (Prim (..), sizeOfType)
import Data.Primitive.ByteArray
  ( ByteArray (..)
  , MutableByteArray
  , cloneByteArray
  , copyByteArray
  , copyByteArrayToPtr
  , freezeByteArray
  , indexByteArray
  , newByteArray
  , setByteArray
  , unsafeFreezeByteArray
  , unsafeThawByteArray
  , writeByteArray
  )
import Data.Primitive.Ptr (copyPtrToMutableByteArray, indexOffPtr, writeOffPtr)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)

data MemPtr s = MemPtr
  { mpForeign :: !(ForeignPtr Word8)
  , mpOffset :: !ByteCount
  , mpLength :: !ByteCount
  }
  deriving stock (Eq, Ord, Show)

emptyMemPtr :: IO (MemPtr RealWorld)
emptyMemPtr = allocPtrMem 0

withMemPtr :: MemPtr RealWorld -> (Ptr Word8 -> IO a) -> IO a
withMemPtr (MemPtr fp off _) f = withForeignPtr fp (\ptr -> f (plusPtr ptr (unByteCount off)))

class (PrimMonad m) => MutableMem r w m | w m -> r where
  unsafeThawMem :: r -> m w
  unsafeUseThawedMem :: r -> (w -> m a) -> m a
  unsafeUseThawedMem r f = unsafeThawMem r >>= f
  unsafeFreezeMem :: w -> m r
  unsafeUseFrozenMem :: w -> (r -> m a) -> m a
  unsafeUseFrozenMem w f = unsafeFreezeMem w >>= f

instance (MonadPrim s m) => MutableMem ByteArray (MutableByteArray s) m where
  unsafeThawMem = unsafeThawByteArray
  unsafeFreezeMem = unsafeFreezeByteArray

instance MutableMem (VS.Vector Word8) (IOVector Word8) IO where
  unsafeThawMem = VS.unsafeThaw
  unsafeFreezeMem = VS.unsafeFreeze

class (PrimMonad m) => ReadMem r m where
  indexMemInBytes :: (Prim a) => r -> ByteCount -> m a
  cloneArrayMemInBytes :: r -> ByteCount -> ByteCount -> m ByteArray

instance (PrimMonad m) => ReadMem ByteArray m where
  indexMemInBytes arr off = pure (indexByteArray arr (unByteCount off))
  cloneArrayMemInBytes arr off len = pure (cloneByteArray arr (unByteCount off) (unByteCount len))

cloneMemPtr :: MemPtr RealWorld -> ByteCount -> ByteCount -> IO ByteArray
cloneMemPtr mem off len = do
  marr <- newByteArray (unByteCount len)
  withMemPtr mem $ \ptr -> do
    let wptr = plusPtr ptr (unByteCount off) :: Ptr Word8
    copyPtrToMutableByteArray marr 0 wptr (unByteCount len)
  unsafeFreezeByteArray marr

instance ReadMem (MemPtr RealWorld) IO where
  indexMemInBytes :: forall a. (Prim a) => MemPtr RealWorld -> ByteCount -> IO a
  indexMemInBytes mem off = withMemPtr mem $ \ptr ->
    let off' = sizeOfType @a * unByteCount off
    in  pure (indexOffPtr (castPtr ptr) off')
  cloneArrayMemInBytes = cloneMemPtr

readSBSMem :: (ReadMem r m) => r -> ByteCount -> ByteCount -> m ShortByteString
readSBSMem mem off len = do
  ByteArray frozArr <- cloneArrayMemInBytes mem off len
  pure (SBS frozArr)

viewSBSMem :: ShortByteString -> ByteArray
viewSBSMem (SBS harr) = ByteArray harr

viewBSMem :: ByteString -> MemPtr RealWorld
viewBSMem bs = let (fp, off, len) = BSI.toForeignPtr bs in MemPtr fp (ByteCount off) (ByteCount len)

viewVecMem :: Vector Word8 -> MemPtr RealWorld
viewVecMem vec = let (fp, off, len) = VS.unsafeToForeignPtr vec in MemPtr fp (ByteCount off) (ByteCount len)

mutViewVecMem :: IOVector Word8 -> MemPtr RealWorld
mutViewVecMem mvec = let (fp, off, len) = VSM.unsafeToForeignPtr mvec in MemPtr fp (ByteCount off) (ByteCount len)

class (PrimMonad m) => WriteMem q m where
  writeMemInBytes :: (Prim a, StaticByteSized a) => a -> q (PrimState m) -> ByteCount -> m ()
  copyArrayMemInBytes :: ByteArray -> ByteCount -> ByteCount -> q (PrimState m) -> ByteCount -> m ()
  setMemInBytes :: (Prim a, StaticByteSized a) => ByteCount -> a -> q (PrimState m) -> ByteCount -> m ()

instance (PrimMonad m) => WriteMem MutableByteArray m where
  writeMemInBytes val mem off = writeByteArray mem (unByteCount off) val
  copyArrayMemInBytes arr arrOff arrLen mem off = copyByteArray mem (unByteCount off) arr (unByteCount arrOff) (unByteCount arrLen)
  setMemInBytes len val mem off = setByteArray mem (unByteCount off) (unByteCount len) val

copyPtr :: (PrimMonad m) => ByteArray -> ByteCount -> ByteCount -> Ptr Word8 -> ByteCount -> m ()
copyPtr arr arrOff arrLen ptr off =
  let wptr = castPtr (plusPtr ptr (unByteCount off)) :: Ptr Word8
  in  copyByteArrayToPtr wptr arr (unByteCount arrOff) (unByteCount arrLen)

setPtr :: (PrimMonad m, Prim a, StaticByteSized a) => ByteCount -> a -> Ptr Word8 -> ByteCount -> m ()
setPtr len val ptr off = do
  let elemSize = staticByteSize (proxyFor val)
      elemLen = div len elemSize
      ptr' = castPtr ptr
  for_ [0 .. elemLen - 1] (\pos -> writeOffPtr ptr' (unByteCount (off + pos * elemSize)) val)

instance WriteMem MemPtr IO where
  writeMemInBytes :: forall a. (Prim a) => a -> MemPtr RealWorld -> ByteCount -> IO ()
  writeMemInBytes val mem off = withMemPtr mem $ \ptr ->
    let off' = sizeOfType @a * unByteCount off
    in  writeOffPtr (castPtr ptr) off' val
  copyArrayMemInBytes arr arrOff arrLen mem off = withMemPtr mem (\ptr -> copyPtr arr arrOff arrLen ptr off)
  setMemInBytes len val mem off = withMemPtr mem (\ptr -> setPtr len val ptr off)

writeSBSMem :: (WriteMem q m) => ShortByteString -> ByteCount -> q (PrimState m) -> ByteCount -> m ()
writeSBSMem (SBS harr) = copyArrayMemInBytes (ByteArray harr) 0

withBAMem :: (MonadPrim s m) => ByteCount -> (MutableByteArray s -> m ByteCount) -> m ByteArray
withBAMem len use = do
  marr <- newByteArray (unByteCount len)
  len' <- use marr
  if len' == len
    then unsafeFreezeByteArray marr
    else freezeByteArray marr 0 (unByteCount len')

withSBSMem :: (MonadPrim s m) => ByteCount -> (MutableByteArray s -> m ByteCount) -> m ShortByteString
withSBSMem len use = fmap (\(ByteArray arr) -> SBS arr) (withBAMem len use)

allocPtrMem :: ByteCount -> IO (MemPtr RealWorld)
allocPtrMem len = do
  fp <- mallocForeignPtrBytes (unByteCount len)
  pure (MemPtr fp 0 len)

freezeVecMem :: MemPtr RealWorld -> ByteCount -> Vector Word8
freezeVecMem (MemPtr fp off _) len =
  VS.unsafeFromForeignPtr fp (unByteCount off) (unByteCount (off + len))

freezeBSMem :: MemPtr RealWorld -> ByteCount -> ByteString
freezeBSMem (MemPtr fp off _) len =
  BSI.fromForeignPtr fp (unByteCount off) (unByteCount (off + len))

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
