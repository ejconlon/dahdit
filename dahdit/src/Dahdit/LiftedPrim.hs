{-# LANGUAGE UndecidableInstances #-}

module Dahdit.LiftedPrim
  ( LiftedPrim (..)
  , readArrayLiftedInBytes
  , readPtrLiftedInBytes
  , indexArrayLiftedInElems
  , readArrayLiftedInElems
  , writeArrayLiftedInElems
  , indexPtrLiftedInElems
  , readPtrLiftedInElems
  , writePtrLiftedInElems
  , setByteArrayLifted
  )
where

import Control.Monad ((>=>))
import Control.Monad.Primitive (PrimMonad (..))
import Dahdit.Internal
  ( EndianPair (..)
  , ViaEndianPair (..)
  , ViaFromIntegral (..)
  , mkDoubleLE
  , mkFloatLE
  , mkWord16LE
  , mkWord24LE
  , mkWord32LE
  , mkWord64LE
  , unMkDoubleLE
  , unMkFloatLE
  , unMkWord16LE
  , unMkWord24LE
  , unMkWord32LE
  , unMkWord64LE
  )
import Dahdit.Nums
  ( DoubleBE
  , DoubleLE (..)
  , FloatBE
  , FloatLE (..)
  , Int16BE
  , Int16LE (..)
  , Int24BE
  , Int24LE (..)
  , Int32BE
  , Int32LE (..)
  , Int64BE
  , Int64LE (..)
  , Word16BE
  , Word16LE (..)
  , Word24BE
  , Word24LE (..)
  , Word32BE
  , Word32LE (..)
  , Word64BE
  , Word64LE (..)
  )
import Dahdit.Proxy (proxyFor)
import Dahdit.Sizes (ByteCount (..), ElemCount (..), StaticByteSized (..))
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray
  , indexByteArray
  , readByteArray
  , writeByteArray
  )
import Data.Primitive.Ptr (indexOffPtr, readOffPtr, writeOffPtr)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)

-- | This is a stripped-down version of 'Prim' that is possible for a human to implement.
-- It's all about reading and writing structures from lifted byte arrays and pointers.
-- The "K" variants here are to trick the role checker when deriving.
class (StaticByteSized a) => LiftedPrim a where
  indexArrayLiftedInBytes :: ByteArray -> ByteCount -> a
  readArrayLiftedInBytesK :: (PrimMonad m) => (a -> m b) -> MutableByteArray (PrimState m) -> ByteCount -> m b
  writeArrayLiftedInBytes :: (PrimMonad m) => MutableByteArray (PrimState m) -> ByteCount -> a -> m ()
  indexPtrLiftedInBytes :: Ptr Word8 -> ByteCount -> a
  readPtrLiftedInBytesK :: (PrimMonad m) => (a -> m b) -> Ptr Word8 -> ByteCount -> m b
  writePtrLiftedInBytes :: (PrimMonad m) => Ptr Word8 -> ByteCount -> a -> m ()

readArrayLiftedInBytes :: (PrimMonad m, LiftedPrim a) => Proxy a -> MutableByteArray (PrimState m) -> ByteCount -> m a
readArrayLiftedInBytes _ = readArrayLiftedInBytesK pure

readPtrLiftedInBytes :: (PrimMonad m, LiftedPrim a) => Proxy a -> Ptr Word8 -> ByteCount -> m a
readPtrLiftedInBytes _ = readPtrLiftedInBytesK pure

indexArrayLiftedInElems :: (LiftedPrim a) => Proxy a -> ByteArray -> ElemCount -> a
indexArrayLiftedInElems prox arr pos =
  indexArrayLiftedInBytes arr (coerce pos * staticByteSize prox)

readArrayLiftedInElems :: (PrimMonad m, LiftedPrim a) => Proxy a -> MutableByteArray (PrimState m) -> ElemCount -> m a
readArrayLiftedInElems prox arr pos = readArrayLiftedInBytes prox arr (coerce pos * staticByteSize prox)

writeArrayLiftedInElems :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> ElemCount -> a -> m ()
writeArrayLiftedInElems arr pos val =
  writeArrayLiftedInBytes arr (coerce pos * staticByteSize (proxyFor val)) val

indexPtrLiftedInElems :: (LiftedPrim a) => Proxy a -> Ptr Word8 -> ElemCount -> a
indexPtrLiftedInElems prox ptr pos =
  indexPtrLiftedInBytes ptr (coerce pos * staticByteSize prox)

readPtrLiftedInElems :: (PrimMonad m, LiftedPrim a) => Proxy a -> Ptr Word8 -> ElemCount -> m a
readPtrLiftedInElems prox ptr pos = readPtrLiftedInBytes prox ptr (coerce pos * staticByteSize prox)

writePtrLiftedInElems :: (PrimMonad m, LiftedPrim a) => Ptr Word8 -> ElemCount -> a -> m ()
writePtrLiftedInElems ptr pos val =
  writePtrLiftedInBytes ptr (coerce pos * staticByteSize (proxyFor val)) val

instance LiftedPrim Word8 where
  indexArrayLiftedInBytes arr = indexByteArray arr . coerce
  readArrayLiftedInBytesK k marr = readByteArray marr . coerce >=> k
  writeArrayLiftedInBytes marr = writeByteArray marr . coerce
  indexPtrLiftedInBytes ptr = indexOffPtr ptr . coerce
  readPtrLiftedInBytesK k ptr = readOffPtr ptr . coerce >=> k
  writePtrLiftedInBytes ptr = writeOffPtr ptr . coerce

instance LiftedPrim Int8 where
  indexArrayLiftedInBytes arr = indexByteArray arr . coerce
  readArrayLiftedInBytesK k marr = readByteArray marr . coerce >=> k
  writeArrayLiftedInBytes marr = writeByteArray marr . coerce
  indexPtrLiftedInBytes ptr = indexOffPtr (coerce ptr) . coerce
  readPtrLiftedInBytesK k ptr = readOffPtr (coerce ptr) . coerce >=> k
  writePtrLiftedInBytes ptr = writeOffPtr (coerce ptr) . coerce

-- | NOTE: Relies on same byte width of both types!
instance (Integral x, LiftedPrim x, Integral y, n ~ StaticSize x) => LiftedPrim (ViaFromIntegral n x y) where
  indexArrayLiftedInBytes arr = ViaFromIntegral . fromIntegral @x @y . indexArrayLiftedInBytes arr
  readArrayLiftedInBytesK k = readArrayLiftedInBytesK (k . ViaFromIntegral . fromIntegral @x @y)
  writeArrayLiftedInBytes marr off = writeArrayLiftedInBytes marr off . fromIntegral @y @x . unViaFromIntegral
  indexPtrLiftedInBytes ptr = ViaFromIntegral . fromIntegral @x @y . indexPtrLiftedInBytes ptr
  readPtrLiftedInBytesK k = readPtrLiftedInBytesK (k . ViaFromIntegral . fromIntegral @x @y)
  writePtrLiftedInBytes ptr off (ViaFromIntegral y) = writePtrLiftedInBytes ptr off (fromIntegral y :: x)

instance LiftedPrim Word16LE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
    in  Word16LE (mkWord16LE b0 b1)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    k (Word16LE (mkWord16LE b0 b1))

  writeArrayLiftedInBytes marr off w = do
    let (!b0, !b1) = unMkWord16LE (unWord16LE w)
    writeByteArray marr (coerce off) b0
    writeByteArray marr (coerce off + 1) b1

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
    in  Word16LE (mkWord16LE b0 b1)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    k (Word16LE (mkWord16LE b0 b1))

  writePtrLiftedInBytes ptr off w = do
    let (!b0, !b1) = unMkWord16LE (unWord16LE w)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1

instance LiftedPrim Word24LE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
    in  Word24LE (mkWord24LE b0 b1 b2)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    !b2 <- readByteArray marr (coerce off + 2)
    k (Word24LE (mkWord24LE b0 b1 b2))

  writeArrayLiftedInBytes marr off w = do
    let (!b0, !b1, !b2) = unMkWord24LE (unWord24LE w)
    writeByteArray marr (coerce off) b0
    writeByteArray marr (coerce off + 1) b1
    writeByteArray marr (coerce off + 2) b2

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
    in  Word24LE (mkWord24LE b0 b1 b2)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    !b2 <- readOffPtr ptr (coerce off + 2)
    k (Word24LE (mkWord24LE b0 b1 b2))

  writePtrLiftedInBytes ptr off w = do
    let (!b0, !b1, !b2) = unMkWord24LE (unWord24LE w)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1
    writeOffPtr ptr (coerce off + 2) b2

instance LiftedPrim Word32LE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
    in  Word32LE (mkWord32LE b0 b1 b2 b3)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    !b2 <- readByteArray marr (coerce off + 2)
    !b3 <- readByteArray marr (coerce off + 3)
    k (Word32LE (mkWord32LE b0 b1 b2 b3))

  writeArrayLiftedInBytes marr off w = do
    let (!b0, !b1, !b2, !b3) = unMkWord32LE (unWord32LE w)
    writeByteArray marr (coerce off) b0
    writeByteArray marr (coerce off + 1) b1
    writeByteArray marr (coerce off + 2) b2
    writeByteArray marr (coerce off + 3) b3

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
    in  Word32LE (mkWord32LE b0 b1 b2 b3)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    !b2 <- readOffPtr ptr (coerce off + 2)
    !b3 <- readOffPtr ptr (coerce off + 3)
    k (Word32LE (mkWord32LE b0 b1 b2 b3))

  writePtrLiftedInBytes ptr off w = do
    let (!b0, !b1, !b2, !b3) = unMkWord32LE (unWord32LE w)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1
    writeOffPtr ptr (coerce off + 2) b2
    writeOffPtr ptr (coerce off + 3) b3

instance LiftedPrim Word64LE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
        !b4 = indexByteArray arr (coerce off + 4)
        !b5 = indexByteArray arr (coerce off + 5)
        !b6 = indexByteArray arr (coerce off + 6)
        !b7 = indexByteArray arr (coerce off + 7)
    in  Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    !b2 <- readByteArray marr (coerce off + 2)
    !b3 <- readByteArray marr (coerce off + 3)
    !b4 <- readByteArray marr (coerce off + 4)
    !b5 <- readByteArray marr (coerce off + 5)
    !b6 <- readByteArray marr (coerce off + 6)
    !b7 <- readByteArray marr (coerce off + 7)
    k (Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7))

  writeArrayLiftedInBytes marr off w = do
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkWord64LE (unWord64LE w)
    writeByteArray marr (coerce off) b0
    writeByteArray marr (coerce off + 1) b1
    writeByteArray marr (coerce off + 2) b2
    writeByteArray marr (coerce off + 3) b3
    writeByteArray marr (coerce off + 4) b4
    writeByteArray marr (coerce off + 5) b5
    writeByteArray marr (coerce off + 6) b6
    writeByteArray marr (coerce off + 7) b7

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
        !b4 = indexOffPtr ptr (coerce off + 4)
        !b5 = indexOffPtr ptr (coerce off + 5)
        !b6 = indexOffPtr ptr (coerce off + 6)
        !b7 = indexOffPtr ptr (coerce off + 7)
    in  Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    !b2 <- readOffPtr ptr (coerce off + 2)
    !b3 <- readOffPtr ptr (coerce off + 3)
    !b4 <- readOffPtr ptr (coerce off + 4)
    !b5 <- readOffPtr ptr (coerce off + 5)
    !b6 <- readOffPtr ptr (coerce off + 6)
    !b7 <- readOffPtr ptr (coerce off + 7)
    k (Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7))

  writePtrLiftedInBytes ptr off w = do
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkWord64LE (unWord64LE w)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1
    writeOffPtr ptr (coerce off + 2) b2
    writeOffPtr ptr (coerce off + 3) b3
    writeOffPtr ptr (coerce off + 4) b4
    writeOffPtr ptr (coerce off + 5) b5
    writeOffPtr ptr (coerce off + 6) b6
    writeOffPtr ptr (coerce off + 7) b7

instance LiftedPrim FloatLE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
    in  FloatLE (mkFloatLE b0 b1 b2 b3)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    !b2 <- readByteArray marr (coerce off + 2)
    !b3 <- readByteArray marr (coerce off + 3)
    k (FloatLE (mkFloatLE b0 b1 b2 b3))

  writeArrayLiftedInBytes marr off f = do
    let (!b0, !b1, !b2, !b3) = unMkFloatLE (unFloatLE f)
    writeByteArray marr (coerce off) b0
    writeByteArray marr (coerce off + 1) b1
    writeByteArray marr (coerce off + 2) b2
    writeByteArray marr (coerce off + 3) b3

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
    in  FloatLE (mkFloatLE b0 b1 b2 b3)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    !b2 <- readOffPtr ptr (coerce off + 2)
    !b3 <- readOffPtr ptr (coerce off + 3)
    k (FloatLE (mkFloatLE b0 b1 b2 b3))

  writePtrLiftedInBytes ptr off f = do
    let (!b0, !b1, !b2, !b3) = unMkFloatLE (unFloatLE f)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1
    writeOffPtr ptr (coerce off + 2) b2
    writeOffPtr ptr (coerce off + 3) b3

instance LiftedPrim DoubleLE where
  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
        !b4 = indexByteArray arr (coerce off + 4)
        !b5 = indexByteArray arr (coerce off + 5)
        !b6 = indexByteArray arr (coerce off + 6)
        !b7 = indexByteArray arr (coerce off + 7)
    in  DoubleLE (mkDoubleLE b0 b1 b2 b3 b4 b5 b6 b7)

  readArrayLiftedInBytesK k marr off = do
    !b0 <- readByteArray marr (coerce off)
    !b1 <- readByteArray marr (coerce off + 1)
    !b2 <- readByteArray marr (coerce off + 2)
    !b3 <- readByteArray marr (coerce off + 3)
    !b4 <- readByteArray marr (coerce off + 4)
    !b5 <- readByteArray marr (coerce off + 5)
    !b6 <- readByteArray marr (coerce off + 6)
    !b7 <- readByteArray marr (coerce off + 7)
    k (DoubleLE (mkDoubleLE b0 b1 b2 b3 b4 b5 b6 b7))

  writeArrayLiftedInBytes arr off f = do
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkDoubleLE (unDoubleLE f)
    writeByteArray arr (coerce off) b0
    writeByteArray arr (coerce off + 1) b1
    writeByteArray arr (coerce off + 2) b2
    writeByteArray arr (coerce off + 3) b3
    writeByteArray arr (coerce off + 4) b4
    writeByteArray arr (coerce off + 5) b5
    writeByteArray arr (coerce off + 6) b6
    writeByteArray arr (coerce off + 7) b7

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
        !b4 = indexOffPtr ptr (coerce off + 4)
        !b5 = indexOffPtr ptr (coerce off + 5)
        !b6 = indexOffPtr ptr (coerce off + 6)
        !b7 = indexOffPtr ptr (coerce off + 7)
    in  DoubleLE (mkDoubleLE b0 b1 b2 b3 b4 b5 b6 b7)

  readPtrLiftedInBytesK k ptr off = do
    !b0 <- readOffPtr ptr (coerce off)
    !b1 <- readOffPtr ptr (coerce off + 1)
    !b2 <- readOffPtr ptr (coerce off + 2)
    !b3 <- readOffPtr ptr (coerce off + 3)
    !b4 <- readOffPtr ptr (coerce off + 4)
    !b5 <- readOffPtr ptr (coerce off + 5)
    !b6 <- readOffPtr ptr (coerce off + 6)
    !b7 <- readOffPtr ptr (coerce off + 7)
    k (DoubleLE (mkDoubleLE b0 b1 b2 b3 b4 b5 b6 b7))

  writePtrLiftedInBytes ptr off f = do
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkDoubleLE (unDoubleLE f)
    writeOffPtr ptr (coerce off) b0
    writeOffPtr ptr (coerce off + 1) b1
    writeOffPtr ptr (coerce off + 2) b2
    writeOffPtr ptr (coerce off + 3) b3
    writeOffPtr ptr (coerce off + 4) b4
    writeOffPtr ptr (coerce off + 5) b5
    writeOffPtr ptr (coerce off + 6) b6
    writeOffPtr ptr (coerce off + 7) b7

instance (LiftedPrim le, EndianPair n le be, n ~ StaticSize le) => LiftedPrim (ViaEndianPair n le be) where
  indexArrayLiftedInBytes arr = ViaEndianPair . toBigEndian . indexArrayLiftedInBytes arr
  readArrayLiftedInBytesK k = readArrayLiftedInBytesK (k . ViaEndianPair . toBigEndian)
  writeArrayLiftedInBytes arr off = writeArrayLiftedInBytes arr off . toLittleEndian . unViaEndianPair
  indexPtrLiftedInBytes ptr = ViaEndianPair . toBigEndian . indexPtrLiftedInBytes ptr
  readPtrLiftedInBytesK k = readPtrLiftedInBytesK (k . ViaEndianPair . toBigEndian)
  writePtrLiftedInBytes ptr off = writePtrLiftedInBytes ptr off . toLittleEndian . unViaEndianPair

deriving via (ViaFromIntegral 2 Word16LE Int16LE) instance LiftedPrim Int16LE

deriving via (ViaFromIntegral 2 Word16LE Word16) instance LiftedPrim Word16

deriving via (ViaFromIntegral 2 Word16LE Int16) instance LiftedPrim Int16

deriving via (ViaFromIntegral 3 Word24LE Int24LE) instance LiftedPrim Int24LE

deriving via (ViaFromIntegral 3 Word24LE Word24) instance LiftedPrim Word24

deriving via (ViaFromIntegral 3 Word24LE Int24) instance LiftedPrim Int24

deriving via (ViaFromIntegral 4 Word32LE Int32LE) instance LiftedPrim Int32LE

deriving via (ViaFromIntegral 4 Word32LE Word32) instance LiftedPrim Word32

deriving via (ViaFromIntegral 4 Word32LE Int32) instance LiftedPrim Int32

deriving via (ViaFromIntegral 8 Word64LE Int64LE) instance LiftedPrim Int64LE

deriving via (ViaFromIntegral 8 Word64LE Word64) instance LiftedPrim Word64

deriving via (ViaFromIntegral 8 Word64LE Int64) instance LiftedPrim Int64

deriving via (ViaEndianPair 2 Word16LE Word16BE) instance LiftedPrim Word16BE

deriving via (ViaEndianPair 2 Int16LE Int16BE) instance LiftedPrim Int16BE

deriving via (ViaEndianPair 3 Word24LE Word24BE) instance LiftedPrim Word24BE

deriving via (ViaEndianPair 3 Int24LE Int24BE) instance LiftedPrim Int24BE

deriving via (ViaEndianPair 4 Word32LE Word32BE) instance LiftedPrim Word32BE

deriving via (ViaEndianPair 4 Int32LE Int32BE) instance LiftedPrim Int32BE

deriving via (ViaEndianPair 8 Word64LE Word64BE) instance LiftedPrim Word64BE

deriving via (ViaEndianPair 8 Int64LE Int64BE) instance LiftedPrim Int64BE

deriving via (ViaEndianPair 4 FloatLE FloatBE) instance LiftedPrim FloatBE

deriving via (ViaEndianPair 8 DoubleLE DoubleBE) instance LiftedPrim DoubleBE

-- | Fill a byte array with the given value
setByteArrayLifted
  :: (PrimMonad m, LiftedPrim a) => MutableByteArray (PrimState m) -> ByteCount -> ByteCount -> a -> m ()
setByteArrayLifted arr off len val = do
  let elemSize = staticByteSize (proxyFor val)
      elemLen = div len elemSize
  for_ [0 .. elemLen - 1] $ \pos ->
    writeArrayLiftedInBytes arr (off + pos * elemSize) val
