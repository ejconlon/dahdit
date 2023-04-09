{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Derived instances rely on the host system being little-endian.
-- If it's not, well... some CPP is in order.
module Dahdit.Nums
  ( EndianPair (..)
  , Word16LE (..)
  , Int16LE (..)
  , Word24LE (..)
  , Int24LE (..)
  , Word32LE (..)
  , Int32LE (..)
  , Word64LE (..)
  , Int64LE (..)
  , FloatLE (..)
  , DoubleLE (..)
  , Word16BE (..)
  , Int16BE (..)
  , Word24BE (..)
  , Int24BE (..)
  , Word32BE (..)
  , Int32BE (..)
  , Word64BE (..)
  , Int64BE (..)
  , FloatBE (..)
  , DoubleBE (..)
  )
where

import Dahdit.Counts (ByteCount (..))
import Dahdit.Internal
  ( ViaFromIntegral (..)
  , mkDoubleLE
  , mkFloatLE
  , mkWord16LE
  , mkWord24LE
  , mkWord32LE
  , mkWord64LE
  , swapEndian
  , unMkDoubleLE
  , unMkFloatLE
  , unMkWord16LE
  , unMkWord24LE
  , unMkWord32LE
  , unMkWord64LE
  )
import Dahdit.LiftedPrim (LiftedPrim (..))
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray (indexByteArray, writeByteArray)
import Data.Primitive.Ptr (indexOffPtr, writeOffPtr)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)

class (Num le, Num be) => EndianPair le be | le -> be, be -> le where
  toLittleEndian :: be -> le
  toBigEndian :: le -> be

newtype Word16LE = Word16LE {unWord16LE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

instance LiftedPrim Word16LE where
  elemSizeLifted _ = 2

  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
    in  Word16LE (mkWord16LE b0 b1)

  writeArrayLiftedInBytes arr off w =
    let (!b0, !b1) = unMkWord16LE (unWord16LE w)
    in  writeByteArray arr (coerce off) b0
          *> writeByteArray arr (coerce off + 1) b1

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
    in  Word16LE (mkWord16LE b0 b1)

  writePtrLiftedInBytes ptr off w =
    let (!b0, !b1) = unMkWord16LE (unWord16LE w)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1

newtype Int16LE = Int16LE {unInt16LE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaFromIntegral Word16LE Int16LE)

newtype Word24LE = Word24LE {unWord24LE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)

instance Default Word24LE where
  def = 0

instance LiftedPrim Word24LE where
  elemSizeLifted _ = 3

  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
    in  Word24LE (mkWord24LE b0 b1 b2)

  writeArrayLiftedInBytes arr off w = do
    let (!b0, !b1, !b2) = unMkWord24LE (unWord24LE w)
    writeByteArray arr (coerce off) b0
    writeByteArray arr (coerce off + 1) b1
    writeByteArray arr (coerce off + 2) b2

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
    in  Word24LE (mkWord24LE b0 b1 b2)

  writePtrLiftedInBytes ptr off w =
    let (!b0, !b1, !b2) = unMkWord24LE (unWord24LE w)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1
          *> writeOffPtr ptr (coerce off + 2) b2

newtype Int24LE = Int24LE {unInt24LE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)
  deriving (LiftedPrim) via (ViaFromIntegral Word24LE Int24LE)

instance Default Int24LE where
  def = 0

newtype Word32LE = Word32LE {unWord32LE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

instance LiftedPrim Word32LE where
  elemSizeLifted _ = 4

  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
    in  Word32LE (mkWord32LE b0 b1 b2 b3)

  writeArrayLiftedInBytes arr off w = do
    let (!b0, !b1, !b2, !b3) = unMkWord32LE (unWord32LE w)
    writeByteArray arr (coerce off) b0
    writeByteArray arr (coerce off + 1) b1
    writeByteArray arr (coerce off + 2) b2
    writeByteArray arr (coerce off + 3) b3

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
    in  Word32LE (mkWord32LE b0 b1 b2 b3)

  writePtrLiftedInBytes ptr off w =
    let (!b0, !b1, !b2, !b3) = unMkWord32LE (unWord32LE w)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1
          *> writeOffPtr ptr (coerce off + 2) b2
          *> writeOffPtr ptr (coerce off + 3) b3

newtype Word64LE = Word64LE {unWord64LE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

instance LiftedPrim Word64LE where
  elemSizeLifted _ = 8

  indexArrayLiftedInBytes arr off =
    let !b0 = indexArrayLiftedInBytes arr (coerce off)
        !b1 = indexArrayLiftedInBytes arr (coerce off + 1)
        !b2 = indexArrayLiftedInBytes arr (coerce off + 2)
        !b3 = indexArrayLiftedInBytes arr (coerce off + 3)
        !b4 = indexArrayLiftedInBytes arr (coerce off + 4)
        !b5 = indexArrayLiftedInBytes arr (coerce off + 5)
        !b6 = indexArrayLiftedInBytes arr (coerce off + 6)
        !b7 = indexArrayLiftedInBytes arr (coerce off + 7)
    in  Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7)

  writeArrayLiftedInBytes arr off w = do
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkWord64LE (unWord64LE w)
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
    in  Word64LE (mkWord64LE b0 b1 b2 b3 b4 b5 b6 b7)

  writePtrLiftedInBytes ptr off w =
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkWord64LE (unWord64LE w)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1
          *> writeOffPtr ptr (coerce off + 2) b2
          *> writeOffPtr ptr (coerce off + 3) b3
          *> writeOffPtr ptr (coerce off + 4) b4
          *> writeOffPtr ptr (coerce off + 5) b5
          *> writeOffPtr ptr (coerce off + 6) b6
          *> writeOffPtr ptr (coerce off + 7) b7

newtype Int32LE = Int32LE {unInt32LE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaFromIntegral Word32LE Int32LE)

newtype Int64LE = Int64LE {unInt64LE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaFromIntegral Word64LE Int64LE)

newtype FloatLE = FloatLE {unFloatLE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

instance LiftedPrim FloatLE where
  elemSizeLifted _ = 4

  indexArrayLiftedInBytes arr off =
    let !b0 = indexByteArray arr (coerce off)
        !b1 = indexByteArray arr (coerce off + 1)
        !b2 = indexByteArray arr (coerce off + 2)
        !b3 = indexByteArray arr (coerce off + 3)
    in  FloatLE (mkFloatLE b0 b1 b2 b3)

  writeArrayLiftedInBytes arr off f = do
    let (!b0, !b1, !b2, !b3) = unMkFloatLE (unFloatLE f)
    writeByteArray arr (coerce off) b0
    writeByteArray arr (coerce off + 1) b1
    writeByteArray arr (coerce off + 2) b2
    writeByteArray arr (coerce off + 3) b3

  indexPtrLiftedInBytes ptr off =
    let !b0 = indexOffPtr ptr (coerce off)
        !b1 = indexOffPtr ptr (coerce off + 1)
        !b2 = indexOffPtr ptr (coerce off + 2)
        !b3 = indexOffPtr ptr (coerce off + 3)
    in  FloatLE (mkFloatLE b0 b1 b2 b3)

  writePtrLiftedInBytes ptr off f =
    let (!b0, !b1, !b2, !b3) = unMkFloatLE (unFloatLE f)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1
          *> writeOffPtr ptr (coerce off + 2) b2
          *> writeOffPtr ptr (coerce off + 3) b3

newtype DoubleLE = DoubleLE {unDoubleLE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

instance LiftedPrim DoubleLE where
  elemSizeLifted _ = 8

  indexArrayLiftedInBytes arr off =
    let !b0 = indexArrayLiftedInBytes arr (coerce off)
        !b1 = indexArrayLiftedInBytes arr (coerce off + 1)
        !b2 = indexArrayLiftedInBytes arr (coerce off + 2)
        !b3 = indexArrayLiftedInBytes arr (coerce off + 3)
        !b4 = indexArrayLiftedInBytes arr (coerce off + 4)
        !b5 = indexArrayLiftedInBytes arr (coerce off + 5)
        !b6 = indexArrayLiftedInBytes arr (coerce off + 6)
        !b7 = indexArrayLiftedInBytes arr (coerce off + 7)
    in  DoubleLE (mkDoubleLE b0 b1 b2 b3 b4 b5 b6 b7)

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

  writePtrLiftedInBytes ptr off f =
    let (!b0, !b1, !b2, !b3, !b4, !b5, !b6, !b7) = unMkDoubleLE (unDoubleLE f)
    in  writeOffPtr ptr (coerce off) b0
          *> writeOffPtr ptr (coerce off + 1) b1
          *> writeOffPtr ptr (coerce off + 2) b2
          *> writeOffPtr ptr (coerce off + 3) b3
          *> writeOffPtr ptr (coerce off + 4) b4
          *> writeOffPtr ptr (coerce off + 5) b5
          *> writeOffPtr ptr (coerce off + 6) b6
          *> writeOffPtr ptr (coerce off + 7) b7

newtype Word16BE = Word16BE {unWord16BE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Word16LE Word16BE)

newtype Int16BE = Int16BE {unInt16BE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Int16LE Int16BE)

newtype Word24BE = Word24BE {unWord24BE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)
  deriving (LiftedPrim) via (ViaEndianPair Word24LE Word24BE)

instance Default Word24BE where
  def = 0

newtype Int24BE = Int24BE {unInt24BE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)
  deriving (LiftedPrim) via (ViaEndianPair Int24LE Int24BE)

instance Default Int24BE where
  def = 0

newtype Word32BE = Word32BE {unWord32BE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Word32LE Word32BE)

newtype Int32BE = Int32BE {unInt32BE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Int32LE Int32BE)

newtype Word64BE = Word64BE {unWord64BE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Word64LE Word64BE)

newtype Int64BE = Int64BE {unInt64BE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaEndianPair Int64LE Int64BE)

newtype FloatBE = FloatBE {unFloatBE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (LiftedPrim) via (ViaEndianPair FloatLE FloatBE)

newtype DoubleBE = DoubleBE {unDoubleBE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (LiftedPrim) via (ViaEndianPair DoubleLE DoubleBE)

instance EndianPair Word8 Word8 where
  toLittleEndian = id
  toBigEndian = id

instance EndianPair Int8 Int8 where
  toLittleEndian = id
  toBigEndian = id

instance EndianPair Word16LE Word16BE where
  toLittleEndian = Word16LE . swapEndian . unWord16BE
  toBigEndian = Word16BE . swapEndian . unWord16LE

instance EndianPair Int16LE Int16BE where
  toLittleEndian = Int16LE . swapEndian . unInt16BE
  toBigEndian = Int16BE . swapEndian . unInt16LE

instance EndianPair Word24LE Word24BE where
  toLittleEndian = Word24LE . swapEndian . unWord24BE
  toBigEndian = Word24BE . swapEndian . unWord24LE

instance EndianPair Int24LE Int24BE where
  toLittleEndian = Int24LE . swapEndian . unInt24BE
  toBigEndian = Int24BE . swapEndian . unInt24LE

instance EndianPair Word32LE Word32BE where
  toLittleEndian = Word32LE . swapEndian . unWord32BE
  toBigEndian = Word32BE . swapEndian . unWord32LE

instance EndianPair Int32LE Int32BE where
  toLittleEndian = Int32LE . swapEndian . unInt32BE
  toBigEndian = Int32BE . swapEndian . unInt32LE

instance EndianPair Word64LE Word64BE where
  toLittleEndian = Word64LE . swapEndian . unWord64BE
  toBigEndian = Word64BE . swapEndian . unWord64LE

instance EndianPair Int64LE Int64BE where
  toLittleEndian = Int64LE . swapEndian . unInt64BE
  toBigEndian = Int64BE . swapEndian . unInt64LE

instance EndianPair FloatLE FloatBE where
  toLittleEndian = FloatLE . swapEndian . unFloatBE
  toBigEndian = FloatBE . swapEndian . unFloatLE

instance EndianPair DoubleLE DoubleBE where
  toLittleEndian = DoubleLE . swapEndian . unDoubleBE
  toBigEndian = DoubleBE . swapEndian . unDoubleLE

newtype ViaEndianPair le be = ViaEndianPair {unViaEndianPair :: be}

instance (LiftedPrim le, EndianPair le be) => LiftedPrim (ViaEndianPair le be) where
  elemSizeLifted _ = elemSizeLifted (Proxy :: Proxy le)
  indexArrayLiftedInBytes arr off = ViaEndianPair (toBigEndian (indexArrayLiftedInBytes arr off))
  writeArrayLiftedInBytes arr off = writeArrayLiftedInBytes arr off . toLittleEndian . unViaEndianPair
  indexPtrLiftedInBytes ptr off = ViaEndianPair (toBigEndian (indexPtrLiftedInBytes ptr off))
  writePtrLiftedInBytes ptr off = writePtrLiftedInBytes ptr off . toLittleEndian . unViaEndianPair
