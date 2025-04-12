{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internally, all numbers are represented in little-endian format
-- (since that is what the host endianness is on any arch we'd be using).
-- 'Prim' instances for big endian variants actually do the conversion
-- at read/write time.
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

import Dahdit.ShortWord ()
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import System.ByteOrder (Bytes (..))

newtype Word16LE = Word16LE {unWord16LE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word24LE = Word24LE {unWord24LE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word32LE = Word32LE {unWord32LE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word64LE = Word64LE {unWord64LE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int16LE = Int16LE {unInt16LE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int24LE = Int24LE {unInt24LE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int32LE = Int32LE {unInt32LE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int64LE = Int64LE {unInt64LE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype FloatLE = FloatLE {unFloatLE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

newtype DoubleLE = DoubleLE {unDoubleLE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

newtype Word16BE = Word16BE {unWord16BE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Word16 Word16BE)

newtype Word24BE = Word24BE {unWord24BE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Word24 Word24BE)

newtype Word32BE = Word32BE {unWord32BE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Word32 Word32BE)

newtype Word64BE = Word64BE {unWord64BE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Word64 Word64BE)

newtype Int16BE = Int16BE {unInt16BE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Int16 Int16BE)

newtype Int24BE = Int24BE {unInt24BE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Int24 Int24BE)

newtype Int32BE = Int32BE {unInt32BE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Int32 Int32BE)

newtype Int64BE = Int64BE {unInt64BE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaLittleToBig Int64 Int64BE)

newtype FloatBE = FloatBE {unFloatBE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (Prim) via (ViaLittleToBig Float FloatBE)

newtype DoubleBE = DoubleBE {unDoubleBE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (Prim) via (ViaLittleToBig Double DoubleBE)

class (Coercible le x, Coercible be x) => EndianPair x le be | x -> le be, le -> x be, be -> x le where
  toLittleEndianType :: x -> le
  toLittleEndianType = coerce
  fromLittleEndianType :: le -> x
  fromLittleEndianType = coerce
  toBigEndianType :: x -> be
  toBigEndianType = coerce
  fromBigEndianType :: be -> x
  fromBigEndianType = coerce
  swapToLittleEndian :: x -> x
  swapToBigEndian :: x -> x

floatToLittleEndian :: Float -> Float
floatToLittleEndian = castWord32ToFloat . toLittleEndian . castFloatToWord32

floatToBigEndian :: Float -> Float
floatToBigEndian = castWord32ToFloat . toBigEndian . castFloatToWord32

doubleToLittleEndian :: Double -> Double
doubleToLittleEndian = castWord64ToDouble . toLittleEndian . castDoubleToWord64

doubleToBigEndian :: Double -> Double
doubleToBigEndian = castWord64ToDouble . toBigEndian . castDoubleToWord64

instance EndianPair Word8 Word8 Word8 where
  swapToLittleEndian = id
  swapToBigEndian = id

instance EndianPair Int8 Int8 Int8 where
  swapToLittleEndian = id
  swapToBigEndian = id

instance EndianPair Word16 Word16LE Word16BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Int16 Int16LE Int16BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Word24 Word24LE Word24BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Int24 Int24LE Int24BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Word32 Word32LE Word32BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Int32 Int32LE Int32BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Word64 Word64LE Word64BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Int64 Int64LE Int64BE where
  swapToLittleEndian = toLittleEndian
  swapToBigEndian = toBigEndian

instance EndianPair Float FloatLE FloatBE where
  swapToLittleEndian = floatToLittleEndian
  swapToBigEndian = floatToBigEndian

instance EndianPair Double DoubleLE DoubleBE where
  swapToLittleEndian = doubleToLittleEndian
  swapToBigEndian = doubleToBigEndian

newtype ViaLittleToBig x be = ViaLittleToBig {unViaLittleToBig :: be}

instance (Prim x, EndianPair x le be) => Prim (ViaLittleToBig x be) where
  sizeOfType# _ = sizeOfType# (Proxy @x)
  sizeOf# _ = sizeOf# (undefined :: x)
  alignmentOfType# _ = alignmentOfType# (Proxy @x)
  alignment# _ = alignment# (undefined :: x)
  indexByteArray# ba i = ViaLittleToBig (toBigEndianType (swapToBigEndian (indexByteArray# ba i)))
  readByteArray# ba i s =
    let !(# s', x #) = readByteArray# ba i s
    in  (# s', ViaLittleToBig (toBigEndianType (swapToBigEndian x)) #)
  writeByteArray# ba i (ViaLittleToBig be) = writeByteArray# ba i (swapToBigEndian (fromBigEndianType be))
  indexOffAddr# addr i = ViaLittleToBig (toBigEndianType (swapToBigEndian (indexOffAddr# addr i)))
  readOffAddr# addr i s =
    let !(# s', x #) = readOffAddr# addr i s
    in  (# s', ViaLittleToBig (toBigEndianType (swapToBigEndian x)) #)
  writeOffAddr# addr i (ViaLittleToBig be) =
    writeOffAddr# addr i (swapToBigEndian (fromBigEndianType be))
