{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim)
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import System.ByteOrder (Bytes (..))

class (Num le, Num be) => EndianPair le be | le -> be, be -> le where
  toLittleEndianType :: be -> le
  toBigEndianType :: le -> be

instance EndianPair Word8 Word8 where
  toLittleEndianType = id
  toBigEndianType = id

instance EndianPair Int8 Int8 where
  toLittleEndianType = id
  toBigEndianType = id

floatToLittleEndian :: Float -> Float
floatToLittleEndian = castWord32ToFloat . toLittleEndian . castFloatToWord32

floatToBigEndian :: Float -> Float
floatToBigEndian = castWord32ToFloat . toBigEndian . castFloatToWord32

doubleToLittleEndian :: Double -> Double
doubleToLittleEndian = castWord64ToDouble . toLittleEndian . castDoubleToWord64

doubleToBigEndian :: Double -> Double
doubleToBigEndian = castWord64ToDouble . toBigEndian . castDoubleToWord64

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
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word24BE = Word24BE {unWord24BE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word32BE = Word32BE {unWord32BE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Word64BE = Word64BE {unWord64BE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int16BE = Int16BE {unInt16BE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int24BE = Int24BE {unInt24BE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int32BE = Int32BE {unInt32BE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype Int64BE = Int64BE {unInt64BE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Prim, Default, Bits, FiniteBits, Bounded)

newtype FloatBE = FloatBE {unFloatBE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

newtype DoubleBE = DoubleBE {unDoubleBE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

instance EndianPair Word16LE Word16BE where
  toLittleEndianType = Word16LE . toLittleEndian . unWord16BE
  toBigEndianType = Word16BE . toBigEndian . unWord16LE

instance EndianPair Int16LE Int16BE where
  toLittleEndianType = Int16LE . toLittleEndian . unInt16BE
  toBigEndianType = Int16BE . toBigEndian . unInt16LE

instance EndianPair Word24LE Word24BE where
  toLittleEndianType = Word24LE . toLittleEndian . unWord24BE
  toBigEndianType = Word24BE . toBigEndian . unWord24LE

instance EndianPair Int24LE Int24BE where
  toLittleEndianType = Int24LE . toLittleEndian . unInt24BE
  toBigEndianType = Int24BE . toBigEndian . unInt24LE

instance EndianPair Word32LE Word32BE where
  toLittleEndianType = Word32LE . toLittleEndian . unWord32BE
  toBigEndianType = Word32BE . toBigEndian . unWord32LE

instance EndianPair Int32LE Int32BE where
  toLittleEndianType = Int32LE . toLittleEndian . unInt32BE
  toBigEndianType = Int32BE . toBigEndian . unInt32LE

instance EndianPair Word64LE Word64BE where
  toLittleEndianType = Word64LE . toLittleEndian . unWord64BE
  toBigEndianType = Word64BE . toBigEndian . unWord64LE

instance EndianPair Int64LE Int64BE where
  toLittleEndianType = Int64LE . toLittleEndian . unInt64BE
  toBigEndianType = Int64BE . toBigEndian . unInt64LE

instance EndianPair FloatLE FloatBE where
  toLittleEndianType = FloatLE . floatToLittleEndian . unFloatBE
  toBigEndianType = FloatBE . floatToBigEndian . unFloatLE

instance EndianPair DoubleLE DoubleBE where
  toLittleEndianType = DoubleLE . doubleToLittleEndian . unDoubleBE
  toBigEndianType = DoubleBE . doubleToBigEndian . unDoubleLE
