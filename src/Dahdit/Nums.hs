{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Derived instances rely on the host system being little-endian.
-- If it's not, well... some CPP is in order.
module Dahdit.Nums
  ( Word16LE (..)
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

import Dahdit.Internal (EndianPair (..), swapEndian)
import Data.Bits (Bits)
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64)
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64)

newtype Word16LE = Word16LE {unWord16LE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int16LE = Int16LE {unInt16LE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Word24LE = Word24LE {unWord24LE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)

instance Default Word24LE where
  def = 0

newtype Int24LE = Int24LE {unInt24LE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)

instance Default Int24LE where
  def = 0

newtype Word32LE = Word32LE {unWord32LE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Word64LE = Word64LE {unWord64LE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int32LE = Int32LE {unInt32LE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int64LE = Int64LE {unInt64LE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype FloatLE = FloatLE {unFloatLE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

newtype DoubleLE = DoubleLE {unDoubleLE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

newtype Word16BE = Word16BE {unWord16BE :: Word16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int16BE = Int16BE {unInt16BE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Word24BE = Word24BE {unWord24BE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)

instance Default Word24BE where
  def = 0

newtype Int24BE = Int24BE {unInt24BE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits)

instance Default Int24BE where
  def = 0

newtype Word32BE = Word32BE {unWord32BE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int32BE = Int32BE {unInt32BE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Word64BE = Word64BE {unWord64BE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype Int64BE = Int64BE {unInt64BE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Bounded, Real, Integral, Bits, Default)

newtype FloatBE = FloatBE {unFloatBE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

newtype DoubleBE = DoubleBE {unDoubleBE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)

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
