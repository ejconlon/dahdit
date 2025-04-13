{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internally, all numbers are represented in little-endian format
-- (since that is what the host endianness is on any arch we'd be using).
-- 'Prim' instances for big endian variants actually do the conversion
-- at read/write time.
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

import Dahdit.ShortWord ()
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)

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
  deriving (Prim) via (ViaSwapEndian Word16 Word16BE)

newtype Word24BE = Word24BE {unWord24BE :: Word24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Word24 Word24BE)

newtype Word32BE = Word32BE {unWord32BE :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Word32 Word32BE)

newtype Word64BE = Word64BE {unWord64BE :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Word64 Word64BE)

newtype Int16BE = Int16BE {unInt16BE :: Int16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Int16 Int16BE)

newtype Int24BE = Int24BE {unInt24BE :: Int24}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Int24 Int24BE)

newtype Int32BE = Int32BE {unInt32BE :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Int32 Int32BE)

newtype Int64BE = Int64BE {unInt64BE :: Int64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Default, Bits, FiniteBits, Bounded)
  deriving (Prim) via (ViaSwapEndian Int64 Int64BE)

newtype FloatBE = FloatBE {unFloatBE :: Float}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (Prim) via (ViaSwapEndian Float FloatBE)

newtype DoubleBE = DoubleBE {unDoubleBE :: Double}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default)
  deriving (Prim) via (ViaSwapEndian Double DoubleBE)

byteSwap24 :: Word24 -> Word24
byteSwap24 = fromIntegral . flip shiftR 8 . byteSwap32 . fromIntegral
{-# INLINE byteSwap24 #-}

class (Coercible le x, Coercible be x) => SwapEndian x le be | x -> le be, le -> x be, be -> x le where
  swapEndian :: x -> x

instance SwapEndian Word8 Word8 Word8 where
  swapEndian = id

instance SwapEndian Int8 Int8 Int8 where
  swapEndian = id

instance SwapEndian Word16 Word16LE Word16BE where
  swapEndian = byteSwap16

instance SwapEndian Int16 Int16LE Int16BE where
  swapEndian = fromIntegral . byteSwap16 . fromIntegral

instance SwapEndian Word24 Word24LE Word24BE where
  swapEndian = byteSwap24

instance SwapEndian Int24 Int24LE Int24BE where
  swapEndian = fromIntegral . byteSwap24 . fromIntegral

instance SwapEndian Word32 Word32LE Word32BE where
  swapEndian = byteSwap32

instance SwapEndian Int32 Int32LE Int32BE where
  swapEndian = fromIntegral . byteSwap32 . fromIntegral

instance SwapEndian Word64 Word64LE Word64BE where
  swapEndian = byteSwap64

instance SwapEndian Int64 Int64LE Int64BE where
  swapEndian = fromIntegral . byteSwap64 . fromIntegral

instance SwapEndian Float FloatLE FloatBE where
  swapEndian = castWord32ToFloat . byteSwap32 . castFloatToWord32

instance SwapEndian Double DoubleLE DoubleBE where
  swapEndian = castWord64ToDouble . byteSwap64 . castDoubleToWord64

newtype ViaSwapEndian x be = ViaSwapEndian {unViaSwapEndian :: be}

instance (Prim x, SwapEndian x le be) => Prim (ViaSwapEndian x be) where
  sizeOfType# _ = sizeOfType# (Proxy @x)
  sizeOf# _ = sizeOf# (undefined :: x)
  alignmentOfType# _ = alignmentOfType# (Proxy @x)
  alignment# _ = alignment# (undefined :: x)
  indexByteArray# ba i = ViaSwapEndian (coerce @x @be (swapEndian (indexByteArray# ba i)))
  readByteArray# ba i s =
    let !(# s', x #) = readByteArray# ba i s
    in  (# s', ViaSwapEndian (coerce @x @be (swapEndian x)) #)
  writeByteArray# ba i (ViaSwapEndian be) = writeByteArray# ba i (swapEndian (coerce @be @x be))
  indexOffAddr# addr i = ViaSwapEndian (coerce @x @be (swapEndian (indexOffAddr# addr i)))
  readOffAddr# addr i s =
    let !(# s', x #) = readOffAddr# addr i s
    in  (# s', ViaSwapEndian (coerce @x @be (swapEndian x)) #)
  writeOffAddr# addr i (ViaSwapEndian be) =
    writeOffAddr# addr i (swapEndian (coerce @be @x be))
