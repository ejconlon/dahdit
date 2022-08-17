{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- The derived instances here work for little-endian, which covers intel/arm.
-- Custom instances will have to be provided for big endian or to support portability.
module Dahdit.Nums
  ( Word16LE (..)
  , Int16LE (..)
  , Word24LE (..)
  , Int24LE (..)
  , Word32LE (..)
  , Int32LE (..)
  , FloatLE (..)
  , Word16BE (..)
  , Int16BE (..)
  , Word32BE (..)
  , Int32BE (..)
  ) where

import Dahdit.Sizes (ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits (..))
import Data.Default (Default (..))
import Data.Int (Int16, Int32)
import Data.Primitive.ByteArray (indexByteArray, writeByteArray)
import Data.Primitive.Types (Prim (..))
import Data.Word (Word16, Word32)
import Data.ShortWord (Int24, Word24)
import Dahdit.LiftedPrim (LiftedPrim (..))
import Dahdit.Internal (ViaFromIntegral (..), mkWord16LE, unMkWord16LE, mkWord24LE, unMkWord24LE, mkWord32LE, unMkWord32LE, unMkFloatLE, mkFloatLE)

newtype Word16LE = Word16LE { unWord16LE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)

instance ByteSized Word16LE where
  byteSize _ = 2

instance StaticByteSized Word16LE where
  staticByteSize _ = 2

instance LiftedPrim Word16LE where
  elemSizeLifted _ = 2

  indexByteArrayLiftedInBytes arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
    in Word16LE (mkWord16LE b0 b1)

  writeByteArrayLiftedInBytes w arr pos =
    let !(b0, b1) = unMkWord16LE (unWord16LE w)
    in writeByteArray arr pos b0 *> writeByteArray arr (pos + 1) b1

newtype Int16LE = Int16LE { unInt16LE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)
  deriving (LiftedPrim) via (ViaFromIntegral Word16LE Int16LE)

instance ByteSized Int16LE where
  byteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2

newtype Word24LE = Word24LE { unWord24LE :: Word24 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Word24LE where
  byteSize _ = 3

instance StaticByteSized Word24LE where
  staticByteSize _ = 3

instance LiftedPrim Word24LE where
  elemSizeLifted _ = 3

  indexByteArrayLiftedInBytes arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
        !b2 = indexByteArray arr (pos + 2)
    in Word24LE (mkWord24LE b0 b1 b2)

  writeByteArrayLiftedInBytes w arr pos = do
    let !(b0, b1, b2) = unMkWord24LE (unWord24LE w)
    writeByteArray arr pos b0
    writeByteArray arr (pos + 1) b1
    writeByteArray arr (pos + 2) b2

newtype Int24LE = Int24LE { unInt24LE :: Int24 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)
  deriving (LiftedPrim) via (ViaFromIntegral Word24LE Int24LE)

instance ByteSized Int24LE where
  byteSize _ = 3

instance StaticByteSized Int24LE where
  staticByteSize _ = 3

newtype Word32LE = Word32LE { unWord32LE :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)

instance ByteSized Word32LE where
  byteSize _ = 4

instance StaticByteSized Word32LE where
  staticByteSize _ = 4

instance LiftedPrim Word32LE where
  elemSizeLifted _ = 4

  indexByteArrayLiftedInBytes arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
        !b2 = indexByteArray arr (pos + 2)
        !b3 = indexByteArray arr (pos + 3)
    in Word32LE (mkWord32LE b0 b1 b2 b3)

  writeByteArrayLiftedInBytes w arr pos = do
    let !(b0, b1, b2, b3) = unMkWord32LE (unWord32LE w)
    writeByteArray arr pos b0
    writeByteArray arr (pos + 1) b1
    writeByteArray arr (pos + 2) b2
    writeByteArray arr (pos + 3) b3

newtype Int32LE = Int32LE { unInt32LE :: Int32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default, Prim)
  deriving (LiftedPrim) via (ViaFromIntegral Word32LE Int32LE)

instance ByteSized Int32LE where
  byteSize _ = 4

instance StaticByteSized Int32LE where
  staticByteSize _ = 4

newtype FloatLE = FloatLE { unFloatLE :: Float }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Real, Fractional, Floating, RealFrac, Default, Prim)

instance ByteSized FloatLE where
  byteSize _ = 4

instance StaticByteSized FloatLE where
  staticByteSize _ = 4

instance LiftedPrim FloatLE where
  elemSizeLifted _ = 4

  indexByteArrayLiftedInBytes arr pos =
    let !b0 = indexByteArray arr pos
        !b1 = indexByteArray arr (pos + 1)
        !b2 = indexByteArray arr (pos + 2)
        !b3 = indexByteArray arr (pos + 3)
    in FloatLE (mkFloatLE b0 b1 b2 b3)

  writeByteArrayLiftedInBytes f arr pos = do
    let !(b0, b1, b2, b3) = unMkFloatLE (unFloatLE f)
    writeByteArray arr pos b0
    writeByteArray arr (pos + 1) b1
    writeByteArray arr (pos + 2) b2
    writeByteArray arr (pos + 3) b3

newtype Word16BE = Word16BE { unWord16BE :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Word16BE where
  byteSize _ = 2

instance StaticByteSized Word16BE where
  staticByteSize _ = 2

newtype Int16BE = Int16BE { unInt16BE :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Int16BE where
  byteSize _ = 2

instance StaticByteSized Int16BE where
  staticByteSize _ = 2

newtype Word32BE = Word32BE { unWord32BE :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Word32BE where
  byteSize _ = 4

instance StaticByteSized Word32BE where
  staticByteSize _ = 4

newtype Int32BE = Int32BE { unInt32BE :: Int32 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bits, Default)

instance ByteSized Int32BE where
  byteSize _ = 4

instance StaticByteSized Int32BE where
  staticByteSize _ = 4
