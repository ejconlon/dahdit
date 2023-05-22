{-# LANGUAGE UndecidableInstances #-}

module Dahdit.Sizes
  ( ByteCount (..)
  , ElemCount (..)
  , StaticByteSized (..)
  , staticByteSizeFoldable
  , byteSizeViaStatic
  )
where

import Dahdit.Internal (ViaEndianPair (..), ViaFromIntegral (..))
import Dahdit.Nums
  ( DoubleBE
  , DoubleLE
  , FloatBE
  , FloatLE
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , Word16BE
  , Word16LE
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  , Word64BE
  , Word64LE
  )
import Dahdit.Proxy (proxyFor, proxyForF)
import Data.Coerce (coerce)
import Data.Default (Default)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, Nat, natVal)

-- Counts

newtype ByteCount = ByteCount {unByteCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

newtype ElemCount = ElemCount {unElemCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

-- StaticByteSized

class KnownNat (StaticSize a) => StaticByteSized a where
  type StaticSize a :: Nat
  staticByteSize :: Proxy a -> ByteCount
  staticByteSize = fromInteger . natVal . staticByteProxy

staticByteProxy :: Proxy a -> Proxy (StaticSize a)
staticByteProxy _ = Proxy

instance StaticByteSized () where
  type StaticSize () = 0
  staticByteSize _ = 0

instance StaticByteSized Word8 where
  type StaticSize Word8 = 1
  staticByteSize _ = 1

instance StaticByteSized Int8 where
  type StaticSize Int8 = 1
  staticByteSize _ = 1

instance StaticByteSized Word16 where
  type StaticSize Word16 = 2
  staticByteSize _ = 2

instance StaticByteSized Int16 where
  type StaticSize Int16 = 2
  staticByteSize _ = 2

instance StaticByteSized Word24 where
  type StaticSize Word24 = 3
  staticByteSize _ = 3

instance StaticByteSized Int24 where
  type StaticSize Int24 = 3
  staticByteSize _ = 3

instance StaticByteSized Word32 where
  type StaticSize Word32 = 4
  staticByteSize _ = 4

instance StaticByteSized Int32 where
  type StaticSize Int32 = 4
  staticByteSize _ = 4

instance StaticByteSized Word64 where
  type StaticSize Word64 = 8
  staticByteSize _ = 8

instance StaticByteSized Int64 where
  type StaticSize Int64 = 8
  staticByteSize _ = 8

instance StaticByteSized Float where
  type StaticSize Float = 4
  staticByteSize _ = 4

instance StaticByteSized Double where
  type StaticSize Double = 8
  staticByteSize _ = 8

instance StaticByteSized Bool where
  type StaticSize Bool = 1
  staticByteSize _ = 1

instance StaticByteSized Char where
  type StaticSize Char = 1
  staticByteSize _ = 1

instance StaticByteSized Int where
  type StaticSize Int = 8
  staticByteSize _ = 8

instance StaticByteSized Word16LE where
  type StaticSize Word16LE = 2
  staticByteSize _ = 2

instance StaticByteSized Int16LE where
  type StaticSize Int16LE = 2
  staticByteSize _ = 2

instance StaticByteSized Word24LE where
  type StaticSize Word24LE = 3
  staticByteSize _ = 3

instance StaticByteSized Int24LE where
  type StaticSize Int24LE = 3
  staticByteSize _ = 3

instance StaticByteSized Word32LE where
  type StaticSize Word32LE = 4
  staticByteSize _ = 4

instance StaticByteSized Int32LE where
  type StaticSize Int32LE = 4
  staticByteSize _ = 4

instance StaticByteSized Word64LE where
  type StaticSize Word64LE = 8
  staticByteSize _ = 8

instance StaticByteSized Int64LE where
  type StaticSize Int64LE = 8
  staticByteSize _ = 8

instance StaticByteSized FloatLE where
  type StaticSize FloatLE = 4
  staticByteSize _ = 4

instance StaticByteSized DoubleLE where
  type StaticSize DoubleLE = 8
  staticByteSize _ = 8

instance (StaticByteSized x, n ~ StaticSize x) => StaticByteSized (ViaFromIntegral n x y) where
  type StaticSize (ViaFromIntegral n x y) = n
  staticByteSize _ = staticByteSize (Proxy :: Proxy x)

instance (StaticByteSized le, n ~ StaticSize le) => StaticByteSized (ViaEndianPair n le be) where
  type StaticSize (ViaEndianPair n le be) = n
  staticByteSize _ = staticByteSize (Proxy :: Proxy le)

deriving via (ViaEndianPair 2 Word16LE Word16BE) instance StaticByteSized Word16BE

deriving via (ViaEndianPair 2 Int16LE Int16BE) instance StaticByteSized Int16BE

deriving via (ViaEndianPair 3 Word24LE Word24BE) instance StaticByteSized Word24BE

deriving via (ViaEndianPair 3 Int24LE Int24BE) instance StaticByteSized Int24BE

deriving via (ViaEndianPair 4 Word32LE Word32BE) instance StaticByteSized Word32BE

deriving via (ViaEndianPair 4 Int32LE Int32BE) instance StaticByteSized Int32BE

deriving via (ViaEndianPair 8 Word64LE Word64BE) instance StaticByteSized Word64BE

deriving via (ViaEndianPair 8 Int64LE Int64BE) instance StaticByteSized Int64BE

deriving via (ViaEndianPair 4 FloatLE FloatBE) instance StaticByteSized FloatBE

deriving via (ViaEndianPair 8 DoubleLE DoubleBE) instance StaticByteSized DoubleBE

staticByteSizeFoldable :: (Foldable f, StaticByteSized a) => f a -> ByteCount
staticByteSizeFoldable fa = staticByteSize (proxyForF fa) * coerce (length fa)

byteSizeViaStatic :: StaticByteSized a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor
