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

-- Counts

newtype ByteCount = ByteCount {unByteCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

newtype ElemCount = ElemCount {unElemCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

-- StaticByteSized

class StaticByteSized a where
  staticByteSize :: Proxy a -> ByteCount

instance StaticByteSized () where
  staticByteSize _ = 0

instance StaticByteSized Word8 where
  staticByteSize _ = 1

instance StaticByteSized Int8 where
  staticByteSize _ = 1

instance StaticByteSized Word16 where
  staticByteSize _ = 2

instance StaticByteSized Int16 where
  staticByteSize _ = 2

instance StaticByteSized Word24 where
  staticByteSize _ = 3

instance StaticByteSized Int24 where
  staticByteSize _ = 3

instance StaticByteSized Word32 where
  staticByteSize _ = 4

instance StaticByteSized Int32 where
  staticByteSize _ = 4

instance StaticByteSized Word64 where
  staticByteSize _ = 8

instance StaticByteSized Int64 where
  staticByteSize _ = 8

instance StaticByteSized Float where
  staticByteSize _ = 4

instance StaticByteSized Double where
  staticByteSize _ = 8

instance StaticByteSized Bool where
  staticByteSize _ = 1

instance StaticByteSized Char where
  staticByteSize _ = 1

instance StaticByteSized Int where
  staticByteSize _ = 8

instance StaticByteSized Word16LE where
  staticByteSize _ = 2

instance StaticByteSized Int16LE where
  staticByteSize _ = 2

instance StaticByteSized Word24LE where
  staticByteSize _ = 3

instance StaticByteSized Int24LE where
  staticByteSize _ = 3

instance StaticByteSized Word32LE where
  staticByteSize _ = 4

instance StaticByteSized Int32LE where
  staticByteSize _ = 4

instance StaticByteSized Word64LE where
  staticByteSize _ = 8

instance StaticByteSized Int64LE where
  staticByteSize _ = 8

instance StaticByteSized FloatLE where
  staticByteSize _ = 4

instance StaticByteSized DoubleLE where
  staticByteSize _ = 8

instance StaticByteSized x => StaticByteSized (ViaFromIntegral x y) where
  staticByteSize _ = staticByteSize (Proxy :: Proxy x)

instance StaticByteSized le => StaticByteSized (ViaEndianPair le be) where
  staticByteSize _ = staticByteSize (Proxy :: Proxy le)

deriving via (ViaEndianPair Word16LE Word16BE) instance StaticByteSized Word16BE

deriving via (ViaEndianPair Int16LE Int16BE) instance StaticByteSized Int16BE

deriving via (ViaEndianPair Word24LE Word24BE) instance StaticByteSized Word24BE

deriving via (ViaEndianPair Int24LE Int24BE) instance StaticByteSized Int24BE

deriving via (ViaEndianPair Word32LE Word32BE) instance StaticByteSized Word32BE

deriving via (ViaEndianPair Int32LE Int32BE) instance StaticByteSized Int32BE

deriving via (ViaEndianPair Word64LE Word64BE) instance StaticByteSized Word64BE

deriving via (ViaEndianPair Int64LE Int64BE) instance StaticByteSized Int64BE

deriving via (ViaEndianPair FloatLE FloatBE) instance StaticByteSized FloatBE

deriving via (ViaEndianPair DoubleLE DoubleBE) instance StaticByteSized DoubleBE

staticByteSizeFoldable :: (Foldable f, StaticByteSized a) => f a -> ByteCount
staticByteSizeFoldable fa = staticByteSize (proxyForF fa) * coerce (length fa)

byteSizeViaStatic :: StaticByteSized a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor
