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
import GHC.TypeLits (Nat, natVal, KnownNat)

-- Counts

newtype ByteCount = ByteCount {unByteCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

newtype ElemCount = ElemCount {unElemCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

-- StaticByteSized

class KnownNat n => StaticByteSized (n :: Nat) a | a -> n where
  staticByteSize :: Proxy a -> ByteCount
  staticByteSize = fromInteger . natVal . staticByteProxy

-- NOTE Class is there to allow correct type inference via fun deps
staticByteProxy :: StaticByteSized n a => Proxy a -> Proxy n
staticByteProxy _ = Proxy

instance StaticByteSized 0 () where
  staticByteSize _ = 0

instance StaticByteSized 1 Word8 where
  staticByteSize _ = 1

instance StaticByteSized 1 Int8 where
  staticByteSize _ = 1

instance StaticByteSized 2 Word16 where
  staticByteSize _ = 2

instance StaticByteSized 2 Int16 where
  staticByteSize _ = 2

instance StaticByteSized 3 Word24 where
  staticByteSize _ = 3

instance StaticByteSized 3 Int24 where
  staticByteSize _ = 3

instance StaticByteSized 4 Word32 where
  staticByteSize _ = 4

instance StaticByteSized 4 Int32 where
  staticByteSize _ = 4

instance StaticByteSized 8 Word64 where
  staticByteSize _ = 8

instance StaticByteSized 8 Int64 where
  staticByteSize _ = 8

instance StaticByteSized 4 Float where
  staticByteSize _ = 4

instance StaticByteSized 8 Double where
  staticByteSize _ = 8

instance StaticByteSized 1 Bool where
  staticByteSize _ = 1

instance StaticByteSized 1 Char where
  staticByteSize _ = 1

instance StaticByteSized 8 Int where
  staticByteSize _ = 8

instance StaticByteSized 2 Word16LE where
  staticByteSize _ = 2

instance StaticByteSized 2 Int16LE where
  staticByteSize _ = 2

instance StaticByteSized 3 Word24LE where
  staticByteSize _ = 3

instance StaticByteSized 3 Int24LE where
  staticByteSize _ = 3

instance StaticByteSized 4 Word32LE where
  staticByteSize _ = 4

instance StaticByteSized 4 Int32LE where
  staticByteSize _ = 4

instance StaticByteSized 8 Word64LE where
  staticByteSize _ = 8

instance StaticByteSized 8 Int64LE where
  staticByteSize _ = 8

instance StaticByteSized 4 FloatLE where
  staticByteSize _ = 4

instance StaticByteSized 8 DoubleLE where
  staticByteSize _ = 8

instance StaticByteSized n x => StaticByteSized n (ViaFromIntegral n x y) where
  staticByteSize _ = staticByteSize (Proxy :: Proxy x)

instance StaticByteSized n le => StaticByteSized n (ViaEndianPair n le be) where
  staticByteSize _ = staticByteSize (Proxy :: Proxy le)

deriving via (ViaEndianPair 2 Word16LE Word16BE) instance StaticByteSized 2 Word16BE

deriving via (ViaEndianPair 2 Int16LE Int16BE) instance StaticByteSized 2 Int16BE

deriving via (ViaEndianPair 3 Word24LE Word24BE) instance StaticByteSized 3 Word24BE

deriving via (ViaEndianPair 3 Int24LE Int24BE) instance StaticByteSized 3 Int24BE

deriving via (ViaEndianPair 4 Word32LE Word32BE) instance StaticByteSized 4 Word32BE

deriving via (ViaEndianPair 4 Int32LE Int32BE) instance StaticByteSized 4 Int32BE

deriving via (ViaEndianPair 8 Word64LE Word64BE) instance StaticByteSized 8 Word64BE

deriving via (ViaEndianPair 8 Int64LE Int64BE) instance StaticByteSized 8 Int64BE

deriving via (ViaEndianPair 4 FloatLE FloatBE) instance StaticByteSized 4 FloatBE

deriving via (ViaEndianPair 8 DoubleLE DoubleBE) instance StaticByteSized 8 DoubleBE

staticByteSizeFoldable :: (Foldable f, StaticByteSized n a) => f a -> ByteCount
staticByteSizeFoldable fa = staticByteSize (proxyForF fa) * coerce (length fa)

byteSizeViaStatic :: StaticByteSized n a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor
