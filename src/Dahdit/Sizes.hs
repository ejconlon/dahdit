module Dahdit.Sizes
  ( ElementCount (..)
  , ByteCount (..)
  , StaticByteSized (..)
  , byteSizeViaStatic
  , ByteSized (..)
  , ViaStaticByteSized (..)
  , byteSizeFoldable
  , staticByteSizeFoldable
  ) where

import Dahdit.LiftedPrim (LiftedPrim, LiftedPrimArray, sizeofLiftedPrimArray)
import Dahdit.Nums (FloatBE, FloatLE, Int16BE, Int16LE, Int24BE, Int24LE, Int32BE, Int32LE, Word16BE, Word16LE,
                    Word24BE, Word24LE, Word32BE, Word32LE)
import Dahdit.Proxy (proxyFor, proxyForF)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray (PrimArray, sizeofPrimArray)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word16, Word32, Word64, Word8)

newtype ElementCount = ElementCount { unElementCount :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bounded, Default)

newtype ByteCount = ByteCount { unByteCount :: Word64 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Bounded, Default)

class ByteSized a where
  byteSize :: a -> ByteCount

instance ByteSized () where
  byteSize _ = 0

instance ByteSized Word8 where
  byteSize _ = 1

instance ByteSized Int8 where
  byteSize _ = 1

instance ByteSized Word16 where
  byteSize _ = 2

instance ByteSized Int16 where
  byteSize _ = 2

instance ByteSized Word32 where
  byteSize _ = 4

instance ByteSized Int32 where
  byteSize _ = 4

instance ByteSized Word64 where
  byteSize _ = 8

instance ByteSized Int64 where
  byteSize _ = 8

instance ByteSized Word16LE where
  byteSize _ = 2

instance ByteSized Int16LE where
  byteSize _ = 2

instance ByteSized Word24LE where
  byteSize _ = 3

instance ByteSized Int24LE where
  byteSize _ = 3

instance ByteSized Word32LE where
  byteSize _ = 4

instance ByteSized Int32LE where
  byteSize _ = 4

instance ByteSized FloatLE where
  byteSize _ = 4

instance ByteSized Word16BE where
  byteSize _ = 2

instance ByteSized Int16BE where
  byteSize _ = 2

instance ByteSized Word24BE where
  byteSize _ = 3

instance ByteSized Int24BE where
  byteSize _ = 3

instance ByteSized Word32BE where
  byteSize _ = 4

instance ByteSized Int32BE where
  byteSize _ = 4

instance ByteSized FloatBE where
  byteSize _ = 4

instance ByteSized ShortByteString where
  byteSize = fromIntegral . BSS.length

instance StaticByteSized a => ByteSized (Seq a) where
  byteSize ss =
    let !elen = staticByteSize (Proxy :: Proxy a)
        !alen = fromIntegral (Seq.length ss)
    in elen * alen

instance (StaticByteSized a, Prim a) => ByteSized (PrimArray a) where
  byteSize pa =
    let !elen = staticByteSize (Proxy :: Proxy a)
        !alen = fromIntegral (sizeofPrimArray pa)
    in elen * alen

instance (StaticByteSized a, LiftedPrim a) => ByteSized (LiftedPrimArray a) where
  byteSize lpa =
    let !elen = staticByteSize (Proxy :: Proxy a)
        !alen = fromIntegral (sizeofLiftedPrimArray lpa)
    in elen * alen

class ByteSized a => StaticByteSized a where
  staticByteSize :: Proxy a -> ByteCount

byteSizeViaStatic :: StaticByteSized a => a -> ByteCount
byteSizeViaStatic = staticByteSize . proxyFor

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

instance StaticByteSized Word32 where
  staticByteSize _ = 4

instance StaticByteSized Int32 where
  staticByteSize _ = 4

instance StaticByteSized Word64 where
  staticByteSize _ = 8

instance StaticByteSized Int64 where
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

instance StaticByteSized FloatLE where
  staticByteSize _ = 4

instance StaticByteSized Word16BE where
  staticByteSize _ = 2

instance StaticByteSized Int16BE where
  staticByteSize _ = 2

instance StaticByteSized Word24BE where
  staticByteSize _ = 3

instance StaticByteSized Int24BE where
  staticByteSize _ = 3

instance StaticByteSized Word32BE where
  staticByteSize _ = 4

instance StaticByteSized Int32BE where
  staticByteSize _ = 4

instance StaticByteSized FloatBE where
  staticByteSize _ = 4

newtype ViaStaticByteSized a = ViaStaticByteSized { unViaStaticByteSized :: a }

instance StaticByteSized a => ByteSized (ViaStaticByteSized a) where
  byteSize _ = staticByteSize (Proxy :: Proxy a)

byteSizeFoldable :: (Foldable f, ByteSized a) => f a -> ByteCount
byteSizeFoldable = getSum . foldMap' (Sum . byteSize)

staticByteSizeFoldable :: (Foldable f, StaticByteSized a) => f a -> ByteCount
staticByteSizeFoldable fa = staticByteSize (proxyForF fa) * fromIntegral (length fa)
