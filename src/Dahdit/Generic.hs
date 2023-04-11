{-# LANGUAGE UndecidableInstances #-}

module Dahdit.Generic
  ( ViaGeneric (..)
  , ViaStaticGeneric (..)
  )
where

import Control.Applicative (liftA2)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs (putStaticHint)
import Dahdit.Nums (Word16LE, Word32LE)
import Dahdit.Sizes (ByteCount, ByteSized (..), StaticByteSized (..))
import Data.Bits (Bits (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (C1, Generic (..), K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))

-- | Use: deriving (ByteSized, Binary) via (ViaGeneric Foo)
newtype ViaGeneric a = ViaGeneric {unViaGeneric :: a}

-- | Use: deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric Foo)
newtype ViaStaticGeneric a = ViaStaticGeneric {unViaStaticGeneric :: a}

-- ByteSized:

class GByteSized f where
  gbyteSize :: f a -> ByteCount

-- Unit
instance GByteSized U1 where
  gbyteSize _ = 0

-- Product
instance (GByteSized a, GByteSized b) => GByteSized (a :*: b) where
  gbyteSize (x :*: y) = gbyteSize x + gbyteSize y

-- Metadata
instance GByteSized a => GByteSized (M1 i c a) where
  gbyteSize = gbyteSize . unM1

-- Sum
instance (GByteSized a, GByteSized b, SumSize a, SumSize b) => GByteSized (a :+: b) where
  gbyteSize s =
    sumSizeBytes s + case s of
      L1 a -> gbyteSize a
      R1 b -> gbyteSize b

-- Field
instance ByteSized a => GByteSized (K1 i a) where
  gbyteSize = byteSize . unK1

instance (Generic t, GByteSized (Rep t)) => ByteSized (ViaGeneric t) where
  byteSize = gbyteSize . from . unViaGeneric

instance (Generic t, GByteSized (Rep t)) => ByteSized (ViaStaticGeneric t) where
  byteSize = gbyteSize . from . unViaStaticGeneric

-- StaticByteSized:

class GByteSized f => GStaticByteSized (f :: Type -> Type) where
  gstaticByteSize :: Proxy f -> ByteCount

instance GStaticByteSized U1 where
  gstaticByteSize _ = 0

instance (GStaticByteSized a, GStaticByteSized b) => GStaticByteSized (a :*: b) where
  gstaticByteSize _ = gstaticByteSize (Proxy :: Proxy a) + gstaticByteSize (Proxy :: Proxy b)

instance GStaticByteSized a => GStaticByteSized (M1 i c a) where
  gstaticByteSize _ = gstaticByteSize (Proxy :: Proxy a)

instance StaticByteSized a => GStaticByteSized (K1 i a) where
  gstaticByteSize _ = staticByteSize (Proxy :: Proxy a)

instance (Generic t, GStaticByteSized (Rep t)) => StaticByteSized (ViaStaticGeneric t) where
  staticByteSize _ = gstaticByteSize (Proxy :: Proxy (Rep t))

-- Binary:

class GBinary (f :: Type -> Type) where
  gget :: Get (f a)
  gput :: f a -> Put

instance GBinary U1 where
  gget = pure U1
  gput _ = pure ()

instance (GBinary a, GBinary b) => GBinary (a :*: b) where
  gget = liftA2 (:*:) gget gget
  gput (x :*: y) = gput x *> gput y

instance GBinary a => GBinary (M1 i c a) where
  gget = fmap M1 gget
  gput = gput . unM1

instance Binary a => GBinary (K1 i a) where
  gget = fmap K1 get
  gput = put . unK1

instance (Generic t, GBinary (Rep t)) => Binary (ViaGeneric t) where
  get = fmap (ViaGeneric . to) gget
  put = gput . from . unViaGeneric

instance (Generic t, GStaticByteSized (Rep t), GBinary (Rep t)) => Binary (ViaStaticGeneric t) where
  get = fmap (ViaStaticGeneric . to) gget
  put = putStaticHint (gput . from . unViaStaticGeneric)

-- Everything that follows is borrowed from the binary package, which
-- borrows from the cereal package!

-- The following GBinary instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

instance
  ( GSumBinary a
  , GSumBinary b
  , SumSize a
  , SumSize b
  )
  => GBinary (a :+: b)
  where
  gget
    | size - 1 <= fromIntegral (maxBound :: Word8) = (get :: Get Word8) >>= checkGetSum (fromIntegral size)
    | size - 1 <= fromIntegral (maxBound :: Word16LE) = (get :: Get Word16LE) >>= checkGetSum (fromIntegral size)
    | size - 1 <= (maxBound :: Word32LE) = (get :: Get Word32LE) >>= checkGetSum size
    | otherwise = sizeError "decode" size
   where
    size = unTagged (sumSize :: Tagged (a :+: b))
  gput
    | size - 1 <= fromIntegral (maxBound :: Word8) = putSum (0 :: Word8) (fromIntegral size)
    | size - 1 <= fromIntegral (maxBound :: Word16LE) = putSum (0 :: Word16LE) (fromIntegral size)
    | size - 1 <= (maxBound :: Word32LE) = putSum (0 :: Word32LE) size
    | otherwise = sizeError "encode" size
   where
    size = unTagged (sumSize :: Tagged (a :+: b))

sizeError :: Show size => String -> size -> error
sizeError s size = error ("Can't " ++ s ++ " a type with " ++ show size ++ " constructors")

checkGetSum
  :: (Ord word, Num word, Bits word, GSumBinary f)
  => word
  -> word
  -> Get (f a)
checkGetSum size code
  | code < size = getSum code size
  | otherwise = fail "Unknown encoding for constructor"
{-# INLINE checkGetSum #-}

class GSumBinary f where
  getSum :: (Ord word, Num word, Bits word) => word -> word -> Get (f a)
  putSum :: (Num w, Bits w, Binary w) => w -> w -> f a -> Put

instance (GSumBinary a, GSumBinary b) => GSumBinary (a :+: b) where
  getSum !code !size
    | code < sizeL = L1 <$> getSum code sizeL
    | otherwise = R1 <$> getSum (code - sizeL) sizeR
   where
    sizeL = size `shiftR` 1
    sizeR = size - sizeL
  putSum !code !size s = case s of
    L1 x -> putSum code sizeL x
    R1 x -> putSum (code + sizeL) sizeR x
   where
    sizeL = size `shiftR` 1
    sizeR = size - sizeL

instance GBinary a => GSumBinary (C1 c a) where
  getSum _ _ = gget
  putSum !code _ x = put code <> gput x

class SumSize (f :: Type -> Type) where
  sumSize :: Tagged f

newtype Tagged (s :: Type -> Type) = Tagged {unTagged :: Word32LE}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
  sumSize = Tagged (unTagged (sumSize :: Tagged a) + unTagged (sumSize :: Tagged b))

instance SumSize (C1 c a) where
  sumSize = Tagged 1

sumSizeFor :: SumSize f => f a -> Tagged f
sumSizeFor = const sumSize

taggedBytes :: Tagged f -> ByteCount
taggedBytes (Tagged size)
  | size - 1 <= fromIntegral (maxBound :: Word8) = 1
  | size - 1 <= fromIntegral (maxBound :: Word16LE) = 2
  | size - 1 <= (maxBound :: Word32LE) = 4
  | otherwise = sizeError "size" size

sumSizeBytes :: SumSize f => f a -> ByteCount
sumSizeBytes = taggedBytes . sumSizeFor
