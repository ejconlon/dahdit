{-# LANGUAGE UndecidableInstances #-}

module Test.Dahdit.GenDefault
  ( genSigned
  , genUnsigned
  , genFractional
  , genEnum
  , genSum
  , genList
  , genSeq
  , genString
  , genSBS
  , genText
  , LengthBounds (..)
  , DahditTag
  )
where

import Dahdit
  ( BoolByte (..)
  , DoubleBE (..)
  , DoubleLE (..)
  , ExactBytes (..)
  , FloatBE (..)
  , FloatLE (..)
  , Int16BE (..)
  , Int16LE (..)
  , Int32BE (..)
  , Int32LE (..)
  , Int64BE (..)
  , Int64LE (..)
  , StaticBytes (..)
  , StaticSeq (..)
  , TermBytes16 (..)
  , TermBytes8 (..)
  , Word16BE (..)
  , Word16LE (..)
  , Word32BE (..)
  , Word32LE (..)
  , Word64BE (..)
  , Word64LE (..)
  )
import Data.ByteString.Internal (w2c)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, natVal)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PropUnit (GenDefault (..), genDefaultGeneric)

genPrintableChar :: Gen Char
genPrintableChar = fmap w2c (Gen.integral (Range.constant 32 126))

genSigned :: (Integral a, Bounded a) => Gen a
genSigned = Gen.integral (Range.constantFrom 0 minBound maxBound)

genUnsigned :: (Integral a, Bounded a) => Gen a
genUnsigned = Gen.integral (Range.constant 0 maxBound)

genFractional :: (Fractional a) => Gen a
genFractional = do
  -- Picked so bound**2 fits in int
  let bound = 3037000499 :: Int
  n <- Gen.integral (Range.constant 0 bound)
  b <- Gen.integral (Range.constant 1 bound)
  a <- Gen.integral (Range.constantFrom 0 ((-n) * b) (n * b))
  return (fromRational (fromIntegral a % fromIntegral b))

genEnum :: (Enum a, Bounded a) => Gen a
genEnum = Gen.enumBounded

genSum :: NonEmpty (Gen a) -> Gen a
genSum (g :| gs) = Gen.choice (g : gs)

genList :: Word -> Word -> Gen a -> Gen [a]
genList mn mx = Gen.list (Range.constant (fromIntegral mn) (fromIntegral mx))

genSeq :: Word -> Word -> Gen a -> Gen (Seq a)
genSeq mn mx = fmap Seq.fromList . genList mn mx

genString :: Word -> Word -> Gen String
genString mn mx = genList mn mx genPrintableChar

genSBS :: Word -> Word -> Gen ShortByteString
genSBS mn mx = fmap BSS.pack (genList mn mx genUnsigned)

genText :: Word -> Word -> Gen Text
genText mn mx = fmap T.pack (genString mn mx)

newtype ViaSigned a = ViaSigned {unViaSigned :: a}

instance (Integral a, Bounded a) => GenDefault p (ViaSigned a) where
  genDefault _ = fmap ViaSigned genSigned

newtype ViaUnsigned a = ViaUnsigned {unViaUnsigned :: a}

instance (Integral a, Bounded a) => GenDefault p (ViaUnsigned a) where
  genDefault _ = fmap ViaUnsigned genUnsigned

newtype ViaFractional a = ViaFractional {unViaFractional :: a}

instance (Fractional a) => GenDefault p (ViaFractional a) where
  genDefault _ = fmap ViaFractional genFractional

class LengthBounds p a where
  lengthBounds :: Proxy p -> Proxy a -> (Word, Word)

proxyForRange :: (x -> b) -> Proxy b
proxyForRange _ = Proxy

genListLike :: (LengthBounds p b, GenDefault p a) => ([a] -> b) -> Proxy p -> Gen b
genListLike f p =
  let (mn, mx) = lengthBounds p (proxyForRange f)
  in  fmap f (genList mn mx (genDefault p))

data DahditTag p

type D = DahditTag

instance GenDefault (D p) Word8 where
  genDefault _ = genUnsigned

instance GenDefault (D p) Int8 where
  genDefault _ = genSigned

instance GenDefault (D p) Word16 where
  genDefault _ = genUnsigned

instance GenDefault (D p) Int16 where
  genDefault _ = genSigned

instance GenDefault (D p) Word32 where
  genDefault _ = genUnsigned

instance GenDefault (D p) Int32 where
  genDefault _ = genSigned

instance GenDefault (D p) Word64 where
  genDefault _ = genUnsigned

instance GenDefault (D p) Int64 where
  genDefault _ = genSigned

instance GenDefault (D p) Float where
  genDefault _ = genFractional

instance GenDefault (D p) Double where
  genDefault _ = genFractional

instance GenDefault (D p) Int where
  genDefault _ = genSigned

instance GenDefault (D p) Word16LE where
  genDefault = fmap Word16LE . genDefault

instance GenDefault (D p) Int16LE where
  genDefault = fmap Int16LE . genDefault

instance GenDefault (D p) Word32LE where
  genDefault = fmap Word32LE . genDefault

instance GenDefault (D p) Int32LE where
  genDefault = fmap Int32LE . genDefault

instance GenDefault (D p) Word64LE where
  genDefault = fmap Word64LE . genDefault

instance GenDefault (D p) Int64LE where
  genDefault = fmap Int64LE . genDefault

instance GenDefault (D p) FloatLE where
  genDefault = fmap FloatLE . genDefault

instance GenDefault (D p) DoubleLE where
  genDefault = fmap DoubleLE . genDefault

instance GenDefault (D p) Word16BE where
  genDefault = fmap Word16BE . genDefault

instance GenDefault (D p) Int16BE where
  genDefault = fmap Int16BE . genDefault

instance GenDefault (D p) Word32BE where
  genDefault = fmap Word32BE . genDefault

instance GenDefault (D p) Int32BE where
  genDefault = fmap Int32BE . genDefault

instance GenDefault (D p) Word64BE where
  genDefault = fmap Word64BE . genDefault

instance GenDefault (D p) Int64BE where
  genDefault = fmap Int64BE . genDefault

instance GenDefault (D p) FloatBE where
  genDefault = fmap FloatBE . genDefault

instance GenDefault (D p) DoubleBE where
  genDefault = fmap DoubleBE . genDefault

instance GenDefault (D p) Char where
  genDefault = fmap w2c . genDefault

instance GenDefault (D p) () where
  genDefault = genDefaultGeneric

instance GenDefault (D p) Bool where
  genDefault = genDefaultGeneric

instance (GenDefault (D p) a) => GenDefault (D p) (Maybe a) where
  genDefault = genDefaultGeneric

instance (GenDefault (D p) a, GenDefault (D p) b) => GenDefault (D p) (Either a b) where
  genDefault = genDefaultGeneric

instance (GenDefault (D p) a, GenDefault (D p) b) => GenDefault (D p) (a, b) where
  genDefault = genDefaultGeneric

instance (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c) => GenDefault (D p) (a, b, c) where
  genDefault = genDefaultGeneric

instance (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c, GenDefault (D p) d) => GenDefault (D p) (a, b, c, d) where
  genDefault = genDefaultGeneric

instance
  (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c, GenDefault (D p) d, GenDefault (D p) e)
  => GenDefault (D p) (a, b, c, d, e)
  where
  genDefault = genDefaultGeneric

instance (LengthBounds (D p) [a], GenDefault (D p) a) => GenDefault (D p) [a] where
  genDefault = genListLike id

instance (LengthBounds (D p) (Seq a), GenDefault (D p) a) => GenDefault (D p) (Seq a) where
  genDefault = genListLike Seq.fromList

instance (LengthBounds (D p) (Map a b), GenDefault (D p) a, GenDefault (D p) b, Ord a) => GenDefault (D p) (Map a b) where
  genDefault = genListLike Map.fromList

instance (LengthBounds (D p) (Set a), GenDefault (D p) a, Ord a) => GenDefault (D p) (Set a) where
  genDefault = genListLike Set.fromList

instance (LengthBounds (D p) IntSet) => GenDefault (D p) IntSet where
  genDefault = genListLike IntSet.fromList

instance (LengthBounds (D p) (IntMap a), GenDefault (D p) a) => GenDefault (D p) (IntMap a) where
  genDefault = genListLike IntMap.fromList

instance (LengthBounds (D p) TermBytes8) => GenDefault (D p) TermBytes8 where
  genDefault = genListLike (TermBytes8 . BSS.pack)

instance (LengthBounds (D p) TermBytes16) => GenDefault (D p) TermBytes16 where
  genDefault = genListLike (TermBytes16 . BSS.pack)

instance (KnownNat n) => GenDefault (D p) (StaticBytes n) where
  genDefault _ =
    let mn = fromInteger (natVal (Proxy @n))
    in  fmap (StaticBytes . BSS.pack) (genList mn mn (genDefault @(D p) Proxy))

instance (KnownNat n, GenDefault (D p) a) => GenDefault (D p) (StaticSeq n a) where
  genDefault _ =
    let mn = fromInteger (natVal (Proxy @n))
    in  fmap (StaticSeq . Seq.fromList) (genList mn mn (genDefault @(D p) Proxy))

instance GenDefault (D p) BoolByte where
  genDefault = fmap BoolByte . genDefault

instance GenDefault (D p) (ExactBytes n s) where
  genDefault _ = pure (ExactBytes ())
