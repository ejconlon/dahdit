{-# LANGUAGE UndecidableInstances #-}

module Test.Dahdit.Arb
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
  , Arb (..)
  , ArbSigned (..)
  , ArbUnsigned (..)
  , ArbFractional (..)
  , ArbEnum (..)
  , ArbGeneric (..)
  , LengthBounds (..)
  , DahditIdx
  )
where

import Control.Applicative (liftA2)
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
  -- , StaticBytes (..)
  -- , StaticSeq (..)

  , TermBytes16 (..)
  , TermBytes8 (..)
  , Word16BE (..)
  , Word16LE (..)
  , Word32BE (..)
  , Word32LE (..)
  , Word64BE (..)
  , Word64LE (..)
  )
import Data.Bits (FiniteBits (..))
import Data.ByteString.Internal (w2c)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce (coerce)
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
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as FG
import Test.Falsify.Range qualified as FR

genPrintableChar :: Gen Char
genPrintableChar = fmap w2c (FG.integral (FR.between (32, 126)))

genSigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genSigned = FG.integral (FR.withOrigin (minBound, maxBound) 0)

genUnsigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genUnsigned = FG.integral (FR.between (0, maxBound))

genFractional :: Fractional a => Gen a
genFractional = do
  -- Picked so bound**2 fits in int
  let bound = 3037000499 :: Int
  n <- FG.integral (FR.between (0, bound))
  b <- FG.integral (FR.between (1, bound))
  a <- FG.integral (FR.withOrigin ((-n) * b, n * b) 0)
  return (fromRational (fromIntegral a % fromIntegral b))

genEnum :: (Enum a, Bounded a) => Gen a
genEnum = let b = minBound in FG.elem (b :| drop 1 [b .. maxBound])

genSum :: NonEmpty (Gen a) -> Gen a
genSum (g :| gs) = foldr FG.choose g gs

genList :: Word -> Word -> Gen a -> Gen [a]
genList mn mx = FG.list (FR.between (mn, mx))

genSeq :: Word -> Word -> Gen a -> Gen (Seq a)
genSeq mn mx = fmap Seq.fromList . genList mn mx

genString :: Word -> Word -> Gen String
genString mn mx = genList mn mx genPrintableChar

genSBS :: Word -> Word -> Gen ShortByteString
genSBS mn mx = fmap BSS.pack (genList mn mx genUnsigned)

genText :: Word -> Word -> Gen Text
genText mn mx = fmap T.pack (genString mn mx)

class Arb p a where
  arb :: Proxy p -> Proxy a -> Gen a

newtype ArbSigned a = ArbSigned {unArbSigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb p (ArbSigned a) where
  arb _ _ = fmap ArbSigned genSigned

newtype ArbUnsigned a = ArbUnsigned {unArbUnsigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb p (ArbUnsigned a) where
  arb _ _ = fmap ArbUnsigned genUnsigned

newtype ArbFractional a = ArbFractional {unArbFractional :: a}

instance (Fractional a) => Arb p (ArbFractional a) where
  arb _ _ = fmap ArbFractional genFractional

newtype ArbEnum a = ArbEnum {unArbEnum :: a}

instance (Enum a, Bounded a) => Arb p (ArbEnum a) where
  arb _ _ = fmap ArbEnum genEnum

class GArb p f where
  garb :: Proxy p -> Proxy (f a) -> Gen (f a)

-- Unit
instance GArb p U1 where
  garb _ _ = pure U1

-- Metadata
instance GArb p a => GArb p (M1 i c a) where
  garb p = fmap M1 . garb p . coerce

-- Product
instance (GArb p a, GArb p b) => GArb p (a :*: b) where
  garb p _ = liftA2 (:*:) (garb p Proxy) (garb p Proxy)

-- Sum
instance (GArb p a, GArb p b) => GArb p (a :+: b) where
  garb p _ = FG.choose (fmap L1 (garb p Proxy)) (fmap R1 (garb p Proxy))

-- Field
instance Arb p a => GArb p (K1 i a) where
  garb p = fmap K1 . arb p . coerce

newtype ArbGeneric p a = ArbGeneric {unArbGeneric :: a}

instance (Generic t, GArb p (Rep t)) => Arb p (ArbGeneric p t) where
  arb p = fmap (ArbGeneric . to) . garb p . coerce

class LengthBounds p a where
  lengthBounds :: Proxy p -> Proxy a -> (Word, Word)

proxyForSrcElem :: ([a] -> b) -> Proxy b -> Proxy a
proxyForSrcElem _ _ = Proxy

arbList :: (LengthBounds p b, Arb p a) => ([a] -> b) -> Proxy p -> Proxy b -> Gen b
arbList f p pb =
  let g = arb p (proxyForSrcElem f pb)
      (mn, mx) = lengthBounds p pb
  in  fmap f (genList mn mx g)

data DahditIdx p

type D = DahditIdx

deriving via (ArbUnsigned Word8) instance Arb (D p) Word8

deriving via (ArbSigned Int8) instance Arb (D p) Int8

deriving via (ArbUnsigned Word16) instance Arb (D p) Word16

deriving via (ArbSigned Int16) instance Arb (D p) Int16

deriving via (ArbUnsigned Word32) instance Arb (D p) Word32

deriving via (ArbSigned Int32) instance Arb (D p) Int32

deriving via (ArbUnsigned Word64) instance Arb (D p) Word64

deriving via (ArbSigned Int64) instance Arb (D p) Int64

deriving via (ArbFractional Float) instance Arb (D p) Float

deriving via (ArbFractional Double) instance Arb (D p) Double

deriving via (ArbSigned Int) instance Arb (D p) Int

deriving newtype instance Arb (D p) Word16LE

deriving newtype instance Arb (D p) Int16LE

deriving newtype instance Arb (D p) Word32LE

deriving newtype instance Arb (D p) Int32LE

deriving newtype instance Arb (D p) Word64LE

deriving newtype instance Arb (D p) Int64LE

deriving newtype instance Arb (D p) FloatLE

deriving newtype instance Arb (D p) DoubleLE

deriving newtype instance Arb (D p) Word16BE

deriving newtype instance Arb (D p) Int16BE

deriving newtype instance Arb (D p) Word32BE

deriving newtype instance Arb (D p) Int32BE

deriving newtype instance Arb (D p) Word64BE

deriving newtype instance Arb (D p) Int64BE

deriving newtype instance Arb (D p) FloatBE

deriving newtype instance Arb (D p) DoubleBE

instance Arb (D p) Char where
  arb p _ = fmap w2c (arb p Proxy)

deriving via
  (ArbGeneric (D p) ())
  instance
    Arb (D p) ()

deriving via
  (ArbGeneric (D p) Bool)
  instance
    Arb (D p) Bool

deriving via
  (ArbGeneric (D p) (Maybe a))
  instance
    Arb (D p) a => Arb (D p) (Maybe a)

deriving via
  (ArbGeneric (D p) (Either a b))
  instance
    (Arb (D p) a, Arb (D p) b) => Arb (D p) (Either a b)

deriving via
  (ArbGeneric (D p) (a, b))
  instance
    (Arb (D p) a, Arb (D p) b) => Arb (D p) (a, b)

deriving via
  (ArbGeneric (D p) (a, b, c))
  instance
    (Arb (D p) a, Arb (D p) b, Arb (D p) c) => Arb (D p) (a, b, c)

deriving via
  (ArbGeneric (D p) (a, b, c, d))
  instance
    (Arb (D p) a, Arb (D p) b, Arb (D p) c, Arb (D p) d) => Arb (D p) (a, b, c, d)

deriving via
  (ArbGeneric (D p) (a, b, c, d, e))
  instance
    (Arb (D p) a, Arb (D p) b, Arb (D p) c, Arb (D p) d, Arb (D p) e) => Arb (D p) (a, b, c, d, e)

instance (LengthBounds (D p) [a], Arb (D p) a) => Arb (D p) [a] where
  arb = arbList id

instance (LengthBounds (D p) (Seq a), Arb (D p) a) => Arb (D p) (Seq a) where
  arb = arbList Seq.fromList

instance (LengthBounds (D p) (Map a b), Arb (D p) a, Arb (D p) b, Ord a) => Arb (D p) (Map a b) where
  arb = arbList Map.fromList

instance (LengthBounds (D p) (Set a), Arb (D p) a, Ord a) => Arb (D p) (Set a) where
  arb = arbList Set.fromList

instance (LengthBounds (D p) IntSet) => Arb (D p) IntSet where
  arb = arbList IntSet.fromList

instance (LengthBounds (D p) (IntMap a), Arb (D p) a) => Arb (D p) (IntMap a) where
  arb = arbList IntMap.fromList

instance LengthBounds (D p) TermBytes8 => Arb (D p) TermBytes8 where
  arb = arbList (TermBytes8 . BSS.pack)

instance LengthBounds (D p) TermBytes16 => Arb (D p) TermBytes16 where
  arb = arbList (TermBytes16 . BSS.pack)

-- instance LengthBounds (D p) => Arb (D p) (StaticBytes n) where
--   arb = arbListN (StaticBytes . BSS.pack)

-- instance (LengthBounds (D p), Arb (D p) a) => Arb (D p) (StaticSeq n a) where
--   arb = arbListN (StaticSeq . Seq.fromList)

instance Arb (D p) BoolByte where
  arb p _ = fmap BoolByte (arb p Proxy)

instance Arb (D p) (ExactBytes n s) where
  arb _ _ = pure (ExactBytes ())
