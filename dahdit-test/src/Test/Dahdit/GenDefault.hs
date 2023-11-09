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
  , ViaSigned (..)
  , ViaUnsigned (..)
  , ViaFractional (..)
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
import Test.Falsify.GenDefault (GenDefault (..), ViaGeneric (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as FG
import Test.Falsify.Range qualified as FR

genPrintableChar :: Gen Char
genPrintableChar = fmap w2c (FG.inRange (FR.between (32, 126)))

genSigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genSigned = FG.inRange (FR.withOrigin (minBound, maxBound) 0)

genUnsigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genUnsigned = FG.inRange (FR.between (0, maxBound))

genFractional :: (Fractional a) => Gen a
genFractional = do
  -- Picked so bound**2 fits in int
  let bound = 3037000499 :: Int
  n <- FG.inRange (FR.between (0, bound))
  b <- FG.inRange (FR.between (1, bound))
  a <- FG.inRange (FR.withOrigin ((-n) * b, n * b) 0)
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

newtype ViaSigned a = ViaSigned {unViaSigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => GenDefault p (ViaSigned a) where
  genDefault _ = fmap ViaSigned genSigned

newtype ViaUnsigned a = ViaUnsigned {unViaUnsigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => GenDefault p (ViaUnsigned a) where
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

deriving via (ViaUnsigned Word8) instance GenDefault (D p) Word8

deriving via (ViaSigned Int8) instance GenDefault (D p) Int8

deriving via (ViaUnsigned Word16) instance GenDefault (D p) Word16

deriving via (ViaSigned Int16) instance GenDefault (D p) Int16

deriving via (ViaUnsigned Word32) instance GenDefault (D p) Word32

deriving via (ViaSigned Int32) instance GenDefault (D p) Int32

deriving via (ViaUnsigned Word64) instance GenDefault (D p) Word64

deriving via (ViaSigned Int64) instance GenDefault (D p) Int64

deriving via (ViaFractional Float) instance GenDefault (D p) Float

deriving via (ViaFractional Double) instance GenDefault (D p) Double

deriving via (ViaSigned Int) instance GenDefault (D p) Int

deriving newtype instance GenDefault (D p) Word16LE

deriving newtype instance GenDefault (D p) Int16LE

deriving newtype instance GenDefault (D p) Word32LE

deriving newtype instance GenDefault (D p) Int32LE

deriving newtype instance GenDefault (D p) Word64LE

deriving newtype instance GenDefault (D p) Int64LE

deriving newtype instance GenDefault (D p) FloatLE

deriving newtype instance GenDefault (D p) DoubleLE

deriving newtype instance GenDefault (D p) Word16BE

deriving newtype instance GenDefault (D p) Int16BE

deriving newtype instance GenDefault (D p) Word32BE

deriving newtype instance GenDefault (D p) Int32BE

deriving newtype instance GenDefault (D p) Word64BE

deriving newtype instance GenDefault (D p) Int64BE

deriving newtype instance GenDefault (D p) FloatBE

deriving newtype instance GenDefault (D p) DoubleBE

instance GenDefault (D p) Char where
  genDefault = fmap w2c . genDefault

deriving via (ViaGeneric (D p) ()) instance GenDefault (D p) ()

deriving via
  (ViaGeneric (D p) Bool)
  instance
    GenDefault (D p) Bool

deriving via
  (ViaGeneric (D p) (Maybe a))
  instance
    (GenDefault (D p) a) => GenDefault (D p) (Maybe a)

deriving via
  (ViaGeneric (D p) (Either a b))
  instance
    (GenDefault (D p) a, GenDefault (D p) b) => GenDefault (D p) (Either a b)

deriving via
  (ViaGeneric (D p) (a, b))
  instance
    (GenDefault (D p) a, GenDefault (D p) b) => GenDefault (D p) (a, b)

deriving via
  (ViaGeneric (D p) (a, b, c))
  instance
    (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c) => GenDefault (D p) (a, b, c)

deriving via
  (ViaGeneric (D p) (a, b, c, d))
  instance
    (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c, GenDefault (D p) d) => GenDefault (D p) (a, b, c, d)

deriving via
  (ViaGeneric (D p) (a, b, c, d, e))
  instance
    (GenDefault (D p) a, GenDefault (D p) b, GenDefault (D p) c, GenDefault (D p) d, GenDefault (D p) e) => GenDefault (D p) (a, b, c, d, e)

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

-- instance LengthBounds (D p) => GenDefault (D p) (StaticBytes n) where
--   genDefault = genListLike (StaticBytes . BSS.pack)

-- instance (LengthBounds (D p), GenDefault (D p) a) => GenDefault (D p) (StaticSeq n a) where
--   genDefault = genListLike (StaticSeq . Seq.fromList)

instance GenDefault (D p) BoolByte where
  genDefault = fmap BoolByte . genDefault

instance GenDefault (D p) (ExactBytes n s) where
  genDefault _ = pure (ExactBytes ())
