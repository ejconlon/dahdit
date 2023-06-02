{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Primitive (RealWorld)
import Dahdit
  ( Binary (..)
  , BinaryTarget (..)
  , BoolByte (..)
  , ByteCount (..)
  , ByteString
  , DoubleBE (..)
  , DoubleLE (..)
  , FloatBE (..)
  , FloatLE (..)
  , Generic
  , Get
  , GetError (..)
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , LiftedPrimArray (..)
  , MutBinaryTarget (..)
  , Proxy (..)
  , Put
  , ShortByteString
  , StaticByteSized (..)
  , StaticBytes (..)
  , TermBytes16 (..)
  , TermBytes8 (..)
  , ViaGeneric (..)
  , ViaStaticGeneric (..)
  , Word16BE
  , Word16LE (..)
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE (..)
  , Word64BE
  , Word64LE
  , decodeEnd
  , encode
  , getByteArray
  , getByteString
  , getDoubleBE
  , getDoubleLE
  , getExact
  , getFloatBE
  , getFloatLE
  , getInt16BE
  , getInt16LE
  , getInt24BE
  , getInt24LE
  , getInt32BE
  , getInt32LE
  , getInt64BE
  , getInt64LE
  , getInt8
  , getLiftedPrimArray
  , getLookAhead
  , getRemainingSize
  , getSeq
  , getSkip
  , getStaticArray
  , getStaticSeq
  , getTarget
  , getText
  , getWithin
  , getWord16BE
  , getWord16LE
  , getWord24BE
  , getWord24LE
  , getWord32BE
  , getWord32LE
  , getWord64BE
  , getWord64LE
  , getWord8
  , lengthLiftedPrimArray
  , liftedPrimArrayFromList
  , mutPutTarget
  , mutPutTargetOffset
  , putByteArray
  , putByteString
  , putDoubleBE
  , putDoubleLE
  , putFloatBE
  , putFloatLE
  , putInt16BE
  , putInt16LE
  , putInt24BE
  , putInt24LE
  , putInt32BE
  , putInt32LE
  , putInt64BE
  , putInt64LE
  , putInt8
  , putLiftedPrimArray
  , putSeq
  , putStaticArray
  , putStaticSeq
  , putText
  , putWord16BE
  , putWord16LE
  , putWord24BE
  , putWord24LE
  , putWord32BE
  , putWord32LE
  , putWord64BE
  , putWord64LE
  , putWord8
  , runCount
  , sizeofLiftedPrimArray
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray, byteArrayFromList, freezeByteArray, newByteArray, sizeofMutableByteArray)
import Data.Proxy (asProxyTypeOf)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.ShortWord (Int24, Word24)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Debug.Trace
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Predicate as FC
import Test.Falsify.Property (Property)
import qualified Test.Falsify.Property as FP
import qualified Test.Falsify.Range as FR
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

class BinaryTarget z => CaseTarget z where
  initSource :: [Word8] -> z
  consumeSink :: z -> [Word8]

instance CaseTarget ShortByteString where
  initSource = BSS.pack
  consumeSink = BSS.unpack

instance CaseTarget ByteString where
  initSource = BS.pack
  consumeSink = BS.unpack

instance CaseTarget (Vector Word8) where
  initSource = VS.fromList
  consumeSink = VS.toList

class MutBinaryTarget u IO => MutCaseTarget u where
  newSink :: ByteCount -> IO u
  freezeSink :: u -> IO [Word8]

instance MutCaseTarget (MutableByteArray RealWorld) where
  newSink = newByteArray . unByteCount
  freezeSink u = fmap (\(ByteArray arr) -> BSS.unpack (SBS arr)) (freezeByteArray u 0 (sizeofMutableByteArray u))

instance MutCaseTarget (IOVector Word8) where
  newSink = VSM.new . unByteCount
  freezeSink = fmap consumeSink . VS.freeze

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)

data TagFoo = TagFooOne !Word8 | TagFooTwo !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric TagFoo)

type StaBytes = StaticBytes 2

mkStaBytes :: String -> StaBytes
mkStaBytes = StaticBytes . BSS.toShort . BSC.pack

data GetCase where
  GetCase :: (Show a, Eq a) => String -> Get a -> Maybe (ByteCount, ByteCount, a) -> [Word8] -> GetCase

runGetCase :: CaseTarget z => Proxy z -> GetCase -> TestTree
runGetCase p (GetCase name getter mayRes buf) = testCase name $ do
  let src = initSource buf `asProxyTypeOf` p
      totLen = coerce (length buf)
      (result, actOff) = getTarget getter src
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, _, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecOff, expecLeft, expecVal)) -> do
      actVal @?= expecVal
      actOff @?= expecOff
      totLen - actOff @?= expecLeft

data PutCase where
  PutCase :: String -> Put -> [Word8] -> PutCase

runPutCase :: CaseTarget z => Proxy z -> PutCase -> TestTree
runPutCase p (PutCase name putter expecBs) = testCase name $ do
  let expecBc = coerce (length expecBs)
      estBc = runCount putter
  estBc @?= expecBc
  let actSink = putTargetUnsafe putter expecBc `asProxyTypeOf` p
      actBs = consumeSink actSink
      actBc = coerce (length actBs)
  actBs @?= expecBs
  actBc @?= expecBc

mutRunPutCase :: MutCaseTarget z => Proxy z -> PutCase -> TestTree
mutRunPutCase p (PutCase name putter expecBs) = testCase name $ do
  let expecBc = coerce (length expecBs)
  actSink <- fmap (`asProxyTypeOf` p) (newSink expecBc)
  endOff <- mutPutTarget putter actSink
  endOff @?= expecBc
  actBs <- freezeSink actSink
  let actBc = coerce (length actBs)
  actBs @?= expecBs
  actBc @?= expecBc

testByteSize :: TestTree
testByteSize =
  testGroup
    "byteSize"
    [ testCase "Word8" (byteSize @Word8 0 @?= 1)
    , testCase "Int8" (byteSize @Int8 0 @?= 1)
    , testCase "Word16" (byteSize @Word16 0 @?= 2)
    , testCase "Int16" (byteSize @Int16 0 @?= 2)
    , testCase "Word24" (byteSize @Word24 0 @?= 3)
    , testCase "Int24" (byteSize @Int24 0 @?= 3)
    , testCase "Word32" (byteSize @Word32 0 @?= 4)
    , testCase "Int32" (byteSize @Int32 0 @?= 4)
    , testCase "Word64" (byteSize @Word64 0 @?= 8)
    , testCase "Int64" (byteSize @Int64 0 @?= 8)
    , testCase "Float" (byteSize @Float 0 @?= 4)
    , testCase "Double" (byteSize @Double 0 @?= 8)
    , testCase "()" (byteSize @() () @?= 0)
    , testCase "Bool" (byteSize @Bool False @?= 1)
    , testCase "Char" (byteSize @Char 'a' @?= 1)
    , testCase "Int" (byteSize @Int 0 @?= 8)
    , testCase "Word16LE" (byteSize @Word16LE 0 @?= 2)
    , testCase "Int16LE" (byteSize @Int16LE 0 @?= 2)
    , testCase "Word24LE" (byteSize @Word24LE 0 @?= 3)
    , testCase "Int24LE" (byteSize @Int24LE 0 @?= 3)
    , testCase "Word32LE" (byteSize @Word32LE 0 @?= 4)
    , testCase "Int32LE" (byteSize @Int32LE 0 @?= 4)
    , testCase "Word64LE" (byteSize @Word64LE 0 @?= 8)
    , testCase "Int64LE" (byteSize @Int64LE 0 @?= 8)
    , testCase "FloatLE" (byteSize (FloatLE (castWord32ToFloat 0)) @?= 4)
    , testCase "DoubleLE" (byteSize (DoubleLE (castWord64ToDouble 0)) @?= 8)
    , testCase "Word16BE" (byteSize @Word16BE 0 @?= 2)
    , testCase "Int16BE" (byteSize @Int16BE 0 @?= 2)
    , testCase "Word24BE" (byteSize @Word24BE 0 @?= 3)
    , testCase "Int24BE" (byteSize @Int24BE 0 @?= 3)
    , testCase "Word32BE" (byteSize @Word32BE 0 @?= 4)
    , testCase "Int32BE" (byteSize @Int32BE 0 @?= 4)
    , testCase "Word64BE" (byteSize @Word64BE 0 @?= 8)
    , testCase "Int64BE" (byteSize @Int64BE 0 @?= 8)
    , testCase "FloatBE" (byteSize (FloatBE (castWord32ToFloat 0)) @?= 4)
    , testCase "DoubleBE" (byteSize (DoubleBE (castWord64ToDouble 0)) @?= 8)
    , testCase "DynFoo" (byteSize (DynFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaFoo" (byteSize (StaFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaBytes" (byteSize (mkStaBytes "hi") @?= 2)
    , testCase "StaBytes (less)" (byteSize (mkStaBytes "h") @?= 2)
    , testCase "StaBytes (more)" (byteSize (mkStaBytes "hi!") @?= 2)
    , testCase "TagFoo (one)" (byteSize (TagFooOne 7) @?= 2)
    , testCase "TagFoo (two)" (byteSize (TagFooTwo 7) @?= 3)
    ]

testStaticByteSize :: TestTree
testStaticByteSize =
  testGroup
    "staticByteSize"
    [ testCase "Word8" (staticByteSize @Word8 Proxy @?= 1)
    , testCase "Int8" (staticByteSize @Int8 Proxy @?= 1)
    , testCase "Word16" (staticByteSize @Word16 Proxy @?= 2)
    , testCase "Int16" (staticByteSize @Int16 Proxy @?= 2)
    , testCase "Word24" (staticByteSize @Word24 Proxy @?= 3)
    , testCase "Int24" (staticByteSize @Int24 Proxy @?= 3)
    , testCase "Word32" (staticByteSize @Word32 Proxy @?= 4)
    , testCase "Int32" (staticByteSize @Int32 Proxy @?= 4)
    , testCase "Word64" (staticByteSize @Word64 Proxy @?= 8)
    , testCase "Int64" (staticByteSize @Int64 Proxy @?= 8)
    , testCase "Float" (staticByteSize @Float Proxy @?= 4)
    , testCase "Double" (staticByteSize @Double Proxy @?= 8)
    , testCase "()" (staticByteSize @() Proxy @?= 0)
    , testCase "Bool" (staticByteSize @Bool Proxy @?= 1)
    , testCase "Char" (staticByteSize @Char Proxy @?= 1)
    , testCase "Int" (staticByteSize @Int Proxy @?= 8)
    , testCase "Word16LE" (staticByteSize @Word16LE Proxy @?= 2)
    , testCase "Int16LE" (staticByteSize @Int16LE Proxy @?= 2)
    , testCase "Word24LE" (staticByteSize @Word24LE Proxy @?= 3)
    , testCase "Int24LE" (staticByteSize @Int24LE Proxy @?= 3)
    , testCase "Word32LE" (staticByteSize @Word32LE Proxy @?= 4)
    , testCase "Int32LE" (staticByteSize @Int32LE Proxy @?= 4)
    , testCase "Word64LE" (staticByteSize @Word64LE Proxy @?= 8)
    , testCase "Int64LE" (staticByteSize @Int64LE Proxy @?= 8)
    , testCase "FloatLE" (staticByteSize @FloatLE Proxy @?= 4)
    , testCase "DoubleLE" (staticByteSize @DoubleLE Proxy @?= 8)
    , testCase "Word16BE" (staticByteSize @Word16BE Proxy @?= 2)
    , testCase "Int16BE" (staticByteSize @Int16BE Proxy @?= 2)
    , testCase "Word24BE" (staticByteSize @Word24BE Proxy @?= 3)
    , testCase "Int24BE" (staticByteSize @Int24BE Proxy @?= 3)
    , testCase "Word32BE" (staticByteSize @Word32BE Proxy @?= 4)
    , testCase "Int32BE" (staticByteSize @Int32BE Proxy @?= 4)
    , testCase "Word64BE" (staticByteSize @Word64BE Proxy @?= 8)
    , testCase "Int64BE" (staticByteSize @Int64BE Proxy @?= 8)
    , testCase "FloatBE" (staticByteSize @FloatBE Proxy @?= 4)
    , testCase "DoubleBE" (staticByteSize @DoubleBE Proxy @?= 8)
    , testCase "StaFoo" (staticByteSize @StaFoo Proxy @?= 3)
    , testCase "BoolByte" (staticByteSize @BoolByte Proxy @?= 1)
    , testCase "StaBytes" (staticByteSize @StaBytes Proxy @?= 2)
    ]

getCases :: [GetCase]
getCases =
  [ GetCase "Word8 zero" getWord8 Nothing []
  , GetCase "Word8 one" getWord8 (Just (1, 0, 0x5D)) [0x5D]
  , GetCase "Word8 two" getWord8 (Just (1, 1, 0x5D)) [0x5D, 0xBB]
  , GetCase "Int8" getInt8 (Just (1, 0, 0x5D)) [0x5D]
  , GetCase "Word16LE zero" getWord16LE Nothing []
  , GetCase "Word16LE one" getWord16LE Nothing [0x5D]
  , GetCase "Word16LE two" getWord16LE (Just (2, 0, 0x5DEC)) [0xEC, 0x5D]
  , GetCase "Word16LE three" getWord16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "Int16LE" getInt16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "Word16BE" getWord16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB]
  , GetCase "Int16BE" getInt16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB]
  , GetCase "Word24LE" getWord24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Int24LE" getInt24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Word24BE" getWord24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D]
  , GetCase "Int24BE" getInt24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D]
  , GetCase "Word32LE" getWord32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Int32LE" getInt32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Word32BE" getWord32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD]
  , GetCase "Int32BE" getInt32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD]
  , GetCase "Word64LE" getWord64LE (Just (8, 0, 0x5DEC6EFD12345678)) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Word64BE" getWord64BE (Just (8, 0, 0x5DEC6EFD12345678)) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , GetCase "Int64LE" getInt64LE (Just (8, 0, 0x5DEC6EFD12345678)) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "Int64BE" getInt64BE (Just (8, 0, 0x5DEC6EFD12345678)) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , GetCase "FloatLE" getFloatLE (Just (4, 0, FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "FloatBE" getFloatBE (Just (4, 0, FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD]
  , GetCase "DoubleLE" getDoubleLE (Just (8, 0, DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "DoubleBE" getDoubleBE (Just (8, 0, DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , GetCase "ShortByteString" (getByteString 2) (Just (2, 1, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB]
  , GetCase "Text" (getText 2) (Just (2, 1, "hi")) [0x68, 0x69, 0xBB]
  , GetCase "Two Word8" ((,) <$> getWord8 <*> getWord8) (Just (2, 0, (0x5D, 0xBB))) [0x5D, 0xBB]
  , GetCase "Two Word16LE" ((,) <$> getWord16LE <*> getWord16LE) (Just (4, 0, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "Seq" (getSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "StaticSeq" (getStaticSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "StaticArray" (getStaticArray @Word16LE 2) (Just (4, 0, liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "DynFoo" (get @DynFoo) (Just (3, 0, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , GetCase "StaFoo" (get @StaFoo) (Just (3, 0, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , GetCase "getRemainingSize" getRemainingSize (Just (0, 3, 3)) [0xBB, 0xEC, 0x5D]
  , GetCase "getSkip" (getSkip 2) (Just (2, 1, ())) [0xBB, 0xEC, 0x5D]
  , GetCase "getLookAhead" (getLookAhead getWord16LE) (Just (0, 3, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getExact eq" (getExact 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getExact lt" (getExact 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getExact gt" (getExact 3 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin eq" (getWithin 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin lt" (getWithin 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin gt" (getWithin 3 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "BoolByte True" (get @BoolByte) (Just (1, 0, BoolByte True)) [0x01]
  , GetCase "BoolByte False" (get @BoolByte) (Just (1, 0, BoolByte False)) [0x00]
  , GetCase "getByteArray" (getByteArray 3) (Just (3, 1, byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase "getLiftedPrimArray" (getLiftedPrimArray (Proxy :: Proxy Word16LE) 3) (Just (6, 1, liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00, 0x5D]
  , GetCase "StaBytes" (get @StaBytes) (Just (2, 1, mkStaBytes "hi")) [0x68, 0x69, 0x21]
  , GetCase "TagFoo (one)" (get @TagFoo) (Just (2, 0, TagFooOne 7)) [0x00, 0x07]
  , GetCase "TagFoo (two)" (get @TagFoo) (Just (3, 0, TagFooTwo 7)) [0x01, 0x07, 0x00]
  , GetCase "TB8 0" (get @TermBytes8) (Just (1, 0, TermBytes8 BSS.empty)) [0]
  , GetCase "TB8 1" (get @TermBytes8) (Just (2, 0, TermBytes8 (BSS.pack [1]))) [1, 0]
  , GetCase "TB8 2" (get @TermBytes8) (Just (3, 0, TermBytes8 (BSS.pack [1, 2]))) [1, 2, 0]
  , GetCase "TB8 3" (get @TermBytes8) (Just (4, 0, TermBytes8 (BSS.pack [1, 2, 3]))) [1, 2, 3, 0]
  , GetCase "TB16 0" (get @TermBytes16) (Just (2, 0, TermBytes16 BSS.empty)) [0, 0]
  , GetCase "TB16 1" (get @TermBytes16) (Just (2, 0, TermBytes16 (BSS.pack [1]))) [1, 0]
  , GetCase "TB16 2" (get @TermBytes16) (Just (4, 0, TermBytes16 (BSS.pack [1, 2]))) [1, 2, 0, 0]
  , GetCase "TB16 3" (get @TermBytes16) (Just (4, 0, TermBytes16 (BSS.pack [1, 2, 3]))) [1, 2, 3, 0]
  , GetCase "Seq Word16LE" (get @(Seq Word16LE)) (Just (12, 0, Seq.fromList [0xEC, 0x5D])) [2, 0, 0, 0, 0, 0, 0, 0, 0xEC, 0, 0x5D, 0]
  ]

testGet :: CaseTarget z => String -> Proxy z -> TestTree
testGet n p = testGroup ("get (" ++ n ++ ")") (fmap (runGetCase p) getCases)

putCases :: [PutCase]
putCases =
  [ PutCase "Word8" (putWord8 0x5D) [0x5D]
  , PutCase "Int8" (putInt8 0x5D) [0x5D]
  , PutCase "Word16LE" (putWord16LE 0x5DEC) [0xEC, 0x5D]
  , PutCase "Int16LE" (putInt16LE 0x5DEC) [0xEC, 0x5D]
  , PutCase "Word16BE" (putWord16BE 0x5DEC) [0x5D, 0xEC]
  , PutCase "Int16BE" (putInt16BE 0x5DEC) [0x5D, 0xEC]
  , PutCase "Word24LE" (putWord24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC]
  , PutCase "Int24LE" (putInt24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC]
  , PutCase "Word24BE" (putWord24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD]
  , PutCase "Int24BE" (putInt24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD]
  , PutCase "Word32LE" (putWord32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "Int32LE" (putInt32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "Word32BE" (putWord32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD]
  , PutCase "Int32BE" (putInt32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD]
  , PutCase "Word64LE" (putWord64LE 0x5DEC6EFD12345678) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "Int64LE" (putInt64LE 0x5DEC6EFD12345678) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "Word64BE" (putWord64BE 0x5DEC6EFD12345678) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , PutCase "Int64BE" (putInt64BE 0x5DEC6EFD12345678) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , PutCase "FloatLE" (putFloatLE (FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "FloatBE" (putFloatBE (FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD]
  , PutCase "DoubleLE" (putDoubleLE (DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase "DoubleBE" (putDoubleBE (DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , PutCase "ShortByteString" (putByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D]
  , PutCase "Text" (putText "hi") [0x68, 0x69]
  , PutCase "Two Word8" (putWord8 0x5D *> putWord8 0xBB) [0x5D, 0xBB]
  , PutCase "Two Word16LE" (putWord16LE 0x5DEC *> putWord16LE 0x4020) [0xEC, 0x5D, 0x20, 0x40]
  , PutCase "Seq" (putSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , PutCase "StaticSeq" (putStaticSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , PutCase "StaticArray" (putStaticArray @Word16LE (liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , PutCase "DynFoo" (put (DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , PutCase "StaFoo" (put (StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , PutCase "BoolByte True" (put (BoolByte True)) [0x01]
  , PutCase "BoolByte False" (put (BoolByte False)) [0x00]
  , PutCase "putByteArray" (putByteArray (byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC]
  , PutCase "putLiftedPrimArray" (putLiftedPrimArray (liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00]
  , PutCase "StaBytes" (put (mkStaBytes "hi")) [0x68, 0x69]
  , PutCase "StaBytes (less)" (put (mkStaBytes "h")) [0x68, 0x00]
  , PutCase "StaBytes (more)" (put (mkStaBytes "hi!")) [0x68, 0x69]
  , PutCase "TagFoo (one)" (put (TagFooOne 7)) [0x00, 0x07]
  , PutCase "TagFoo (two)" (put (TagFooTwo 7)) [0x01, 0x07, 0x00]
  , PutCase "TB8 0" (put (TermBytes8 BSS.empty)) [0]
  , PutCase "TB8 1" (put (TermBytes8 (BSS.pack [1]))) [1, 0]
  , PutCase "TB8 2" (put (TermBytes8 (BSS.pack [1, 2]))) [1, 2, 0]
  , PutCase "TB8 3" (put (TermBytes8 (BSS.pack [1, 2, 3]))) [1, 2, 3, 0]
  , PutCase "TB16 0" (put (TermBytes16 BSS.empty)) [0, 0]
  , PutCase "TB16 1" (put (TermBytes16 (BSS.pack [1]))) [1, 0]
  , PutCase "TB16 2" (put (TermBytes16 (BSS.pack [1, 2]))) [1, 2, 0, 0]
  , PutCase "TB16 3" (put (TermBytes16 (BSS.pack [1, 2, 3]))) [1, 2, 3, 0]
  , PutCase "Seq Word16LE" (put @(Seq Word16LE) (Seq.fromList [0xEC, 0x5D])) [2, 0, 0, 0, 0, 0, 0, 0, 0xEC, 0, 0x5D, 0]
  ]

testPut :: CaseTarget z => String -> Proxy z -> TestTree
testPut n p = testGroup ("put (" ++ n ++ ")") (fmap (runPutCase p) putCases)

testLiftedPrimArray :: TestTree
testLiftedPrimArray = testCase "liftedPrimArray" $ do
  let arr = LiftedPrimArray (byteArrayFromList @Word8 [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00]) :: LiftedPrimArray Word16LE
  liftedPrimArrayFromList [0xFD, 0x6E, 0xEC] @?= arr
  sizeofLiftedPrimArray arr @?= 6
  lengthLiftedPrimArray arr @?= 3

testGetOffset :: CaseTarget z => String -> Proxy z -> TestTree
testGetOffset n p = testCase ("get offset (" ++ n ++ ")") $ do
  let buf = [0x12, 0x34, 0x56, 0x78]
      src = initSource buf `asProxyTypeOf` p
      (ez1, c1) = getTargetOffset 0 getWord8 src
  ez1 @?= Right 0x12
  c1 @?= 1
  let (ez2, c2) = getTargetOffset 1 getWord16LE src
  ez2 @?= Right 0x5634
  c2 @?= 3
  let (ez3, c3) = getTargetOffset 3 getWord16LE src
  ez3 @?= Left (GetErrorGlobalCap "Word16LE" 1 2)
  c3 @?= 3

data WordX
  = WordX8 !Word8
  | WordX16 !Word16LE
  | WordX32 !Word32LE
  deriving stock (Eq, Ord, Show)

instance Binary WordX where
  byteSize = \case
    WordX8 _ -> 1
    WordX16 _ -> 2
    WordX32 _ -> 4
  get = do
    w <- get @Word8
    case w of
      1 -> fmap WordX8 get
      2 -> fmap WordX16 get
      4 -> fmap WordX32 get
      _ -> fail ("Bad size for WordX: " ++ show w)
  put = \case
    WordX8 w -> put @Word8 1 >> put w
    WordX16 w -> put @Word8 2 >> put w
    WordX32 w -> put @Word8 4 >> put w

wordXGen :: Gen WordX
wordXGen = FG.choose gen8 (FG.choose gen16 gen32)
 where
  gen8 = fmap WordX8 (FG.integral (FR.between (0, maxBound)))
  gen16 = fmap (WordX16 . Word16LE) (FG.integral (FR.between (0, maxBound)))
  gen32 = fmap (WordX32 . Word32LE) (FG.integral (FR.between (0, maxBound)))

assertEq :: (Eq a, Show a) => a -> a -> Property ()
assertEq x y = FP.assert (FC.eq FC..$ ("LHS", x) FC..$ ("RHS", y))

testGetInc :: CaseTarget z => String -> Proxy z -> TestTree
testGetInc n p = testProperty ("get inc (" ++ n ++ ")") $ do
  numElems <- FP.gen @Int (FG.integral (FR.between (0, 20)))
  xs <- FP.gen (fmap Seq.fromList (replicateM numElems wordXGen))
  assertEq (Seq.length xs) numElems
  let vec = encode @(Seq WordX) @ShortByteString xs
  traceShowM xs
  traceShowM vec
  --     (exs, _) = decodeEnd @_ @(Seq WordX) vec
  -- case exs of
  --   Left err -> fail (show err)
  --   Right xs' -> assertEq xs' xs
  pure ()

testMutPut :: MutCaseTarget u => String -> Proxy u -> TestTree
testMutPut n p = testGroup ("mut put (" ++ n ++ ")") (fmap (mutRunPutCase p) putCases)

testMutPutOffset :: MutCaseTarget u => String -> Proxy u -> TestTree
testMutPutOffset n p = testCase ("mut put offset (" ++ n ++ ")") $ do
  u <- newSink 4
  c1 <- mutPutTargetOffset 0 (putWord8 0x12) u
  c1 @?= 1
  c2 <- mutPutTargetOffset 1 (putWord16LE 0x5634) u
  c2 @?= 3
  x <- freezeSink (u `asProxyTypeOf` p)
  take 3 x @?= [0x12, 0x34, 0x56]
  pure ()

data TargetDef where
  TargetDef :: CaseTarget z => String -> Proxy z -> TargetDef

targets :: [TargetDef]
targets =
  [ TargetDef "ShortByteString" (Proxy :: Proxy ShortByteString)
  , TargetDef "ByteString" (Proxy :: Proxy ByteString)
  , TargetDef "Vector" (Proxy :: Proxy (Vector Word8))
  ]

data MutTargetDef where
  MutTargetDef :: MutCaseTarget u => String -> Proxy u -> MutTargetDef

mutTargets :: [MutTargetDef]
mutTargets =
  [ MutTargetDef "MutableByteArray" (Proxy :: Proxy (MutableByteArray RealWorld))
  , MutTargetDef "IOVector" (Proxy :: Proxy (IOVector Word8))
  ]

testDahdit :: TestTree
testDahdit = testGroup "Dahdit" trees
 where
  trees = baseTrees ++ targetTrees ++ mutTargetTrees
  baseTrees = [testByteSize, testStaticByteSize, testLiftedPrimArray]
  targetTrees =
    targets >>= \(TargetDef name prox) ->
      [ testGet name prox
      , testPut name prox
      , testGetOffset name prox
      -- , testGetInc name prox
      ]
  mutTargetTrees =
    mutTargets >>= \(MutTargetDef name prox) ->
      [ testMutPut name prox
      , testMutPutOffset name prox
      ]

main :: IO ()
main = defaultMain testDahdit
