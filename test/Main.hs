module Main (main) where

import Dahdit
  ( Binary (..)
  , BinaryTarget (..)
  , BoolByte (..)
  , ByteCount (..)
  , ByteSized (..)
  , ByteString
  , DoubleBE (..)
  , DoubleLE (..)
  , FloatBE (..)
  , FloatLE (..)
  , Generic
  , Get
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , LiftedPrimArray (..)
  , Proxy (..)
  , Put
  , ShortByteString
  , StaticByteSized (..)
  , StaticBytes (..)
  , ViaGeneric (..)
  , ViaStaticGeneric (..)
  , Word16BE
  , Word16LE
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  , Word64BE
  , Word64LE
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
  , putTarget
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
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray (byteArrayFromList)
import Data.Proxy (asProxyTypeOf)
import qualified Data.Sequence as Seq
import Data.ShortWord (Int24, Word24)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Test.Tasty (TestTree, defaultMain, testGroup)
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

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)

data TagFoo = TagFooOne !Word8 | TagFooTwo !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric TagFoo)

type StaBytes = StaticBytes 2

mkStaBytes :: String -> StaBytes
mkStaBytes = StaticBytes . BSS.toShort . BSC.pack

runGetCase :: (Show a, Eq a, CaseTarget z) => Proxy z -> Get a -> Maybe (ByteCount, ByteCount, a) -> [Word8] -> IO ()
runGetCase p getter mayRes buf = do
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

runPutCase :: CaseTarget z => Proxy z -> Put -> [Word8] -> IO ()
runPutCase p putter expecList = do
  let expecBc = coerce (length expecList)
      expecBs = expecList
      estBc = runCount putter
  estBc @?= expecBc
  let actSink = putTarget putter estBc `asProxyTypeOf` p
      actBs = consumeSink actSink
      actBc = coerce (length actBs)
  actBs @?= expecBs
  actBc @?= expecBc

testDahditByteSize :: TestTree
testDahditByteSize =
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
    , testCase "ShortByteString" (byteSize @ShortByteString (BSS.pack [0xEC, 0x5D]) @?= 2)
    , testCase "DynFoo" (byteSize (DynFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaFoo" (byteSize (StaFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaBytes" (byteSize (mkStaBytes "hi") @?= 2)
    , testCase "StaBytes (less)" (byteSize (mkStaBytes "h") @?= 2)
    , testCase "StaBytes (more)" (byteSize (mkStaBytes "hi!") @?= 2)
    , testCase "TagFoo (one)" (byteSize (TagFooOne 7) @?= 2)
    , testCase "TagFoo (two)" (byteSize (TagFooTwo 7) @?= 3)
    ]

testDahditStaticByteSize :: TestTree
testDahditStaticByteSize =
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

testDahditGet :: CaseTarget z => String -> Proxy z -> TestTree
testDahditGet n p =
  testGroup
    ("get (" ++ n ++ ")")
    [ testCase "Word8 zero" (runGetCase p getWord8 Nothing [])
    , testCase "Word8 one" (runGetCase p getWord8 (Just (1, 0, 0x5D)) [0x5D])
    , testCase "Word8 two" (runGetCase p getWord8 (Just (1, 1, 0x5D)) [0x5D, 0xBB])
    , testCase "Int8" (runGetCase p getInt8 (Just (1, 0, 0x5D)) [0x5D])
    , testCase "Word16LE zero" (runGetCase p getWord16LE Nothing [])
    , testCase "Word16LE one" (runGetCase p getWord16LE Nothing [0x5D])
    , testCase "Word16LE two" (runGetCase p getWord16LE (Just (2, 0, 0x5DEC)) [0xEC, 0x5D])
    , testCase "Word16LE three" (runGetCase p getWord16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "Int16LE" (runGetCase p getInt16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "Word16BE" (runGetCase p getWord16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB])
    , testCase "Int16BE" (runGetCase p getInt16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB])
    , testCase "Word24LE" (runGetCase p getWord24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int24LE" (runGetCase p getInt24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word24BE" (runGetCase p getWord24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D])
    , testCase "Int24BE" (runGetCase p getInt24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D])
    , testCase "Word32LE" (runGetCase p getWord32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int32LE" (runGetCase p getInt32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word32BE" (runGetCase p getWord32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Int32BE" (runGetCase p getInt32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Word64LE" (runGetCase p getWord64LE (Just (8, 0, 0x5DEC6EFD12345678)) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word64BE" (runGetCase p getWord64BE (Just (8, 0, 0x5DEC6EFD12345678)) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "Int64LE" (runGetCase p getInt64LE (Just (8, 0, 0x5DEC6EFD12345678)) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int64BE" (runGetCase p getInt64BE (Just (8, 0, 0x5DEC6EFD12345678)) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "FloatLE" (runGetCase p getFloatLE (Just (4, 0, FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "FloatBE" (runGetCase p getFloatBE (Just (4, 0, FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "DoubleLE" (runGetCase p getDoubleLE (Just (8, 0, DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "DoubleBE" (runGetCase p getDoubleBE (Just (8, 0, DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "ShortByteString" (runGetCase p (getByteString 2) (Just (2, 1, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB])
    , testCase "Two Word8" (runGetCase p ((,) <$> getWord8 <*> getWord8) (Just (2, 0, (0x5D, 0xBB))) [0x5D, 0xBB])
    , testCase "Two Word16LE" (runGetCase p ((,) <$> getWord16LE <*> getWord16LE) (Just (4, 0, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "Seq" (runGetCase p (getSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticSeq" (runGetCase p (getStaticSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticArray" (runGetCase p (getStaticArray @Word16LE 2) (Just (4, 0, liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "DynFoo" (runGetCase p (get @DynFoo) (Just (3, 0, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "StaFoo" (runGetCase p (get @StaFoo) (Just (3, 0, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "getRemainingSize" (runGetCase p getRemainingSize (Just (0, 3, 3)) [0xBB, 0xEC, 0x5D])
    , testCase "getSkip" (runGetCase p (getSkip 2) (Just (2, 1, ())) [0xBB, 0xEC, 0x5D])
    , testCase "getLookAhead" (runGetCase p (getLookAhead getWord16LE) (Just (0, 3, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getExact eq" (runGetCase p (getExact 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getExact lt" (runGetCase p (getExact 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getExact gt" (runGetCase p (getExact 3 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getWithin eq" (runGetCase p (getWithin 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getWithin lt" (runGetCase p (getWithin 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getWithin gt" (runGetCase p (getWithin 3 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "BoolByte True" (runGetCase p (get @BoolByte) (Just (1, 0, BoolByte True)) [0x01])
    , testCase "BoolByte False" (runGetCase p (get @BoolByte) (Just (1, 0, BoolByte False)) [0x00])
    , testCase "getByteArray" (runGetCase p (getByteArray 3) (Just (3, 1, byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "getLiftedPrimArray" (runGetCase p (getLiftedPrimArray (Proxy :: Proxy Word16LE) 3) (Just (6, 1, liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00, 0x5D])
    , testCase "StaBytes" (runGetCase p (get @StaBytes) (Just (2, 1, mkStaBytes "hi")) [0x68, 0x69, 0x21])
    , testCase "TagFoo (one)" (runGetCase p (get @TagFoo) (Just (2, 0, TagFooOne 7)) [0x00, 0x07])
    , testCase "TagFoo (two)" (runGetCase p (get @TagFoo) (Just (3, 0, TagFooTwo 7)) [0x01, 0x07, 0x00])
    ]

testDahditPut :: CaseTarget z => String -> Proxy z -> TestTree
testDahditPut n p =
  testGroup
    ("put (" ++ n ++ ")")
    [ testCase "Word8" (runPutCase p (putWord8 0x5D) [0x5D])
    , testCase "Int8" (runPutCase p (putInt8 0x5D) [0x5D])
    , testCase "Word16LE" (runPutCase p (putWord16LE 0x5DEC) [0xEC, 0x5D])
    , testCase "Int16LE" (runPutCase p (putInt16LE 0x5DEC) [0xEC, 0x5D])
    , testCase "Word16BE" (runPutCase p (putWord16BE 0x5DEC) [0x5D, 0xEC])
    , testCase "Int16BE" (runPutCase p (putInt16BE 0x5DEC) [0x5D, 0xEC])
    , testCase "Word24LE" (runPutCase p (putWord24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC])
    , testCase "Int24LE" (runPutCase p (putInt24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC])
    , testCase "Word24BE" (runPutCase p (putWord24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD])
    , testCase "Int24BE" (runPutCase p (putInt24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD])
    , testCase "Word32LE" (runPutCase p (putWord32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int32LE" (runPutCase p (putInt32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word32BE" (runPutCase p (putWord32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Int32BE" (runPutCase p (putInt32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Word64LE" (runPutCase p (putWord64LE 0x5DEC6EFD12345678) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int64LE" (runPutCase p (putInt64LE 0x5DEC6EFD12345678) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word64BE" (runPutCase p (putWord64BE 0x5DEC6EFD12345678) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "Int64BE" (runPutCase p (putInt64BE 0x5DEC6EFD12345678) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "FloatLE" (runPutCase p (putFloatLE (FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "FloatBE" (runPutCase p (putFloatBE (FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "DoubleLE" (runPutCase p (putDoubleLE (DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "DoubleBE" (runPutCase p (putDoubleBE (DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678))) [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78])
    , testCase "ShortByteString" (runPutCase p (putByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D])
    , testCase "Two Word8" (runPutCase p (putWord8 0x5D *> putWord8 0xBB) [0x5D, 0xBB])
    , testCase "Two Word16LE" (runPutCase p (putWord16LE 0x5DEC *> putWord16LE 0x4020) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "Seq" (runPutCase p (putSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticSeq" (runPutCase p (putStaticSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticArray" (runPutCase p (putStaticArray @Word16LE (liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "DynFoo" (runPutCase p (put (DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "StaFoo" (runPutCase p (put (StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "BoolByte True" (runPutCase p (put (BoolByte True)) [0x01])
    , testCase "BoolByte False" (runPutCase p (put (BoolByte False)) [0x00])
    , testCase "putByteArray" (runPutCase p (putByteArray (byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC])
    , testCase "putLiftedPrimArray" (runPutCase p (putLiftedPrimArray (liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00])
    , testCase "StaBytes" (runPutCase p (put (mkStaBytes "hi")) [0x68, 0x69])
    , testCase "StaBytes (less)" (runPutCase p (put (mkStaBytes "h")) [0x68, 0x00])
    , testCase "StaBytes (more)" (runPutCase p (put (mkStaBytes "hi!")) [0x68, 0x69])
    , testCase "TagFoo (one)" (runPutCase p (put (TagFooOne 7)) [0x00, 0x07])
    , testCase "TagFoo (two)" (runPutCase p (put (TagFooTwo 7)) [0x01, 0x07, 0x00])
    ]

testDahditLiftedPrimArray :: TestTree
testDahditLiftedPrimArray = testCase "liftedPrimArray" $ do
  let arr = LiftedPrimArray (byteArrayFromList @Word8 [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00]) :: LiftedPrimArray Word16LE
  liftedPrimArrayFromList [0xFD, 0x6E, 0xEC] @?= arr
  sizeofLiftedPrimArray arr @?= 6
  lengthLiftedPrimArray arr @?= 3

testDahdit :: TestTree
testDahdit =
  testGroup
    "Dahdit"
    [ testDahditByteSize
    , testDahditStaticByteSize
    , testDahditGet "ShortByteString" (Proxy :: Proxy ShortByteString)
    , testDahditGet "ByteString" (Proxy :: Proxy ByteString)
    , testDahditGet "Vector" (Proxy :: Proxy (Vector Word8))
    , testDahditPut "ShortByteString" (Proxy :: Proxy ShortByteString)
    , testDahditPut "ByteString" (Proxy :: Proxy ByteString)
    , testDahditPut "Vector" (Proxy :: Proxy (Vector Word8))
    , testDahditLiftedPrimArray
    ]

main :: IO ()
main = defaultMain testDahdit
