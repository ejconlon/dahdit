module Main (main) where

import Dahdit
  ( Binary (..)
  , BoolByte (..)
  , ByteCount (..)
  , ByteSized (..)
  , FloatBE (..)
  , FloatLE (..)
  , Generic
  , Get
  , Int16LE
  , Int32LE
  , Int8
  , LiftedPrimArray (..)
  , Proxy (..)
  , Put
  , ShortByteString
  , StaticByteSized (..)
  , StaticBytes (..)
  , ViaGeneric (..)
  , ViaStaticGeneric (..)
  , Word16LE
  , Word32LE
  , Word8
  , getByteArray
  , getByteString
  , getExact
  , getFloatBE
  , getFloatLE
  , getInt16BE
  , getInt16LE
  , getInt24BE
  , getInt24LE
  , getInt32BE
  , getInt32LE
  , getInt8
  , getLiftedPrimArray
  , getLookAhead
  , getRemainingSize
  , getSeq
  , getSkip
  , getStaticArray
  , getStaticSeq
  , getWithin
  , getWord16BE
  , getWord16LE
  , getWord24BE
  , getWord24LE
  , getWord32BE
  , getWord32LE
  , getWord8
  , lengthLiftedPrimArray
  , liftedPrimArrayFromList
  , putByteArray
  , putByteString
  , putFloatBE
  , putFloatLE
  , putInt16BE
  , putInt16LE
  , putInt24BE
  , putInt24LE
  , putInt32BE
  , putInt32LE
  , putInt8
  , putLiftedPrimArray
  , putSeq
  , putStaticArray
  , putStaticSeq
  , putWord16BE
  , putWord16LE
  , putWord24BE
  , putWord24LE
  , putWord32BE
  , putWord32LE
  , putWord8
  , runCount
  , runGetSBS
  , runPutSBS
  , sizeofLiftedPrimArray
  )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (byteArrayFromList)
import qualified Data.Sequence as Seq
import GHC.Float (castWord32ToFloat)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)

type StaBytes = StaticBytes 2

mkStaBytes :: String -> StaBytes
mkStaBytes = StaticBytes . BSS.toShort . BSC.pack

runGetCase :: (Show a, Eq a) => Get a -> Maybe (ByteCount, ByteCount, a) -> [Word8] -> IO ()
runGetCase getter mayRes bsl = do
  let bs = BSS.pack bsl
      (result, actBc) = runGetSBS getter bs
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, _, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecBc, expecLeft, expecVal)) -> do
      actVal @?= expecVal
      actBc @?= expecBc
      coerce (BSS.length bs) - actBc @?= expecLeft

runPutCase :: Put -> [Word8] -> IO ()
runPutCase putter expecList = do
  let expecBc = coerce (length expecList)
      expecBs = BSS.pack expecList
      estBc = runCount putter
  estBc @?= expecBc
  let actBs = runPutSBS putter
      actBc = byteSize actBs
  actBs @?= expecBs
  actBc @?= expecBc

testDahditByteSize :: TestTree
testDahditByteSize =
  testGroup
    "byteSize"
    [ testCase "Word8" (byteSize @Word8 0x5D @?= 1)
    , testCase "Int8" (byteSize @Int8 0x5D @?= 1)
    , testCase "Word16LE" (byteSize @Word16LE 0x5DEC @?= 2)
    , testCase "Int16LE" (byteSize @Int16LE 0x5DEC @?= 2)
    , testCase "Word32LE" (byteSize @Word32LE 0x5DEC6EFD @?= 4)
    , testCase "Int32LE" (byteSize @Int32LE 0x5DEC6EFD @?= 4)
    , testCase "FloatLE" (byteSize (FloatLE (castWord32ToFloat 0x5DEC6EFD)) @?= 4)
    , testCase "ShortByteString" (byteSize @ShortByteString (BSS.pack [0xEC, 0x5D]) @?= 2)
    , testCase "DynFoo" (byteSize (DynFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaFoo" (byteSize (StaFoo 0xBB 0x5DEC) @?= 3)
    , testCase "StaBytes" (byteSize (mkStaBytes "hi") @?= 2)
    , testCase "StaBytes (less)" (byteSize (mkStaBytes "h") @?= 2)
    , testCase "StaBytes (more)" (byteSize (mkStaBytes "hi!") @?= 2)
    ]

testDahditStaticByteSize :: TestTree
testDahditStaticByteSize =
  testGroup
    "staticByteSize"
    [ testCase "Word8" (staticByteSize @Word8 Proxy @?= 1)
    , testCase "Int8" (staticByteSize @Int8 Proxy @?= 1)
    , testCase "Word16LE" (staticByteSize @Word16LE Proxy @?= 2)
    , testCase "Int16LE" (staticByteSize @Int16LE Proxy @?= 2)
    , testCase "Word32LE" (staticByteSize @Word32LE Proxy @?= 4)
    , testCase "Int32LE" (staticByteSize @Int32LE Proxy @?= 4)
    , testCase "FloatLE" (staticByteSize @FloatLE Proxy @?= 4)
    , testCase "StaFoo" (staticByteSize @StaFoo Proxy @?= 3)
    , testCase "BoolByte" (staticByteSize @BoolByte Proxy @?= 1)
    , testCase "StaBytes" (staticByteSize @StaBytes Proxy @?= 2)
    ]

testDahditGet :: TestTree
testDahditGet =
  testGroup
    "get"
    [ testCase "Word8 zero" (runGetCase getWord8 Nothing [])
    , testCase "Word8 one" (runGetCase getWord8 (Just (1, 0, 0x5D)) [0x5D])
    , testCase "Word8 two" (runGetCase getWord8 (Just (1, 1, 0x5D)) [0x5D, 0xBB])
    , testCase "Int8" (runGetCase getInt8 (Just (1, 0, 0x5D)) [0x5D])
    , testCase "Word16LE zero" (runGetCase getWord16LE Nothing [])
    , testCase "Word16LE one" (runGetCase getWord16LE Nothing [0x5D])
    , testCase "Word16LE two" (runGetCase getWord16LE (Just (2, 0, 0x5DEC)) [0xEC, 0x5D])
    , testCase "Word16LE three" (runGetCase getWord16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "Int16LE" (runGetCase getInt16LE (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "Word16BE" (runGetCase getWord16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB])
    , testCase "Int16BE" (runGetCase getInt16BE (Just (2, 1, 0x5DEC)) [0x5D, 0xEC, 0xBB])
    , testCase "Word24LE" (runGetCase getWord24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int24LE" (runGetCase getInt24LE (Just (3, 1, 0xEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word24BE" (runGetCase getWord24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D])
    , testCase "Int24BE" (runGetCase getInt24BE (Just (3, 1, 0xEC6EFD)) [0xEC, 0x6E, 0xFD, 0x5D])
    , testCase "Word32LE" (runGetCase getWord32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int32LE" (runGetCase getInt32LE (Just (4, 0, 0x5DEC6EFD)) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word32BE" (runGetCase getWord32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Int32BE" (runGetCase getInt32BE (Just (4, 0, 0x5DEC6EFD)) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "FloatLE" (runGetCase getFloatLE (Just (4, 0, FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "FloatBE" (runGetCase getFloatBE (Just (4, 0, FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "ShortByteString" (runGetCase (getByteString 2) (Just (2, 1, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB])
    , testCase "Two Word8" (runGetCase ((,) <$> getWord8 <*> getWord8) (Just (2, 0, (0x5D, 0xBB))) [0x5D, 0xBB])
    , testCase "Two Word16LE" (runGetCase ((,) <$> getWord16LE <*> getWord16LE) (Just (4, 0, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "Seq" (runGetCase (getSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticSeq" (runGetCase (getStaticSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticArray" (runGetCase (getStaticArray @Word16LE 2) (Just (4, 0, liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "DynFoo" (runGetCase (get @DynFoo) (Just (3, 0, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "StaFoo" (runGetCase (get @StaFoo) (Just (3, 0, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "getRemainingSize" (runGetCase getRemainingSize (Just (0, 3, 3)) [0xBB, 0xEC, 0x5D])
    , testCase "getSkip" (runGetCase (getSkip 2) (Just (2, 1, ())) [0xBB, 0xEC, 0x5D])
    , testCase "getLookAhead" (runGetCase (getLookAhead getWord16LE) (Just (0, 3, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getExact eq" (runGetCase (getExact 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getExact lt" (runGetCase (getExact 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getExact gt" (runGetCase (getExact 3 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getWithin eq" (runGetCase (getWithin 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "getWithin lt" (runGetCase (getWithin 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB])
    , testCase "getWithin gt" (runGetCase (getWithin 3 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB])
    , testCase "BoolByte True" (runGetCase (get @BoolByte) (Just (1, 0, BoolByte True)) [0x01])
    , testCase "BoolByte False" (runGetCase (get @BoolByte) (Just (1, 0, BoolByte False)) [0x00])
    , testCase "getByteArray" (runGetCase (getByteArray 3) (Just (3, 1, byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "getLiftedPrimArray" (runGetCase (getLiftedPrimArray (Proxy :: Proxy Word16LE) 3) (Just (6, 1, liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00, 0x5D])
    , testCase "StaBytes" (runGetCase (get @StaBytes) (Just (2, 1, mkStaBytes "hi")) [0x68, 0x69, 0x21])
    ]

testDahditPut :: TestTree
testDahditPut =
  testGroup
    "put"
    [ testCase "Word8" (runPutCase (putWord8 0x5D) [0x5D])
    , testCase "Int8" (runPutCase (putInt8 0x5D) [0x5D])
    , testCase "Word16LE" (runPutCase (putWord16LE 0x5DEC) [0xEC, 0x5D])
    , testCase "Int16LE" (runPutCase (putInt16LE 0x5DEC) [0xEC, 0x5D])
    , testCase "Word16BE" (runPutCase (putWord16BE 0x5DEC) [0x5D, 0xEC])
    , testCase "Int16BE" (runPutCase (putInt16BE 0x5DEC) [0x5D, 0xEC])
    , testCase "Word24LE" (runPutCase (putWord24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC])
    , testCase "Int24LE" (runPutCase (putInt24LE 0xEC6EFD) [0xFD, 0x6E, 0xEC])
    , testCase "Word24BE" (runPutCase (putWord24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD])
    , testCase "Int24BE" (runPutCase (putInt24BE 0xEC6EFD) [0xEC, 0x6E, 0xFD])
    , testCase "Word32LE" (runPutCase (putWord32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Int32LE" (runPutCase (putInt32LE 0x5DEC6EFD) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "Word32BE" (runPutCase (putWord32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "Int32BE" (runPutCase (putInt32BE 0x5DEC6EFD) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "FloatLE" (runPutCase (putFloatLE (FloatLE (castWord32ToFloat 0x5DEC6EFD))) [0xFD, 0x6E, 0xEC, 0x5D])
    , testCase "FloatBE" (runPutCase (putFloatBE (FloatBE (castWord32ToFloat 0x5DEC6EFD))) [0x5D, 0xEC, 0x6E, 0xFD])
    , testCase "ShortByteString" (runPutCase (putByteString (BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D])
    , testCase "Two Word8" (runPutCase (putWord8 0x5D *> putWord8 0xBB) [0x5D, 0xBB])
    , testCase "Two Word16LE" (runPutCase (putWord16LE 0x5DEC *> putWord16LE 0x4020) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "Seq" (runPutCase (putSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticSeq" (runPutCase (putStaticSeq putWord16LE (Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "StaticArray" (runPutCase (putStaticArray @Word16LE (liftedPrimArrayFromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40])
    , testCase "DynFoo" (runPutCase (put (DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "StaFoo" (runPutCase (put (StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D])
    , testCase "BoolByte True" (runPutCase (put (BoolByte True)) [0x01])
    , testCase "BoolByte False" (runPutCase (put (BoolByte False)) [0x00])
    , testCase "putByteArray" (runPutCase (putByteArray (byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC])) [0xFD, 0x6E, 0xEC])
    , testCase "putLiftedPrimArray" (runPutCase (putLiftedPrimArray (liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC])) [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00])
    , testCase "StaBytes" (runPutCase (put (mkStaBytes "hi")) [0x68, 0x69])
    , testCase "StaBytes (less)" (runPutCase (put (mkStaBytes "h")) [0x68, 0x00])
    , testCase "StaBytes (more)" (runPutCase (put (mkStaBytes "hi!")) [0x68, 0x69])
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
    , testDahditGet
    , testDahditPut
    , testDahditLiftedPrimArray
    ]

main :: IO ()
main = defaultMain testDahdit
