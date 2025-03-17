{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad (replicateM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Dahdit
  ( Binary (..)
  , BinaryGetTarget (..)
  , BinaryPutTarget (..)
  , BoolByte (..)
  , ByteCount (..)
  , ByteString
  , DoubleBE (..)
  , DoubleLE (..)
  , ElemCount
  , FloatBE (..)
  , FloatLE (..)
  , Generic
  , Get
  , GetError (..)
  , GetIncRequest (..)
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , LiftedPrimArray (..)
  , MutBinaryPutTarget (..)
  , MutableLiftedPrimArray
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
  , concatLiftedPrimArray
  , copyLiftedPrimArray
  , decodeEnd
  , decodeInc
  , encode
  , freezeLiftedPrimArray
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
  , mapLiftedPrimArray
  , mergeLiftedPrimArray
  , mutPutTarget
  , mutPutTargetOffset
  , newLiftedPrimArray
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
  , replicateLiftedPrimArray
  , runCount
  , setLiftedPrimArray
  , sizeofLiftedPrimArray
  , writeLiftedPrimArray
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray
  ( ByteArray (..)
  , MutableByteArray
  , byteArrayFromList
  , freezeByteArray
  , getSizeofMutableByteArray
  , newByteArray
  )
import Data.Proxy (asProxyTypeOf)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.ShortWord (Int24, Word24)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import PropUnit (Gen, TestLimit, TestT, TestTree, forAll, testGroup, testMain, testProp, testUnit, (===))
import qualified PropUnit.Hedgehog.Gen as Gen
import qualified PropUnit.Hedgehog.Range as Range

class (Eq z, Show z) => CaseTarget z where
  initSource :: [Word8] -> IO z
  consumeSink :: z -> IO [Word8]
  sliceBuffer :: z -> ByteCount -> ByteCount -> IO z

instance CaseTarget ShortByteString where
  initSource = pure . BSS.pack
  consumeSink = pure . BSS.unpack
  sliceBuffer buf off len = pure (BSS.take (coerce len) (BSS.drop (coerce off) buf))

instance CaseTarget ByteString where
  initSource = pure . BS.pack
  consumeSink = pure . BS.unpack
  sliceBuffer buf off len = pure (BS.take (coerce len) (BS.drop (coerce off) buf))

instance CaseTarget (Vector Word8) where
  initSource = pure . VS.fromList
  consumeSink = pure . VS.toList
  sliceBuffer buf off len = pure (VS.take (coerce len) (VS.drop (coerce off) buf))

class MutCaseTarget u where
  newSink :: ByteCount -> IO u
  freezeSink :: u -> IO [Word8]

instance MutCaseTarget (MutableByteArray RealWorld) where
  newSink = newByteArray . unByteCount
  freezeSink u = fmap (\(ByteArray arr) -> BSS.unpack (SBS arr)) (getSizeofMutableByteArray u >>= freezeByteArray u 0)

instance MutCaseTarget (IOVector Word8) where
  newSink = VSM.new . unByteCount
  freezeSink = VS.freeze >=> consumeSink

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

runGetCase :: (BinaryGetTarget z IO, CaseTarget z) => Proxy z -> GetCase -> TestTree
runGetCase p (GetCase name getter mayRes buf) = testUnit name $ do
  src <- liftIO (initSource buf)
  let totLen = coerce (length buf)
  (result, actOff) <- liftIO (getTarget getter (src `asProxyTypeOf` p))
  case (result, mayRes) of
    (Left _, Nothing) -> pure ()
    (Left err, Just (_, _, expecVal)) -> fail ("Got error <" ++ show err ++ ">, expected value <" ++ show expecVal ++ ">")
    (Right actVal, Nothing) -> fail ("Got value <" ++ show actVal ++ ">, expected error")
    (Right actVal, Just (expecOff, expecLeft, expecVal)) -> do
      actVal === expecVal
      actOff === expecOff
      totLen - actOff === expecLeft

data PutCase where
  PutCase :: String -> Put -> [Word8] -> PutCase

runPutCase :: (BinaryPutTarget z IO, CaseTarget z) => Proxy z -> PutCase -> TestTree
runPutCase p (PutCase name putter expecBs) = testUnit name $ do
  let expecBc = coerce (length expecBs)
      estBc = runCount putter
  estBc === expecBc
  actSink <- liftIO (putTargetUnsafe putter expecBc)
  actBs <- liftIO (consumeSink (actSink `asProxyTypeOf` p))
  let actBc = coerce (length actBs)
  actBs === expecBs
  actBc === expecBc

mutRunPutCase :: (MutBinaryPutTarget z IO, MutCaseTarget z) => Proxy z -> PutCase -> TestTree
mutRunPutCase p (PutCase name putter expecBs) = testUnit name $ do
  let expecBc = coerce (length expecBs)
  actSink <- liftIO (fmap (`asProxyTypeOf` p) (newSink expecBc))
  endOff <- liftIO (mutPutTarget putter actSink)
  endOff === expecBc
  actBs <- liftIO (freezeSink actSink)
  let actBc = coerce (length actBs)
  actBs === expecBs
  actBc === expecBc

testByteSize :: TestTree
testByteSize =
  testGroup
    "byteSize"
    [ testUnit "Word8" (byteSize @Word8 0 === 1)
    , testUnit "Int8" (byteSize @Int8 0 === 1)
    , testUnit "Word16" (byteSize @Word16 0 === 2)
    , testUnit "Int16" (byteSize @Int16 0 === 2)
    , testUnit "Word24" (byteSize @Word24 0 === 3)
    , testUnit "Int24" (byteSize @Int24 0 === 3)
    , testUnit "Word32" (byteSize @Word32 0 === 4)
    , testUnit "Int32" (byteSize @Int32 0 === 4)
    , testUnit "Word64" (byteSize @Word64 0 === 8)
    , testUnit "Int64" (byteSize @Int64 0 === 8)
    , testUnit "Float" (byteSize @Float 0 === 4)
    , testUnit "Double" (byteSize @Double 0 === 8)
    , testUnit "()" (byteSize @() () === 0)
    , testUnit "Bool" (byteSize @Bool False === 1)
    , testUnit "Char" (byteSize @Char 'a' === 1)
    , testUnit "Int" (byteSize @Int 0 === 8)
    , testUnit "Word16LE" (byteSize @Word16LE 0 === 2)
    , testUnit "Int16LE" (byteSize @Int16LE 0 === 2)
    , testUnit "Word24LE" (byteSize @Word24LE 0 === 3)
    , testUnit "Int24LE" (byteSize @Int24LE 0 === 3)
    , testUnit "Word32LE" (byteSize @Word32LE 0 === 4)
    , testUnit "Int32LE" (byteSize @Int32LE 0 === 4)
    , testUnit "Word64LE" (byteSize @Word64LE 0 === 8)
    , testUnit "Int64LE" (byteSize @Int64LE 0 === 8)
    , testUnit "FloatLE" (byteSize (FloatLE (castWord32ToFloat 0)) === 4)
    , testUnit "DoubleLE" (byteSize (DoubleLE (castWord64ToDouble 0)) === 8)
    , testUnit "Word16BE" (byteSize @Word16BE 0 === 2)
    , testUnit "Int16BE" (byteSize @Int16BE 0 === 2)
    , testUnit "Word24BE" (byteSize @Word24BE 0 === 3)
    , testUnit "Int24BE" (byteSize @Int24BE 0 === 3)
    , testUnit "Word32BE" (byteSize @Word32BE 0 === 4)
    , testUnit "Int32BE" (byteSize @Int32BE 0 === 4)
    , testUnit "Word64BE" (byteSize @Word64BE 0 === 8)
    , testUnit "Int64BE" (byteSize @Int64BE 0 === 8)
    , testUnit "FloatBE" (byteSize (FloatBE (castWord32ToFloat 0)) === 4)
    , testUnit "DoubleBE" (byteSize (DoubleBE (castWord64ToDouble 0)) === 8)
    , testUnit "DynFoo" (byteSize (DynFoo 0xBB 0x5DEC) === 3)
    , testUnit "StaFoo" (byteSize (StaFoo 0xBB 0x5DEC) === 3)
    , testUnit "StaBytes" (byteSize (mkStaBytes "hi") === 2)
    , testUnit "StaBytes (less)" (byteSize (mkStaBytes "h") === 2)
    , testUnit "StaBytes (more)" (byteSize (mkStaBytes "hi!") === 2)
    , testUnit "TagFoo (one)" (byteSize (TagFooOne 7) === 2)
    , testUnit "TagFoo (two)" (byteSize (TagFooTwo 7) === 3)
    ]

testStaticByteSize :: TestTree
testStaticByteSize =
  testGroup
    "staticByteSize"
    [ testUnit "Word8" (staticByteSize @Word8 Proxy === 1)
    , testUnit "Int8" (staticByteSize @Int8 Proxy === 1)
    , testUnit "Word16" (staticByteSize @Word16 Proxy === 2)
    , testUnit "Int16" (staticByteSize @Int16 Proxy === 2)
    , testUnit "Word24" (staticByteSize @Word24 Proxy === 3)
    , testUnit "Int24" (staticByteSize @Int24 Proxy === 3)
    , testUnit "Word32" (staticByteSize @Word32 Proxy === 4)
    , testUnit "Int32" (staticByteSize @Int32 Proxy === 4)
    , testUnit "Word64" (staticByteSize @Word64 Proxy === 8)
    , testUnit "Int64" (staticByteSize @Int64 Proxy === 8)
    , testUnit "Float" (staticByteSize @Float Proxy === 4)
    , testUnit "Double" (staticByteSize @Double Proxy === 8)
    , testUnit "()" (staticByteSize @() Proxy === 0)
    , testUnit "Bool" (staticByteSize @Bool Proxy === 1)
    , testUnit "Char" (staticByteSize @Char Proxy === 1)
    , testUnit "Int" (staticByteSize @Int Proxy === 8)
    , testUnit "Word16LE" (staticByteSize @Word16LE Proxy === 2)
    , testUnit "Int16LE" (staticByteSize @Int16LE Proxy === 2)
    , testUnit "Word24LE" (staticByteSize @Word24LE Proxy === 3)
    , testUnit "Int24LE" (staticByteSize @Int24LE Proxy === 3)
    , testUnit "Word32LE" (staticByteSize @Word32LE Proxy === 4)
    , testUnit "Int32LE" (staticByteSize @Int32LE Proxy === 4)
    , testUnit "Word64LE" (staticByteSize @Word64LE Proxy === 8)
    , testUnit "Int64LE" (staticByteSize @Int64LE Proxy === 8)
    , testUnit "FloatLE" (staticByteSize @FloatLE Proxy === 4)
    , testUnit "DoubleLE" (staticByteSize @DoubleLE Proxy === 8)
    , testUnit "Word16BE" (staticByteSize @Word16BE Proxy === 2)
    , testUnit "Int16BE" (staticByteSize @Int16BE Proxy === 2)
    , testUnit "Word24BE" (staticByteSize @Word24BE Proxy === 3)
    , testUnit "Int24BE" (staticByteSize @Int24BE Proxy === 3)
    , testUnit "Word32BE" (staticByteSize @Word32BE Proxy === 4)
    , testUnit "Int32BE" (staticByteSize @Int32BE Proxy === 4)
    , testUnit "Word64BE" (staticByteSize @Word64BE Proxy === 8)
    , testUnit "Int64BE" (staticByteSize @Int64BE Proxy === 8)
    , testUnit "FloatBE" (staticByteSize @FloatBE Proxy === 4)
    , testUnit "DoubleBE" (staticByteSize @DoubleBE Proxy === 8)
    , testUnit "StaFoo" (staticByteSize @StaFoo Proxy === 3)
    , testUnit "BoolByte" (staticByteSize @BoolByte Proxy === 1)
    , testUnit "StaBytes" (staticByteSize @StaBytes Proxy === 2)
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
  , GetCase
      "DoubleLE"
      getDoubleLE
      (Just (8, 0, DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678)))
      [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase
      "DoubleBE"
      getDoubleBE
      (Just (8, 0, DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678)))
      [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
  , GetCase "ShortByteString" (getByteString 2) (Just (2, 1, BSS.pack [0xEC, 0x5D])) [0xEC, 0x5D, 0xBB]
  , GetCase "Text" (getText 2) (Just (2, 1, "hi")) [0x68, 0x69, 0xBB]
  , GetCase "Two Word8" ((,) <$> getWord8 <*> getWord8) (Just (2, 0, (0x5D, 0xBB))) [0x5D, 0xBB]
  , GetCase "Two Word16LE" ((,) <$> getWord16LE <*> getWord16LE) (Just (4, 0, (0x5DEC, 0x4020))) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "Seq" (getSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "StaticSeq" (getStaticSeq 2 getWord16LE) (Just (4, 0, Seq.fromList [0x5DEC, 0x4020])) [0xEC, 0x5D, 0x20, 0x40]
  , GetCase
      "StaticArray"
      (getStaticArray @Word16LE 2)
      (Just (4, 0, liftedPrimArrayFromList [0x5DEC, 0x4020]))
      [0xEC, 0x5D, 0x20, 0x40]
  , GetCase "DynFoo" (get @DynFoo) (Just (3, 0, DynFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , GetCase "StaFoo" (get @StaFoo) (Just (3, 0, StaFoo 0xBB 0x5DEC)) [0xBB, 0xEC, 0x5D]
  , GetCase "getRemainingSize" getRemainingSize (Just (0, 3, 3)) [0xBB, 0xEC, 0x5D]
  , GetCase "getSkip" (getSkip 2) (Just (2, 1, ())) [0xBB, 0xEC, 0x5D]
  , GetCase "getLookAhead 0" (getLookAhead getWord16LE) (Just (0, 3, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getLookAhead 1" (getWord8 >> getLookAhead getWord16LE) (Just (1, 2, 0xBB5D)) [0xEC, 0x5D, 0xBB]
  , GetCase "getLookAhead 2" (getLookAhead (getWord8 >> getWord16LE)) (Just (0, 3, 0xBB5D)) [0xEC, 0x5D, 0xBB]
  , GetCase "getExact eq" (getExact 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getExact lt" (getExact 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getExact gt" (getExact 3 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin eq" (getWithin 2 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin lt" (getWithin 1 getWord16LE) Nothing [0xEC, 0x5D, 0xBB]
  , GetCase "getWithin gt" (getWithin 3 getWord16LE) (Just (2, 1, 0x5DEC)) [0xEC, 0x5D, 0xBB]
  , GetCase "BoolByte True" (get @BoolByte) (Just (1, 0, BoolByte True)) [0x01]
  , GetCase "BoolByte False" (get @BoolByte) (Just (1, 0, BoolByte False)) [0x00]
  , GetCase
      "getByteArray"
      (getByteArray 3)
      (Just (3, 1, byteArrayFromList @Word8 [0xFD, 0x6E, 0xEC]))
      [0xFD, 0x6E, 0xEC, 0x5D]
  , GetCase
      "getLiftedPrimArray"
      (getLiftedPrimArray (Proxy :: Proxy Word16LE) 3)
      (Just (6, 1, liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC]))
      [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00, 0x5D]
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
  , GetCase
      "Seq Word16LE"
      (get @(Seq Word16LE))
      (Just (12, 0, Seq.fromList [0xEC, 0x5D]))
      [2, 0, 0, 0, 0, 0, 0, 0, 0xEC, 0, 0x5D, 0]
  , GetCase
      "Seq WordX"
      (get @(Seq WordX))
      (Just (18, 0, Seq.fromList [WordX32 0, WordX32 0]))
      [2, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 4, 0, 0, 0, 0]
  ]

testGet :: (BinaryGetTarget z IO, CaseTarget z) => String -> Proxy z -> TestTree
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
  , PutCase
      "DoubleLE"
      (putDoubleLE (DoubleLE (castWord64ToDouble 0x5DEC6EFD12345678)))
      [0x78, 0x56, 0x34, 0x12, 0xFD, 0x6E, 0xEC, 0x5D]
  , PutCase
      "DoubleBE"
      (putDoubleBE (DoubleBE (castWord64ToDouble 0x5DEC6EFD12345678)))
      [0x5D, 0xEC, 0x6E, 0xFD, 0x12, 0x34, 0x56, 0x78]
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
  , PutCase
      "putLiftedPrimArray"
      (putLiftedPrimArray (liftedPrimArrayFromList @Word16LE [0xFD, 0x6E, 0xEC]))
      [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00]
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
  , PutCase
      "Seq WordX"
      (put @(Seq WordX) (Seq.fromList [WordX32 0, WordX32 0]))
      [ 2
      , 0
      , 0
      , 0
      , 0
      , 0
      , 0
      , 0
      , 4
      , 0
      , 0
      , 0
      , 0
      , 4
      , 0
      , 0
      , 0
      , 0
      ]
  ]

testPut :: (BinaryPutTarget z IO, CaseTarget z) => String -> Proxy z -> TestTree
testPut n p = testGroup ("put (" ++ n ++ ")") (fmap (runPutCase p) putCases)

testLiftedPrimArray :: TestTree
testLiftedPrimArray =
  testGroup
    "liftedPrimArray"
    [ testLiftedPrimArrayFromList
    , testLiftedPrimArrayCopy
    , testLiftedPrimArraySet
    , testLiftedPrimArrayMap
    , testLiftedPrimArrayConcat
    , testLiftedPrimArrayMerge
    ]

mkArr :: [Word16LE] -> LiftedPrimArray Word16LE
mkArr = liftedPrimArrayFromList @Word16LE

mkDestArr :: ElemCount -> (MutableLiftedPrimArray (PrimState IO) Word16LE -> IO ()) -> IO (LiftedPrimArray Word16LE)
mkDestArr len f = do
  darr <- newLiftedPrimArray len (Proxy @Word16LE)
  liftIO (for_ [0 .. len - 1] (\ix -> writeLiftedPrimArray darr ix 0))
  f darr
  freezeLiftedPrimArray darr 0 len

assertArrEq :: LiftedPrimArray a -> IO (LiftedPrimArray a) -> TestT IO ()
assertArrEq expected actualAct = do
  actual <- liftIO actualAct
  actual === expected

testLiftedPrimArrayMap :: TestTree
testLiftedPrimArrayMap = testUnit "map" $ do
  let mkArr32 = liftedPrimArrayFromList @Word32LE
      mkMap = mapLiftedPrimArray @Word16LE @Word32LE ((+ 1) . fromIntegral) . mkArr
  mkMap [] === mkArr32 []
  mkMap [1, 2] === mkArr32 [2, 3]

testLiftedPrimArrayConcat :: TestTree
testLiftedPrimArrayConcat = testUnit "concat" $ do
  let mkConcat = concatLiftedPrimArray . fmap mkArr
  mkConcat [] === mkArr []
  mkConcat [[1, 2]] === mkArr [1, 2]
  mkConcat [[1, 2], []] === mkArr [1, 2]
  mkConcat [[], [1, 2]] === mkArr [1, 2]
  mkConcat [[1, 2], [3]] === mkArr [1, 2, 3]
  mkConcat [[0], [1, 2], [3]] === mkArr [0, 1, 2, 3]

testLiftedPrimArrayMerge :: TestTree
testLiftedPrimArrayMerge = testUnit "merge" $ do
  let mkMerge = mergeLiftedPrimArray 0 (+) . fmap mkArr
  mkMerge [] === mkArr []
  mkMerge [[1, 2]] === mkArr [1, 2]
  mkMerge [[1, 2], []] === mkArr [1, 2]
  mkMerge [[], [1, 2]] === mkArr [1, 2]
  mkMerge [[1, 2], [3]] === mkArr [4, 2]
  mkMerge [[0], [1, 2], [3]] === mkArr [4, 2]

testLiftedPrimArrayFromList :: TestTree
testLiftedPrimArrayFromList = testUnit "fromList" $ do
  let arr = LiftedPrimArray (byteArrayFromList @Word8 [0xFD, 0x00, 0x6E, 0x00, 0xEC, 0x00]) :: LiftedPrimArray Word16LE
  mkArr [0xFD, 0x6E, 0xEC] === arr
  sizeofLiftedPrimArray arr === 6
  lengthLiftedPrimArray arr === 3

testLiftedPrimArrayCopy :: TestTree
testLiftedPrimArrayCopy = testUnit "copy" $ do
  let sarr = replicateLiftedPrimArray @Word16LE 2 1
  sarr === mkArr [1, 1]
  assertArrEq
    (mkArr [1, 1, 0, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 0 sarr 0 2))
  assertArrEq
    (mkArr [0, 1, 1, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 1 sarr 0 2))
  assertArrEq
    (mkArr [0, 0, 1, 1])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 2 sarr 0 2))
  assertArrEq
    (mkArr [0, 0, 0, 1])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 3 sarr 0 2))
  assertArrEq
    (mkArr [0, 0, 0, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 4 sarr 0 2))
  assertArrEq
    (mkArr [0, 0, 0, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr (-1) sarr 0 2))
  assertArrEq
    (mkArr [0, 1, 0, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 1 sarr 0 1))
  assertArrEq
    (mkArr [0, 1, 1, 0])
    (mkDestArr 4 (\darr -> copyLiftedPrimArray darr 1 sarr 0 3))

testLiftedPrimArraySet :: TestTree
testLiftedPrimArraySet = testUnit "set" $ do
  assertArrEq
    (mkArr [1, 0, 0])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 0 1 1))
  assertArrEq
    (mkArr [1, 1, 0])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 0 2 1))
  assertArrEq
    (mkArr [1, 1, 1])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 0 3 1))
  assertArrEq
    (mkArr [1, 1, 1])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 0 4 1))
  assertArrEq
    (mkArr [0, 1, 1])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 1 2 1))
  assertArrEq
    (mkArr [0, 1, 1])
    (mkDestArr 3 (\darr -> setLiftedPrimArray darr 1 3 1))

testGetOffset :: (BinaryGetTarget z IO, CaseTarget z) => String -> Proxy z -> TestTree
testGetOffset n p = testUnit ("get offset (" ++ n ++ ")") $ do
  let buf = [0x12, 0x34, 0x56, 0x78]
  src <- liftIO (initSource buf)
  (ez1, c1) <- liftIO (getTargetOffset 0 getWord8 (src `asProxyTypeOf` p))
  ez1 === Right 0x12
  c1 === 1
  (ez2, c2) <- liftIO (getTargetOffset 1 getWord16LE src)
  ez2 === Right 0x5634
  c2 === 3
  (ez3, c3) <- liftIO (getTargetOffset 3 getWord16LE src)
  ez3 === Left (GetErrorGlobalCap "Word16LE" 1 2)
  c3 === 3

data WordX
  = WordX8 !Word8
  | WordX16 !Word16LE
  | WordX32 !Word32LE
  deriving stock (Eq, Ord, Show)

instance Binary WordX where
  byteSize =
    succ . \case
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
wordXGen = Gen.choice [gen8, gen16, gen32]
 where
  gen8 = fmap WordX8 (Gen.integral (Range.constant 0 maxBound))
  gen16 = fmap (WordX16 . Word16LE) (Gen.integral (Range.constant 0 maxBound))
  gen32 = fmap (WordX32 . Word32LE) (Gen.integral (Range.constant 0 maxBound))

takeDiff
  :: (CaseTarget z) => z -> ByteCount -> ByteCount -> ByteCount -> [ByteCount] -> IO ([ByteCount], (ByteCount, z))
takeDiff z pos had diff = go 0
 where
  go !acc = \case
    y : ys | acc < diff -> go (acc + y) ys
    ys -> do
      zz <- sliceBuffer z pos (had + acc)
      pure (ys, (acc, zz))

testGetInc :: (BinaryPutTarget z IO, CaseTarget z) => TestLimit -> String -> Proxy z -> TestTree
testGetInc lim n p = testProp ("get inc (" ++ n ++ ")") lim $ do
  -- Generate some random elements
  numElems <- forAll (Gen.integral (Range.constant 0 20))
  xs <- forAll (replicateM numElems wordXGen)
  -- First some sanity checks that encode/decode work
  length xs === numElems
  wholeBuf <- liftIO (encode xs)
  (exs, totLen) <- liftIO (decodeEnd (wholeBuf `asProxyTypeOf` p))
  case exs of
    Left err -> fail (show err)
    Right xs' -> do
      xs' === xs
      totLen === byteSize xs
  wholeBuf' <- liftIO (sliceBuffer wholeBuf 0 totLen)
  wholeBuf' === wholeBuf
  -- Shuffle list to come up with splits and test incremental
  ys <- forAll (Gen.shuffle (8 : fmap byteSize xs))
  ysRef <- liftIO (newIORef ys)
  sentRef <- liftIO (newIORef 0)
  let cb (GetIncRequest pos _ len) = do
        sent <- readIORef sentRef
        let had = sent - pos
        let diff = len - had
        ys' <- readIORef ysRef
        (ys'', (bufLen, buf)) <- takeDiff wholeBuf pos had diff ys'
        writeIORef ysRef ys''
        modifyIORef' sentRef (+ bufLen)
        pure $
          if bufLen == 0
            then Nothing
            else Just (buf `asProxyTypeOf` p)
  (ezs, totLen', _) <- liftIO (decodeInc (Just totLen) cb)
  case ezs of
    Left err -> fail (show err)
    Right zs -> do
      zs === xs
      totLen' === totLen

testMutPut :: (MutBinaryPutTarget z IO, MutCaseTarget z) => String -> Proxy z -> TestTree
testMutPut n p = testGroup ("mut put (" ++ n ++ ")") (fmap (mutRunPutCase p) putCases)

testMutPutOffset :: (MutBinaryPutTarget z IO, MutCaseTarget z) => String -> Proxy z -> TestTree
testMutPutOffset n p = testUnit ("mut put offset (" ++ n ++ ")") $ do
  u <- liftIO (newSink 4)
  c1 <- liftIO (mutPutTargetOffset 0 (putWord8 0x12) u)
  c1 === 1
  c2 <- liftIO (mutPutTargetOffset 1 (putWord16LE 0x5634) u)
  c2 === 3
  x <- liftIO (freezeSink (u `asProxyTypeOf` p))
  take 3 x === [0x12, 0x34, 0x56]
  -- Use this opportunity to test getting
  (w, h) <- liftIO (getTarget (liftA2 (,) (get @Word8) (get @Word16LE)) u)
  w === Right (0x12, 0x5634)
  h === 3
  (y, i) <- liftIO (getTargetOffset 0 (get @Word8) u)
  y === Right 0x12
  i === 1
  (z, j) <- liftIO (getTargetOffset 1 (get @Word16LE) u)
  z === Right 0x5634
  j === 3

data TargetDef where
  TargetDef :: (BinaryPutTarget z IO, CaseTarget z) => String -> Proxy z -> TargetDef

targets :: [TargetDef]
targets =
  [ TargetDef "ShortByteString" (Proxy :: Proxy ShortByteString)
  , TargetDef "ByteString" (Proxy :: Proxy ByteString)
  , TargetDef "Vector" (Proxy :: Proxy (Vector Word8))
  ]

data MutTargetDef where
  MutTargetDef :: (MutBinaryPutTarget z IO, MutCaseTarget z) => String -> Proxy z -> MutTargetDef

mutTargets :: [MutTargetDef]
mutTargets =
  [ MutTargetDef "MutableByteArray" (Proxy :: Proxy (MutableByteArray RealWorld))
  , MutTargetDef "IOVector" (Proxy :: Proxy (IOVector Word8))
  ]

testDahdit :: TestLimit -> TestTree
testDahdit lim = testGroup "Dahdit" trees
 where
  trees = baseTrees ++ targetTrees ++ mutTargetTrees
  baseTrees = [testByteSize, testStaticByteSize, testLiftedPrimArray]
  targetTrees =
    targets >>= \(TargetDef name prox) ->
      [ testGet name prox
      , testGetOffset name prox
      , testGetInc lim name prox
      , testPut name prox
      ]
  mutTargetTrees =
    mutTargets >>= \(MutTargetDef name prox) ->
      [ testMutPut name prox
      , testMutPutOffset name prox
      ]

main :: IO ()
main = testMain testDahdit
