{-# LANGUAGE TemplateHaskell #-}

module Dahdit.Midi.Binary
  ( getTermText
  , putTermText
  , BoundedBinary (..)
  , MidiWord7 (..)
  , MidiInt7 (..)
  , MidiWord14 (..)
  , MidiInt14 (..)
  , VarWord (..)
  , Word14
  , Int14
  )
where

import Control.Newtype (Newtype (..))
import Dahdit (Binary (..), Get, Put, StaticByteSized (..), TermBytes8 (..), Word16BE (..), putText)
import Data.Bits (Bits (..))
import Data.ByteString.Short qualified as BSS
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int7, Word7)
import Data.ShortWord.TH (mkShortWord)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

getTermText :: Get Text
getTermText = do
  TermBytes8 bs <- get
  case TE.decodeUtf8' (BSS.fromShort bs) of
    Left err -> fail ("Invalid UTF-8: " ++ show err)
    Right s -> pure s

putTermText :: Text -> Put
putTermText s = putText s *> put @Word8 0

newtype BoundedBinary (s :: Symbol) a b = BoundedBinary {unBoundedBinary :: a}

instance
  (KnownSymbol s, Bounded a, Ord a, Show a, Newtype a b, Binary b)
  => Binary (BoundedBinary s a b)
  where
  get = do
    w <- get
    let v = pack w
    if v < minBound || v > maxBound
      then fail (symbolVal (Proxy :: Proxy s) ++ " value out of bounds: " ++ show v)
      else pure (BoundedBinary v)
  put = put . unpack . unBoundedBinary

newtype MidiWord7 = MidiWord7 {unMidiWord7 :: Word7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable)

instance StaticByteSized MidiWord7 where
  type StaticSize MidiWord7 = 1
  staticByteSize _ = 1

instance Binary MidiWord7 where
  byteSize _ = 1
  get = do
    w <- get @Word8
    if w .&. 0x80 == 0
      then pure (MidiWord7 (fromIntegral w))
      else fail ("Word7 high bit set: " ++ show w)
  put v = put @Word8 (0x7F .&. fromIntegral (unMidiWord7 v))

newtype MidiInt7 = MidiInt7 {unMidiInt7 :: Int7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable)

instance StaticByteSized MidiInt7 where
  type StaticSize MidiInt7 = 1
  staticByteSize _ = 1

instance Binary MidiInt7 where
  byteSize _ = 1
  get = do
    w <- get @Word8
    if w .&. 0x80 == 0
      then pure (MidiInt7 (fromIntegral w))
      else fail ("Int7 high bit set: " ++ show w)
  put v = put @Word8 (0x7F .&. fromIntegral (unMidiInt7 v))

mkShortWord "Word14" "Word14" "aWord14" "Int14" "Int14" "anInt14" ''Word16 14 []

expandW14 :: Word14 -> Word16
expandW14 w =
  let x = fromIntegral w :: Word16
      xLo = x .&. 0x007F
      xHi = shiftL x 1 .&. 0x7F00
  in  xHi .|. xLo

contractW14 :: Word16 -> Word14
contractW14 v =
  let vLo = v .&. 0x007F
      vHi = v .&. 0x7F00
      x = shiftR vHi 1 .|. vLo
  in  fromIntegral x

newtype MidiWord14 = MidiWord14 {unMidiWord14 :: Word14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable)

instance StaticByteSized MidiWord14 where
  type StaticSize MidiWord14 = 2
  staticByteSize _ = 2

instance Binary MidiWord14 where
  byteSize _ = 2
  get = fmap (MidiWord14 . contractW14 . unWord16BE) get
  put = put . Word16BE . expandW14 . unMidiWord14

newtype MidiInt14 = MidiInt14 {unMidiInt14 :: Int14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Hashable)

instance StaticByteSized MidiInt14 where
  type StaticSize MidiInt14 = 2
  staticByteSize _ = 2

instance Binary MidiInt14 where
  byteSize _ = 2
  get = fmap (MidiInt14 . fromIntegral . contractW14 . unWord16BE) get
  put = put . Word16BE . expandW14 . fromIntegral . unMidiInt14

newtype VarWord = VarWord {unVarWord :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Integral, Real, Hashable)

instance Bounded VarWord where
  minBound = VarWord 0
  maxBound = VarWord 0x00FFFFFF

instance Binary VarWord where
  byteSize (VarWord w) =
    if
      | w .&. 0xFFFFFF80 == 0 -> 1
      | w .&. 0xFFFFC000 == 0 -> 2
      | w .&. 0xFFE00000 == 0 -> 3
      | otherwise -> 4
  get = go 0 0
   where
    go !off !acc = do
      w <- get @Word8
      let wLow = fromIntegral (w .&. 0x7F)
          wShift = shiftL wLow off
          accNext = acc .|. wShift
      if w .&. 0x80 == 0
        then pure (VarWord accNext)
        else go (off + 7) accNext

  put (VarWord acc) = go (0 :: Int) acc
   where
    go !off !w = do
      let wLow = fromIntegral (w .&. 0x7F)
          wShift = shiftR w 7
      if wShift == 0 || off == 3
        then put @Word8 wLow
        else put @Word8 (wLow .|. 0x80) *> go (off + 1) wShift
