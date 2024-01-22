{-# LANGUAGE OverloadedStrings #-}

module Test.Dahdit.Midi.GenDefault
  ( genDefaultI
  )
where

import Dahdit.Midi.Binary qualified as MB
import Dahdit.Midi.Midi qualified as MM
import Dahdit.Midi.Osc qualified as MO
import Dahdit.Midi.OscAddr qualified as MOA
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int7, Word7)
import Data.Text qualified as T
import Nanotime (NtpTime (..))
import Test.Dahdit.GenDefault
  ( DahditTag
  , ViaSigned (..)
  , ViaUnsigned (..)
  , genFractional
  , genList
  , genSBS
  , genSeq
  , genSigned
  , genSum
  , genUnsigned
  )
import Test.Falsify.GenDefault (GenDefault (..), ViaEnum (..), ViaGeneric (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as FG
import Test.Falsify.Range qualified as FR

data P

type I = DahditTag P

genDefaultI :: (GenDefault I a) => Gen a
genDefaultI = genDefault (Proxy @I)

-- Binary

deriving via (ViaUnsigned Word7) instance GenDefault I MB.MidiWord7

deriving via (ViaSigned Int7) instance GenDefault I MB.MidiInt7

deriving via (ViaUnsigned MB.Word14) instance GenDefault I MB.MidiWord14

deriving via (ViaSigned MB.Int14) instance GenDefault I MB.MidiInt14

instance GenDefault I MB.VarWord where
  genDefault _ = fmap MB.VarWord (FG.inRange (FR.between (0, 0x00FFFFFF)))

-- OscAddr

-- TODO generate addr pat and serialize
instance GenDefault I MOA.RawAddrPat where
  genDefault _ = MOA.RawAddrPat . ("/" <>) . T.intercalate "/" <$> genList 1 3 g
   where
    g = FG.choose (pure "x") (pure "y")

-- Midi

deriving via (ViaEnum MM.Channel) instance GenDefault I MM.Channel

deriving newtype instance GenDefault I MM.ChannelCount

deriving newtype instance GenDefault I MM.Note

deriving newtype instance GenDefault I MM.Velocity

deriving newtype instance GenDefault I MM.ControlNum

deriving newtype instance GenDefault I MM.ControlVal

deriving newtype instance GenDefault I MM.Pressure

deriving newtype instance GenDefault I MM.ProgramNum

deriving newtype instance GenDefault I MM.PitchBend

deriving newtype instance GenDefault I MM.Song

deriving newtype instance GenDefault I MM.Position

instance GenDefault I MM.ShortManf where
  genDefault p = go
   where
    go = do
      i <- genDefault p
      if i == 0x00 || i == 0x7E || i == 0x7F
        then go
        else pure (MM.ShortManf i)

deriving newtype instance (GenDefault I MM.LongManf)

deriving via (ViaGeneric I MM.Manf) instance GenDefault I MM.Manf

deriving via (ViaGeneric I MM.QuarterTimeUnit) instance GenDefault I MM.QuarterTimeUnit

instance GenDefault I MM.QuarterTime where
  genDefault p = MM.QuarterTime <$> genDefault p <*> genUnsigned

deriving via (ViaEnum MM.ChanStatusType) instance GenDefault I MM.ChanStatusType

deriving via (ViaEnum MM.CommonStatus) instance GenDefault I MM.CommonStatus

deriving via (ViaEnum MM.RtStatus) instance GenDefault I MM.RtStatus

deriving via (ViaGeneric I MM.ChanStatus) instance GenDefault I MM.ChanStatus

deriving via (ViaGeneric I MM.LiveStatus) instance GenDefault I MM.LiveStatus

deriving via (ViaGeneric I MM.RecStatus) instance GenDefault I MM.RecStatus

deriving via (ViaGeneric I MM.ShortStatus) instance GenDefault I MM.ShortStatus

instance GenDefault I MM.ChanVoiceData where
  genDefault p = genCVD
   where
    genCVD =
      genSum $
        NE.fromList
          [ MM.ChanVoiceDataNoteOff <$> genDefault p <*> genDefault p
          , MM.ChanVoiceDataNoteOn <$> genDefault p <*> genDefault p
          , MM.ChanVoiceKeyAftertouch <$> genDefault p <*> genDefault p
          , MM.ChanVoiceControlChange <$> genCN <*> genDefault p
          , MM.ChanVoiceProgramChange <$> genDefault p
          , MM.ChanVoiceChanAftertouch <$> genDefault p
          , MM.ChanVoicePitchBend <$> genDefault p
          ]
    genCN = fmap (MM.ControlNum . MB.MidiWord7) (FG.inRange (FR.between (0x00, 0x77)))

instance GenDefault I MM.MetaString where
  genDefault _ = fmap MM.MetaString (genSBS 0 3)

deriving via (ViaGeneric I MM.MetaData) instance GenDefault I MM.MetaData

deriving via (ViaGeneric I MM.ChanModeData) instance GenDefault I MM.ChanModeData

deriving via (ViaGeneric I MM.ChanData) instance GenDefault I MM.ChanData

-- Generate a bytestring not including the delimiter
genPayload :: Gen ShortByteString
genPayload = fmap (BSS.pack . fmap fromIntegral) (genList 0 3 (genDefault @I @MB.MidiWord7 (Proxy @I)))

instance GenDefault I MM.UnivSysEx where
  genDefault _ = MM.UnivSysEx <$> FG.choose (pure 0x7E) (pure 0x7F) <*> genPayload

instance GenDefault I MM.ManfSysEx where
  genDefault p = MM.ManfSysEx <$> genDefault p <*> genPayload

deriving via (ViaGeneric I MM.SysExData) instance GenDefault I MM.SysExData

deriving via (ViaGeneric I MM.CommonData) instance GenDefault I MM.CommonData

deriving via (ViaGeneric I MM.LiveMsg) instance GenDefault I MM.LiveMsg

deriving via (ViaGeneric I MM.RecMsg) instance GenDefault I MM.RecMsg

deriving via (ViaGeneric I MM.ShortMsg) instance GenDefault I MM.ShortMsg

deriving via (ViaGeneric I MM.Event) instance GenDefault I MM.Event

instance GenDefault I MM.Track where
  genDefault = fmap MM.Track . genSeq 0 3 . genDefault

deriving via (ViaEnum MM.MidFileType) instance GenDefault I MM.MidFileType

instance GenDefault I MM.MidFile where
  genDefault p = MM.MidFile <$> genDefault p <*> genDefault p <*> genSeq 0 3 (genDefault p)

instance GenDefault I MM.SysExDump where
  genDefault = fmap MM.SysExDump . genSeq 0 3 . genDefault

-- Osc

deriving via (ViaEnum MO.DatumType) instance GenDefault I MO.DatumType

deriving newtype instance GenDefault I MO.Port

deriving via (ViaGeneric I MO.PortMsg) instance GenDefault I MO.PortMsg

instance GenDefault I MO.Sig where
  genDefault = fmap MO.Sig . genSeq 0 3 . genDefault

instance GenDefault I MO.Datum where
  genDefault p =
    foldr1
      FG.choose
      [ MO.DatumInt32 <$> genSigned
      , MO.DatumInt64 <$> genSigned
      , MO.DatumFloat <$> genFractional
      , MO.DatumDouble <$> genFractional
      , MO.DatumString . T.pack <$> genList 0 3 (FG.choose (pure 'x') (pure 'y'))
      , MO.DatumBlob <$> genSBS 0 3
      , MO.DatumTime . NtpTime <$> genUnsigned
      , MO.DatumMidi <$> genDefault p
      ]

instance GenDefault I MO.Msg where
  genDefault p = MO.Msg <$> genDefault p <*> genSeq 0 3 (genDefault p)

instance GenDefault I MO.Bundle where
  genDefault p = MO.Bundle <$> (NtpTime <$> genUnsigned) <*> genSeq 0 3 (genDefault p)

deriving via (ViaGeneric I MO.Packet) instance GenDefault I MO.Packet
