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
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Nanotime (NtpTime (..))
import PropUnit (GenDefault (..), genDefaultEnum, genDefaultGeneric)
import Test.Dahdit.GenDefault
  ( DahditTag
  , genFractional
  , genList
  , genSBS
  , genSeq
  , genSigned
  , genSum
  , genUnsigned
  )

data P

type I = DahditTag P

genDefaultI :: (GenDefault I a) => Gen a
genDefaultI = genDefault (Proxy @I)

-- Binary

instance GenDefault I MB.MidiWord7 where
  genDefault _ = fmap MB.MidiWord7 genUnsigned

instance GenDefault I MB.MidiInt7 where
  genDefault _ = fmap MB.MidiInt7 genSigned

instance GenDefault I MB.MidiWord14 where
  genDefault _ = fmap MB.MidiWord14 genUnsigned

instance GenDefault I MB.MidiInt14 where
  genDefault _ = fmap MB.MidiInt14 genSigned

instance GenDefault I MB.VarWord where
  genDefault _ = fmap MB.VarWord (Gen.integral (Range.constant 0 0x00FFFFFF))

-- OscAddr

-- TODO generate addr pat and serialize
instance GenDefault I MOA.RawAddrPat where
  genDefault _ = MOA.RawAddrPat . ("/" <>) . T.intercalate "/" <$> genList 1 3 g
   where
    g = Gen.element ["x", "y"]

-- Midi

instance GenDefault I MM.Channel where
  genDefault _ = genDefaultEnum Proxy

instance GenDefault I MM.ChannelCount where
  genDefault = fmap MM.ChannelCount . genDefault

instance GenDefault I MM.Note where
  genDefault = fmap MM.Note . genDefault

instance GenDefault I MM.Velocity where
  genDefault = fmap MM.Velocity . genDefault

instance GenDefault I MM.ControlNum where
  genDefault = fmap MM.ControlNum . genDefault

instance GenDefault I MM.ControlVal where
  genDefault = fmap MM.ControlVal . genDefault

instance GenDefault I MM.Pressure where
  genDefault = fmap MM.Pressure . genDefault

instance GenDefault I MM.ProgramNum where
  genDefault = fmap MM.ProgramNum . genDefault

instance GenDefault I MM.PitchBend where
  genDefault = fmap MM.PitchBend . genDefault

instance GenDefault I MM.Song where
  genDefault = fmap MM.Song . genDefault

instance GenDefault I MM.Position where
  genDefault = fmap MM.Position . genDefault

instance GenDefault I MM.ShortManf where
  genDefault p = go
   where
    go = do
      i <- genDefault p
      if i == 0x00 || i == 0x7E || i == 0x7F
        then go
        else pure (MM.ShortManf i)

instance (GenDefault I MM.LongManf) where
  genDefault = fmap MM.LongManf . genDefault

instance GenDefault I MM.Manf where
  genDefault = genDefaultGeneric

instance GenDefault I MM.QuarterTimeUnit where
  genDefault = genDefaultGeneric

instance GenDefault I MM.QuarterTime where
  genDefault p = MM.QuarterTime <$> genDefault p <*> genUnsigned

instance GenDefault I MM.ChanStatusType where
  genDefault = genDefaultEnum

instance GenDefault I MM.CommonStatus where
  genDefault = genDefaultEnum

instance GenDefault I MM.RtStatus where
  genDefault = genDefaultEnum

instance GenDefault I MM.ChanStatus where
  genDefault = genDefaultGeneric

instance GenDefault I MM.LiveStatus where
  genDefault = genDefaultGeneric

instance GenDefault I MM.RecStatus where
  genDefault = genDefaultGeneric

instance GenDefault I MM.ShortStatus where
  genDefault = genDefaultGeneric

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
    genCN = fmap (MM.ControlNum . MB.MidiWord7) (Gen.integral (Range.constant 0x00 0x77))

instance GenDefault I MM.MetaString where
  genDefault _ = fmap MM.MetaString (genSBS 0 3)

instance GenDefault I MM.MetaData where
  genDefault = genDefaultGeneric

instance GenDefault I MM.ChanModeData where
  genDefault = genDefaultGeneric

instance GenDefault I MM.ChanData where
  genDefault = genDefaultGeneric

-- Generate a bytestring not including the delimiter
genPayload :: Gen ShortByteString
genPayload = fmap (BSS.pack . fmap fromIntegral) (genList 0 3 (genDefault @I @MB.MidiWord7 (Proxy @I)))

instance GenDefault I MM.UnivSysEx where
  genDefault _ = MM.UnivSysEx <$> Gen.element [0x7E, 0x7F] <*> genPayload

instance GenDefault I MM.ManfSysEx where
  genDefault p = MM.ManfSysEx <$> genDefault p <*> genPayload

instance GenDefault I MM.SysExData where
  genDefault = genDefaultGeneric

instance GenDefault I MM.CommonData where
  genDefault = genDefaultGeneric

instance GenDefault I MM.LiveMsg where
  genDefault = genDefaultGeneric

instance GenDefault I MM.RecMsg where
  genDefault = genDefaultGeneric

instance GenDefault I MM.ShortMsg where
  genDefault = genDefaultGeneric

instance GenDefault I MM.Event where
  genDefault = genDefaultGeneric

instance GenDefault I MM.Track where
  genDefault = fmap MM.Track . genSeq 0 3 . genDefault

instance GenDefault I MM.MidFileType where
  genDefault = genDefaultEnum

instance GenDefault I MM.MidFile where
  genDefault p = MM.MidFile <$> genDefault p <*> genDefault p <*> genSeq 0 3 (genDefault p)

instance GenDefault I MM.SysExDump where
  genDefault = fmap MM.SysExDump . genSeq 0 3 . genDefault

-- Osc

instance GenDefault I MO.DatumType where
  genDefault = genDefaultEnum

instance GenDefault I MO.Port where
  genDefault = fmap MO.Port . genDefault

instance GenDefault I MO.PortMsg where
  genDefault = genDefaultGeneric

instance GenDefault I MO.Sig where
  genDefault = fmap MO.Sig . genSeq 0 3 . genDefault

instance GenDefault I MO.Datum where
  genDefault p =
    Gen.choice
      [ MO.DatumInt32 <$> genSigned
      , MO.DatumInt64 <$> genSigned
      , MO.DatumFloat <$> genFractional
      , MO.DatumDouble <$> genFractional
      , MO.DatumString . T.pack <$> genList 0 3 (Gen.element ['x', 'y'])
      , MO.DatumBlob <$> genSBS 0 3
      , MO.DatumTime . NtpTime <$> genUnsigned
      , MO.DatumMidi <$> genDefault p
      ]

instance GenDefault I MO.Msg where
  genDefault p = MO.Msg <$> genDefault p <*> genSeq 0 3 (genDefault p)

instance GenDefault I MO.Bundle where
  genDefault p = (MO.Bundle . NtpTime <$> genUnsigned) <*> genSeq 0 3 (genDefault p)

instance GenDefault I MO.Packet where
  genDefault = genDefaultGeneric
