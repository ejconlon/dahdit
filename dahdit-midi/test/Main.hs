{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized, decodeFileEnd)
import Dahdit.Midi.Binary
  ( MidiInt14
  , MidiInt7
  , MidiWord14
  , MidiWord7
  , VarWord
  )
import Dahdit.Midi.Midi qualified as MM
import Dahdit.Midi.Osc qualified as MO
import Dahdit.Midi.OscAddr qualified as MOA
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import PropUnit (MonadTest, PropertyT, testGroup, testMain)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Dahdit.Daytripper (expectBytes, expectCodecErr, expectCodecOk, expectStatic)
import Test.Dahdit.Midi.GenDefault (genDefaultI)
import Test.Daytripper (Expect, RT, mkFileRT, mkPropRT, mkUnitRT, testRT)

expectStaticOk
  :: (MonadTest m, MonadIO m, Binary a, Eq a, Show a, StaticByteSized a) => Expect m a ByteString (Either GetError a)
expectStaticOk = expectStatic expectCodecOk

genCases :: [RT]
genCases =
  [ mkPropRT "MidiWord7" expectStaticOk (genDefaultI @MidiWord7)
  , mkPropRT "MidiInt7" expectStaticOk (genDefaultI @MidiInt7)
  , mkPropRT "MidiWord14" expectStaticOk (genDefaultI @MidiWord14)
  , mkPropRT "MidiInt14" expectStaticOk (genDefaultI @MidiInt14)
  , mkPropRT "VarWord" expectCodecOk (genDefaultI @VarWord)
  , mkPropRT "Channel" expectStaticOk (genDefaultI @MM.Channel)
  , mkPropRT "Note" expectStaticOk (genDefaultI @MM.Note)
  , mkPropRT "Velocity" expectStaticOk (genDefaultI @MM.Velocity)
  , mkPropRT "ControlNum" expectStaticOk (genDefaultI @MM.ControlNum)
  , mkPropRT "ControlVal" expectStaticOk (genDefaultI @MM.ControlVal)
  , mkPropRT "Pressure" expectStaticOk (genDefaultI @MM.Pressure)
  , mkPropRT "ProgramNum" expectStaticOk (genDefaultI @MM.ProgramNum)
  , mkPropRT "PitchBend" expectStaticOk (genDefaultI @MM.PitchBend)
  , mkPropRT "Song" expectStaticOk (genDefaultI @MM.Song)
  , mkPropRT "Position" expectStaticOk (genDefaultI @MM.Position)
  , mkPropRT "ShortManf" expectStaticOk (genDefaultI @MM.ShortManf)
  , mkPropRT "LongManf" expectStaticOk (genDefaultI @MM.LongManf)
  , mkPropRT "Manf" expectCodecOk (genDefaultI @MM.Manf)
  , mkPropRT "QuarterTime" expectStaticOk (genDefaultI @MM.QuarterTime)
  , mkPropRT "UnivSysEx" expectCodecOk (genDefaultI @MM.UnivSysEx)
  , mkPropRT "ManfSysEx" expectCodecOk (genDefaultI @MM.ManfSysEx)
  , mkPropRT "SysExData" expectCodecOk (genDefaultI @MM.SysExData)
  , mkPropRT "LiveStatus" expectStaticOk (genDefaultI @MM.LiveStatus)
  , mkPropRT "RecStatus" expectStaticOk (genDefaultI @MM.RecStatus)
  , mkPropRT "ShortStatus" expectStaticOk (genDefaultI @MM.ShortStatus)
  , mkPropRT "MetaString" expectCodecOk (genDefaultI @MM.MetaString)
  , mkPropRT "MetaData" expectCodecOk (genDefaultI @MM.MetaData)
  , mkPropRT "LiveMsg" expectCodecOk (genDefaultI @MM.LiveMsg)
  , mkPropRT "RecMsg" expectCodecOk (genDefaultI @MM.RecMsg)
  , mkPropRT "ShortMsg" expectCodecOk (genDefaultI @MM.ShortMsg)
  , mkPropRT "Track" expectCodecOk (genDefaultI @MM.Track)
  , mkPropRT "MidFile" expectCodecOk (genDefaultI @MM.MidFile)
  , mkPropRT "SysExDump" expectCodecOk (genDefaultI @MM.SysExDump)
  , mkPropRT "RawAddrPat" expectCodecOk (genDefaultI @MOA.RawAddrPat)
  , mkPropRT "PortMsg" expectCodecOk (genDefaultI @MO.PortMsg)
  , mkPropRT "Msg" expectCodecOk (genDefaultI @MO.Msg)
  , mkPropRT "Bundle" expectCodecOk (genDefaultI @MO.Bundle)
  , mkPropRT "Packet" expectCodecOk (genDefaultI @MO.Packet)
  ]

findFiles :: IO [FilePath]
findFiles = do
  let tdDir = "testdata"
      midiDir = tdDir </> "midi"
  let xtraFiles = fmap (tdDir </>) ["twinkle.mid", "parse_me.mid"]
  midiFiles <- fmap (fmap (midiDir </>)) (listDirectory midiDir)
  pure (xtraFiles ++ midiFiles)

decodeFileAs :: (Binary a) => Proxy a -> FilePath -> IO (Either GetError a, ByteCount)
decodeFileAs _ = decodeFileEnd

shouldFail :: FilePath -> Bool
shouldFail fn =
  let xs = fmap (\p -> BSC.pack ("/test-" ++ p ++ "-")) ["illegal", "non-midi", "corrupt"]
  in  any (`BSC.isInfixOf` BSC.pack fn) xs

suiteFileExpect :: (Binary a, Eq a, Show a) => FilePath -> Expect (PropertyT IO) a ByteString (Either GetError a)
suiteFileExpect fn =
  if shouldFail fn
    then expectCodecErr
    else expectCodecOk

suiteFileRT :: FilePath -> IO RT
suiteFileRT fn =
  let ext = takeExtension fn
  in  case ext of
        ".mid" -> pure (mkFileRT fn (suiteFileExpect @MM.MidFile fn) fn Nothing)
        ".syx" -> pure (mkFileRT fn (suiteFileExpect @MM.SysExDump fn) fn Nothing)
        _ -> fail ("Unhandled file format: " ++ ext)

{- FOURMOLU_DISABLE -}
oscBytes :: [Word8]
oscBytes =
  [ 0x2f, 0x6f, 0x73, 0x63, 0x69, 0x6c, 0x6c, 0x61
  , 0x74, 0x6f, 0x72, 0x2f, 0x34, 0x2f, 0x66, 0x72
  , 0x65, 0x71, 0x75, 0x65, 0x6e, 0x63, 0x79, 0x00
  , 0x2c, 0x66, 0x00, 0x00, 0x43, 0xdc, 0x00, 0x00
  ]
{- FOURMOLU_ENABLE -}

unitCases :: [RT]
unitCases =
  [ mkUnitRT
      "OSC msg"
      (expectBytes oscBytes expectCodecOk)
      (MO.Msg "/oscillator/4/frequency" (Seq.singleton (MO.DatumFloat 440.0)))
  ]

-- Increase number of examples with TASTY_FALSIFY_TESTS=1000 etc
main :: IO ()
main = do
  files <- findFiles
  fileCases <- traverse suiteFileRT files
  testMain $ \lim ->
    let testGenCases = testGroup "Gen" (fmap (testRT lim) genCases)
        testFileCases = testGroup "File" (fmap (testRT lim) fileCases)
        testUnitCases = testGroup "Unit" (fmap (testRT lim) unitCases)
    in  testGroup
          "dahdit-midi"
          [ testGenCases
          , testUnitCases
          , testFileCases
          ]
