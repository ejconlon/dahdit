{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>))
import Dahdit
  ( Binary (..)
  , ByteCount
  , ElemCount (..)
  , Get
  , Int16LE (..)
  , ShortByteString
  , StaticByteSized (..)
  , Word8
  , byteSize
  , decode
  , decodeFile
  , decodeFileEnd
  , encode
  , encodeFile
  , getExact
  , getSkip
  , getTarget
  )
import Dahdit.Audio.Aiff (Aiff (..), AiffDataBody (..), lookupAiffDataChunk)
import Dahdit.Audio.Aiff qualified as Aiff
import Dahdit.Audio.Common
  ( LoopMarks (..)
  , UnparsedBody (..)
  , chunkHeaderSize
  , defaultLoopMarkNames
  , defaultNoteNumber
  , defineLoopMarks
  , getChunkSizeLE
  , getExpectLabel
  , guardChunk
  , rethrow
  )
import Dahdit.Audio.Convert
  ( Neutral (..)
  , aiffToNeutral
  , neutralMono
  , neutralToSampleWav
  , neutralToWav
  , wavToNeutral
  )
import Dahdit.Audio.Dsp (ModMeta (..), PcmContainer (..), linearCrossFade, monoFromLeft, runMod)
import Dahdit.Audio.Riff (Chunk (..), KnownChunk (..), KnownListChunk (..), KnownOptChunk (..), labelRiff)
import Dahdit.Audio.Sfont
  ( Bag
  , Gen
  , InfoChunk (..)
  , Inst
  , Mod
  , PdtaChunk (..)
  , Phdr
  , Sdta (..)
  , SdtaChunk (..)
  , Sfont (..)
  , Shdr
  , labelSfbk
  )
import Dahdit.Audio.Wav
  ( Wav (..)
  , WavAdtlChunk
  , WavAdtlData (..)
  , WavAdtlElem (..)
  , WavChunk (..)
  , WavDataBody (..)
  , WavFormatBody (..)
  , WavFormatChunk
  , WavHeader (..)
  , WavInfoElem (..)
  , lookupWavDataChunk
  , lookupWavFormatChunk
  , wavToPcmContainer
  )
import Dahdit.Audio.Wav.Simple qualified as DAWS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Foldable (for_, toList)
import Data.Int (Int8)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (indexByteArray, sizeofByteArray)
import Data.Primitive.PrimArray (primArrayFromList, sizeofPrimArray)
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Test.Daytripper (daytripperMain)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

drumFmtOffset :: ByteCount
drumFmtOffset = 12

drumDataOffset :: ByteCount
drumDataOffset = 36

drumPostDataOffset :: ByteCount
drumPostDataOffset = 497816

drumFmtChunk :: WavFormatChunk
drumFmtChunk = KnownChunk (WavFormatBody 1 2 44100 16 mempty)

drumFileSize :: ByteCount
drumFileSize = 497896

drumEndOffset :: ByteCount
drumEndOffset = drumFileSize + 8

drumHeader :: WavHeader
drumHeader = WavHeader (drumFileSize - 4)

drumDataLen :: ElemCount
drumDataLen = 248886

readShort :: FilePath -> IO ShortByteString
readShort = fmap (BSS.toShort . BSL.toStrict) . BSL.readFile

throwFirst :: (Exception e) => (Either e a, b) -> IO (a, b)
throwFirst (ea, b) =
  case ea of
    Left e -> throwIO e
    Right a -> pure (a, b)

decodeThrow :: (Binary a) => ShortByteString -> IO (a, ByteCount)
decodeThrow = decode >=> throwFirst

getTargetThrow :: Get a -> ShortByteString -> IO (a, ByteCount)
getTargetThrow g z = getTarget g z >>= throwFirst

decodeFileThrow :: (Binary a) => FilePath -> IO (a, ByteCount)
decodeFileThrow = decodeFile >=> throwFirst

testWavSerde :: TestTree
testWavSerde = testCase "serde" $ do
  -- KnownChunk
  byteSize drumFmtChunk @?= 24
  fmtBs <- encode drumFmtChunk
  (fmt, fmtSz) <- decodeThrow fmtBs
  byteSize fmt @?= fmtSz
  fmt @?= drumFmtChunk
  -- Odd and even unparsed chunks
  let unpSz = 10
      unpOdd = Chunk "FOOB" (UnparsedBody "A")
      unpEven = Chunk "FOOB" (UnparsedBody "AB")
  byteSize unpOdd @?= unpSz
  byteSize unpEven @?= unpSz
  unpOddBs <- encode unpOdd
  (unpOdd', unpOddSz) <- decodeThrow unpOddBs
  byteSize unpOdd' @?= unpOddSz
  unpOdd' @?= unpOdd
  unpEvenBs <- encode unpEven
  (unpEven', unpEvenSz) <- decodeThrow unpEvenBs
  byteSize unpEven' @?= unpEvenSz
  unpEven' @?= unpEven
  -- Adtl chunks
  let adtlChunkEven = KnownListChunk (Seq.singleton (WavAdtlElem 42 (WavAdtlDataNote "hi")))
  assertReparses @WavAdtlChunk adtlChunkEven
  let adtlChunkOdd = KnownListChunk (Seq.singleton (WavAdtlElem 42 (WavAdtlDataNote "h")))
  assertReparses @WavAdtlChunk adtlChunkOdd

testWavHeader :: TestTree
testWavHeader = testCase "header" $ do
  bs <- readShort "testdata/drums.wav"
  (header, bc) <- decodeThrow bs
  header @?= drumHeader
  bc @?= drumFmtOffset

testWavData :: TestTree
testWavData = testCase "data" $ do
  bs <- readShort "testdata/drums.wav"
  (arr, _) <- flip getTargetThrow bs $ do
    getSkip drumDataOffset
    chunk <- get
    case chunk of
      WavChunkData (KnownChunk (WavDataBody arr)) -> pure arr
      _ -> fail "expected data"
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2 -- x2 for 2-byte samples

testWavInfo :: TestTree
testWavInfo = testCase "info" $ do
  bs <- readShort "testdata/drums.wav"
  (info, _) <- flip getTargetThrow bs $ do
    getSkip drumPostDataOffset
    chunk <- get
    case chunk of
      WavChunkInfo (KnownListChunk info) -> pure info
      _ -> fail "expected info"
  info @?= Seq.fromList [WavInfoElem "IART" "freewavesamples.com"]

testWavWhole :: TestTree
testWavWhole = testCase "whole" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, _) <- decodeThrow bs
  fmtChunk <- rethrow (guardChunk "format" (lookupWavFormatChunk wav))
  fmtChunk @?= drumFmtChunk
  KnownChunk (WavDataBody arr) <- rethrow (guardChunk "data" (lookupWavDataChunk wav))
  fromIntegral (sizeofByteArray arr) @?= drumDataLen * 2 -- x2 for 2-byte samples
  Seq.length (wavChunks wav) @?= 4

testWavWrite :: TestTree
testWavWrite = testCase "write" $ do
  bs <- readShort "testdata/drums.wav"
  (wav, bc) <- decodeThrow bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  bs' <- encode wav
  bs' @?= bs

testWavWrite2 :: TestTree
testWavWrite2 = testCase "write 2" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.wav"
  (wav, bc) <- decodeThrow bs
  byteSize wav @?= bc
  for_ (wavChunks wav) assertReparses
  bs' <- encode wav
  bs' @?= bs

testWav :: TestTree
testWav = testGroup "wav" [testWavSerde, testWavHeader, testWavData, testWavInfo, testWavWhole, testWavWrite, testWavWrite2]

testAiff :: TestTree
testAiff = testCase "aiff" $ do
  bs <- readShort "testdata/M1F1-int16s-AFsp.aif"
  (aiff, bc) <- decodeThrow bs
  byteSize aiff @?= bc
  bc @?= fromIntegral (BSS.length bs)
  for_ (aiffChunks aiff) assertReparses
  bs' <- encode aiff
  fromIntegral (BSS.length bs') @?= bc
  bs' @?= bs

testAiff2 :: TestTree
testAiff2 = testCase "aiff2" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.aif"
  (aiff :: Aiff, _) <- decodeThrow bs
  bs' <- encode aiff
  bs' @?= bs

testSfontWhole :: TestTree
testSfontWhole = testCase "whole" $ do
  bs <- readShort "testdata/timpani.sf2"
  ( Sfont (InfoChunk (KnownListChunk infos)) (SdtaChunk (KnownOptChunk maySdta)) (PdtaChunk (KnownListChunk pdtaBlocks))
    , _
    ) <-
    decodeThrow bs
  Seq.length infos @?= 5
  case maySdta of
    Nothing -> fail "Missing sdta"
    Just sdta -> do
      sizeofPrimArray (sdtaHighBits sdta) @?= 1365026
      sdtaLowBits sdta @?= Nothing
  Seq.length pdtaBlocks @?= 9

testSfontWrite :: TestTree
testSfontWrite = testCase "write" $ do
  bs <- readShort "testdata/timpani.sf2"
  (sfont, _) <- decodeThrow @Sfont bs
  bs' <- encode sfont
  bs' @?= bs

testSfontManual :: TestTree
testSfontManual = testCase "manual" $ do
  bs <- readShort "testdata/timpani.sf2"
  ((info, sdta, pdta), _) <- flip getTargetThrow bs $ do
    getExpectLabel labelRiff
    chunkSize <- getChunkSizeLE
    getExact chunkSize $ do
      getExpectLabel labelSfbk
      info <- get @InfoChunk
      sdta <- get @SdtaChunk
      pdta <- get @PdtaChunk
      pure (info, sdta, pdta)
  let expecInfoSize = 124 + chunkHeaderSize
      expecSdtaSize = 2730064 + chunkHeaderSize
      expecPdtaSize = 3010 + chunkHeaderSize
  byteSize info @?= expecInfoSize
  byteSize sdta @?= expecSdtaSize
  byteSize pdta @?= expecPdtaSize
  infoBs <- encode info
  BSS.length infoBs @?= fromIntegral expecInfoSize
  sdtaBs <- encode sdta
  BSS.length sdtaBs @?= fromIntegral expecSdtaSize
  pdtaBs <- encode pdta
  BSS.length pdtaBs @?= fromIntegral expecPdtaSize

testSfontSizes :: TestTree
testSfontSizes = testCase "sizes" $ do
  staticByteSize (Proxy :: Proxy Phdr) @?= 38
  staticByteSize (Proxy :: Proxy Bag) @?= 4
  staticByteSize (Proxy :: Proxy Mod) @?= 10
  staticByteSize (Proxy :: Proxy Gen) @?= 4
  staticByteSize (Proxy :: Proxy Inst) @?= 22
  staticByteSize (Proxy :: Proxy Shdr) @?= 46

testSfont :: TestTree
testSfont = testGroup "sfont" [testSfontSizes, testSfontWhole, testSfontWrite, testSfontManual]

assertReparses :: (Binary a, Eq a, Show a) => a -> IO ()
assertReparses a = do
  bs <- encode a
  (a', bc) <- decodeThrow bs
  byteSize a' @?= bc
  a' @?= a

testConvertDx :: TestTree
testConvertDx = testCase "DX" $ do
  bs <- readShort "testdata/DX-EPiano1-C1.aif"
  (aif, _) <- decodeThrow bs
  ne <- rethrow (aiffToNeutral 44100 aif (Just defaultLoopMarkNames))
  -- Test that a standard translation of the wav works
  let !wav = neutralToWav defaultNoteNumber ne
  pc <- rethrow (wavToPcmContainer wav)
  pc @?= neCon ne
  assertReparses wav
  xne <- rethrow (wavToNeutral wav (Just defaultLoopMarkNames))
  xne @?= ne
  -- Test that the sample wav works
  let !width = 2500 -- double this is 0.1s of total fade
  swav <- rethrow (neutralToSampleWav width defaultNoteNumber ne)
  assertReparses swav
  pure ()

aifSamples :: Aiff -> [Word8]
aifSamples aif =
  let Aiff.KnownChunk (AiffDataBody _ _ wavData) = fromMaybe (error "no data") (lookupAiffDataChunk aif)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

wavSamples :: Wav -> [Word8]
wavSamples wav =
  let KnownChunk (WavDataBody wavData) = fromMaybe (error "no data") (lookupWavDataChunk wav)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

neutralSamples :: Neutral -> [Word8]
neutralSamples ne =
  let !wavData = pcData (neCon ne)
      !sz = sizeofByteArray wavData
  in  fmap (indexByteArray wavData) [0 .. sz - 1]

wavFmt :: Wav -> WavFormatBody
wavFmt wav = fmtBody
 where
  KnownChunk fmtBody = fromMaybe (error "no format") (lookupWavFormatChunk wav)

takeN :: Int -> [Word8] -> [Word8]
takeN n xs = let f = take n in f xs ++ reverse (f (reverse xs))

evenShorts :: [Word8] -> [Word8]
evenShorts (x : y : zs) = x : y : oddShorts zs
evenShorts _ = []

oddShorts :: [Word8] -> [Word8]
oddShorts (_ : _ : zs) = evenShorts zs
oddShorts _ = []

takeSome :: [Word8] -> [Word8]
takeSome = takeN 10

takeSomeEvens :: [Word8] -> [Word8]
takeSomeEvens = evenShorts . takeN 20

tryAifToWav :: Int -> String -> IO ()
tryAifToWav channels variant = do
  (aif, _) <- decodeFileThrow ("testdata/sin_" ++ variant ++ ".aifc")
  (wav, _) <- decodeFileThrow ("testdata/sin_" ++ variant ++ ".wav")
  ne <- rethrow (aiffToNeutral 44100 aif Nothing)
  let !conv = neutralToWav defaultNoteNumber ne
  let !aifPart = takeSome (aifSamples aif)
  let !wavPart = takeSome (wavSamples wav)
  wavPart @?= aifPart
  let !convPart = takeSome (wavSamples conv)
  convPart @?= aifPart
  -- conv and wav will be different bc of addl chunks like cue
  -- but their formats will be the same
  let !convFx = wavFmt conv
  wfbNumChannels convFx @?= fromIntegral channels
  let !wavFx = wavFmt wav
  convFx @?= wavFx

tryWavMono :: IO ()
tryWavMono = do
  -- Read stereo + mono versions of same wav, assert their samples are correct
  (wavStereo, _) <- decodeFileThrow "testdata/sin_stereo.wav"
  (wavMono, _) <- decodeFileThrow "testdata/sin_mono.wav"
  let !leftStereoPart = takeSomeEvens (wavSamples wavStereo)
  let !monoPart = takeSome (wavSamples wavMono)
  monoPart @?= leftStereoPart
  -- Now make the stereo wav mono and assert the same
  neExtract <- rethrow (wavToNeutral wavStereo Nothing >>= neutralMono)
  let !extractPart = takeSome (neutralSamples neExtract)
  extractPart @?= monoPart

testConvertSin :: TestTree
testConvertSin = testCase "sin" $ do
  tryAifToWav 1 "mono"
  tryAifToWav 2 "stereo"
  tryWavMono

testConvertLoop :: TestTree
testConvertLoop = testCase "loop" $ do
  (wav, _) <- decodeFileThrow "testdata/sin_mono.wav"
  con <- rethrow (wavToPcmContainer wav)
  let mkLoopMarks = defineLoopMarks @Int defaultLoopMarkNames . fmap (* 6000)
      !loopMarks = mkLoopMarks (LoopMarks 1 2 3 4)
      !marks = Seq.fromList (fmap snd (toList loopMarks))
  let !ne = Neutral {neCon = con, neLoopMarks = Just loopMarks, neMarks = marks}
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber ne
  encodeFile fullWav "testoutput/sin_mono_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 2500 defaultNoteNumber ne)
  encodeFile sampleWav "testoutput/sin_mono_sample.wav"
  -- Test full wav marks
  fullNe <- rethrow (wavToNeutral fullWav (Just defaultLoopMarkNames))
  let !actualFullLoopMarks = neLoopMarks fullNe
  actualFullLoopMarks @?= Just loopMarks
  -- Test sample wav marks
  let !expectedSampleLoopMarks = mkLoopMarks (LoopMarks 0 1 2 2)
  sampleNe <- rethrow (wavToNeutral sampleWav (Just defaultLoopMarkNames))
  let !actualSampleLoopMarks = neLoopMarks sampleNe
  actualSampleLoopMarks @?= Just expectedSampleLoopMarks

testConvertFade :: TestTree
testConvertFade = testCase "fade" $ do
  (wav, _) <- decodeFileThrow "testdata/fadeout_post.wav"
  con <- rethrow (wavToPcmContainer wav)
  let mkLoopMarks = defineLoopMarks @Int defaultLoopMarkNames . fmap (* 4410)
      !loopMarks = mkLoopMarks (LoopMarks 0 3 6 10)
      !marks = Seq.fromList (fmap snd (toList loopMarks))
  let !ne = Neutral {neCon = con, neLoopMarks = Just loopMarks, neMarks = marks}
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber ne
  encodeFile fullWav "testoutput/fadeout_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 1000 defaultNoteNumber ne)
  encodeFile sampleWav "testoutput/fadeout_sample.wav"

testConvertDxFade :: TestTree
testConvertDxFade = testCase "dx fade" $ do
  (wav, _) <- decodeFileThrow "testdata/DX-EPiano1-C1.wav"
  ne <- rethrow (wavToNeutral wav (Just defaultLoopMarkNames))
  neMon <- rethrow (neutralMono ne)
  -- Convert and write out for inspection
  let !fullWav = neutralToWav defaultNoteNumber neMon
  encodeFile fullWav "testoutput/dx_full.wav"
  sampleWav <- rethrow (neutralToSampleWav 2500 defaultNoteNumber neMon)
  encodeFile sampleWav "testoutput/dx_sample.wav"

testConvert :: TestTree
testConvert = testGroup "convert" [testConvertDx, testConvertSin, testConvertLoop, testConvertFade, testConvertDxFade]

testDspMono :: TestTree
testDspMono = testCase "mono" $ do
  let !larr1 = primArrayFromList @Int16LE [0, 1, 2, 3, 4, 5]
      !larr2 = primArrayFromList @Int16LE [0, 2, 4]
      !mm1 = ModMeta {mmNumChannels = 2, mmBitsPerSample = 16, mmSampleRate = 44100}
      !mm2 = mm1 {mmNumChannels = 1}
  (!mmx, !larrx) <- rethrow (runMod monoFromLeft mm1 larr1)
  mmx @?= mm2
  larrx @?= larr2

testDspFadeOne :: TestTree
testDspFadeOne = testCase "fade one" $ do
  let !larr1 = primArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      --                                                               ^                       ^
      !width = 1
      !loopStart = 7
      !loopEnd = 15
      !larr2 = primArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 3, 5, 1, 1, 1, 1, 1, 5, 3, 1]
      --                                                               ^                       ^
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 8, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDspFadeSome :: TestTree
testDspFadeSome = testCase "fade some" $ do
  let !larr1 = primArrayFromList @Int8 [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      !width = 3
      !loopStart = 7
      !loopEnd = 15
      !larr2 = primArrayFromList @Int8 [5, 5, 5, 5, 5, 4, 3, 3, 3, 1, 1, 1, 5, 4, 3, 3, 2]
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 8, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDspFadeWider :: TestTree
testDspFadeWider = testCase "fade wider" $ do
  let !larr1 = primArrayFromList @Int16LE [5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1]
      !width = 3
      !loopStart = 7
      !loopEnd = 15
      !larr2 = primArrayFromList @Int16LE [5, 5, 5, 5, 5, 4, 3, 3, 3, 1, 1, 1, 5, 4, 3, 3, 2]
      !mm = ModMeta {mmNumChannels = 1, mmBitsPerSample = 16, mmSampleRate = 44100}
  (!mmx, !larrx) <- rethrow (runMod (linearCrossFade width loopStart loopEnd) mm larr1)
  mmx @?= mm
  larrx @?= larr2

testDsp :: TestTree
testDsp = testGroup "dsp" [testDspMono, testDspFadeOne, testDspFadeSome, testDspFadeWider]

testWavSimple :: TestTree
testWavSimple = testCase "wav simple" $ do
  wav <- rethrow . fst =<< decodeFileEnd "testdata/drums.wav"
  pcBefore <- rethrow (wavToPcmContainer wav)
  simple <- rethrow (DAWS.fromComplex wav)
  complex <- rethrow (DAWS.toComplex simple)
  pcAfter <- rethrow (wavToPcmContainer complex)
  -- There is more to check, but a good approximation is that
  -- the wavs are equal as PCM containers before and after converting to
  -- the simplified representation.
  pcAfter @?= pcBefore

testDahditAudio :: TestTree
testDahditAudio =
  testGroup
    "dahdit-audio"
    [testWav, testAiff, testAiff2, testSfont, testConvert, testDsp, testWavSimple]

main :: IO ()
main = daytripperMain (const testDahditAudio)
