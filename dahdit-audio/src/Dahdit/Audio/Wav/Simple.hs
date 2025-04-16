-- | Interface semi-compatible with the WAVE library (but more efficient)
module Dahdit.Audio.Wav.Simple
  ( WAVE (..)
  , WAVEHeader (..)
  , WAVESample
  , WAVESamples (..)
  , toPcmContainer
  , toComplex
  , fromComplex
  , getWAVEFile
  , putWAVEFile
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, (>=>))
import Dahdit.Audio.Common (ConvertErr (..), guardChunk, rethrow)
import Dahdit.Audio.Convert (convertMod)
import Dahdit.Audio.Dsp (DspErr (..), PcmContainer (..), PcmMeta (..), SampleCount (..), changeBitDepth)
import Dahdit.Audio.Riff (KnownChunk (..))
import Dahdit.Audio.Wav
  ( Wav (..)
  , WavChunk (..)
  , WavDataBody (..)
  , WavFormatBody (..)
  , lookupWavFormatChunk
  , wavToPcmContainer
  )
import Dahdit.Iface (decodeFileEnd, encodeFile)
import Dahdit.Nums (Int16LE, Int24LE)
import Dahdit.Proxy (proxyForF)
import Dahdit.Sizes (ByteCount (..), staticByteSize)
import Data.Int (Int32)
import Data.Primitive.ByteArray (ByteArray (..), sizeofByteArray)
import Data.Primitive.PrimArray (PrimArray (..), sizeofPrimArray)
import Data.Sequence (Seq (..))

data WAVEHeader = WAVEHeader
  { waveNumChannels :: Int
  , waveFrameRate :: Int
  , waveBitsPerSample :: Int
  , waveFrames :: Maybe Int
  }
  deriving stock (Eq, Show)

type WAVESample = Int32

newtype WAVESamples = WAVESamples {unWAVESamples :: PrimArray Int32}
  deriving stock (Eq, Show)

data WAVE = WAVE
  { waveHeader :: WAVEHeader
  , waveSamples :: WAVESamples
  }
  deriving stock (Eq, Show)

toPcmContainer :: WAVE -> Either ConvertErr PcmContainer
toPcmContainer (WAVE hdr (WAVESamples arr@(PrimArray ba))) = do
  let elemSize = staticByteSize (proxyForF arr)
      nc = waveNumChannels hdr
      ns = div (sizeofPrimArray arr) nc
      bps = unByteCount elemSize * 8
      sr = waveFrameRate hdr
      extraElems = rem (sizeofPrimArray arr) nc
      pm = PcmMeta nc (SampleCount ns) bps sr
  unless (extraElems == 0) (Left (ConvertErrDsp DspErrBadElemSize))
  pure (PcmContainer pm (ByteArray ba))

toComplex :: WAVE -> Either ConvertErr Wav
toComplex wave0@(WAVE hdr _) = do
  pc0 <- toPcmContainer wave0
  let bps = waveBitsPerSample hdr
  pc1 <- case bps of
    16 -> convertMod (changeBitDepth @Int32 @Int16LE 32 16) pc0
    24 -> convertMod (changeBitDepth @Int32 @Int24LE 32 24) pc0
    32 -> pure pc0
    y -> Left (ConvertErrBadBps y)
  let samps1 = pcData pc1
      wfb =
        WavFormatBody
          { wfbFormatType = 1
          , wfbNumChannels = fromIntegral (waveNumChannels hdr)
          , wfbSampleRate = fromIntegral (waveFrameRate hdr)
          , wfbBitsPerSample = fromIntegral bps
          , wfbExtra = mempty
          }
      headerChunk = WavChunkFormat (KnownChunk wfb)
      dataChunk = WavChunkData (KnownChunk (WavDataBody samps1))
  pure (Wav (headerChunk :<| dataChunk :<| Empty))

fromComplex :: Wav -> Either ConvertErr WAVE
fromComplex wav0 = do
  pc0 <- wavToPcmContainer wav0
  let bps = pmBitsPerSample (pcMeta pc0)
  pc1 <- case bps of
    16 -> convertMod (changeBitDepth @Int16LE @Int32 16 32) pc0
    24 -> convertMod (changeBitDepth @Int24LE @Int32 24 32) pc0
    32 -> pure pc0
    y -> Left (ConvertErrBadBps y)
  KnownChunk wfb <- guardChunk "format" (lookupWavFormatChunk wav0)
  let !samps@(ByteArray ba) = pcData pc1
      hdr =
        WAVEHeader
          { waveNumChannels = fromIntegral (wfbNumChannels wfb)
          , waveFrameRate = fromIntegral (wfbSampleRate wfb)
          , waveBitsPerSample = bps
          , waveFrames = Just (div (sizeofByteArray samps) 4)
          }

  pure (WAVE hdr (WAVESamples (PrimArray ba)))

getWAVEFile :: FilePath -> IO WAVE
getWAVEFile = decodeFileEnd >=> rethrow . fst >=> rethrow . fromComplex

putWAVEFile :: FilePath -> WAVE -> IO ()
putWAVEFile fp wave = either throwIO (`encodeFile` fp) (toComplex wave)
