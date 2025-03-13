-- | Interface semi-compatible with the WAVE library (but more efficient)
module Dahdit.Audio.Wav.Simple
  ( WAVE (..)
  , WAVEHeader (..)
  , WAVESample
  , WAVESamples
  , simplify
  , complexify
  , getWAVEFile
  , putWAVEFile
  )
where

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Dahdit (decodeFileEnd, encodeFile)
import Dahdit.Audio.Binary (QuietArray (..))
import Dahdit.Audio.Riff (KnownChunk (..))
import Dahdit.Audio.Wav
  ( Wav (..)
  , WavChunk (..)
  , WavDataBody (..)
  , WavFormatBody (..)
  , lookupWavDataChunk
  , lookupWavFormatChunk
  )
import Data.Int (Int32)
import Data.Primitive.ByteArray (sizeofByteArray)
import Data.Sequence (Seq (..))

data WAVEHeader = WAVEHeader
  { waveNumChannels :: Int
  , waveFrameRate :: Int
  , waveBitsPerSample :: Int
  , waveFrames :: Maybe Int
  }
  deriving stock (Eq, Show)

type WAVESample = Int32

type WAVESamples = QuietArray

data WAVE = WAVE
  { waveHeader :: WAVEHeader
  , waveSamples :: WAVESamples
  }
  deriving stock (Eq, Show)

complexify :: WAVE -> Wav
complexify (WAVE hdr samps) = Wav (headerChunk :<| dataChunk :<| Empty)
 where
  wfb =
    WavFormatBody
      { wfbFormatType = 1
      , wfbNumChannels = fromIntegral (waveNumChannels hdr)
      , wfbSampleRate = fromIntegral (waveFrameRate hdr)
      , wfbBitsPerSample = fromIntegral (waveBitsPerSample hdr)
      , wfbExtra = mempty
      }
  headerChunk = WavChunkFormat (KnownChunk wfb)
  dataChunk = WavChunkData (KnownChunk (WavDataBody samps))

simplify :: Wav -> Maybe WAVE
simplify wav = do
  KnownChunk wfb <- lookupWavFormatChunk wav
  KnownChunk (WavDataBody samps) <- lookupWavDataChunk wav
  let hdr =
        WAVEHeader
          { waveNumChannels = fromIntegral (wfbNumChannels wfb)
          , waveFrameRate = fromIntegral (wfbSampleRate wfb)
          , waveBitsPerSample = fromIntegral (wfbBitsPerSample wfb)
          , waveFrames = Just (sizeofByteArray (unQuietArray samps))
          }
  pure (WAVE hdr samps)

getWAVEFile :: FilePath -> IO WAVE
getWAVEFile = decodeFileEnd >=> either throwIO pure . fst >=> maybe (fail "missing format or data chunk") pure . simplify

putWAVEFile :: FilePath -> WAVE -> IO ()
putWAVEFile fp wave = encodeFile (complexify wave) fp
