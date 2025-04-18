{-# LANGUAGE RecordWildCards #-}

module Dahdit.Audio.Convert
  ( convertMod
  , convertModGeneric
  , loadAiff
  , loadWav
  , Neutral (..)
  , aiffToNeutral
  , wavToNeutral
  , loadNeutral
  , neutralToWav
  , neutralMono
  , neutralCrossFade
  , neutralCropLoop
  , neutralToSampleWav
  , neutralToMonoWav
  )
where

import Control.Monad (unless, (>=>))
import Dahdit (Int16LE, Int24LE, Int32LE, Seq (..), StaticByteSized, decodeFile)
import Dahdit.Audio.Aiff (Aiff, aiffGatherMarkers, aiffToPcmContainer)
import Dahdit.Audio.Common
  ( ConvertErr (..)
  , LoopMarkNames
  , LoopMarkPoints
  , LoopMarks (..)
  , SimpleMarker (..)
  , adjustMarker
  , findLoopMarks
  , recallLoopMarkNames
  , rethrow
  )
import Dahdit.Audio.Dsp
  ( Mod
  , PcmContainer (..)
  , PcmMeta (..)
  , SampleCount
  , applyMod
  , applyModGeneric
  , changeBitDepth
  , crop
  , ensureMonoFromLeft
  , linearCrossFade
  )
import Dahdit.Audio.Wav
  ( Wav
  , WavChunk (..)
  , wavAddChunks
  , wavFromPcmContainer
  , wavGatherMarkers
  , wavToPcmContainer
  , wavUseLoopPoints
  , wavUseMarkers
  )
import Data.Maybe (isJust)
import Data.Primitive (Prim)
import Data.Sequence qualified as Seq
import System.FilePath (splitExtension)

convertMod
  :: (StaticByteSized a, Prim b, StaticByteSized b) => Mod a b -> PcmContainer -> Either ConvertErr PcmContainer
convertMod modx con = either (Left . ConvertErrDsp) Right (applyMod modx con)

convertModGeneric
  :: (forall a. (Prim a, StaticByteSized a, Integral a) => Mod a a) -> PcmContainer -> Either ConvertErr PcmContainer
convertModGeneric modx con = either (Left . ConvertErrDsp) Right (applyModGeneric modx con)

loadAiff :: FilePath -> IO Aiff
loadAiff = decodeFile >=> rethrow . fst

loadWav :: FilePath -> IO Wav
loadWav = decodeFile >=> rethrow . fst

data Neutral = Neutral
  { neCon :: !PcmContainer
  , neMarks :: !(Seq SimpleMarker)
  , neLoopMarks :: !(Maybe LoopMarkPoints)
  }
  deriving stock (Eq, Show)

guardSr :: Int -> Neutral -> IO ()
guardSr expectedSr ne = do
  let actualSr = pmSampleRate (pcMeta (neCon ne))
  unless (expectedSr == actualSr) (fail ("Expected SR: " ++ show expectedSr ++ " but got " ++ show actualSr))

-- NOTE: Taking sr as a param here so we don't have to interpret extended fp
aiffToNeutral :: Int -> Aiff -> Maybe LoopMarkNames -> Either ConvertErr Neutral
aiffToNeutral sr aiff mayNames = do
  neCon <- aiffToPcmContainer sr aiff
  let !neMarks = aiffGatherMarkers aiff
  neLoopMarks <- maybe (pure Nothing) (fmap Just . (`findLoopMarks` neMarks)) mayNames
  pure $! Neutral {..}

wavToNeutral :: Wav -> Maybe LoopMarkNames -> Either ConvertErr Neutral
wavToNeutral wav mayNames = do
  neCon <- wavToPcmContainer wav
  let !neMarks = wavGatherMarkers wav
  neLoopMarks <- maybe (pure Nothing) (fmap Just . (`findLoopMarks` neMarks)) mayNames
  pure $! Neutral {..}

loadNeutral :: Int -> Maybe LoopMarkNames -> FilePath -> IO Neutral
loadNeutral sr mayNames fp = do
  let (_, ext) = splitExtension fp
  ne <-
    if
      | ext == ".wav" -> do
          wav <- loadWav fp
          rethrow (wavToNeutral wav mayNames)
      | ext == ".aif" || ext == ".aifc" || ext == ".aiff" -> do
          aiff <- loadAiff fp
          rethrow (aiffToNeutral sr aiff mayNames)
      | otherwise -> fail ("Could not load with unknown extension: " ++ fp)
  guardSr sr ne
  pure ne

neutralToWav :: Int -> Neutral -> Wav
neutralToWav note (Neutral {..}) =
  let !sr = pmSampleRate (pcMeta neCon)
      !maySampleChunk = fmap (WavChunkSample . wavUseLoopPoints sr note) neLoopMarks
      !markChunks = if Seq.null neMarks then [] else let (!wcc, !wac) = wavUseMarkers neMarks in [WavChunkCue wcc, WavChunkAdtl wac]
      !chunks = Seq.fromList (markChunks ++ maybe [] pure maySampleChunk)
      !wav = wavFromPcmContainer neCon
  in  wavAddChunks chunks wav

neutralMono :: Neutral -> Either ConvertErr Neutral
neutralMono ne@(Neutral {..}) = do
  con' <- convertModGeneric ensureMonoFromLeft neCon
  pure $! ne {neCon = con'}

neutralReduceDepth :: Neutral -> Either ConvertErr Neutral
neutralReduceDepth ne@(Neutral {..}) = do
  case pmBitsPerSample (pcMeta neCon) of
    16 -> pure ne
    24 -> do
      con' <- convertMod (changeBitDepth @Int24LE @Int16LE 24 16) neCon
      pure $! ne {neCon = con'}
    32 -> do
      con' <- convertMod (changeBitDepth @Int32LE @Int16LE 32 16) neCon
      pure $! ne {neCon = con'}
    y -> Left (ConvertErrBadBps y)

neutralCrossFade :: SampleCount -> Neutral -> Either ConvertErr Neutral
neutralCrossFade width ne@(Neutral {..}) = do
  LoopMarks _ (_, !loopStart) (_, !loopEnd) _ <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let !loopStartPos = smPosition loopStart
      !loopEndPos = smPosition loopEnd
  con' <- convertModGeneric (linearCrossFade width (fromIntegral loopStartPos) (fromIntegral loopEndPos)) neCon
  pure $! ne {neCon = con'}

neutralCropLoop :: Neutral -> Either ConvertErr Neutral
neutralCropLoop (Neutral {..}) = do
  initLoopMarks <- maybe (Left ConvertErrNoLoopMarks) Right neLoopMarks
  let !names = recallLoopMarkNames initLoopMarks
      LoopMarks (_, !start) _ (_, !loopEnd) (_, !end) = initLoopMarks
      !startPos = smPosition start
      !loopEndPos = smPosition loopEnd
      !endPos = smPosition end
      !filteredMarks = Seq.filter (\sm -> let p = smPosition sm in p >= startPos && p <= loopEndPos) neMarks
      !withEndMarks = if endPos <= loopEndPos then filteredMarks else filteredMarks :|> end {smPosition = loopEndPos}
      !finalMarks = fmap (adjustMarker (-startPos)) withEndMarks
  !finalLoopMarks <- findLoopMarks names finalMarks
  con' <- convertModGeneric (crop (fromIntegral startPos) (fromIntegral loopEndPos)) neCon
  pure $! Neutral {neCon = con', neLoopMarks = Just finalLoopMarks, neMarks = finalMarks}

neutralIfHasMarks :: (Neutral -> Either e Neutral) -> Neutral -> Either e Neutral
neutralIfHasMarks f ne = do
  if isJust (neLoopMarks ne)
    then f ne
    else Right ne

-- | Example converting to instrument sample representing a particular note (with loop marks)
neutralToSampleWav :: SampleCount -> Int -> Neutral -> Either ConvertErr Wav
neutralToSampleWav width note ne =
  fmap
    (neutralToWav note)
    ( neutralMono ne
        >>= neutralReduceDepth
        >>= neutralIfHasMarks (neutralCrossFade width)
        >>= neutralIfHasMarks neutralCropLoop
    )

-- | Example simply converting to mono 16-bit
neutralToMonoWav :: Neutral -> Either ConvertErr Wav
neutralToMonoWav ne = fmap (wavFromPcmContainer . neCon) (neutralMono ne >>= neutralReduceDepth)
