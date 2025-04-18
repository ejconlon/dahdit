{-# LANGUAGE RecordWildCards #-}

module Dahdit.Audio.Dsp
  ( SampleCount (..)
  , DspErr (..)
  , monoFromLeft
  , monoFromRight
  , monoFromAvg
  , ensureMonoFromLeft
  , changeBitDepth
  , stereoFromMono
  , linearCrossFade
  , crop
  , ModMeta (..)
  , Mod (..)
  , modId
  , modAndThen
  , PcmMeta (..)
  , PcmContainer (..)
  , applyMod
  , applyModGeneric
  )
where

import Control.Exception (Exception)
import Control.Monad (unless)
import Dahdit
  ( ByteCount (..)
  , ElemCount (..)
  , Int16LE
  , Int24LE
  , Int32LE
  , Int8
  , StaticByteSized
  , proxyForF
  , staticByteSize
  )
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
import Data.Primitive (Prim)
import Data.Primitive.ByteArray (ByteArray (..), sizeofByteArray)
import Data.Primitive.PrimArray (PrimArray (..), clonePrimArray, generatePrimArray, indexPrimArray, sizeofPrimArray)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)

newtype SampleCount = SampleCount {unSampleCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)

data Sampled f where
  Sampled :: (Prim a, StaticByteSized a, Integral a) => !(f a) -> Sampled f

getSampled :: Int -> Maybe (Sampled Proxy)
getSampled = \case
  8 -> Just (Sampled (Proxy :: Proxy Int8))
  16 -> Just (Sampled (Proxy :: Proxy Int16LE))
  24 -> Just (Sampled (Proxy :: Proxy Int24LE))
  32 -> Just (Sampled (Proxy :: Proxy Int32LE))
  _ -> Nothing

data DspErr
  = DspErrOddSamples
  | DspErrBadElemSize
  | DspErrBadBitWidth
  | DspErrNotStereo
  | DspErrNotMono
  | DspErrBadFade
  | DspErrBadCrop
  deriving stock (Eq, Show)

instance Exception DspErr

newtype Sel a = Sel {runSel :: PrimArray a -> ElemCount -> a}

selMonoLeft, selMonoRight :: (Prim a) => Sel a
selMonoLeft = Sel $ \arr i -> indexPrimArray arr (unElemCount i * 2)
selMonoRight = Sel $ \arr i -> indexPrimArray arr (unElemCount i * 2 + 1)

selMonoAvg :: (Prim a, Integral a, Bits a) => Sel a
selMonoAvg = Sel $ \arr i ->
  let !ix = i * 2
      !lval = indexPrimArray arr (unElemCount ix)
      !rval = indexPrimArray arr (unElemCount ix + 1)
      !halfLval = div lval 2
      !halfRval = div rval 2
      !extra = lval .&. rval .&. 1
  in  halfLval + halfRval + extra

data ModMeta = ModMeta
  { mmNumChannels :: !Int
  , mmBitsPerSample :: !Int
  , mmSampleRate :: !Int
  }
  deriving stock (Eq, Show)

-- Array layout: samples are interspersed: [chan1 samp1, chan2 samp1, chan1 samp2, chan2 samp2, chan1 samp3, ...]
-- numChannels -> array -> (newNumChannels, newArray)
newtype Mod a b = Mod {runMod :: ModMeta -> PrimArray a -> Either DspErr (ModMeta, PrimArray b)}

modId :: Mod a a
modId = Mod (curry Right)

modAndThen :: Mod a b -> Mod b c -> Mod a c
modAndThen modAB modBC = Mod $ \nc src -> do
  (nc', src') <- runMod modAB nc src
  runMod modBC nc' src'

monoFromSel :: (Prim a) => Sel a -> Mod a a
monoFromSel sel = Mod $ \mm src -> do
  unless (mmNumChannels mm == 2) (Left DspErrNotStereo)
  let !srcLen = sizeofPrimArray src
  unless (even srcLen) (Left DspErrOddSamples)
  let !destLen = div srcLen 2
      !dest = generatePrimArray destLen (runSel sel src . ElemCount)
  Right (mm {mmNumChannels = 1}, dest)

monoFromLeft, monoFromRight :: (Prim a) => Mod a a
monoFromLeft = monoFromSel selMonoLeft
monoFromRight = monoFromSel selMonoRight

monoFromAvg :: (Prim a, Integral a, Bits a) => Mod a a
monoFromAvg = monoFromSel selMonoAvg

ensureMonoFromSel :: (Prim a) => Sel a -> Mod a a
ensureMonoFromSel sel = Mod $ \mm src -> do
  if mmNumChannels mm == 1
    then pure (mm, src)
    else runMod (monoFromSel sel) mm src

ensureMonoFromLeft :: (Prim a) => Mod a a
ensureMonoFromLeft = ensureMonoFromSel selMonoLeft

changeBitDepth :: (Prim a, Integral a, Prim b, Num b) => Int -> Int -> Mod a b
changeBitDepth srcBitDepth destBitDepth = Mod $ \mm0 src ->
  let bitShift = destBitDepth - srcBitDepth
  in  pure $
        let mm1 = mm0 {mmBitsPerSample = destBitDepth}
            dest = generatePrimArray (sizeofPrimArray src) $ \i ->
              let x = indexPrimArray src i
                  y = shift (fromIntegral @_ @Word64 x) bitShift
              in  fromIntegral y
        in  (mm1, dest)

stereoFromMono :: (Prim a) => Mod a a
stereoFromMono = Mod $ \mm src -> do
  unless (mmNumChannels mm == 1) (Left DspErrNotMono)
  let !srcLen = sizeofPrimArray src
      !destLen = srcLen * 2
      !dest = generatePrimArray destLen (\i -> indexPrimArray src (div i 2))
  Right (mm {mmNumChannels = 2}, dest)

guardFade :: SampleCount -> SampleCount -> SampleCount -> Either DspErr ()
guardFade width loopStart loopEnd = do
  if width <= 0
    || loopEnd <= loopStart
    || loopStart <= loopStart - width
    || loopStart + width <= loopStart
    || loopEnd <= loopEnd - width
    || loopEnd + width <= loopEnd
    then Left DspErrBadFade
    else Right ()

combine :: (Integral a) => Int -> Int -> a -> a -> a
combine intDistTot intDist1 one two =
  let dist1 = fromIntegral intDist1
      dist2 = fromIntegral (intDistTot - intDist1)
      distTot = fromIntegral intDistTot
  in  fromInteger (div (dist1 * fromIntegral one + dist2 * fromIntegral two) distTot)

-- Cross fade:  | --------- PreStart Start PostStart PreEnd End PostEnd ---- |
-- Guarded to ensure inequalities are strict
-- Width here is one-sided
-- Width is number of samples to fade over
linearCrossFade :: (Prim a, Integral a) => SampleCount -> SampleCount -> SampleCount -> Mod a a
linearCrossFade width loopStart loopEnd = Mod $ \mm src -> do
  guardFade width loopStart loopEnd
  let !nc = mmNumChannels mm
      !sampWidth = nc * unSampleCount width
      !sampTotDist = 2 * sampWidth
      !sampBetween = nc * unSampleCount (loopEnd - loopStart)
      !sampPreStart = nc * unSampleCount (loopStart - width)
      !sampStart = nc * unSampleCount loopStart
      !sampPostStart = nc * unSampleCount (loopStart + width)
      !sampPreEnd = nc * unSampleCount (loopEnd - width)
      !sampEnd = nc * unSampleCount loopStart
      !sampPostEnd = nc * unSampleCount (loopEnd + width)
      !sz = sizeofPrimArray src
      !sampLast = sz - sampBetween
      genElem i =
        let !v = indexPrimArray src i
        in  if
              | i >= sampPreStart && i <= sampPostStart ->
                  if i >= sampLast
                    then v
                    else
                      let !w = indexPrimArray src (i + sampBetween)
                          f = combine sampTotDist (coerce (sampPostStart - i))
                      in  if i < sampStart then f v w else f w v
              | i >= sampPreEnd && i <= sampPostEnd ->
                  if i < sampBetween
                    then v
                    else
                      let !w = indexPrimArray src (i - sampBetween)
                          f = combine sampTotDist (coerce (sampPostEnd - i))
                      in  if i < sampEnd then f v w else f w v
              | otherwise -> v
      !dest = generatePrimArray sz genElem
  Right (mm, dest)

guardCrop :: SampleCount -> SampleCount -> Either DspErr ()
guardCrop start end = do
  if end <= start
    then Left DspErrBadCrop
    else Right ()

crop :: (Prim a) => SampleCount -> SampleCount -> Mod a a
crop start end = Mod $ \mm src -> do
  guardCrop start end
  let !nc = mmNumChannels mm
      !sampStart = nc * unSampleCount start
      !sampEnd = nc * unSampleCount end
      !dest = clonePrimArray src sampStart (sampEnd - sampStart)
  Right (mm, dest)

data PcmMeta = PcmMeta
  { pmNumChannels :: !Int
  , pmNumSamples :: !SampleCount
  , pmBitsPerSample :: !Int
  , pmSampleRate :: !Int
  }
  deriving stock (Eq, Show)

data PcmContainer = PcmContainer
  { pcMeta :: !PcmMeta
  , pcData :: !ByteArray
  }
  deriving stock (Eq, Show)

pmToMm :: PcmMeta -> ModMeta
pmToMm (PcmMeta {..}) = ModMeta {mmNumChannels = pmNumChannels, mmBitsPerSample = pmBitsPerSample, mmSampleRate = pmSampleRate}

toLifted :: (StaticByteSized a) => Proxy a -> PcmContainer -> Either DspErr (ModMeta, PrimArray a)
toLifted prox (PcmContainer pm arr@(ByteArray ba)) = do
  let !elemSize = staticByteSize prox
  let !actualNs = div (sizeofByteArray arr) (unByteCount elemSize * pmNumChannels pm)
  unless
    (unByteCount elemSize * 8 == pmBitsPerSample pm && actualNs == unSampleCount (pmNumSamples pm))
    (Left DspErrBadElemSize)
  let !mm = pmToMm pm
  Right (mm, PrimArray ba)

fromLifted :: (Prim b, StaticByteSized b) => ModMeta -> PrimArray b -> Either DspErr PcmContainer
fromLifted mm arr@(PrimArray ba) = do
  let !elemSize = staticByteSize (proxyForF arr)
      !nc = mmNumChannels mm
      !ns = div (sizeofPrimArray arr) nc
      !bps = unByteCount elemSize * 8
      !sr = mmSampleRate mm
      !extraElems = rem (sizeofPrimArray arr) nc
  unless (extraElems == 0) (Left DspErrBadElemSize)
  let !pm = PcmMeta nc (SampleCount ns) bps sr
  Right $! PcmContainer pm (ByteArray ba)

proxyFromFirst :: m a b -> Proxy a
proxyFromFirst _ = Proxy

applyMod :: (StaticByteSized a, Prim b, StaticByteSized b) => Mod a b -> PcmContainer -> Either DspErr PcmContainer
applyMod modx con = do
  (mm, src) <- toLifted (proxyFromFirst modx) con
  (mm', dest) <- runMod modx mm src
  fromLifted mm' dest

proxMod
  :: (Prim a, StaticByteSized a, Integral a)
  => Proxy a
  -> (forall x. (Prim x, StaticByteSized x, Integral x) => Mod x x)
  -> Mod a a
proxMod _ allMod = allMod

applyModGeneric
  :: (forall a. (Prim a, StaticByteSized a, Integral a) => Mod a a) -> PcmContainer -> Either DspErr PcmContainer
applyModGeneric allMod con =
  case getSampled (pmBitsPerSample (pcMeta con)) of
    Nothing -> Left DspErrBadBitWidth
    Just (Sampled prox) -> applyMod (proxMod prox allMod) con
