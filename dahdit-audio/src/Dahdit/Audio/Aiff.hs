{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Dahdit.Audio.Aiff
  ( PascalString (..)
  , Chunk (..)
  , KnownChunk (..)
  , Variant (..)
  , AiffCommonBody (..)
  , AiffCommonChunk
  , AiffDataBody (..)
  , AiffDataChunk
  , AiffChunk (..)
  , Aiff (..)
  , lookupAiffChunk
  , lookupAiffCommonChunk
  , lookupAiffDataChunk
  , aiffToPcmContainer
  , aiffGatherMarkers
  )
where

import Control.Monad (unless)
import Dahdit
  ( Binary (..)
  , ByteArray
  , ByteCount (..)
  , Get
  , Put
  , ShortByteString
  , StaticByteSized (..)
  , StaticBytes (..)
  , ViaGeneric (..)
  , ViaStaticGeneric (..)
  , Word16BE
  , Word32BE (..)
  , byteSizeFoldable
  , getByteString
  , getExact
  , getLookAhead
  , getRemainingByteArray
  , getRemainingSeq
  , getSeq
  , getSkip
  , getWord16BE
  , getWord8
  , putByteArray
  , putByteString
  , putSeq
  , putWord16BE
  , putWord8
  )
import Dahdit.Audio.Common
  ( ChunkHeaderSize
  , ConvertErr
  , CountSize
  , KnownLabel (..)
  , Label
  , LabelSize
  , PadCount
  , SimpleMarker (..)
  , UnparsedBody
  , chunkHeaderSize
  , countSize
  , dedupeSimpleMarkers
  , getChunkSizeBE
  , getExpectLabel
  , guardChunk
  , labelSize
  , padCount
  , putChunkSizeBE
  )
import Dahdit.Audio.Dsp (PcmContainer (PcmContainer), PcmMeta (PcmMeta))
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Short qualified as BSS
import Data.Default (Default (..))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray (byteArrayFromListN, indexByteArray, sizeofByteArray)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.TypeLits (type (*), type (+))

-- AIFF-C file parsing according to
-- http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AIFF/AIFF.html
-- We only bother to support "sowt" compression (little endian samples)

-- AIFF shares a similar 4-byte label + size + payload structure with RIFF
-- We could use a lot of the same structures to read the file... If they were
-- big-endian.

labelForm, labelAifc, labelAiff, labelComm, labelSsnd, labelFver, labelAnno, labelMark, labelSowt :: Label
labelForm = "FORM"
labelAifc = "AIFC"
labelAiff = "AIFF"
labelComm = "COMM"
labelSsnd = "SSND"
labelFver = "FVER"
labelAnno = "ANNO"
labelMark = "MARK"
labelSowt = "sowt"

data Chunk a = Chunk
  { chunkLabel :: !Label
  , chunkBody :: !a
  }
  deriving stock (Eq, Show)

chunkUnpaddedByteSize :: (Binary a) => Chunk a -> ByteCount
chunkUnpaddedByteSize (Chunk _ body) = byteSize body

instance (StaticByteSized a) => StaticByteSized (Chunk a) where
  type StaticSize (Chunk a) = PadCount (ChunkHeaderSize + StaticSize a)

instance (Binary a) => Binary (Chunk a) where
  byteSize c = padCount (chunkHeaderSize + chunkUnpaddedByteSize c)
  get = do
    lab <- get
    usz <- getChunkSizeBE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! Chunk lab body
  put c@(Chunk lab body) = do
    put lab
    let !usz = chunkUnpaddedByteSize c
    putChunkSizeBE usz
    put body
    unless (even usz) (putWord8 0)

newtype KnownChunk a = KnownChunk
  { knownChunkBody :: a
  }
  deriving stock (Show)
  deriving newtype (Eq, Default)

knownChunkUnpaddedByteSize :: (Binary a) => KnownChunk a -> ByteCount
knownChunkUnpaddedByteSize (KnownChunk body) = byteSize body

instance (StaticByteSized a) => StaticByteSized (KnownChunk a) where
  type StaticSize (KnownChunk a) = PadCount (ChunkHeaderSize + StaticSize a)

instance (Binary a, KnownLabel a) => Binary (KnownChunk a) where
  byteSize kc = padCount (chunkHeaderSize + knownChunkUnpaddedByteSize kc)
  get = do
    getExpectLabel (knownLabel (Proxy :: Proxy a))
    usz <- getChunkSizeBE
    body <- getExact usz get
    unless (even usz) (getSkip 1)
    pure $! KnownChunk body
  put kc@(KnownChunk body) = do
    put (knownLabel (Proxy :: Proxy a))
    let !usz = knownChunkUnpaddedByteSize kc
    putChunkSizeBE usz
    put body
    unless (even usz) (putWord8 0)

-- | A "Pascal-style string" with a leading byte count and optional
-- trailing padding byte to make total length even.
newtype PascalString = PascalString {unPascalString :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq)

instance Default PascalString where
  def = PascalString BSS.empty

instance Binary PascalString where
  byteSize (PascalString sbs) = padCount (ByteCount (BSS.length sbs + 1))
  get = do
    usz <- fmap fromIntegral getWord8
    sbs <- getByteString usz
    unless (odd usz) (getSkip 1)
    pure $! PascalString sbs
  put (PascalString sbs) = do
    let !usz = fromIntegral (BSS.length sbs)
    putWord8 usz
    putByteString sbs
    unless (odd usz) (putWord8 0)

notCompressed :: PascalString
notCompressed = PascalString (BSS.toShort (BSC.pack "not compressed"))

-- | "80 bit IEEE Standard 754 floating point number"
newtype ExtendedFloat = ExtendedFloat {unExtendedFloat :: StaticBytes 10}
  deriving stock (Show)
  deriving newtype (Eq, Default, StaticByteSized, Binary)

data AiffCommonBody = AiffCommonBody
  { aceNumChannels :: !Word16BE
  , aceNumSampleFrames :: !Word32BE
  , aceSampleSize :: !Word16BE
  , aceSampleRate :: !ExtendedFloat
  , aceCompressionType :: !Label
  , aceCompressionName :: !PascalString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric AiffCommonBody)

instance KnownLabel AiffCommonBody where
  knownLabel _ = labelComm

type AiffCommonChunk = KnownChunk AiffCommonBody

data AiffCommonBodyOld = AiffCommonBodyOld
  { aceoNumChannels :: !Word16BE
  , aceoNumSampleFrames :: !Word32BE
  , aceoSampleSize :: !Word16BE
  , aceoSampleRate :: !ExtendedFloat
  }
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaStaticGeneric AiffCommonBodyOld)

instance KnownLabel AiffCommonBodyOld where
  knownLabel _ = labelComm

type AiffCommonChunkOld = KnownChunk AiffCommonBodyOld

oldToNewCommonChunk :: KnownChunk AiffCommonBodyOld -> KnownChunk AiffCommonBody
oldToNewCommonChunk (KnownChunk (AiffCommonBodyOld x y z w)) = KnownChunk (AiffCommonBody x y z w labelSowt notCompressed)

newToOldCommonChunk :: KnownChunk AiffCommonBody -> KnownChunk AiffCommonBodyOld
newToOldCommonChunk (KnownChunk (AiffCommonBody x y z w _ _)) = KnownChunk (AiffCommonBodyOld x y z w)

getCommonChunk :: Variant -> Get AiffCommonChunk
getCommonChunk = \case
  VariantAiff -> fmap oldToNewCommonChunk get
  VariantAifc -> get

putCommonChunk :: Variant -> AiffCommonChunk -> Put
putCommonChunk = \case
  VariantAiff -> put . newToOldCommonChunk
  VariantAifc -> put

data AiffDataBody = AiffDataBody
  { adbOffset :: !Word32BE
  , adbBlockSize :: !Word32BE
  , adbSoundData :: !ByteArray
  }
  deriving stock (Eq, Show, Generic)

instance Binary AiffDataBody where
  byteSize (AiffDataBody _ _ arr) = 8 + fromIntegral (sizeofByteArray arr)
  get = do
    adbOffset <- get
    unless (adbOffset == 0) (fail "need zero offset")
    adbBlockSize <- get
    adbSoundData <- getRemainingByteArray
    pure $! AiffDataBody {..}
  put (AiffDataBody {..}) = do
    put adbOffset
    put adbBlockSize
    putByteArray adbSoundData

instance KnownLabel AiffDataBody where
  knownLabel _ = labelSsnd

type AiffDataChunk = KnownChunk AiffDataBody

type AiffVersionChunk = Chunk UnparsedBody

type AiffAnnoChunk = Chunk UnparsedBody

data AiffMark = AiffMark
  { amId :: !Word16BE
  , amPosition :: !Word32BE
  , amName :: !PascalString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric AiffMark)

newtype AiffMarkBody = AiffMarkBody
  { ambMarkers :: Seq AiffMark
  }
  deriving stock (Show)
  deriving newtype (Eq)

instance Binary AiffMarkBody where
  byteSize (AiffMarkBody marks) = 2 + byteSizeFoldable marks
  get = do
    ec <- fmap fromIntegral getWord16BE
    marks <- getSeq ec get
    pure $! AiffMarkBody marks
  put (AiffMarkBody marks) = do
    putWord16BE (fromIntegral (Seq.length marks))
    putSeq put marks

instance KnownLabel AiffMarkBody where
  knownLabel _ = labelMark

type AiffMarkChunk = KnownChunk AiffMarkBody

type AiffUnparsedChunk = Chunk UnparsedBody

data AiffChunk
  = AiffChunkCommon !AiffCommonChunk
  | AiffChunkData !AiffDataChunk
  | AiffChunkVersion !AiffVersionChunk
  | AiffChunkAnno !AiffAnnoChunk
  | AiffChunkMark !AiffMarkChunk
  | AiffChunkUnparsed !AiffUnparsedChunk
  deriving stock (Eq, Show, Generic)

getChunk :: Variant -> Get AiffChunk
getChunk variant = do
  label <- getLookAhead get
  if
    | label == labelComm -> fmap AiffChunkCommon (getCommonChunk variant)
    | label == labelSsnd -> fmap AiffChunkData get
    | label == labelFver -> fmap AiffChunkVersion get
    | label == labelAnno -> fmap AiffChunkAnno get
    | label == labelMark -> fmap AiffChunkMark get
    | otherwise -> fmap AiffChunkUnparsed get

putChunk :: Variant -> AiffChunk -> Put
putChunk variant = \case
  AiffChunkCommon x -> putCommonChunk variant x
  AiffChunkData x -> put x
  AiffChunkVersion x -> put x
  AiffChunkAnno x -> put x
  AiffChunkMark x -> put x
  AiffChunkUnparsed x -> put x

instance Binary AiffChunk where
  byteSize = \case
    AiffChunkCommon x -> byteSize x
    AiffChunkData x -> byteSize x
    AiffChunkVersion x -> byteSize x
    AiffChunkAnno x -> byteSize x
    AiffChunkMark x -> byteSize x
    AiffChunkUnparsed x -> byteSize x
  get = getChunk VariantAifc
  put = putChunk VariantAifc

data Variant = VariantAiff | VariantAifc
  deriving stock (Eq, Show)

data AiffHeader = AiffHeader
  { ahVariant :: !Variant
  , ahRemainingSize :: ByteCount
  }
  deriving stock (Eq, Show, Generic)

type AiffHeaderSize = 2 * LabelSize + CountSize

aiffHeaderSize :: ByteCount
aiffHeaderSize = 2 * labelSize + countSize

instance StaticByteSized AiffHeader where
  type StaticSize AiffHeader = AiffHeaderSize
  staticByteSize _ = aiffHeaderSize

instance Binary AiffHeader where
  get = do
    getExpectLabel labelForm
    sz <- getChunkSizeBE
    label <- get @Label
    variant <-
      if
        | label == labelAiff -> pure VariantAiff
        | label == labelAifc -> pure VariantAifc
        | otherwise -> fail ("Expected label AIFC or AIFF in header but got: " ++ show (unStaticBytes label))
    pure $! AiffHeader variant (sz - labelSize)
  put (AiffHeader variant remSz) = do
    put labelForm
    putChunkSizeBE (remSz + labelSize)
    put $ case variant of
      VariantAiff -> labelAiff
      VariantAifc -> labelAifc

data Aiff = Aiff
  { aiffVariant :: !Variant
  , aiffChunks :: !(Seq AiffChunk)
  }
  deriving stock (Eq, Show)

instance Binary Aiff where
  byteSize (Aiff _ chunks) = aiffHeaderSize + byteSizeFoldable chunks
  get = do
    AiffHeader variant remSz <- get
    chunks <- getExact remSz (getRemainingSeq (getChunk variant))
    let !chunks' = swapDataEndian variant chunks
    pure $! Aiff variant chunks'
  put (Aiff variant chunks) = do
    let !chunks' = swapDataEndian variant chunks
    let !remSz = byteSizeFoldable chunks'
    put (AiffHeader variant remSz)
    putSeq (putChunk variant) chunks'

swapDataEndian :: Variant -> Seq AiffChunk -> Seq AiffChunk
swapDataEndian variant chunks =
  case variant of
    VariantAifc -> chunks
    VariantAiff -> fromMaybe chunks $ do
      let aiff = Aiff variant chunks
      KnownChunk comBody <- lookupAiffCommonChunk aiff
      KnownChunk (AiffDataBody x y arr) <- lookupAiffDataChunk aiff
      let arr' = swapArrayEndian (fromIntegral (aceSampleSize comBody)) arr
      let datChunk' = KnownChunk (AiffDataBody x y arr')
      Just $! flip fmap chunks $ \c ->
        case c of
          AiffChunkData _ -> AiffChunkData datChunk'
          _ -> c

swappedByteArray2 :: ByteArray -> [Word8]
swappedByteArray2 arr = go 0 Empty
 where
  sz = sizeofByteArray arr
  go !i !acc =
    if i == sz
      then toList acc
      else go (i + 2) (acc :|> indexByteArray arr (i + 1) :|> indexByteArray arr i)

swappedByteArray3 :: ByteArray -> [Word8]
swappedByteArray3 arr = go 0 Empty
 where
  sz = sizeofByteArray arr
  go !i !acc =
    if i == sz
      then toList acc
      else go (i + 3) (acc :|> indexByteArray arr (i + 2) :|> indexByteArray arr (i + 1) :|> indexByteArray arr i)

swapArrayEndian :: Int -> ByteArray -> ByteArray
swapArrayEndian bitDepth arr =
  if
    | bitDepth == 8 -> arr
    | bitDepth == 16 -> byteArrayFromListN (sizeofByteArray arr) (swappedByteArray2 arr)
    | bitDepth == 24 -> byteArrayFromListN (sizeofByteArray arr) (swappedByteArray3 arr)
    | otherwise -> error ("Unsupported endian swap depth: " ++ show bitDepth)

lookupAiffChunk :: (AiffChunk -> Bool) -> Aiff -> Maybe AiffChunk
lookupAiffChunk p (Aiff _ chunks) = fmap (Seq.index chunks) (Seq.findIndexL p chunks)

lookupAiffCommonChunk :: Aiff -> Maybe AiffCommonChunk
lookupAiffCommonChunk w =
  case lookupAiffChunk (\case AiffChunkCommon _ -> True; _ -> False) w of
    Just (AiffChunkCommon x) -> Just x
    _ -> Nothing

lookupAiffDataChunk :: Aiff -> Maybe AiffDataChunk
lookupAiffDataChunk w =
  case lookupAiffChunk (\case AiffChunkData _ -> True; _ -> False) w of
    Just (AiffChunkData x) -> Just x
    _ -> Nothing

lookupAiffMarkChunk :: Aiff -> Maybe AiffMarkChunk
lookupAiffMarkChunk w =
  case lookupAiffChunk (\case AiffChunkMark _ -> True; _ -> False) w of
    Just (AiffChunkMark x) -> Just x
    _ -> Nothing

-- NOTE: Taking sr as a param here so we don't have to interpret extended fp
aiffToPcmContainer :: Int -> Aiff -> Either ConvertErr PcmContainer
aiffToPcmContainer sr aiff = do
  KnownChunk commBody <- guardChunk "common" (lookupAiffCommonChunk aiff)
  KnownChunk dataBody <- guardChunk "data" (lookupAiffDataChunk aiff)
  let !nc = fromIntegral (aceNumChannels commBody)
      !ns = fromIntegral (aceNumSampleFrames commBody)
      !bps = fromIntegral (aceSampleSize commBody)
      !meta = PcmMeta nc ns bps sr
      !arr = adbSoundData dataBody
  pure $! PcmContainer meta arr

aiffGatherMarkers :: Aiff -> Seq SimpleMarker
aiffGatherMarkers aiff =
  case lookupAiffMarkChunk aiff of
    Nothing -> Empty
    Just (KnownChunk (AiffMarkBody marks)) ->
      dedupeSimpleMarkers (fmap (\am -> SimpleMarker (unPascalString (amName am)) (unWord32BE (amPosition am))) marks)
