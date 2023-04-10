module Dahdit.Binary
  ( Binary (..)
  )
where

import Control.Monad (unless)
import Dahdit.Counts (ByteCount (..))
import Dahdit.Fancy (BoolByte (..), ExactBytes (..), StaticArray (..), StaticBytes (..), StaticSeq (..), TermBytes (..))
import Dahdit.Free (Get, Put)
import Dahdit.Funs
  ( getByteString
  , getDoubleBE
  , getDoubleLE
  , getExpect
  , getFloatBE
  , getFloatLE
  , getInt16BE
  , getInt16LE
  , getInt24BE
  , getInt24LE
  , getInt32BE
  , getInt32LE
  , getInt64BE
  , getInt64LE
  , getInt8
  , getStaticArray
  , getStaticSeq
  , getWord16BE
  , getWord16LE
  , getWord24BE
  , getWord24LE
  , getWord32BE
  , getWord32LE
  , getWord64BE
  , getWord64LE
  , getWord8
  , putByteString
  , putDoubleBE
  , putDoubleLE
  , putFixedString
  , putFloatBE
  , putFloatLE
  , putInt16BE
  , putInt16LE
  , putInt24BE
  , putInt24LE
  , putInt32BE
  , putInt32LE
  , putInt64BE
  , putInt64LE
  , putInt8
  , putWord16BE
  , putWord16LE
  , putWord24BE
  , putWord24LE
  , putWord32BE
  , putWord32LE
  , putWord64BE
  , putWord64LE
  , putWord8
  , unsafePutStaticArrayN
  , unsafePutStaticSeqN
  )
import Dahdit.LiftedPrim (LiftedPrim)
import Dahdit.Nums
  ( DoubleBE (..)
  , DoubleLE (..)
  , FloatBE (..)
  , FloatLE (..)
  , Int16BE (..)
  , Int16LE (..)
  , Int24BE (..)
  , Int24LE (..)
  , Int32BE (..)
  , Int32LE (..)
  , Int64BE (..)
  , Int64LE (..)
  , Word16BE (..)
  , Word16LE (..)
  , Word24BE (..)
  , Word24LE (..)
  , Word32BE (..)
  , Word32LE (..)
  , Word64BE (..)
  , Word64LE (..)
  )
import Dahdit.Sizes (StaticByteSized)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Primitive.ByteArray (ByteArray (..), byteArrayFromListN)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeLits (KnownNat, KnownSymbol, natVal, symbolVal)

class Binary a where
  get :: Get a
  put :: a -> Put

-- Basic types

instance Binary () where
  get = pure ()
  put _ = pure ()

instance Binary Word8 where
  get = getWord8
  put = putWord8

instance Binary Int8 where
  get = getInt8
  put = putInt8

instance Binary Word16LE where
  get = getWord16LE
  put = putWord16LE

instance Binary Int16LE where
  get = getInt16LE
  put = putInt16LE

instance Binary Word24LE where
  get = getWord24LE
  put = putWord24LE

instance Binary Int24LE where
  get = getInt24LE
  put = putInt24LE

instance Binary Word32LE where
  get = getWord32LE
  put = putWord32LE

instance Binary Int32LE where
  get = getInt32LE
  put = putInt32LE

instance Binary Word64LE where
  get = getWord64LE
  put = putWord64LE

instance Binary Int64LE where
  get = getInt64LE
  put = putInt64LE

instance Binary FloatLE where
  get = getFloatLE
  put = putFloatLE

instance Binary DoubleLE where
  get = getDoubleLE
  put = putDoubleLE

instance Binary Word16BE where
  get = getWord16BE
  put = putWord16BE

instance Binary Int16BE where
  get = getInt16BE
  put = putInt16BE

instance Binary Word24BE where
  get = getWord24BE
  put = putWord24BE

instance Binary Int24BE where
  get = getInt24BE
  put = putInt24BE

instance Binary Word32BE where
  get = getWord32BE
  put = putWord32BE

instance Binary Int32BE where
  get = getInt32BE
  put = putInt32BE

instance Binary Word64BE where
  get = getWord64BE
  put = putWord64BE

instance Binary Int64BE where
  get = getInt64BE
  put = putInt64BE

instance Binary FloatBE where
  get = getFloatBE
  put = putFloatBE

instance Binary DoubleBE where
  get = getDoubleBE
  put = putDoubleBE

deriving via Word16LE instance Binary Word16

deriving via Int16LE instance Binary Int16

deriving via Word24LE instance Binary Word24

deriving via Int24LE instance Binary Int24

deriving via Word32LE instance Binary Word32

deriving via Int32LE instance Binary Int32

deriving via Word64LE instance Binary Word64

deriving via Int64LE instance Binary Int64

deriving via FloatLE instance Binary Float

deriving via DoubleLE instance Binary Double

instance Binary Char where
  get = fmap w2c getWord8
  put = putWord8 . c2w

instance Binary Int where
  get = fmap fromIntegral getInt64LE
  put = putInt64LE . fromIntegral

-- Fancy

getUntilNull :: Get (ByteCount, [Word8])
getUntilNull = go 0 []
 where
  go !i !racc = do
    w <- getWord8
    if w == 0
      then pure (i, reverse racc)
      else go (i + 1) (w : racc)

mkSBS :: ByteCount -> [Word8] -> ShortByteString
mkSBS n bs = let !(ByteArray ba) = byteArrayFromListN (coerce n) bs in SBS ba

instance Binary TermBytes where
  get = do
    (i, acc) <- getUntilNull
    unless (odd i) $ do
      w <- getWord8
      unless (w == 0) (fail "TermBytes missing word pad")
    let sbs = mkSBS i acc
    pure (TermBytes sbs)

  put (TermBytes sbs) = do
    putByteString sbs
    putWord8 0
    unless (odd (BSS.length sbs)) (putWord8 0)

instance KnownNat n => Binary (StaticBytes n) where
  get = fmap StaticBytes (getByteString (fromInteger (natVal (Proxy :: Proxy n))))
  put fb@(StaticBytes sbs) = putFixedString 0 (fromInteger (natVal fb)) sbs

instance (KnownNat n, Binary a, StaticByteSized a, Default a) => Binary (StaticSeq n a) where
  get = fmap StaticSeq (getStaticSeq (fromInteger (natVal (Proxy :: Proxy n))) get)
  put = unsafePutStaticSeqN (fromInteger (natVal (Proxy :: Proxy n))) (Just def) put . unStaticSeq

instance (KnownNat n, LiftedPrim a, StaticByteSized a, Default a) => Binary (StaticArray n a) where
  get = fmap StaticArray (getStaticArray (fromInteger (natVal (Proxy :: Proxy n))))
  put = unsafePutStaticArrayN (fromInteger (natVal (Proxy :: Proxy n))) (Just def) . unStaticArray

instance Binary BoolByte where
  get = fmap (BoolByte . (/= 0)) getWord8
  put (BoolByte b) = putWord8 (if b then 1 else 0)

instance KnownSymbol s => Binary (ExactBytes s) where
  get = do
    let s = symbolVal (Proxy :: Proxy s)
        bc = coerce (length s)
        bs = BSS.pack (fmap c2w s)
    getExpect s (getByteString bc) bs
    pure (ExactBytes ())
  put _ = do
    let s = symbolVal (Proxy :: Proxy s)
    putByteString (BSS.pack (fmap c2w s))
