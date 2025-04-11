module Dahdit.Binary
  ( Binary (..)
  , byteSizeFoldable
  )
where

import Dahdit.Free (Get, Put)
import Dahdit.Funs
  ( getDoubleBE
  , getDoubleLE
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
  , getList
  , getSeq
  , getWord16BE
  , getWord16LE
  , getWord24BE
  , getWord24LE
  , getWord32BE
  , getWord32LE
  , getWord64BE
  , getWord64LE
  , getWord8
  , putDoubleBE
  , putDoubleLE
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
  , putList
  , putSeq
  , putWord16BE
  , putWord16LE
  , putWord24BE
  , putWord24LE
  , putWord32BE
  , putWord32LE
  , putWord64BE
  , putWord64LE
  , putWord8
  )
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
import Dahdit.Run (runCount)
import Dahdit.Sizes (ByteCount (..), ElemCount (..))
import Data.ByteString.Internal (c2w, w2c)
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Sum (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word64, Word8)

class Binary a where
  byteSize :: a -> ByteCount
  byteSize = runCount . put
  get :: Get a
  put :: a -> Put

-- Basic types

instance Binary () where
  byteSize _ = 0
  get = pure ()
  put _ = pure ()

instance Binary Word8 where
  byteSize _ = 1
  get = getWord8
  put = putWord8

instance Binary Int8 where
  byteSize _ = 1
  get = getInt8
  put = putInt8

instance Binary Word16LE where
  byteSize _ = 2
  get = getWord16LE
  put = putWord16LE

instance Binary Int16LE where
  byteSize _ = 2
  get = getInt16LE
  put = putInt16LE

instance Binary Word24LE where
  byteSize _ = 3
  get = getWord24LE
  put = putWord24LE

instance Binary Int24LE where
  byteSize _ = 3
  get = getInt24LE
  put = putInt24LE

instance Binary Word32LE where
  byteSize _ = 4
  get = getWord32LE
  put = putWord32LE

instance Binary Int32LE where
  byteSize _ = 4
  get = getInt32LE
  put = putInt32LE

instance Binary Word64LE where
  byteSize _ = 8
  get = getWord64LE
  put = putWord64LE

instance Binary Int64LE where
  byteSize _ = 8
  get = getInt64LE
  put = putInt64LE

instance Binary FloatLE where
  byteSize _ = 4
  get = getFloatLE
  put = putFloatLE

instance Binary DoubleLE where
  byteSize _ = 8
  get = getDoubleLE
  put = putDoubleLE

instance Binary Word16BE where
  byteSize _ = 2
  get = getWord16BE
  put = putWord16BE

instance Binary Int16BE where
  byteSize _ = 2
  get = getInt16BE
  put = putInt16BE

instance Binary Word24BE where
  byteSize _ = 3
  get = getWord24BE
  put = putWord24BE

instance Binary Int24BE where
  byteSize _ = 3
  get = getInt24BE
  put = putInt24BE

instance Binary Word32BE where
  byteSize _ = 4
  get = getWord32BE
  put = putWord32BE

instance Binary Int32BE where
  byteSize _ = 4
  get = getInt32BE
  put = putInt32BE

instance Binary Word64BE where
  byteSize _ = 8
  get = getWord64BE
  put = putWord64BE

instance Binary Int64BE where
  byteSize _ = 8
  get = getInt64BE
  put = putInt64BE

instance Binary FloatBE where
  byteSize _ = 4
  get = getFloatBE
  put = putFloatBE

instance Binary DoubleBE where
  byteSize _ = 8
  get = getDoubleBE
  put = putDoubleBE

-- Assuming we are on little-endian arch...

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

instance Binary Bool where
  byteSize _ = 1
  get = fmap (/= 0) getWord8
  put b = putWord8 (if b then 1 else 0)

instance Binary Char where
  byteSize _ = 1
  get = fmap w2c getWord8
  put = putWord8 . c2w

instance Binary Int where
  byteSize _ = 8
  get = fmap fromIntegral getInt64LE
  put = putInt64LE . fromIntegral

instance (Binary a) => Binary [a] where
  byteSize as = 8 + byteSizeFoldable as
  get = do
    ec <- get @Int
    getList (coerce ec) get
  put s = put @Int (length s) *> putList put s

instance (Binary a) => Binary (Seq a) where
  byteSize as = 8 + byteSizeFoldable as
  get = do
    ec <- get @Int
    getSeq (coerce ec) get
  put s = put @Int (Seq.length s) *> putSeq put s

instance (Binary a) => Binary (Set a) where
  byteSize = byteSize . Set.toAscList
  get = fmap Set.fromDistinctAscList get
  put = put . Set.toAscList

instance (Binary k, Binary v) => Binary (Map k v) where
  byteSize = byteSize . Map.toAscList
  get = fmap Map.fromDistinctAscList get
  put = put . Map.toAscList

instance Binary IntSet where
  byteSize = byteSize . IntSet.toAscList
  get = fmap IntSet.fromDistinctAscList get
  put = put . IntSet.toAscList

instance (Binary v) => Binary (IntMap v) where
  byteSize = byteSize . IntMap.toAscList
  get = fmap IntMap.fromDistinctAscList get
  put = put . IntMap.toAscList

instance (Binary a) => Binary (Maybe a) where
  byteSize = \case
    Nothing -> 1
    Just a -> 1 + byteSize a
  get = do
    tag <- get @Int
    case tag of
      0 -> pure Nothing
      1 -> fmap Just get
      _ -> fail "Unknown encoding for constructor"
  put = \case
    Nothing -> put @Int 0
    Just a -> put @Int 1 *> put a

instance (Binary b, Binary a) => Binary (Either b a) where
  byteSize = \case
    Left b -> 1 + byteSize b
    Right a -> 1 + byteSize a
  get = do
    tag <- get @Int
    case tag of
      0 -> fmap Left get
      1 -> fmap Right get
      _ -> fail "Unknown encoding for constructor"
  put = \case
    Left b -> put @Int 0 *> put b
    Right a -> put @Int 1 *> put a

instance (Binary a, Binary b) => Binary (a, b) where
  byteSize (a, b) = byteSize a + byteSize b
  get = do
    a <- get
    b <- get
    pure (a, b)
  put (a, b) = put a *> put b

instance (Binary a, Binary b, Binary c) => Binary (a, b, c) where
  byteSize (a, b, c) = byteSize a + byteSize b + byteSize c
  get = do
    a <- get
    b <- get
    c <- get
    pure (a, b, c)
  put (a, b, c) = put a *> put b *> put c

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a, b, c, d) where
  byteSize (a, b, c, d) = byteSize a + byteSize b + byteSize c + byteSize d
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    pure (a, b, c, d)
  put (a, b, c, d) = put a *> put b *> put c *> put d

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a, b, c, d, e) where
  byteSize (a, b, c, d, e) = byteSize a + byteSize b + byteSize c + byteSize d + byteSize e
  get = do
    a <- get
    b <- get
    c <- get
    d <- get
    e <- get
    pure (a, b, c, d, e)
  put (a, b, c, d, e) = put a *> put b *> put c *> put d *> put e

byteSizeFoldable :: (Foldable f, Binary a) => f a -> ByteCount
byteSizeFoldable = getSum . foldMap' (Sum . byteSize)
