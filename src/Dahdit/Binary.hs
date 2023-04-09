module Dahdit.Binary
  ( Binary (..)
  , byteSizeViaPut
  )
where

import Dahdit.Counts (ByteCount)
import Dahdit.Free (Get, Put)
import Dahdit.Funs
  ( getFloatBE
  , getFloatLE
  , getInt16BE
  , getInt16LE
  , getInt24BE
  , getInt24LE
  , getInt32BE
  , getInt32LE
  , getInt8
  , getWord16BE
  , getWord16LE
  , getWord24BE
  , getWord24LE
  , getWord32BE
  , getWord32LE
  , getWord8
  , putFloatBE
  , putFloatLE
  , putInt16BE
  , putInt16LE
  , putInt24BE
  , putInt24LE
  , putInt32BE
  , putInt32LE
  , putInt8
  , putWord16BE
  , putWord16LE
  , putWord24BE
  , putWord24LE
  , putWord32BE
  , putWord32LE
  , putWord8
  )
import Dahdit.Nums
  ( FloatBE (..)
  , FloatLE
  , Int16BE (..)
  , Int16LE
  , Int24BE (..)
  , Int24LE
  , Int32BE (..)
  , Int32LE
  , Word16BE (..)
  , Word16LE
  , Word24BE (..)
  , Word24LE
  , Word32BE (..)
  , Word32LE
  )
import Dahdit.Run (runCount)
import Dahdit.Sizes (ByteSized)
import Data.Int (Int8)
import Data.Word (Word8)

class Binary a where
  get :: Get a
  put :: a -> Put

byteSizeViaPut :: Binary a => a -> ByteCount
byteSizeViaPut = runCount . put

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

instance Binary FloatLE where
  get = getFloatLE
  put = putFloatLE

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

instance Binary FloatBE where
  get = getFloatBE
  put = putFloatBE
