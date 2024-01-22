module Dahdit.Midi.Pad
  ( pad32
  , staticByteSizePad32
  , byteSizePad32
  , getPad32
  , putPad32
  )
where

import Control.Monad (replicateM_, unless)
import Dahdit (Binary (..), ByteCount (..), Get, Put, getExpect, getRemainingSize)
import Data.Proxy (Proxy)
import Data.Word (Word8)

pad32 :: ByteCount -> ByteCount
pad32 x = let y = rem x 4 in x + if y == 0 then 0 else 4 - y

staticByteSizePad32 :: (Proxy a -> ByteCount) -> Proxy a -> ByteCount
staticByteSizePad32 staticSizer p = pad32 (staticSizer p)

byteSizePad32 :: (a -> ByteCount) -> a -> ByteCount
byteSizePad32 sizer a = pad32 (sizer a)

getPad32 :: Get a -> Get a
getPad32 getter = do
  x <- getRemainingSize
  a <- getter
  y <- getRemainingSize
  let z = rem (unByteCount (x - y)) 4
  unless (z == 0) (replicateM_ (4 - z) (getExpect "pad" (get @Word8) 0))
  pure a

putPad32 :: (a -> ByteCount) -> (a -> Put) -> a -> Put
putPad32 sizer putter a = do
  let x = sizer a
  putter a
  let y = rem (unByteCount x) 4
  unless (y == 0) (replicateM_ (4 - y) (put @Word8 0))
