module Dahdit.Internal where

import Data.Bits (Bits (..))
import Data.Int (Int16, Int32, Int8)
import Data.ShortWord (Int24, Word24)
import Data.Word (Word16, Word32, Word8)
import GHC.Float (castFloatToWord32, castWord32ToFloat)

newtype ViaFromIntegral x y = ViaFromIntegral {unViaFromIntegral :: y}
  deriving newtype (Num)

-- Types that can swap endianness - swapEndian is its own inverse
class Num w => SwapEndian w where
  swapEndian :: w -> w

instance (SwapEndian x, Integral x, Integral y) => SwapEndian (ViaFromIntegral x y) where
  swapEndian = ViaFromIntegral . fromIntegral @x @y . swapEndian . fromIntegral @y @x . unViaFromIntegral

instance SwapEndian Word8 where
  swapEndian = id

instance SwapEndian Int8 where
  swapEndian = id

instance SwapEndian Word16 where
  swapEndian w =
    let (b0, b1) = unMkWord16LE w
    in  mkWord16LE b1 b0

deriving via (ViaFromIntegral Word16 Int16) instance SwapEndian Int16

instance SwapEndian Word24 where
  swapEndian w =
    let (b0, b1, b2) = unMkWord24LE w
    in  mkWord24LE b2 b1 b0

deriving via (ViaFromIntegral Word24 Int24) instance SwapEndian Int24

instance SwapEndian Word32 where
  swapEndian w =
    let (b0, b1, b2, b3) = unMkWord32LE w
    in  mkWord32LE b3 b2 b1 b0

deriving via (ViaFromIntegral Word32 Int32) instance SwapEndian Int32

instance SwapEndian Float where
  swapEndian w =
    let (b0, b1, b2, b3) = unMkFloatLE w
    in  mkFloatLE b3 b2 b1 b0

mkWord16LE :: Word8 -> Word8 -> Word16
mkWord16LE b0 b1 = (fromIntegral b1 `unsafeShiftL` 8) .|. fromIntegral b0

unMkWord16LE :: Word16 -> (Word8, Word8)
unMkWord16LE w =
  let b0 = fromIntegral w
      b1 = fromIntegral (w `shiftR` 8)
  in  (b0, b1)

mkWord24LE :: Word8 -> Word8 -> Word8 -> Word24
mkWord24LE b0 b1 b2 = fromIntegral (mkWord32LE b0 b1 b2 0)

unMkWord24LE :: Word24 -> (Word8, Word8, Word8)
unMkWord24LE w =
  let v = fromIntegral w
      (b0, b1, b2, _) = unMkWord32LE v
  in  (b0, b1, b2)

mkWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkWord32LE b0 b1 b2 b3 =
  (fromIntegral b3 `unsafeShiftL` 24)
    .|. (fromIntegral b2 `unsafeShiftL` 16)
    .|. (fromIntegral b1 `unsafeShiftL` 8)
    .|. fromIntegral b0

unMkWord32LE :: Word32 -> (Word8, Word8, Word8, Word8)
unMkWord32LE w =
  let b0 = fromIntegral w
      b1 = fromIntegral (w `shiftR` 8)
      b2 = fromIntegral (w `shiftR` 16)
      b3 = fromIntegral (w `shiftR` 24)
  in  (b0, b1, b2, b3)

mkFloatLE :: Word8 -> Word8 -> Word8 -> Word8 -> Float
mkFloatLE b0 b1 b2 b3 = castWord32ToFloat (mkWord32LE b0 b1 b2 b3)

unMkFloatLE :: Float -> (Word8, Word8, Word8, Word8)
unMkFloatLE f = unMkWord32LE (castFloatToWord32 f)
