{-# HLINT ignore "Use const" #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Exists to give orphan Prim instances to 24-bit integral types.
module Dahdit.ShortWord () where

import Data.Bits (Bits (..), shiftR)
import Data.Default (Default (..))
import Data.Primitive (Prim (..))
import Data.ShortWord (Int24, Word24)
import Data.Word (Word8)
import GHC.Exts (Int (..), Int#, addIntC#, mulIntMayOflo#)

mkWord24 :: Word8 -> Word8 -> Word8 -> Word24
mkWord24 b0 b1 b2 =
  let !w0 = fromIntegral b0
      !w1 = shiftL (fromIntegral b1) 8
      !w2 = shiftL (fromIntegral b2) 16
      !w = w0 .|. w1 .|. w2
  in  w
{-# INLINE mkWord24 #-}

unMkWord24 :: Word24 -> (# Word8, Word8, Word8 #)
unMkWord24 w =
  let !b0 = fromIntegral (w .&. 255)
      !b1 = fromIntegral (shiftR w 8 .&. 255)
      !b2 = fromIntegral (shiftR w 16 .&. 255)
      !bs = (# b0, b1, b2 #)
  in  bs
{-# INLINE unMkWord24 #-}

mkInt24 :: Word8 -> Word8 -> Word8 -> Int24
mkInt24 b0 b1 b2 =
  let !w0 = fromIntegral b0
      !w1 = shiftL (fromIntegral b1) 8
      !w2 = shiftL (fromIntegral b2) 16
      !w = w0 .|. w1 .|. w2
  in  w
{-# INLINE mkInt24 #-}

unMkInt24 :: Int24 -> (# Word8, Word8, Word8 #)
unMkInt24 w =
  let !b0 = fromIntegral (w .&. 255)
      !b1 = fromIntegral (shiftR w 8 .&. 255)
      !b2 = fromIntegral (shiftR w 16 .&. 255)
      !bs = (# b0, b1, b2 #)
  in  bs
{-# INLINE unMkInt24 #-}

k3 :: a -> Int#
k3 = let !(I# i) = 3 in \_ -> i
{-# INLINE k3 #-}

x3 :: Int# -> Int#
x3 =
  let !(I# three) = 3
  in  mulIntMayOflo# three
{-# INLINE x3 #-}

x3p1 :: Int# -> Int#
x3p1 =
  let !(I# three) = 3
      !(I# one) = 1
  in  \i ->
        let !(# x, _ #) = addIntC# one (mulIntMayOflo# three i)
        in  x
{-# INLINE x3p1 #-}

x3p2 :: Int# -> Int#
x3p2 =
  let !(I# three) = 3
      !(I# two) = 2
  in  \i ->
        let !(# x, _ #) = addIntC# two (mulIntMayOflo# three i)
        in  x
{-# INLINE x3p2 #-}

instance Default Word24 where
  def = 0

instance Default Int24 where
  def = 0

instance Prim Word24 where
  sizeOfType# = k3
  sizeOf# = k3
  alignmentOfType# = k3
  alignment# = k3
  indexByteArray# a i =
    let !b0 = indexByteArray# a (x3 i)
        !b1 = indexByteArray# a (x3p1 i)
        !b2 = indexByteArray# a (x3p2 i)
        !w = mkWord24 b0 b1 b2
    in  w
  readByteArray# a i s =
    let !(# s0, b0 #) = readByteArray# a (x3 i) s
        !(# s1, b1 #) = readByteArray# a (x3p1 i) s0
        !(# s2, b2 #) = readByteArray# a (x3p2 i) s1
        !w = mkWord24 b0 b1 b2
        !t = (# s2, w #)
    in  t
  writeByteArray# a i w s =
    let !(# b0, b1, b2 #) = unMkWord24 w
        !s0 = writeByteArray# a (x3 i) b0 s
        !s1 = writeByteArray# a (x3p1 i) b1 s0
        !s2 = writeByteArray# a (x3p2 i) b2 s1
    in  s2
  indexOffAddr# a i =
    let !b0 = indexOffAddr# a (x3 i)
        !b1 = indexOffAddr# a (x3p1 i)
        !b2 = indexOffAddr# a (x3p2 i)
        !w = mkWord24 b0 b1 b2
    in  w
  readOffAddr# a i s =
    let !(# s0, b0 #) = readOffAddr# a (x3 i) s
        !(# s1, b1 #) = readOffAddr# a (x3p1 i) s0
        !(# s2, b2 #) = readOffAddr# a (x3p2 i) s1
        !w = mkWord24 b0 b1 b2
        !t = (# s2, w #)
    in  t
  writeOffAddr# a i w s =
    let !(# b0, b1, b2 #) = unMkWord24 w
        !s0 = writeOffAddr# a (x3 i) b0 s
        !s1 = writeOffAddr# a (x3p1 i) b1 s0
        !s2 = writeOffAddr# a (x3p2 i) b2 s1
    in  s2

instance Prim Int24 where
  sizeOfType# = k3
  sizeOf# = k3
  alignmentOfType# = k3
  alignment# = k3
  indexByteArray# a i =
    let !b0 = indexByteArray# a (x3 i)
        !b1 = indexByteArray# a (x3p1 i)
        !b2 = indexByteArray# a (x3p2 i)
        !w = mkInt24 b0 b1 b2
    in  w
  readByteArray# a i s =
    let !(# s0, b0 #) = readByteArray# a (x3 i) s
        !(# s1, b1 #) = readByteArray# a (x3p1 i) s0
        !(# s2, b2 #) = readByteArray# a (x3p2 i) s1
        !w = mkInt24 b0 b1 b2
        !t = (# s2, w #)
    in  t
  writeByteArray# a i w s =
    let !(# b0, b1, b2 #) = unMkInt24 w
        !s0 = writeByteArray# a (x3 i) b0 s
        !s1 = writeByteArray# a (x3p1 i) b1 s0
        !s2 = writeByteArray# a (x3p2 i) b2 s1
    in  s2
  indexOffAddr# a i =
    let !b0 = indexOffAddr# a (x3 i)
        !b1 = indexOffAddr# a (x3p1 i)
        !b2 = indexOffAddr# a (x3p2 i)
        !w = mkInt24 b0 b1 b2
    in  w
  readOffAddr# a i s =
    let !(# s0, b0 #) = readOffAddr# a (x3 i) s
        !(# s1, b1 #) = readOffAddr# a (x3p1 i) s0
        !(# s2, b2 #) = readOffAddr# a (x3p2 i) s1
        !w = mkInt24 b0 b1 b2
        !t = (# s2, w #)
    in  t
  writeOffAddr# a i w s =
    let !(# b0, b1, b2 #) = unMkInt24 w
        !s0 = writeOffAddr# a (x3 i) b0 s
        !s1 = writeOffAddr# a (x3p1 i) b1 s0
        !s2 = writeOffAddr# a (x3p2 i) b2 s1
    in  s2
