module Dahdit.Funs
  ( getWord8
  , getInt8
  , getWord16LE
  , getInt16LE
  , getWord24LE
  , getInt24LE
  , getWord32LE
  , getInt32LE
  , getFloatLE
  , getWord16BE
  , getInt16BE
  , getWord24BE
  , getInt24BE
  , getWord32BE
  , getInt32BE
  , getFloatBE
  , getByteString
  , getSkip
  , getExact
  , getWithin
  , getSeq
  , getStaticSeq
  , getStaticArray
  , getByteArray
  , getLiftedPrimArray
  , getExpect
  , getLookAhead
  , getRemainingSize
  , getRemainingString
  , getRemainingSeq
  , getRemainingStaticSeq
  , getRemainingStaticArray
  , getRemainingByteArray
  , getRemainingLiftedPrimArray
  , getUnfold
  , putWord8
  , putInt8
  , putWord16LE
  , putInt16LE
  , putWord24LE
  , putInt24LE
  , putWord32LE
  , putInt32LE
  , putFloatLE
  , putWord16BE
  , putInt16BE
  , putWord24BE
  , putInt24BE
  , putWord32BE
  , putInt32BE
  , putFloatBE
  , putByteString
  , putFixedString
  , putSeq
  , putStaticSeq
  , unsafePutStaticSeqN
  , putStaticArray
  , unsafePutStaticArrayN
  , putByteArray
  , putLiftedPrimArray
  , putStaticHint
  )
where

import Control.Monad (replicateM_, unless)
import Control.Monad.Free.Church (F (..))
import Dahdit.Free
  ( Get (..)
  , GetF (..)
  , GetLookAheadF (..)
  , GetScopeF (..)
  , GetStaticArrayF (..)
  , GetStaticSeqF (..)
  , Put
  , PutF (..)
  , PutM (..)
  , PutStaticArrayF (..)
  , PutStaticHintF (..)
  , PutStaticSeqF (..)
  , ScopeMode (..)
  )
import Dahdit.LiftedPrim (LiftedPrim (..), LiftedPrimArray (..))
import Dahdit.Nums
  ( FloatBE
  , FloatLE
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Word16BE
  , Word16LE
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  )
import Dahdit.Proxy (proxyForF, proxyForFun)
import Dahdit.Sizes (ByteCount (..), ElementCount (..), StaticByteSized (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (traverse_)
import Data.Int (Int8)
import Data.Primitive (Prim, sizeofByteArray, sizeofPrimArray)
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.PrimArray (PrimArray)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Word (Word8)

getWord8 :: Get Word8
getWord8 = Get (F (\x y -> y (GetFWord8 x)))

getInt8 :: Get Int8
getInt8 = Get (F (\x y -> y (GetFInt8 x)))

getWord16LE :: Get Word16LE
getWord16LE = Get (F (\x y -> y (GetFWord16LE x)))

getInt16LE :: Get Int16LE
getInt16LE = Get (F (\x y -> y (GetFInt16LE x)))

getWord24LE :: Get Word24LE
getWord24LE = Get (F (\x y -> y (GetFWord24LE x)))

getInt24LE :: Get Int24LE
getInt24LE = Get (F (\x y -> y (GetFInt24LE x)))

getWord32LE :: Get Word32LE
getWord32LE = Get (F (\x y -> y (GetFWord32LE x)))

getInt32LE :: Get Int32LE
getInt32LE = Get (F (\x y -> y (GetFInt32LE x)))

getFloatLE :: Get FloatLE
getFloatLE = Get (F (\x y -> y (GetFFloatLE x)))

getWord16BE :: Get Word16BE
getWord16BE = Get (F (\x y -> y (GetFWord16BE x)))

getInt16BE :: Get Int16BE
getInt16BE = Get (F (\x y -> y (GetFInt16BE x)))

getWord24BE :: Get Word24BE
getWord24BE = Get (F (\x y -> y (GetFWord24BE x)))

getInt24BE :: Get Int24BE
getInt24BE = Get (F (\x y -> y (GetFInt24BE x)))

getWord32BE :: Get Word32BE
getWord32BE = Get (F (\x y -> y (GetFWord32BE x)))

getInt32BE :: Get Int32BE
getInt32BE = Get (F (\x y -> y (GetFInt32BE x)))

getFloatBE :: Get FloatBE
getFloatBE = Get (F (\x y -> y (GetFFloatBE x)))

getByteString :: ByteCount -> Get ShortByteString
getByteString bc = Get (F (\x y -> y (GetFShortByteString bc x)))

getSkip :: ByteCount -> Get ()
getSkip bc = Get (F (\x y -> y (GetFSkip bc (x ()))))

getExact :: ByteCount -> Get a -> Get a
getExact bc g = Get (F (\x y -> y (GetFScope (GetScopeF ScopeModeExact bc g x))))

getWithin :: ByteCount -> Get a -> Get a
getWithin bc g = Get (F (\x y -> y (GetFScope (GetScopeF ScopeModeWithin bc g x))))

-- | Get Seq of dynamically-sized elements
getSeq :: ElementCount -> Get a -> Get (Seq a)
getSeq ec g = go Empty 0
 where
  go !acc i =
    if i == ec
      then pure acc
      else do
        x <- g
        x `seq` go (acc :|> x) (i + 1)

-- | Get Seq of statically-sized elements
getStaticSeq :: (StaticByteSized a) => ElementCount -> Get a -> Get (Seq a)
getStaticSeq n g = Get (F (\x y -> y (GetFStaticSeq (GetStaticSeqF n g x))))

-- | Get PrimArray of statically-sized elements
getStaticArray :: (StaticByteSized a, Prim a) => ElementCount -> Get (PrimArray a)
getStaticArray n = Get (F (\x y -> y (GetFStaticArray (GetStaticArrayF n (Proxy :: Proxy a) x))))

getByteArray :: ByteCount -> Get ByteArray
getByteArray bc = Get (F (\x y -> y (GetFByteArray bc x)))

getLiftedPrimArray :: LiftedPrim a => Proxy a -> ElementCount -> Get (LiftedPrimArray a)
getLiftedPrimArray prox ec =
  let !bc = fromIntegral (elemSizeLifted prox * fromIntegral ec)
  in  fmap LiftedPrimArray (getByteArray bc)

getLookAhead :: Get a -> Get a
getLookAhead g = Get (F (\x y -> y (GetFLookAhead (GetLookAheadF g x))))

getRemainingSize :: Get ByteCount
getRemainingSize = Get (F (\x y -> y (GetFRemainingSize x)))

getRemainingString :: Get ShortByteString
getRemainingString = getRemainingSize >>= getByteString

getRemainingSeq :: Get a -> Get (Seq a)
getRemainingSeq g = go Empty
 where
  go !acc = do
    bc <- getRemainingSize
    if bc == 0
      then pure acc
      else do
        x <- g
        x `seq` go (acc :|> x)

getRemainingStaticSeq :: (StaticByteSized a) => Get a -> Get (Seq a)
getRemainingStaticSeq g = do
  let !ebc = staticByteSize (proxyForF g)
  bc <- getRemainingSize
  let !left = rem bc ebc
  if left == 0
    then do
      let !ec = fromIntegral (div bc ebc)
      getStaticSeq ec g
    else fail ("Leftover bytes for remaining static seq (have " ++ show (unByteCount left) ++ ", need " ++ show (unByteCount ebc) ++ ")")

getRemainingStaticArray :: (StaticByteSized a, Prim a) => Proxy a -> Get (PrimArray a)
getRemainingStaticArray prox = do
  let !ebc = staticByteSize prox
  bc <- getRemainingSize
  let !left = rem bc ebc
  if left == 0
    then do
      let !ec = fromIntegral (div bc ebc)
      getStaticArray ec
    else fail ("Leftover bytes for remaining static array (have " ++ show (unByteCount left) ++ ", need " ++ show (unByteCount ebc) ++ ")")

getRemainingByteArray :: Get ByteArray
getRemainingByteArray = getRemainingSize >>= getByteArray

getRemainingLiftedPrimArray :: (LiftedPrim a) => Proxy a -> Get (LiftedPrimArray a)
getRemainingLiftedPrimArray prox = do
  let !ebc = fromIntegral (elemSizeLifted prox)
  bc <- getRemainingSize
  let !left = rem bc ebc
  if left == 0
    then do
      let !ec = fromIntegral (div bc ebc)
      getLiftedPrimArray prox ec
    else fail ("Leftover bytes for remaining lifted prim array (have " ++ show (unByteCount left) ++ ", need " ++ show (unByteCount ebc) ++ ")")

getExpect :: (Eq a, Show a) => String -> Get a -> a -> Get ()
getExpect typ getter expec = do
  actual <- getter
  unless
    (expec == actual)
    (fail ("Expected " ++ " " ++ typ ++ " " ++ show expec ++ " but found " ++ show actual))

getUnfold :: b -> (b -> Get (Either b a)) -> Get a
getUnfold b0 f = go b0
 where
  go !b = do
    eba <- f b
    either go pure eba

putWord8 :: Word8 -> Put
putWord8 d = PutM (F (\x y -> y (PutFWord8 d (x ()))))

putInt8 :: Int8 -> Put
putInt8 d = PutM (F (\x y -> y (PutFInt8 d (x ()))))

putWord16LE :: Word16LE -> Put
putWord16LE d = PutM (F (\x y -> y (PutFWord16LE d (x ()))))

putInt16LE :: Int16LE -> Put
putInt16LE d = PutM (F (\x y -> y (PutFInt16LE d (x ()))))

putWord24LE :: Word24LE -> Put
putWord24LE d = PutM (F (\x y -> y (PutFWord24LE d (x ()))))

putInt24LE :: Int24LE -> Put
putInt24LE d = PutM (F (\x y -> y (PutFInt24LE d (x ()))))

putWord32LE :: Word32LE -> Put
putWord32LE d = PutM (F (\x y -> y (PutFWord32LE d (x ()))))

putInt32LE :: Int32LE -> Put
putInt32LE d = PutM (F (\x y -> y (PutFInt32LE d (x ()))))

putFloatLE :: FloatLE -> Put
putFloatLE d = PutM (F (\x y -> y (PutFFloatLE d (x ()))))

putWord16BE :: Word16BE -> Put
putWord16BE d = PutM (F (\x y -> y (PutFWord16BE d (x ()))))

putInt16BE :: Int16BE -> Put
putInt16BE d = PutM (F (\x y -> y (PutFInt16BE d (x ()))))

putWord24BE :: Word24BE -> Put
putWord24BE d = PutM (F (\x y -> y (PutFWord24BE d (x ()))))

putInt24BE :: Int24BE -> Put
putInt24BE d = PutM (F (\x y -> y (PutFInt24BE d (x ()))))

putWord32BE :: Word32BE -> Put
putWord32BE d = PutM (F (\x y -> y (PutFWord32BE d (x ()))))

putInt32BE :: Int32BE -> Put
putInt32BE d = PutM (F (\x y -> y (PutFInt32BE d (x ()))))

putFloatBE :: FloatBE -> Put
putFloatBE d = PutM (F (\x y -> y (PutFFloatBE d (x ()))))

putByteString :: ShortByteString -> Put
putByteString sbs =
  let !bc = fromIntegral (BSS.length sbs)
  in  PutM (F (\x y -> y (PutFShortByteString bc sbs (x ()))))

putFixedString :: Word8 -> ByteCount -> ShortByteString -> Put
putFixedString pad bc sbs = do
  unless (bc == 0) $ do
    let !len = fromIntegral bc
        !lenSbs = BSS.length sbs
        !mostLen = min len lenSbs
        !mostBc = fromIntegral mostLen
    PutM (F (\x y -> y (PutFShortByteString mostBc sbs (x ()))))
    let !diff = len - lenSbs
    unless (diff <= 0) (replicateM_ diff (putWord8 pad))

-- | Put Seq of dynamically-sized elements
putSeq :: (a -> Put) -> Seq a -> Put
putSeq = traverse_

-- | Put Seq of statically-sized elements
putStaticSeq :: StaticByteSized a => (a -> Put) -> Seq a -> Put
putStaticSeq p s =
  let !n = fromIntegral (Seq.length s)
  in  unsafePutStaticSeqN n Nothing p s

unsafePutStaticSeqN :: StaticByteSized a => ElementCount -> Maybe a -> (a -> Put) -> Seq a -> Put
unsafePutStaticSeqN n mz p s = PutM (F (\x y -> y (PutFStaticSeq (PutStaticSeqF n mz p s (x ())))))

-- | Put Array of statically-sized elements
putStaticArray :: (StaticByteSized a, Prim a) => PrimArray a -> Put
putStaticArray a =
  let !n = fromIntegral (sizeofPrimArray a)
  in  unsafePutStaticArrayN n Nothing a

unsafePutStaticArrayN :: (StaticByteSized a, Prim a) => ElementCount -> Maybe a -> PrimArray a -> Put
unsafePutStaticArrayN n mz a = PutM (F (\x y -> y (PutFStaticArray (PutStaticArrayF n mz a (x ())))))

putByteArray :: ByteArray -> Put
putByteArray arr =
  let !bc = fromIntegral (sizeofByteArray arr)
  in  PutM (F (\x y -> y (PutFByteArray bc arr (x ()))))

putLiftedPrimArray :: LiftedPrimArray a -> Put
putLiftedPrimArray = putByteArray . unLiftedPrimArray

putStaticHint :: StaticByteSized a => (a -> Put) -> a -> Put
putStaticHint p =
  let !bc = staticByteSize (proxyForFun p)
  in  \a -> PutM (F (\x y -> y (PutFStaticHint (PutStaticHintF bc (p a) (x ())))))
