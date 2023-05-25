module Dahdit.Funs
  ( getWord8
  , getInt8
  , getWord16LE
  , getInt16LE
  , getWord24LE
  , getInt24LE
  , getWord32LE
  , getInt32LE
  , getWord64LE
  , getInt64LE
  , getFloatLE
  , getDoubleLE
  , getWord16BE
  , getInt16BE
  , getWord24BE
  , getInt24BE
  , getWord32BE
  , getInt32BE
  , getWord64BE
  , getInt64BE
  , getFloatBE
  , getDoubleBE
  , getText
  , getByteString
  , getSkip
  , getExact
  , getWithin
  , getList
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
  , putWord64LE
  , putInt64LE
  , putFloatLE
  , putDoubleLE
  , putWord16BE
  , putInt16BE
  , putWord24BE
  , putInt24BE
  , putWord32BE
  , putInt32BE
  , putWord64BE
  , putInt64BE
  , putFloatBE
  , putDoubleBE
  , putText
  , putByteString
  , putFixedString
  , putList
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
import Dahdit.LiftedPrim (LiftedPrim (..))
import Dahdit.LiftedPrimArray (LiftedPrimArray (..), lengthLiftedPrimArray)
import Dahdit.Nums
  ( DoubleBE
  , DoubleLE
  , FloatBE
  , FloatLE
  , Int16BE
  , Int16LE
  , Int24BE
  , Int24LE
  , Int32BE
  , Int32LE
  , Int64BE
  , Int64LE
  , Word16BE
  , Word16LE
  , Word24BE
  , Word24LE
  , Word32BE
  , Word32LE
  , Word64BE
  , Word64LE
  )
import Dahdit.Proxy (proxyForF, proxyForFun)
import Dahdit.Sizes (ByteCount (..), ElemCount (..), StaticByteSized (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Int (Int8)
import Data.Primitive (sizeofByteArray)
import Data.Primitive.ByteArray (ByteArray)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
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

getWord64LE :: Get Word64LE
getWord64LE = Get (F (\x y -> y (GetFWord64LE x)))

getInt64LE :: Get Int64LE
getInt64LE = Get (F (\x y -> y (GetFInt64LE x)))

getFloatLE :: Get FloatLE
getFloatLE = Get (F (\x y -> y (GetFFloatLE x)))

getDoubleLE :: Get DoubleLE
getDoubleLE = Get (F (\x y -> y (GetFDoubleLE x)))

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

getWord64BE :: Get Word64BE
getWord64BE = Get (F (\x y -> y (GetFWord64BE x)))

getInt64BE :: Get Int64BE
getInt64BE = Get (F (\x y -> y (GetFInt64BE x)))

getFloatBE :: Get FloatBE
getFloatBE = Get (F (\x y -> y (GetFFloatBE x)))

getDoubleBE :: Get DoubleBE
getDoubleBE = Get (F (\x y -> y (GetFDoubleBE x)))

getText :: ByteCount -> Get ShortText
getText = fmap TSU.fromShortByteStringUnsafe . getByteString

getByteString :: ByteCount -> Get ShortByteString
getByteString bc = Get (F (\x y -> y (GetFShortByteString bc x)))

getSkip :: ByteCount -> Get ()
getSkip bc = Get (F (\x y -> y (GetFSkip bc (x ()))))

getExact :: ByteCount -> Get a -> Get a
getExact bc g = Get (F (\x y -> y (GetFScope (GetScopeF ScopeModeExact bc g x))))

getWithin :: ByteCount -> Get a -> Get a
getWithin bc g = Get (F (\x y -> y (GetFScope (GetScopeF ScopeModeWithin bc g x))))

-- | Get List of dynamically-sized elements
getList :: ElemCount -> Get a -> Get [a]
getList ec g = go [] 0
 where
  go !acc !i =
    if i == ec
      then pure (reverse acc)
      else do
        x <- g
        go (x : acc) (i + 1)

-- | Get Seq of dynamically-sized elements
getSeq :: ElemCount -> Get a -> Get (Seq a)
getSeq ec g = go Empty 0
 where
  go !acc !i =
    if i == ec
      then pure acc
      else do
        x <- g
        go (acc :|> x) (i + 1)

-- | Get Seq of statically-sized elements
getStaticSeq :: StaticByteSized a => ElemCount -> Get a -> Get (Seq a)
getStaticSeq n g = Get (F (\x y -> y (GetFStaticSeq (GetStaticSeqF n g x))))

-- | Get PrimArray of statically-sized elements
getStaticArray :: LiftedPrim a => ElemCount -> Get (LiftedPrimArray a)
getStaticArray n = Get (F (\x y -> y (GetFStaticArray (GetStaticArrayF n (Proxy :: Proxy a) x))))

getByteArray :: ByteCount -> Get ByteArray
getByteArray bc = Get (F (\x y -> y (GetFByteArray bc x)))

getLiftedPrimArray :: LiftedPrim a => Proxy a -> ElemCount -> Get (LiftedPrimArray a)
getLiftedPrimArray prox ec =
  let bc = staticByteSize prox * coerce ec
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
        go (acc :|> x)

getRemainingStaticSeq :: StaticByteSized a => Get a -> Get (Seq a)
getRemainingStaticSeq g = do
  let ebc = staticByteSize (proxyForF g)
  bc <- getRemainingSize
  let left = rem bc ebc
  if left == 0
    then getStaticSeq (coerce (div bc ebc)) g
    else fail ("Leftover bytes for remaining static seq (have " ++ show (unByteCount left) ++ ", need " ++ show (unByteCount ebc) ++ ")")

getRemainingStaticArray :: LiftedPrim a => Proxy a -> Get (LiftedPrimArray a)
getRemainingStaticArray prox = do
  let ebc = staticByteSize prox
  bc <- getRemainingSize
  let left = rem bc ebc
  if left == 0
    then getStaticArray (coerce (div bc ebc))
    else fail ("Leftover bytes for remaining static array (have " ++ show (unByteCount left) ++ ", need " ++ show (unByteCount ebc) ++ ")")

getRemainingByteArray :: Get ByteArray
getRemainingByteArray = getRemainingSize >>= getByteArray

getRemainingLiftedPrimArray :: LiftedPrim a => Proxy a -> Get (LiftedPrimArray a)
getRemainingLiftedPrimArray prox = do
  let ebc = staticByteSize prox
  bc <- getRemainingSize
  let left = rem bc ebc
  if left == 0
    then do
      let ec = coerce (div bc ebc)
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

putWord64LE :: Word64LE -> Put
putWord64LE d = PutM (F (\x y -> y (PutFWord64LE d (x ()))))

putInt64LE :: Int64LE -> Put
putInt64LE d = PutM (F (\x y -> y (PutFInt64LE d (x ()))))

putFloatLE :: FloatLE -> Put
putFloatLE d = PutM (F (\x y -> y (PutFFloatLE d (x ()))))

putDoubleLE :: DoubleLE -> Put
putDoubleLE d = PutM (F (\x y -> y (PutFDoubleLE d (x ()))))

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

putWord64BE :: Word64BE -> Put
putWord64BE d = PutM (F (\x y -> y (PutFWord64BE d (x ()))))

putInt64BE :: Int64BE -> Put
putInt64BE d = PutM (F (\x y -> y (PutFInt64BE d (x ()))))

putFloatBE :: FloatBE -> Put
putFloatBE d = PutM (F (\x y -> y (PutFFloatBE d (x ()))))

putDoubleBE :: DoubleBE -> Put
putDoubleBE d = PutM (F (\x y -> y (PutFDoubleBE d (x ()))))

putText :: ShortText -> Put
putText = putByteString . TS.toShortByteString

putByteString :: ShortByteString -> Put
putByteString sbs =
  let bc = coerce (BSS.length sbs)
  in  PutM (F (\x y -> y (PutFShortByteString bc sbs (x ()))))

putFixedString :: Word8 -> ByteCount -> ShortByteString -> Put
putFixedString pad bc sbs = do
  unless (bc == 0) $ do
    let len = coerce bc
        lenSbs = BSS.length sbs
        mostLen = min len lenSbs
        mostBc = coerce mostLen
    PutM (F (\x y -> y (PutFShortByteString mostBc sbs (x ()))))
    let diff = len - lenSbs
    unless (diff <= 0) (replicateM_ diff (putWord8 pad))

-- | Put List of dynamically-sized elements
putList :: (a -> Put) -> [a] -> Put
putList = traverse_

-- | Put Seq of dynamically-sized elements
putSeq :: (a -> Put) -> Seq a -> Put
putSeq = traverse_

-- | Put Seq of statically-sized elements
putStaticSeq :: StaticByteSized a => (a -> Put) -> Seq a -> Put
putStaticSeq p s =
  let n = coerce (Seq.length s)
  in  unsafePutStaticSeqN n Nothing p s

unsafePutStaticSeqN :: StaticByteSized a => ElemCount -> Maybe a -> (a -> Put) -> Seq a -> Put
unsafePutStaticSeqN n mz p s = PutM (F (\x y -> y (PutFStaticSeq (PutStaticSeqF n mz p s (x ())))))

-- | Put Array of statically-sized elements
putStaticArray :: LiftedPrim a => LiftedPrimArray a -> Put
putStaticArray a =
  let ec = lengthLiftedPrimArray a
  in  unsafePutStaticArrayN ec Nothing a

unsafePutStaticArrayN :: LiftedPrim a => ElemCount -> Maybe a -> LiftedPrimArray a -> Put
unsafePutStaticArrayN n mz a = PutM (F (\x y -> y (PutFStaticArray (PutStaticArrayF n mz a (x ())))))

putByteArray :: ByteArray -> Put
putByteArray arr =
  let bc = coerce (sizeofByteArray arr)
  in  PutM (F (\x y -> y (PutFByteArray bc arr (x ()))))

putLiftedPrimArray :: LiftedPrimArray a -> Put
putLiftedPrimArray = putByteArray . unLiftedPrimArray

putStaticHint :: StaticByteSized a => (a -> Put) -> a -> Put
putStaticHint p a =
  let bc = staticByteSize (proxyForFun p)
  in  PutM (F (\x y -> y (PutFStaticHint (PutStaticHintF bc (p a) (x ())))))
