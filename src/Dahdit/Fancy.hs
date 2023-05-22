{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Dahdit.Fancy
  ( TermBytes (..)
  , StaticBytes (..)
  , mkStaticBytes
  , normStaticBytes
  , StaticSeq (..)
  , StaticArray (..)
  , BoolByte (..)
  , ExactBytes (..)
  )
where

import Control.Monad (unless)
import Dahdit.Binary (Binary (..))
import Dahdit.Free (Get)
import Dahdit.Funs
  ( getByteString
  , getExpect
  , getStaticArray
  , getStaticSeq
  , getWord8
  , putByteString
  , putFixedString
  , putWord8
  , unsafePutStaticArrayN
  , unsafePutStaticSeqN
  )
import Dahdit.LiftedPrim (LiftedPrim)
import Dahdit.LiftedPrimArray (LiftedPrimArray, replicateLiftedPrimArray)
import Dahdit.Proxy (proxyForNatF)
import Dahdit.Sizes (ByteCount (..), StaticByteSized (..), byteSizeViaStatic)
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Primitive.ByteArray (ByteArray (..), byteArrayFromListN)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Word (Word8)
import GHC.TypeLits (ConsSymbol, KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal, type (*), type (+))

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

-- | Bytes terminated with null byte.
-- NOTE: Terminated with TWO null bytes if the string is even length
-- to align to Word16 boundaries, as required for RIFF format, for example.
newtype TermBytes = TermBytes {unTermBytes :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default TermBytes where
  def = TermBytes BSS.empty

instance Binary TermBytes where
  byteSize (TermBytes sbs) =
    let bc = ByteCount (BSS.length sbs + 1)
    in  if even bc then bc else bc + 1

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

-- | A fixed-length bytestring (truncated or zero-padded on put if length does not match).
newtype StaticBytes (n :: Nat) = StaticBytes {unStaticBytes :: ShortByteString}
  deriving stock (Show)
  deriving newtype (IsString)

mkStaticBytes :: KnownNat n => Proxy n -> ShortByteString -> StaticBytes n
mkStaticBytes prox sbs =
  let n = fromInteger (natVal prox)
  in  if BSS.length sbs == n
        then StaticBytes sbs
        else
          let x1 = BSS.take n sbs
              l = BSS.length x1
          in  StaticBytes $
                if l == n
                  then x1
                  else x1 <> BSS.replicate (n - l) 0

normStaticBytes :: KnownNat n => StaticBytes n -> StaticBytes n
normStaticBytes sb@(StaticBytes sbs) = mkStaticBytes (proxyForNatF sb) sbs

instance KnownNat n => Eq (StaticBytes n) where
  x == y =
    let StaticBytes x' = normStaticBytes x
        StaticBytes y' = normStaticBytes y
    in  x' == y'

instance KnownNat n => Ord (StaticBytes n) where
  compare x y =
    let StaticBytes x' = normStaticBytes x
        StaticBytes y' = normStaticBytes y
    in  compare x' y'

instance Default (StaticBytes n) where
  def = StaticBytes BSS.empty

instance KnownNat n => StaticByteSized (StaticBytes n) where
  type StaticSize (StaticBytes n) = n

instance KnownNat n => Binary (StaticBytes n) where
  byteSize = byteSizeViaStatic
  get = fmap StaticBytes (getByteString (fromInteger (natVal (Proxy :: Proxy n))))
  put fb@(StaticBytes sbs) = putFixedString 0 (fromInteger (natVal fb)) sbs

newtype StaticSeq (n :: Nat) a = StaticSeq {unStaticSeq :: Seq a}
  deriving stock (Show)
  deriving newtype (Eq, Functor, Foldable)

instance (KnownNat n, Default a) => Default (StaticSeq n a) where
  def = StaticSeq (Seq.replicate (fromInteger (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, StaticByteSized a, o ~ n * StaticSize a, KnownNat o) => StaticByteSized (StaticSeq n a) where
  type StaticSize (StaticSeq n a) = n * StaticSize a

instance (KnownNat n, Binary a, StaticByteSized a, Default a, o ~ n * StaticSize a, KnownNat o) => Binary (StaticSeq n a) where
  byteSize = byteSizeViaStatic
  get = fmap StaticSeq (getStaticSeq (fromInteger (natVal (Proxy :: Proxy n))) get)
  put = unsafePutStaticSeqN (fromInteger (natVal (Proxy :: Proxy n))) (Just def) put . unStaticSeq

newtype StaticArray (n :: Nat) a = StaticArray {unStaticArray :: LiftedPrimArray a}
  deriving stock (Show)
  deriving newtype (Eq)

instance (KnownNat n, LiftedPrim a, Default a) => Default (StaticArray n a) where
  def = StaticArray (replicateLiftedPrimArray (fromInteger (natVal (Proxy :: Proxy n))) def)

instance (KnownNat n, StaticByteSized a, o ~ n * StaticSize a, KnownNat o) => StaticByteSized (StaticArray n a) where
  type StaticSize (StaticArray n a) = n * StaticSize a

instance (KnownNat n, LiftedPrim a, Default a, o ~ n * StaticSize a, KnownNat o) => Binary (StaticArray n a) where
  byteSize = byteSizeViaStatic
  get = fmap StaticArray (getStaticArray (fromInteger (natVal (Proxy :: Proxy n))))
  put = unsafePutStaticArrayN (fromInteger (natVal (Proxy :: Proxy n))) (Just def) . unStaticArray

newtype BoolByte = BoolByte {unBoolByte :: Bool}
  deriving stock (Show)
  deriving newtype (Eq)

instance Default BoolByte where
  def = BoolByte False

instance StaticByteSized BoolByte where
  type StaticSize BoolByte = 1
  staticByteSize _ = 1

instance Binary BoolByte where
  byteSize = byteSizeViaStatic
  get = fmap (BoolByte . (/= 0)) getWord8
  put (BoolByte b) = putWord8 (if b then 1 else 0)

newtype ExactBytes (n :: Nat) (s :: Symbol) = ExactBytes {unExactBytes :: ()}
  deriving stock (Show)
  deriving newtype (Eq)

instance Default (ExactBytes n s) where
  def = ExactBytes ()

class SymLen (n :: Nat) (s :: Symbol) | s -> n

instance SymLen 0 ""

instance (SymLen n s, m ~ n + 1, t ~ ConsSymbol c s) => SymLen m t

instance (SymLen n s, KnownSymbol s, KnownNat n) => StaticByteSized (ExactBytes n s) where
  type StaticSize (ExactBytes n s) = n

instance (SymLen n s, KnownSymbol s, KnownNat n) => Binary (ExactBytes n s) where
  byteSize = byteSizeViaStatic
  get = do
    let s = symbolVal (Proxy :: Proxy s)
        bc = coerce (length s)
        bs = BSS.pack (fmap c2w s)
    getExpect s (getByteString bc) bs
    pure (ExactBytes ())
  put _ = do
    let s = symbolVal (Proxy :: Proxy s)
    putByteString (BSS.pack (fmap c2w s))
