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

import Dahdit.LiftedPrim (LiftedPrim, LiftedPrimArray, replicateLiftedPrimArray)
import Dahdit.Proxy (proxyForNatF)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Default (Default (..))
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import GHC.TypeLits (KnownNat, Nat, Symbol, natVal)

-- | Bytes terminated with null byte.
-- NOTE: Terminated with TWO null bytes if the string is even length
-- to align to Word16 boundaries, as required for RIFF format, for example.
newtype TermBytes = TermBytes {unTermBytes :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance Default TermBytes where
  def = TermBytes BSS.empty

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

newtype StaticSeq (n :: Nat) a = StaticSeq {unStaticSeq :: Seq a}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Functor, Foldable)

instance (KnownNat n, Default a) => Default (StaticSeq n a) where
  def = StaticSeq (Seq.replicate (fromInteger (natVal (Proxy :: Proxy n))) def)

newtype StaticArray (n :: Nat) a = StaticArray {unStaticArray :: LiftedPrimArray a}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance (KnownNat n, LiftedPrim a, Default a) => Default (StaticArray n a) where
  def = StaticArray (replicateLiftedPrimArray (fromInteger (natVal (Proxy :: Proxy n))) def)

newtype BoolByte = BoolByte {unBoolByte :: Bool}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Default BoolByte where
  def = BoolByte False

newtype ExactBytes (s :: Symbol) = ExactBytes {unExactBytes :: ()}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Default (ExactBytes s) where
  def = ExactBytes ()
