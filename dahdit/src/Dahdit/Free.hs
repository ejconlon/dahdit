module Dahdit.Free
  ( GetStaticSeqF (..)
  , GetStaticArrayF (..)
  , GetLookAheadF (..)
  , GetScopeF (..)
  , ScopeMode (..)
  , GetF (..)
  , Get (..)
  , PutStaticSeqF (..)
  , PutStaticArrayF (..)
  , PutStaticHintF (..)
  , PutF (..)
  , PutM (..)
  , Put
  )
where

import Control.Monad.Free.Church (F (..))
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
import Dahdit.Sizes (ByteCount, ElemCount, StaticByteSized (..))
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int8)
import Data.Primitive (Prim)
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray.Unaligned (PrimUnaligned)
import Data.Primitive.PrimArray (PrimArray)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

data GetStaticSeqF a where
  GetStaticSeqF :: (StaticByteSized z) => !ElemCount -> Get z -> (Seq z -> a) -> GetStaticSeqF a

instance Functor GetStaticSeqF where
  fmap f (GetStaticSeqF n g k) = GetStaticSeqF n g (f . k)

data GetStaticArrayF a where
  GetStaticArrayF :: (Prim z, PrimUnaligned z) => !ElemCount -> Proxy z -> (PrimArray z -> a) -> GetStaticArrayF a

instance Functor GetStaticArrayF where
  fmap f (GetStaticArrayF n p k) = GetStaticArrayF n p (f . k)

data GetLookAheadF a where
  GetLookAheadF :: Get z -> (z -> a) -> GetLookAheadF a

instance Functor GetLookAheadF where
  fmap f (GetLookAheadF g k) = GetLookAheadF g (f . k)

data GetScopeF a where
  GetScopeF :: !ScopeMode -> !ByteCount -> Get z -> (z -> a) -> GetScopeF a

instance Functor GetScopeF where
  fmap f (GetScopeF sm bc g k) = GetScopeF sm bc g (f . k)

data ScopeMode
  = ScopeModeExact
  | ScopeModeWithin
  deriving stock (Eq, Show)

data GetF a
  = GetFWord8 (Word8 -> a)
  | GetFInt8 (Int8 -> a)
  | GetFWord16LE (Word16LE -> a)
  | GetFInt16LE (Int16LE -> a)
  | GetFWord24LE (Word24LE -> a)
  | GetFInt24LE (Int24LE -> a)
  | GetFWord32LE (Word32LE -> a)
  | GetFInt32LE (Int32LE -> a)
  | GetFWord64LE (Word64LE -> a)
  | GetFInt64LE (Int64LE -> a)
  | GetFFloatLE (FloatLE -> a)
  | GetFDoubleLE (DoubleLE -> a)
  | GetFWord16BE (Word16BE -> a)
  | GetFInt16BE (Int16BE -> a)
  | GetFWord24BE (Word24BE -> a)
  | GetFInt24BE (Int24BE -> a)
  | GetFWord32BE (Word32BE -> a)
  | GetFInt32BE (Int32BE -> a)
  | GetFWord64BE (Word64BE -> a)
  | GetFInt64BE (Int64BE -> a)
  | GetFFloatBE (FloatBE -> a)
  | GetFDoubleBE (DoubleBE -> a)
  | GetFShortByteString !ByteCount (ShortByteString -> a)
  | GetFStaticSeq !(GetStaticSeqF a)
  | GetFStaticArray !(GetStaticArrayF a)
  | GetFByteArray !ByteCount (ByteArray -> a)
  | GetFScope !(GetScopeF a)
  | GetFSkip !ByteCount a
  | GetFLookAhead !(GetLookAheadF a)
  | GetFRemainingSize (ByteCount -> a)
  | GetFFail !Text
  deriving stock (Functor)

newtype Get a = Get {unGet :: F GetF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Get where
  fail msg = Get (F (\_ y -> y (GetFFail (T.pack msg))))

data PutStaticSeqF a where
  PutStaticSeqF :: (StaticByteSized z) => !ElemCount -> !(Maybe z) -> (z -> Put) -> !(Seq z) -> a -> PutStaticSeqF a

instance Functor PutStaticSeqF where
  fmap f (PutStaticSeqF n z p s k) = PutStaticSeqF n z p s (f k)

data PutStaticArrayF a where
  PutStaticArrayF :: (Prim z, PrimUnaligned z) => !ElemCount -> !(Maybe z) -> !(PrimArray z) -> a -> PutStaticArrayF a

instance Functor PutStaticArrayF where
  fmap f (PutStaticArrayF n z a k) = PutStaticArrayF n z a (f k)

data PutStaticHintF a where
  PutStaticHintF :: !ByteCount -> Put -> a -> PutStaticHintF a

instance Functor PutStaticHintF where
  fmap f (PutStaticHintF n p k) = PutStaticHintF n p (f k)

data PutF a
  = PutFWord8 !Word8 a
  | PutFInt8 !Int8 a
  | PutFWord16LE !Word16LE a
  | PutFInt16LE !Int16LE a
  | PutFWord24LE !Word24LE a
  | PutFInt24LE !Int24LE a
  | PutFWord32LE !Word32LE a
  | PutFInt32LE !Int32LE a
  | PutFWord64LE !Word64LE a
  | PutFInt64LE !Int64LE a
  | PutFFloatLE !FloatLE a
  | PutFDoubleLE !DoubleLE a
  | PutFWord16BE !Word16BE a
  | PutFInt16BE !Int16BE a
  | PutFWord24BE !Word24BE a
  | PutFInt24BE !Int24BE a
  | PutFWord32BE !Word32BE a
  | PutFWord64BE !Word64BE a
  | PutFInt64BE !Int64BE a
  | PutFInt32BE !Int32BE a
  | PutFFloatBE !FloatBE a
  | PutFDoubleBE !DoubleBE a
  | PutFShortByteString !ByteCount !ShortByteString a
  | PutFStaticSeq !(PutStaticSeqF a)
  | PutFStaticArray !(PutStaticArrayF a)
  | PutFByteArray !ByteCount !ByteArray a
  | PutFStaticHint !(PutStaticHintF a)
  deriving stock (Functor)

newtype PutM a = PutM {unPutM :: F PutF a}
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup (PutM ()) where
  p <> q = p *> q

instance Monoid (PutM ()) where
  mappend = (<>)
  mempty = pure ()

type Put = PutM ()
