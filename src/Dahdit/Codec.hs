-- Experimental - bidirectional codecs
-- See https://hackage.haskell.org/package/codec
-- And https://blog.poisson.chat/posts/2016-10-12-bidirectional-serialization.html
module Dahdit.Codec
  ( Codec
  , build
  , binary
  , parse
  , produce
  , bindPair
  , bindTag
  , HasCodec (..)
  , ViaBinary (..)
  , ViaCodec (..)
  )
where

import Dahdit.Binary (Binary (..))
import Dahdit.Fancy (BoolByte, ExactBytes, StaticArray, StaticSeq, TermBytes)
import Dahdit.Free (Get, Put)
import Dahdit.LiftedPrim (LiftedPrim)
import Dahdit.Nums (FloatBE, FloatLE, Int16BE, Int16LE, Int24BE, Int24LE, Int32BE, Int32LE, Word16BE, Word16LE, Word24BE, Word24LE, Word32BE, Word32LE)
import Dahdit.Sizes (ByteSized (..), StaticByteSized)
import Data.Coerce (coerce)
import Data.Default (Default)
import Data.Int (Int8)
import Data.Word (Word8)
import GHC.TypeLits (KnownNat, KnownSymbol)

data Codec' x a = Codec'
  { parse' :: Get a
  , produce' :: x -> Put
  }

instance Functor (Codec' x) where
  fmap f c = c {parse' = fmap f (parse' c)}

instance Applicative (Codec' x) where
  pure a = Codec' (pure a) (const (pure ()))

  f <*> a =
    Codec'
      { parse' = parse' f <*> parse' a
      , produce' = \x -> produce' f x *> produce' a x
      }

type Codec a = Codec' a a

build :: Get a -> (a -> Put) -> Codec a
build = Codec'

binary :: Binary a => Codec a
binary = build get put

parse :: Codec a -> Get a
parse = parse'

produce :: Codec a -> a -> Put
produce = produce'

bindPair :: Codec a -> (a -> Codec b) -> Codec (a, b)
bindPair c f =
  Codec'
    (parse c >>= \a -> fmap (a,) (parse (f a)))
    (\(a, b) -> produce c a *> produce (f a) b)

bindTag :: (b -> a) -> Codec a -> (a -> Codec b) -> Codec b
bindTag t c f =
  Codec'
    (parse c >>= parse . f)
    (\b -> let a = t b in produce c a *> produce (f a) b)

class HasCodec a where
  codec :: Codec a

newtype ViaBinary a = ViaBinary {unViaBinary :: a}

instance Binary a => HasCodec (ViaBinary a) where
  codec = coerce (binary @a)

deriving via (ViaBinary Word8) instance HasCodec Word8

deriving via (ViaBinary Int8) instance HasCodec Int8

deriving via (ViaBinary Word16LE) instance HasCodec Word16LE

deriving via (ViaBinary Int16LE) instance HasCodec Int16LE

deriving via (ViaBinary Word24LE) instance HasCodec Word24LE

deriving via (ViaBinary Int24LE) instance HasCodec Int24LE

deriving via (ViaBinary Word32LE) instance HasCodec Word32LE

deriving via (ViaBinary Int32LE) instance HasCodec Int32LE

deriving via (ViaBinary FloatLE) instance HasCodec FloatLE

deriving via (ViaBinary Word16BE) instance HasCodec Word16BE

deriving via (ViaBinary Int16BE) instance HasCodec Int16BE

deriving via (ViaBinary Word24BE) instance HasCodec Word24BE

deriving via (ViaBinary Int24BE) instance HasCodec Int24BE

deriving via (ViaBinary Word32BE) instance HasCodec Word32BE

deriving via (ViaBinary Int32BE) instance HasCodec Int32BE

deriving via (ViaBinary FloatBE) instance HasCodec FloatBE

deriving via (ViaBinary TermBytes) instance HasCodec TermBytes

deriving via (ViaBinary (StaticSeq n a)) instance (KnownNat n, Binary a, StaticByteSized a, Default a) => HasCodec (StaticSeq n a)

deriving via (ViaBinary (StaticArray n a)) instance (KnownNat n, LiftedPrim a, StaticByteSized a, Default a) => HasCodec (StaticArray n a)

deriving via (ViaBinary BoolByte) instance HasCodec BoolByte

deriving via (ViaBinary (ExactBytes s)) instance KnownSymbol s => HasCodec (ExactBytes s)

newtype ViaCodec a = ViaCodec {unViaCodec :: a}

instance ByteSized a => ByteSized (ViaCodec a) where
  byteSize = byteSize . unViaCodec

instance (ByteSized a, HasCodec a) => Binary (ViaCodec a) where
  get = coerce (parse (codec @a))
  put = coerce (produce (codec @a))
