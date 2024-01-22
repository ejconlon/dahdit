module Dahdit.Midi.OscAddr
  ( RawAddrPat (..)
  )
where

import Control.Exception (Exception)
import Dahdit (Binary (..), ByteCount (..), putText)
import Dahdit.Midi.Binary (getTermText, putTermText)
import Dahdit.Midi.Pad (byteSizePad32, getPad32, putPad32)
import Data.ByteString.Internal (c2w)
import Data.Foldable (foldMap', for_, toList)
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)

slashByte :: Word8
slashByte = c2w '/'

newtype Addr = Addr {unAddr :: Seq Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString Addr where
  fromString s =
    let t = T.pack s
    in  case parseAddr t of
          Left e -> error ("Invalid address " ++ show s ++ " : " ++ show e)
          Right a -> a

addrSizer :: Addr -> ByteCount
addrSizer (Addr parts) =
  ByteCount (Seq.length parts + getSum (foldMap' (Sum . T.length) parts))

instance Binary Addr where
  byteSize = byteSizePad32 addrSizer
  get = getPad32 $ do
    s <- getTermText
    case parseAddr s of
      Left e -> fail ("Invalid address " ++ show s ++ " : " ++ show e)
      Right a -> pure a
  put = putPad32 addrSizer $ \(Addr parts) -> do
    for_ parts $ \part -> do
      put slashByte
      putText part
    put @Word8 0

isInvalidAddrPartChar :: Char -> Bool
isInvalidAddrPartChar c =
  c == ' '
    || c == '#'
    || c == '*'
    || c == ','
    || c == '/'
    || c == '?'
    || c == '['
    || c == ']'
    || c == '{'
    || c == '}'

data AddrErr = AddrErrPartEmpty | AddrErrInvalidPartChar !Char | AddrErrExpectSlash !Char
  deriving stock (Eq, Ord, Show)

instance Exception AddrErr

parseAddr :: Text -> Either AddrErr Addr
parseAddr = goStart . T.unpack
 where
  goStart = \case
    [] -> Right (Addr Empty)
    c : cs ->
      if c == '/'
        then goRest Empty Empty cs
        else Left (AddrErrExpectSlash c)
  pack = T.pack . toList
  goRest !acc !pacc = \case
    [] ->
      if Seq.null pacc
        then Left AddrErrPartEmpty
        else Right (Addr (acc :|> pack pacc))
    c : cs ->
      if c == '/'
        then
          if Seq.null pacc
            then Left AddrErrPartEmpty
            else goRest (acc :|> pack pacc) Empty cs
        else
          if isInvalidAddrPartChar c
            then Left (AddrErrInvalidPartChar c)
            else goRest acc (pacc :|> c) cs

printAddr :: Addr -> Text
printAddr (Addr xs) =
  if Seq.null xs
    then T.empty
    else T.cons '/' (T.intercalate (T.singleton '/') (toList xs))

data Negate = NegateNo | NegateYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PatFrag
  = PatFragText !Text
  | PatFragAnyMany
  | PatFragAnyOne
  | PatFragChoose !(Seq Text)
  | PatFragRange !Negate !Text !Text
  deriving stock (Eq, Ord, Show)

patFragSizer :: PatFrag -> ByteCount
patFragSizer = \case
  PatFragText t -> ByteCount (T.length t)
  PatFragAnyMany -> 1
  PatFragAnyOne -> 1
  PatFragChoose ts -> ByteCount (1 + Seq.length ts + getSum (foldMap' (Sum . T.length) ts))
  PatFragRange n t1 t2 -> ByteCount (3 + T.length t1 + T.length t2 + if n == NegateNo then 0 else 1)

type PatPart = Seq PatFrag

-- Addr encoding: zero-terminated, aligned to 4-byte boundary
newtype AddrPat = AddrPat {unAddrPat :: Seq PatPart}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString AddrPat where
  fromString s =
    let t = T.pack s
    in  case parseAddrPat t of
          Left e -> error ("Invalid address pattern " ++ show s ++ " : " ++ show e)
          Right a -> a

addrPatSizer :: AddrPat -> ByteCount
addrPatSizer (AddrPat _patParts) = undefined

instance Binary AddrPat where
  byteSize = byteSizePad32 addrPatSizer
  get = getPad32 $ do
    s <- getTermText
    case parseAddrPat s of
      Left e -> fail ("Invalid address pattern " ++ show s ++ " : " ++ show e)
      Right a -> pure a
  put = putPad32 addrPatSizer $ \(AddrPat _patParts) -> error "TODO"

data AddrPatErr = AddrPadErr
  deriving stock (Eq, Ord, Show)

instance Exception AddrPatErr

parseAddrPat :: Text -> Either AddrPatErr AddrPat
parseAddrPat = error "TODO"

printAddrPat :: AddrPat -> Text
printAddrPat = error "TODO"

matchPart :: PatPart -> Text -> Bool
matchPart = error "TODO"

matchAddr :: AddrPat -> Addr -> Bool
matchAddr (AddrPat patParts) (Addr parts) =
  (Seq.length patParts == Seq.length parts)
    && and (zipWith matchPart (toList patParts) (toList parts))

newtype RawAddrPat = RawAddrPat {unRawAddrPat :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

rawAddrPatSizer :: RawAddrPat -> ByteCount
rawAddrPatSizer = ByteCount . succ . T.length . unRawAddrPat

instance Binary RawAddrPat where
  byteSize = byteSizePad32 rawAddrPatSizer
  get = getPad32 (fmap RawAddrPat getTermText)
  put = putPad32 rawAddrPatSizer (putTermText . unRawAddrPat)
