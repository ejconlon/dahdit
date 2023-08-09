module Test.Dahdit.Daytripper
  ( Cmp
  , expectCodec
  , expectCodecOk
  , expectCodecErr
  , expectBytes
  , expectText
  , expectStatic
  )
where

import Dahdit (Binary, ByteCount (..), GetError, StaticByteSized (..), decodeEnd, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Test.Daytripper (Expect, MonadExpect (..), expectDuring, mkExpect)

type Cmp m a = Maybe a -> Either GetError a -> m ()

expectCodec :: (MonadExpect m, Binary a) => Cmp m a -> Expect m a ByteString (Either GetError a)
expectCodec = mkExpect enc dec
 where
  enc = expectLiftIO . encode
  dec = expectLiftIO . fmap fst . decodeEnd

expectCodecOk :: (MonadExpect m, Binary a, Eq a, Show a) => Expect m a ByteString (Either GetError a)
expectCodecOk = expectCodec (maybe (expectAssertBool "expected ok" . isRight) (\a x -> expectAssertEq x (Right a)))

expectCodecErr :: (MonadExpect m, Binary a) => Expect m a ByteString (Either GetError a)
expectCodecErr = expectCodec (const (expectAssertBool "expected error" . isLeft))

expectBytes :: (MonadExpect m) => [Word8] -> Expect m a ByteString c -> Expect m a ByteString c
expectBytes ws = expectDuring (\_ bs -> expectAssertEq bs (BS.pack ws))

expectText :: (MonadExpect m) => Text -> Expect m a ByteString c -> Expect m a ByteString c
expectText t = expectDuring (\_ bs -> expectAssertEq bs (TE.encodeUtf8 t))

proxyBefore :: Expect m a b c -> Proxy a
proxyBefore _ = Proxy

expectStatic :: (MonadExpect m, StaticByteSized a) => Expect m a ByteString c -> Expect m a ByteString c
expectStatic ex =
  let len = unByteCount (staticByteSize (proxyBefore ex))
  in  expectDuring (\_ bs -> expectAssertEq (BS.length bs) len) ex
