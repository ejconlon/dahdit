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

import Control.Monad.IO.Class (MonadIO (..))
import Dahdit (Binary, ByteCount (..), GetError, StaticByteSized (..), decodeEnd, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import PropUnit (MonadTest, assert, (===))
import Test.Daytripper (Expect, expectDuring, mkExpect)

type Cmp m a = Maybe a -> Either GetError a -> m ()

expectCodec :: (MonadTest m, MonadIO m, Binary a) => Cmp m a -> Expect m a ByteString (Either GetError a)
expectCodec = mkExpect enc dec
 where
  enc = liftIO . encode
  dec = liftIO . fmap fst . decodeEnd

expectCodecOk :: (MonadTest m, MonadIO m, Binary a, Eq a, Show a) => Expect m a ByteString (Either GetError a)
expectCodecOk = expectCodec (maybe (assert . isRight) (\a x -> x === Right a))

expectCodecErr :: (MonadTest m, MonadIO m, Binary a) => Expect m a ByteString (Either GetError a)
expectCodecErr = expectCodec (const (assert . isLeft))

expectBytes :: (MonadTest m) => [Word8] -> Expect m a ByteString c -> Expect m a ByteString c
expectBytes ws = expectDuring (\_ bs -> bs === BS.pack ws)

expectText :: (MonadTest m) => Text -> Expect m a ByteString c -> Expect m a ByteString c
expectText t = expectDuring (\_ bs -> bs === TE.encodeUtf8 t)

proxyBefore :: Expect m a b c -> Proxy a
proxyBefore _ = Proxy

expectStatic :: (MonadTest m, StaticByteSized a) => Expect m a ByteString c -> Expect m a ByteString c
expectStatic ex =
  let len = unByteCount (staticByteSize (proxyBefore ex))
  in  expectDuring (\_ bs -> BS.length bs === len) ex
