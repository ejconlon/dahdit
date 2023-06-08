module Dahdit.Network
  ( Decoder (..)
  , runDecoder
  , Encoder (..)
  , runEncoder
  , Conn (..)
  , HostPort (..)
  , TcpOpts (..)
  , resolveAddr
  , tcpClientConn
  , withTcpClientConn
  , tcpServerConn
  , udpClientConn
  , withUdpClientConn
  , udpServerConn
  , withUdpServerConn
  )
where

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dahdit (Binary (..), ByteCount (..), Get, GetError, GetIncCb, GetIncRequest (..), Put, getEnd, getTarget, getTargetInc, putTarget)
import Data.Acquire (Acquire, mkAcquire, withAcquire)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Tuple (swap)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB

maxRecv :: Int
maxRecv = 65535

maxQueue :: Int
maxQueue = 1024

sockRecvUntil :: NS.Socket -> IORef ByteString -> Int -> IO ()
sockRecvUntil sock ref len = go
 where
  go = do
    lastBs <- readIORef ref
    unless (BS.length lastBs >= len) $ do
      chunkBs <- NSB.recv sock maxRecv
      modifyIORef' ref (<> chunkBs)
      unless (BS.null chunkBs) go

-- Appropriate for TCP connections (uses 'recv' instead of 'recvFrom')
sockGetIncCb :: NS.Socket -> IO (GetIncCb ByteString IO)
sockGetIncCb sock = do
  ref <- newIORef mempty
  pure $ \(GetIncRequest _ (ByteCount off) (ByteCount len)) -> do
    modifyIORef' ref (BS.drop off)
    sockRecvUntil sock ref len
    nextBs <- readIORef ref
    pure (if BS.length nextBs >= len then Nothing else Just nextBs)

newtype Decoder k = Decoder {unDecoder :: forall a. Get a -> IO (k, Either GetError a)}
  deriving stock (Functor)

runDecoder :: Binary a => Decoder k -> IO (k, Either GetError a)
runDecoder dec = unDecoder dec get

-- | Decodes a stream of packets incrementally for TCP
streamDecoder :: Maybe ByteCount -> NS.Socket -> IO (Decoder ())
streamDecoder mayLim sock = do
  cb <- sockGetIncCb sock
  pure (Decoder (\getter -> fmap (\(res, _, _) -> ((), res)) (getTargetInc mayLim getter cb)))

-- | Completely decodes one packet at a time for UDP server
datagramServerDecoder :: Maybe ByteCount -> NS.Socket -> Decoder NS.SockAddr
datagramServerDecoder mayLim sock =
  let lim = maybe maxRecv unByteCount mayLim
  in  Decoder $ \getter -> do
        (bs, addr) <- NSB.recvFrom sock lim
        (ea, _) <- getTarget (getEnd getter) bs
        pure (addr, ea)

datagramClientDecoder :: Maybe ByteCount -> NS.Socket -> Decoder ()
datagramClientDecoder mayLim sock =
  let lim = maybe maxRecv unByteCount mayLim
  in  Decoder $ \getter -> do
        bs <- NSB.recv sock lim
        (ea, _) <- getTarget (getEnd getter) bs
        pure ((), ea)

newtype Encoder k = Encoder {unEncoder :: k -> Put -> IO ()}

runEncoder :: Binary a => Encoder k -> k -> a -> IO ()
runEncoder enc k = unEncoder enc k . put

streamEncoder :: NS.Socket -> Encoder ()
streamEncoder sock = Encoder (\_ -> putTarget >=> NSB.sendAll sock)

datagramClientEncoder :: NS.Socket -> Encoder ()
datagramClientEncoder sock = Encoder (\_ -> putTarget >=> NSB.sendAll sock)

datagramServerEncoder :: NS.Socket -> Encoder NS.SockAddr
datagramServerEncoder sock = Encoder (\addr -> putTarget >=> flip (NSB.sendAllTo sock) addr)

data Conn k = Conn {connDecoder :: Decoder k, connEncoder :: Encoder k}

data HostPort = HostPort
  { hpHost :: !(Maybe String)
  , hpPort :: !Int
  }
  deriving stock (Eq, Ord, Show)

newtype TcpOpts = TcpOpts {tcoFinTimeoutMs :: Int}
  deriving newtype (Show)
  deriving stock (Eq, Ord)

data SockTy = SockTyTcp | SockTyUdp
  deriving stock (Eq, Ord, Show, Enum, Bounded)

sockTyReal :: SockTy -> NS.SocketType
sockTyReal = \case
  SockTyTcp -> NS.Stream
  SockTyUdp -> NS.Datagram

data Role = RoleServer | RoleClient
  deriving stock (Eq, Ord, Show)

data Target = Target
  { targetHp :: !HostPort
  , targetSockTy :: !SockTy
  , targetRole :: !Role
  }
  deriving stock (Eq, Ord, Show)

resolveAddr :: HostPort -> IO NS.SockAddr
resolveAddr hp@(HostPort host port) = do
  infos <- NS.getAddrInfo Nothing host (Just (show port))
  case infos of
    [] -> fail ("Could not resolve address: " ++ show hp)
    info : _ -> pure (NS.addrAddress info)

targetResolve :: Target -> IO NS.AddrInfo
targetResolve (Target hp@(HostPort host port) sockTy role) = do
  let hints =
        NS.defaultHints
          { NS.addrSocketType = sockTyReal sockTy
          , NS.addrFlags = [NS.AI_PASSIVE | role == RoleServer]
          }
  infos <- NS.getAddrInfo (Just hints) host (Just (show port))
  case infos of
    [] -> fail ("Could not resolve address: " ++ show hp)
    info : _ -> pure info

targetOpen :: Target -> IO (NS.Socket, NS.SockAddr)
targetOpen t = do
  info <- targetResolve t
  sock <- NS.openSocket info
  pure (sock, NS.addrAddress info)

targetConnect :: Target -> IO (NS.Socket, NS.SockAddr)
targetConnect t = do
  p@(sock, addr) <- targetOpen t
  NS.connect sock addr
  pure p

targetBind :: Target -> IO NS.Socket
targetBind t = do
  (sock, addr) <- targetOpen t
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.withFdSocket sock NS.setCloseOnExecIfNeeded
  NS.bind sock addr
  pure sock

tcpClientSock :: HostPort -> TcpOpts -> Acquire (NS.SockAddr, NS.Socket)
tcpClientSock hp (TcpOpts finTo) = mkAcquire acq rel
 where
  acq = fmap swap (targetConnect (Target hp SockTyTcp RoleClient))
  rel (_, sock) = if finTo > 0 then NS.gracefulClose sock finTo else NS.close sock

tcpClientConn :: Maybe ByteCount -> HostPort -> TcpOpts -> Acquire (NS.SockAddr, Conn ())
tcpClientConn mayLim hp to = do
  (addr, sock) <- tcpClientSock hp to
  dec <- liftIO (streamDecoder mayLim sock)
  let enc = streamEncoder sock
  pure (addr, Conn dec enc)

withTcpClientConn :: MonadUnliftIO m => Maybe ByteCount -> HostPort -> TcpOpts -> (NS.SockAddr -> Conn () -> m a) -> m a
withTcpClientConn mayLim hp to = withAcquire (tcpClientConn mayLim hp to) . uncurry

tcpServerSock :: HostPort -> Acquire NS.Socket
tcpServerSock hp = mkAcquire acq rel
 where
  acq = do
    sock <- targetBind (Target hp SockTyTcp RoleServer)
    NS.listen sock maxQueue
    pure sock
  rel = NS.close

tcpServerConn :: Maybe ByteCount -> HostPort -> TcpOpts -> Acquire (Acquire (NS.SockAddr, Conn ()))
tcpServerConn mayLim hp to = do
  srvSock <- tcpServerSock hp
  pure $ do
    (addr, cliSock) <- tcpAcceptSock to srvSock
    dec <- liftIO (streamDecoder mayLim cliSock)
    let enc = streamEncoder cliSock
    pure (addr, Conn dec enc)

tcpAcceptSock :: TcpOpts -> NS.Socket -> Acquire (NS.SockAddr, NS.Socket)
tcpAcceptSock (TcpOpts finTo) servSock = mkAcquire acq rel
 where
  acq = fmap swap (NS.accept servSock)
  rel (_, sock) = if finTo > 0 then NS.gracefulClose sock finTo else NS.close sock

udpClientSock :: HostPort -> Acquire (NS.SockAddr, NS.Socket)
udpClientSock hp = mkAcquire acq rel
 where
  acq = fmap swap (targetConnect (Target hp SockTyUdp RoleClient))
  rel = NS.close . snd

udpClientConn :: Maybe ByteCount -> HostPort -> Acquire (NS.SockAddr, Conn ())
udpClientConn mayLim hp = do
  (addr, sock) <- udpClientSock hp
  let dec = datagramClientDecoder mayLim sock
      enc = datagramClientEncoder sock
  pure (addr, Conn dec enc)

withUdpClientConn :: MonadUnliftIO m => Maybe ByteCount -> HostPort -> (NS.SockAddr -> Conn () -> m a) -> m a
withUdpClientConn mayLim hp = withAcquire (udpClientConn mayLim hp) . uncurry

udpServerSock :: HostPort -> Acquire NS.Socket
udpServerSock hp = mkAcquire acq rel
 where
  acq = targetBind (Target hp SockTyUdp RoleServer)
  rel = NS.close

udpServerConn :: Maybe ByteCount -> HostPort -> Acquire (Conn NS.SockAddr)
udpServerConn mayLim hp = do
  sock <- udpServerSock hp
  let dec = datagramServerDecoder mayLim sock
      enc = datagramServerEncoder sock
  pure (Conn dec enc)

withUdpServerConn :: MonadUnliftIO m => Maybe ByteCount -> HostPort -> (Conn NS.SockAddr -> m a) -> m a
withUdpServerConn mayLim hp = withAcquire (udpServerConn mayLim hp)
