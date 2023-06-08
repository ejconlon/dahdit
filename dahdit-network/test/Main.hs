module Main
  ( main
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (modifyTVar, newTVarIO, readTVarIO)
import Dahdit (Binary, Generic, ViaGeneric (..))
import Dahdit.Network (Conn (..), HostPort (..), runDecoder, runEncoder, withUdpClientConn, withUdpServerConn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data Foo = Foo !Int !Bool
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Binary) via (ViaGeneric Foo)

data Bar = Bar !Char !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Binary) via (ViaGeneric Bar)

testUdp :: TestTree
testUdp = testCase "udp" $ do
  let someFoo = Foo 42 True
      someBar = Bar 'x' 42
  let lim = Just 1024
      hp = HostPort (Just "127.0.0.1") 58764
  readyVar <- newEmptyTMVarIO
  finishVar <- newTVarIO (0 :: Int)
  sx <- async $ withUdpServerConn lim hp $ \(Conn dec enc) -> do
    atomically (putTMVar readyVar ())
    (addr, gotFoo) <- runDecoder dec
    gotFoo @?= Right someFoo
    runEncoder enc addr someBar
    atomically (modifyTVar finishVar (+ 1))
  cx <- async $ withUdpClientConn lim hp $ \_ (Conn dec enc) -> do
    atomically (takeTMVar readyVar)
    runEncoder enc () someFoo
    ((), gotBar) <- runDecoder dec
    gotBar @?= Right someBar
    atomically (modifyTVar finishVar (+ 1))
  wait cx
  wait sx
  numFinished <- readTVarIO finishVar
  numFinished @?= 2

main :: IO ()
main =
  defaultMain $
    testGroup
      "DahditNetwork"
      [ testUdp
      ]
