{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Dahdit (Binary, StaticByteSized (..), ViaGeneric (..), ViaStaticGeneric (..), Word16LE)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.Dahdit.Arb (Arb (..), ArbGeneric (..), DahditIdx)
import Test.Dahdit.Daytripper (expectBytes, expectCodec, expectText)
import Test.Daytripper (daytripperMain, mkFileRT, mkPropRT, mkUnitRT, testRT)
import Test.Falsify.Generator (Gen)
import Test.Tasty (testGroup)

data P

type I = DahditIdx P

proxyI :: Proxy a -> Proxy (I, a)
proxyI _ = Proxy

arbI :: Arb I a => Proxy a -> Gen a
arbI = arb (Proxy @I)

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric DynFoo)
  deriving (Arb I) via (ArbGeneric I DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)
  deriving (Arb I) via (ArbGeneric I StaFoo)

main :: IO ()
main =
  daytripperMain $
    testGroup "DahditTest" $
      fmap
        testRT
        [ mkPropRT "DynFoo prop" expectCodec (arbI (Proxy @DynFoo))
        , mkUnitRT "DynFoo unit" (expectBytes [1, 2, 0] expectCodec) (DynFoo 1 2)
        , mkFileRT "DynFoo file" expectCodec "testdata/dynfoo.bin" (Just (DynFoo 1 2))
        , mkPropRT "StaFoo prop" expectCodec (arbI (Proxy @StaFoo))
        , mkUnitRT "StaFoo unit" (expectText "\ETX\EOT\NUL" expectCodec) (StaFoo 3 4)
        , mkFileRT "DynFoo file" expectCodec "testdata/stafoo.bin" (Just (StaFoo 1 2))
        ]
