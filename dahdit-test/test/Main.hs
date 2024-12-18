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
import PropUnit (Gen, GenDefault (..), genDefaultGeneric, testGroup)
import Test.Dahdit.Daytripper (expectBytes, expectCodecOk, expectText)
import Test.Dahdit.GenDefault (DahditTag)
import Test.Daytripper (daytripperMain, mkFileRT, mkPropRT, mkUnitRT, testRT)

data P

type I = DahditTag P

genDefaultI :: (GenDefault I a) => Gen a
genDefaultI = genDefault (Proxy @I)

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric DynFoo)

instance GenDefault I DynFoo where
  genDefault = genDefaultGeneric

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)

instance GenDefault I StaFoo where
  genDefault = genDefaultGeneric

main :: IO ()
main =
  daytripperMain $ \lim ->
    testGroup "DahditTest" $
      fmap
        (testRT (Just lim))
        [ mkPropRT "DynFoo prop" expectCodecOk (genDefaultI @DynFoo)
        , mkUnitRT "DynFoo unit" (expectBytes [1, 2, 0] expectCodecOk) (DynFoo 1 2)
        , mkFileRT "DynFoo file" expectCodecOk "testdata/dynfoo.bin" (Just (DynFoo 1 2))
        , mkPropRT "StaFoo prop" expectCodecOk (genDefaultI @StaFoo)
        , mkUnitRT "StaFoo unit" (expectText "\ETX\EOT\NUL" expectCodecOk) (StaFoo 3 4)
        , mkFileRT "DynFoo file" expectCodecOk "testdata/stafoo.bin" (Just (StaFoo 1 2))
        ]
