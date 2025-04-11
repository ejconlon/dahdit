{-# OPTIONS_GHC -Wno-orphans #-}

-- | Exists to give orphan instances to 24-bit integral types.
module Dahdit.ShortWord () where

import Data.Default (Default (..))
import Data.Primitive (Prim (..))
import Data.ShortWord (Int24, Word24)
import System.ByteOrder (Bytes (..))

instance Default Word24 where
  def = 0

instance Default Int24 where
  def = 0

instance Bytes Word24

instance Bytes Int24

instance Prim Word24

instance Prim Int24
