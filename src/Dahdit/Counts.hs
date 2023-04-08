module Dahdit.Counts
  ( ByteCount (..)
  , ElemCount (..)
  )
where

import Data.Default (Default)

newtype ByteCount = ByteCount {unByteCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)

newtype ElemCount = ElemCount {unElemCount :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral, Default)
