module Dahdit.Audio.Binary
  ( QuietByteArray (..)
  , QuietPrimArray (..)
  )
where

import Data.Default (Default (..))
import Data.Primitive (Prim)
import Data.Primitive.ByteArray (ByteArray, emptyByteArray, sizeofByteArray)
import Data.Primitive.PrimArray (PrimArray, emptyPrimArray, sizeofPrimArray)

newtype QuietByteArray = QuietByteArray {unQuietByteArray :: ByteArray}
  deriving newtype (Eq, Ord)

instance Show QuietByteArray where
  show (QuietByteArray arr) = "QuietByteArray{" ++ show (sizeofByteArray arr) ++ "}"

instance Default QuietByteArray where
  def = QuietByteArray emptyByteArray

newtype QuietPrimArray a = QuietPrimArray {unQuietPrimArray :: PrimArray a}
  deriving newtype (Eq, Ord)

instance (Prim a) => Show (QuietPrimArray a) where
  show (QuietPrimArray arr) = "QuietPrimArray{" ++ show (sizeofPrimArray arr) ++ "}"

instance Default (QuietPrimArray a) where
  def = QuietPrimArray emptyPrimArray
