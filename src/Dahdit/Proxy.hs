module Dahdit.Proxy
  ( proxyFor
  , proxyForF
  , proxyForFun
  , proxyForNatF
  ) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat)

proxyFor :: a -> Proxy a
proxyFor _ = Proxy

proxyForF :: f a -> Proxy a
proxyForF _ = Proxy

proxyForFun :: (a -> x) -> Proxy a
proxyForFun _ = Proxy

proxyForNatF :: forall (n :: Nat) f. f n -> Proxy n
proxyForNatF _ = Proxy
