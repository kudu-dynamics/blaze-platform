{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

--- taken from https://stackoverflow.com/questions/11887666/using-standard-haskell-generics-libraries-for-typed-type-isomorphisms

module Blaze.Util.GenericConv
  ( GConv
  , gconv
  ) where

import Prelude
import GHC.Generics

gconv
  :: (Generic a, Generic b, GConv (Rep a) (Rep b))
  => a -> b
gconv = to . cv . from

class GConv a b where
  cv :: a x -> b x

-- skip irrelevant parts: datatype name, constructor name, selector
instance GConv f1 f2 => GConv (M1 i1 c1 f1) (M1 i2 c2 f2) where
  cv = M1 . cv . unM1

instance (GConv a1 a2, GConv b1 b2) => GConv (a1 :*: b1) (a2 :*: b2) where
  cv ~(a :*: b) = cv a :*: cv b

instance (GConv a1 a2, GConv b1 b2) => GConv (a1 :+: b1) (a2 :+: b2) where
  cv (L1 a) = L1 $ cv a
  cv (R1 b) = R1 $ cv b

-- copy values
instance GConv U1 U1 where cv = id
instance GConv (K1 R c) (K1 R c) where cv = id
