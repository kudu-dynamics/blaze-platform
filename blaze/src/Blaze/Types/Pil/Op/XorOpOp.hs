module Blaze.Types.Pil.Op.XorOpOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data XorOpOp expr = XorOpOp
  { _left :: expr
  , _right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (XorOpOp a)
