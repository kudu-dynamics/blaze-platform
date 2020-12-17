module Blaze.Types.Pil.Op.RlcOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data RlcOp expr = RlcOp
  { left :: expr
  , right :: expr
  , carry :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (RlcOp a)
