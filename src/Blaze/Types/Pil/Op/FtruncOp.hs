module Blaze.Types.Pil.Op.FtruncOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data FtruncOp expr = FtruncOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (FtruncOp a)
