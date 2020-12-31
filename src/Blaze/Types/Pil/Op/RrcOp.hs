module Blaze.Types.Pil.Op.RrcOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data RrcOp expr = RrcOp
  { left :: expr
  , right :: expr
  , carry :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (RrcOp a)
