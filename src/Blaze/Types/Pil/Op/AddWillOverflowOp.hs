module Blaze.Types.Pil.Op.AddWillOverflowOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data AddWillOverflowOp expr = AddWillOverflowOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (AddWillOverflowOp a)
