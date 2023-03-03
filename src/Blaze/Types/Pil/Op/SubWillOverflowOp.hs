module Blaze.Types.Pil.Op.SubWillOverflowOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data SubWillOverflowOp expr = SubWillOverflowOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (SubWillOverflowOp a)
