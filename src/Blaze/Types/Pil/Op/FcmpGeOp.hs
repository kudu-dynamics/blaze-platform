module Blaze.Types.Pil.Op.FcmpGeOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data FcmpGeOp expr = FcmpGeOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (FcmpGeOp a)
