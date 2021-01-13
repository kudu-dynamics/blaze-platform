module Blaze.Types.Pil.Op.SbbOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data SbbOp expr = SbbOp
  { left :: expr
  , right :: expr
  , carry :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (SbbOp a)
