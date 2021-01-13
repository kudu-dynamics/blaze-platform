module Blaze.Types.Pil.Op.AdcOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data AdcOp expr = AdcOp
  { left :: expr
  , right :: expr
  , carry :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (AdcOp a)
