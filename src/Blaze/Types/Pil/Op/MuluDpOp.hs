module Blaze.Types.Pil.Op.MuluDpOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data MuluDpOp expr = MuluDpOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (MuluDpOp a)
