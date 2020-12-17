module Blaze.Types.Pil.Op.CmpUgtOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data CmpUgtOp expr = CmpUgtOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (CmpUgtOp a)
