module Blaze.Types.Pil.Op.ConstOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data ConstOp expr = ConstOp
  { constant :: Int64
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (ConstOp a)
