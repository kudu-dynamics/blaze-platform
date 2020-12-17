module Blaze.Types.Pil.Op.ConstPtrOp where

-- This module is generated. Please use gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data ConstPtrOp expr = ConstPtrOp
  { constant :: Int64
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (ConstPtrOp a)
