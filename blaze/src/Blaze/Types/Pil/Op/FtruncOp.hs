{- HLINT ignore "Use newtype instead of data" -}
module Blaze.Types.Pil.Op.FtruncOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data FtruncOp expr = FtruncOp
  { src :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (FtruncOp a)
