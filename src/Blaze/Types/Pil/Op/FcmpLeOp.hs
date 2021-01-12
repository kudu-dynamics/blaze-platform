module Blaze.Types.Pil.Op.FcmpLeOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data FcmpLeOp expr = FcmpLeOp
  { left :: expr
  , right :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (FcmpLeOp a)
