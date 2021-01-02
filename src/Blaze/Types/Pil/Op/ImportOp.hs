{- HLINT ignore "Use newtype instead of data" -}
module Blaze.Types.Pil.Op.ImportOp where

-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify.

import Blaze.Prelude

data ImportOp expr = ImportOp
  { constant :: Int64
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

instance Hashable a => Hashable (ImportOp a)
