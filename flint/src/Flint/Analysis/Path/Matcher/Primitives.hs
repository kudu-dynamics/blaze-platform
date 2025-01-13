module Flint.Analysis.Path.Matcher.Primitives where

import Flint.Prelude hiding (Sym)

data type LocationLabel = Text

-- | The type of primitive and names for input and output vars.
-- These are any vars both input and output, that will be
-- bound to args/globals/ret-vars/immediates during a concrete pattern match
data PrimType = PrimType
  { name :: Text
  , vars :: HashSet Sym -- we might also want to include PilTypes at some point
  , locationLabels :: HashSet LocationLabel -- important locations in the primitive
  }

