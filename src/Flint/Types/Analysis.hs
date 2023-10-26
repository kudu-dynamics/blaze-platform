module Flint.Types.Analysis
  ( module Flint.Types.Analysis
  ) where

import Flint.Prelude
import Blaze.Types.Pil (PilVar, Expression)

type Symbol = Text

type AnalysisCtx = ()

type BndbFilePath = FilePath

-- | Description of an abstract "parameter" that a taint propagator reads from
-- or writes to
data Parameter
  -- | The index of a function parameter (starting at 0)
  = Parameter Int
  -- | The function return parameter
  | ReturnParameter
  -- | Some other (abstract) location. E.g. this is used to model NVRAM storage
  | Other Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Description of an operation which propagates taint from one of its
-- parameters to another of its parameters
data TaintPropagator
  -- | A specific function which, when called, propagates taint from 'from' to
  -- 'to'
  = FunctionCallPropagator
    -- | The name of the function. TODO this needs to be extended to recognize
    -- indirect calls, e.g. UEFI protocol functions
    { function :: Symbol
    -- | Source of taint
    , from :: Parameter
    -- | Destination of taint
    , to :: Parameter
    }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data Taint
  = Tainted
    { src :: Expression
    , dst :: Either PilVar Expression
    }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)
