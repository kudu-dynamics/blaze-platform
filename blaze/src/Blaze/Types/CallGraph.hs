module Blaze.Types.CallGraph where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Function (FuncRef, FunctionRef)


-- TODO: Consider adding information about call sites as edge metadata
type CallGraph = AlgaGraph () Int FuncRef

-- | A callsite exists in an internal function and can call an internal or extern function.
-- Uses lightweight marker types — no params needed for call graph topology.
data CallSite
  = CallSite
      { caller :: FunctionRef
      , address :: Address
      , dest :: FuncRef
      }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
