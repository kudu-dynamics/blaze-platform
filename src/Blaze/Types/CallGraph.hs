module Blaze.Types.CallGraph where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Function (Function)

-- TODO: Consider adding information about call sites as edge metadata
type CallGraph = AlgaGraph () Function
type CallEdge = (Function, Function)

data CallSite
  = CallSite
      { caller :: Function,
        address :: Address,
        dest :: CallDest
      }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype CallDest
  = DestFunc Function
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

