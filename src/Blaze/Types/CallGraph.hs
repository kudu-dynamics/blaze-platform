module Blaze.Types.CallGraph where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Data.BinaryAnalysis (Symbol)

-- TODO: Consider adding information about call sites as edge metadata
type CallGraph = AlgaGraph () Function
type CallEdge = (Function, Function)

-- TODO: Consider moving Function to Data.BinaryAnalysis
data Function
  = Function
      { symbol :: Maybe Symbol,
        name :: Text,
        address :: Address
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)


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

