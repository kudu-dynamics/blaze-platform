module Blaze.Types.CallGraph where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Function (Func, Function)


-- TODO: Consider adding information about call sites as edge metadata
type CallGraph = AlgaGraph () Int Func

-- | A callsite exists in an internal function and can call an internal or extern function
data CallSite
  = CallSite
      { caller :: Function
      , address :: Address
      , dest :: Func
      }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
