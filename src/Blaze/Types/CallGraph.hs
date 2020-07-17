{-# LANGUAGE TemplateHaskell #-}

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
      { _functionSymbol :: Maybe Symbol,
        _functionName :: Text,
        _functionAddress :: Address
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Function

data CallSite
  = CallSite
      { _callSiteCaller :: Function,
        _callSiteAddress :: Address,
        _callSiteDest :: CallDest
      }
  deriving (Eq, Ord, Show)

newtype CallDest
  = DestFunc Function
  deriving (Eq, Ord, Show)

$(makeFields ''Function)
$(makeFields ''CallSite)
