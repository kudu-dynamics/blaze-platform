module Blaze.Types.CallGraph where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Function (FuncRef, FunctionRef)

import qualified Data.HashSet as HashSet


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
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Serialize)

-- | Serializable transport type for CallGraph.
data CallGraphTransport = CallGraphTransport
  { nodes :: [FuncRef]
  , edges :: [(FuncRef, FuncRef)]
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Serialize)

toCallGraphTransport :: CallGraph -> CallGraphTransport
toCallGraphTransport cg = CallGraphTransport
  { nodes = HashSet.toList $ G.nodes cg
  , edges = snd . G.toTupleLEdge <$> G.edges cg
  }

fromCallGraphTransport :: CallGraphTransport -> CallGraph
fromCallGraphTransport t =
  G.addNodes (t ^. #nodes)
    . G.fromEdges
    . fmap (\e -> G.fromTupleLEdge ((), e))
    $ t ^. #edges
