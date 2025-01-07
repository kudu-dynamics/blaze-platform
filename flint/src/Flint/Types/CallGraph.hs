module Flint.Types.CallGraph where

import Flint.Prelude

import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Function as Blaze

import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


data Function
  = Internal Blaze.Function
  | External Pil.Symbol
  deriving (Eq, Ord, Show, Generic, Hashable)

data FuncId
  = InternalId Address
  | ExternalId Pil.Symbol
  deriving (Eq, Ord, Show, Generic, Hashable)

instance G.Identifiable Function FuncId where
  getNodeId func = G.NodeId funcId
    where
      funcId :: FuncId
      funcId = case func of
        Internal bfunc -> InternalId $ bfunc ^. #address
        -- TODO: There could be a , eh
        External x -> ExternalId x

type CallGraph = AlgaGraph () FuncId Function
type CallEdge = (Function, Function)

type CallNodeMap = HashMap Blaze.Function (Cfg.CallNode [Pil.Stmt])

-- | Keys are callers and values are callees
type CallsToMap = HashMap Blaze.Function (HashSet Function)


fromCallsToMap :: CallsToMap -> CallGraph
fromCallsToMap = G.fromEdges
  . concatMap getEdges
  . HashMap.toList
  where
    getEdges :: (Blaze.Function, HashSet Function) -> [G.LEdge () Function]
    getEdges (bfunc, s) = do
      dest <- HashSet.toList s
      return . G.LEdge () $ G.Edge (Internal bfunc) dest
