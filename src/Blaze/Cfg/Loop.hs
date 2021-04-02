module Blaze.Cfg.Loop
  ( module Blaze.Cfg.Loop,
    module Exports,
  )
where

import Blaze.Cfg (getDominators)
import Blaze.Prelude
import Blaze.Types.Cfg
  ( CfEdge,
    CfNode,
    Cfg(root),
    Dominators,
  )
import qualified Blaze.Types.Cfg as Cfg
-- Hiding some names commonly used as variables,
-- re-exporting eplicitly.
import Blaze.Types.Cfg.Loop as Exports hiding
  ( backEdge,
    body,
    cfg,
    edge,
    header,
    tail,
  )
import qualified Blaze.Types.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | Check if an edge is a back edge using graph dominators.
--  We assume the dominators include the nodes referenced in the edge.
--  If that assumption is wrong, isBackEdge defaults to False.
isBackEdge :: (Hashable a, Ord a) => Dominators a -> CfEdge a -> Bool
isBackEdge domMap edge =
  case HM.lookup (edge ^. #src) (coerce domMap) of
    Nothing -> False
    (Just domNodes) -> HS.member (edge ^. #dst) domNodes

getBackEdges :: forall a. (Hashable a, Ord a) => Cfg a -> [BackEdge a]
getBackEdges cfg' =
  [ BackEdge e
    | e <- Cfg.edges cfg',
      isBackEdge doms e
  ]
  where
    doms :: Dominators a
    doms = getDominators cfg'

-- | Find body nodes of loop. If an empty list is returned, the loop
--  only contains a head(er) node and a tail node.
getBodyNodes :: forall a. (Hashable a, Ord a) => Cfg a -> BackEdge a -> HashSet (CfNode a)
getBodyNodes cfg' backEdge =
  HS.delete header
    . HS.delete tail
    . HS.fromList
    . concat
    . G.bfs [tail]
    . G.transpose
    . G.removeNode header
    $ cfg'
  where
    header :: CfNode a
    header = backEdge ^. (#edge . #dst)
    tail :: CfNode a
    tail = backEdge ^. (#edge . #src)

getLoopBody :: forall a. (Hashable a, Ord a) => Cfg a -> BackEdge a -> LoopBody a
getLoopBody cfg' backEdge =
  LoopBody bodyNodes
  where
    bodyNodes :: HashSet (CfNode a)
    bodyNodes = getBodyNodes cfg' backEdge

getLoopCfg :: forall a. (Hashable a, Ord a) => Cfg a -> LoopHeader a -> LoopNodes a -> LoopCfg a
getLoopCfg cfg' header loopNodes =
  LoopCfg $ subCfg_ {root = header ^. #node}
  where
    subCfg_ :: Cfg a
    subCfg_ = G.subgraph (`HS.member` (loopNodes ^. #nodes)) cfg'

fromBackEdge :: forall a b. (Hashable a, Ord a) => Cfg a -> BackEdge a -> NatLoop a b
fromBackEdge cfg' backEdge =
  NatLoop header body tail loopCfg backEdge
  where
    header :: LoopHeader a
    header = LoopHeader $ backEdge ^. (#edge . #dst)
    body :: LoopBody a
    body = getLoopBody cfg' backEdge
    tail :: LoopTail a
    tail = LoopTail $ backEdge ^. (#edge . #src)
    loopNodes :: LoopNodes a
    loopNodes = LoopNodes $ HS.insert (header ^. #node) . HS.insert (tail ^. #node) $ (body ^. #nodes)
    loopCfg :: LoopCfg a
    loopCfg = getLoopCfg cfg' header loopNodes
