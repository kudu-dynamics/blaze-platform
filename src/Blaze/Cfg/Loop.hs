module Blaze.Cfg.Loop
  ( module Blaze.Cfg.Loop,
    module Exports,
  )
where

import Blaze.Cfg (getDominators)
import Blaze.Prelude
import Blaze.Types.Cfg
  ( BranchType,
    CfEdge (CfEdge),
    CfNode,
    Cfg(root),
    Dominators,
  )
import qualified Blaze.Types.Cfg as TCfg
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
isBackEdge :: Dominators -> CfEdge -> Bool
isBackEdge domMap edge =
  case HM.lookup (edge ^. #src) (coerce domMap) of
    Nothing -> False
    (Just domNodes) -> HS.member (edge ^. #dst) domNodes

fromGraphEdge :: (BranchType, (CfNode, CfNode)) -> CfEdge
fromGraphEdge (bType, (srcNode, dstNode)) = CfEdge srcNode dstNode bType

getBackEdges :: Cfg a -> [BackEdge]
getBackEdges cfg' =
  [ BackEdge e
    | e <- fromGraphEdge <$> G.edges cfg',
      isBackEdge doms e
  ]
  where
    doms :: Dominators
    doms = getDominators cfg'

-- | Find body nodes of loop. If an empty list is returned, the loop
--  only contains a head(er) node and a tail node.
getBodyNodes :: Cfg a -> BackEdge -> HashSet CfNode
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
    header :: CfNode
    header = backEdge ^. (#edge . #dst)
    tail :: CfNode
    tail = backEdge ^. (#edge . #src)

getLoopBody :: Cfg a -> BackEdge -> LoopBody
getLoopBody cfg' backEdge =
  LoopBody bodyNodes
  where
    bodyNodes :: HashSet CfNode
    bodyNodes = getBodyNodes cfg' backEdge

getLoopCfg :: forall a. Cfg a -> LoopHeader -> LoopNodes -> LoopCfg a
getLoopCfg cfg' header loopNodes =
  LoopCfg $ subCfg_ {root = header ^. #node}
  where
    subCfg_ :: Cfg a
    subCfg_ = G.subgraph (`HS.member` (loopNodes ^. #nodes)) cfg'

fromBackEdge :: forall a. Cfg a -> BackEdge -> NatLoop a
fromBackEdge cfg' backEdge =
  NatLoop header body tail loopCfg backEdge
  where
    header :: LoopHeader
    header = LoopHeader $ backEdge ^. (#edge . #dst)
    body :: LoopBody
    body = getLoopBody cfg' backEdge
    tail :: LoopTail
    tail = LoopTail $ backEdge ^. (#edge . #src)
    loopNodes :: LoopNodes
    loopNodes = LoopNodes $ HS.insert (header ^. #node) . HS.insert (tail ^. #node) $ (body ^. #nodes)
    loopCfg :: LoopCfg a
    loopCfg = getLoopCfg cfg' header loopNodes
