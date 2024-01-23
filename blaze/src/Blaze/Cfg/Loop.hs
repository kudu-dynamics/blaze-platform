module Blaze.Cfg.Loop
  ( module Blaze.Cfg.Loop,
    module Exports,
  )
where

import Blaze.Cfg (getDominators)
import Blaze.Graph (Dominators, Identifiable)
import Blaze.Prelude
import Blaze.Types.Cfg (
  CfEdge,
  CfNode,
  Cfg (rootId),
 )
import qualified Blaze.Types.Cfg as Cfg

-- Hiding some names commonly used as variables,
-- re-exporting eplicitly.

import Blaze.Types.Cfg.Loop as Exports (
  LoopCfg (LoopCfg),
  NatLoop (NatLoop),
 )
import Blaze.Types.Cfg.Loop as Exports hiding (
  LoopCfg (..),
  NatLoop (..),
  backEdge,
  edge,
 )
import qualified Blaze.Types.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | Check if an edge is a back edge using graph dominators.
--  We assume the dominators include the nodes referenced in the edge.
--  If that assumption is wrong, isBackEdge defaults to False.
isBackEdge :: Hashable a => Dominators (CfNode a) -> CfEdge (CfNode a) -> Bool
isBackEdge domMap edge =
  case HM.lookup (edge ^. #src) (coerce domMap) of
    Nothing -> False
    (Just domNodes) -> HS.member (edge ^. #dst) domNodes

getBackEdges ::
  forall a.
  Hashable a =>
  Cfg (CfNode a) ->
  [BackEdge (CfNode a)]
getBackEdges cfg' =
  [ BackEdge e
  | e <- Cfg.edges cfg'
  , isBackEdge doms e
  ]
 where
  doms :: Dominators (CfNode a)
  doms = getDominators cfg'

{- | Find body nodes of loop. If an empty list is returned, the loop
  only contains a head(er) node and a tail node.
-}
getBodyNodes ::
  forall a.
  (Hashable a, Identifiable (CfNode a) UUID) =>
  Cfg (CfNode a) ->
  BackEdge (CfNode a) ->
  HashSet (CfNode a)
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

getLoopBody ::
  forall a.
  Hashable a =>
  Cfg (CfNode a) ->
  BackEdge (CfNode a) ->
  LoopBody (CfNode a)
getLoopBody cfg' backEdge =
  LoopBody $ getBodyNodes cfg' backEdge

getLoopCfg ::
  forall a.
  (Hashable a, Identifiable (CfNode a) UUID) =>
  Cfg (CfNode a) ->
  LoopHeader (CfNode a) ->
  LoopNodes (CfNode a) ->
  LoopCfg (CfNode a)
getLoopCfg cfg' header loopNodes =
  LoopCfg $ subCfg{rootId = G.getNodeId $ header ^. #node}
 where
  subCfg :: Cfg (CfNode a)
  subCfg = G.subgraph (`HS.member` (loopNodes ^. #nodes)) cfg'

fromBackEdge ::
  forall a b.
  Hashable a =>
  Cfg (CfNode a) ->
  BackEdge (CfNode a) ->
  NatLoop (CfNode a) b
fromBackEdge cfg' backEdge =
  NatLoop header body tail loopCfg backEdge
 where
  header :: LoopHeader (CfNode a)
  header = LoopHeader $ backEdge ^. (#edge . #dst)
  body :: LoopBody (CfNode a)
  body = getLoopBody cfg' backEdge
  tail :: LoopTail (CfNode a)
  tail = LoopTail $ backEdge ^. (#edge . #src)
  loopNodes :: LoopNodes (CfNode a)
  loopNodes = LoopNodes $ HS.insert (header ^. #node) . HS.insert (tail ^. #node) $ (body ^. #nodes)
  loopCfg :: LoopCfg (CfNode a)
  loopCfg = getLoopCfg cfg' header loopNodes
