module Blaze.Cfg.Loop
  ( module Blaze.Cfg.Loop,
    module Exports,
  )
where

import Blaze.Cfg (getDominators)
import Blaze.Prelude
import Blaze.Types.Cfg (BranchType, CfEdge (CfEdge), CfNode, Cfg, Dominators)
import qualified Blaze.Types.Cfg as TCfg
import qualified Blaze.Types.Cfg.Loop as Exports
import qualified Blaze.Types.Cfg.Loop as T
import Blaze.Types.Cfg.Loop
  ( BackEdge (BackEdge),
    LoopBody (LoopBody),
    LoopHeader (LoopHeader),
    NatLoop (NatLoop),
  )
import qualified Blaze.Types.Graph as G
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as Hm
import qualified Data.HashSet as Hs

-- | Check if an edge is a back edge using graph dominators.
--  We assume the dominators include the nodes referenced in the edge.
--  If that assumption is wrong, isBackEdge defaults to False.
isBackEdge :: Dominators -> CfEdge -> Bool
isBackEdge domMap edge =
  case Hm.lookup (edge ^. TCfg.src) (coerce domMap) of
    Nothing -> False
    (Just domNodes) -> Hs.member (edge ^. TCfg.dst) domNodes

fromGraphEdge :: (BranchType, (CfNode, CfNode)) -> CfEdge
fromGraphEdge (bType, (srcNode, dstNode)) = CfEdge srcNode dstNode bType

getBackEdges :: Cfg a -> [BackEdge]
getBackEdges cfg =
  [ BackEdge e
    | e <- fromGraphEdge <$> G.edges cfg,
      isBackEdge doms e
  ]
  where
    doms :: Dominators
    doms = getDominators cfg

fromBackEdge :: Cfg a -> BackEdge -> NatLoop a
fromBackEdge cfg backEdge =
  NatLoop header body backEdge
  where
    header :: LoopHeader
    header = LoopHeader $ backEdge ^. (T.edge . TCfg.src)
    body :: LoopBody a
    body = undefined
