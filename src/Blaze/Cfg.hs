module Blaze.Cfg
  ( module Exports,
    module Blaze.Cfg,
  )
where

import qualified Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Cfg hiding (src, dst, branchType)
import qualified Blaze.Types.Cfg as T
import qualified Blaze.Types.Cfg as Exports
import Data.Coerce (coerce)
import qualified Data.Graph.Dom as Dlt
import qualified Data.HashMap.Strict as Hm
import qualified Data.HashSet as Hs
import qualified Data.IntMap.Strict as Im
import qualified Data.Set as Set

type DltMap = IntMap CfNode

type CfMap = HashMap CfNode Int

buildNodeMap :: Cfg a -> DltMap
buildNodeMap cfg =
  Im.fromList $ zip [0 ..] (Set.toList . G.nodes . _graph $ cfg)

buildAdjMap :: [Dlt.Node] -> [Dlt.Edge] -> IntMap [Dlt.Node]
buildAdjMap ns = 
  foldl' mergeEdges initialAdjMap
    where
      initialAdjMap :: IntMap [Dlt.Node]
      initialAdjMap = Im.fromList $ (, []) <$> ns
      mergeEdges :: IntMap [Dlt.Node] -> Dlt.Edge -> IntMap [Dlt.Node]
      mergeEdges acc e =
        Im.adjust (snd e : ) (fst e) acc

-- |Build a graph for use with Data.Graph.Dom for finding dominators
-- and post-dominators. 
-- Note that we use unchecked HashMap lookups (!) as we know the
-- entries must be present. That is, we know there is a corresponding 
-- Int for every CfNode.
buildDltGraph :: Cfg a -> DltMap -> Dlt.Rooted
buildDltGraph cfg dltMap =
  -- NB: Must use 'fromAdj' since 'fromEdges' will not include nodes
  -- that don't have outgoing edges.
  (cfMap Hm.! _root cfg, Dlt.fromAdj dltAdj)
  where
    cfMap :: CfMap
    cfMap = Hm.fromList $ swap <$> Im.assocs dltMap
    dltNodes :: [Dlt.Node]
    dltNodes = (cfMap Hm.!) <$> (Set.toList . G.nodes . _graph $ cfg)
    dltEdges :: [Dlt.Edge]
    dltEdges = do
      (_, (src, dst)) <- G.edges . _graph $ cfg
      return (cfMap Hm.! src, cfMap Hm.! dst)
    dltAdj :: [(Dlt.Node, [Dlt.Node])]
    dltAdj = Im.toList $ buildAdjMap dltNodes dltEdges

-- | Convert a Blaze CFG to a dom-lt flow graph
dltGraphFromCfg :: Cfg a -> (Dlt.Rooted, DltMap)
dltGraphFromCfg cfg =
  (buildDltGraph cfg dltMap, dltMap)
  where
    dltMap :: DltMap
    dltMap = buildNodeMap cfg

domHelper :: (Dlt.Rooted -> [(Dlt.Node, Dlt.Path)]) -> Cfg a -> HashMap CfNode (HashSet CfNode)
domHelper f cfg =
  Hm.fromList . ((Hs.fromList <$>) <$>) $ domList
  where
    dltRooted :: Dlt.Rooted
    dltMap :: DltMap
    (dltRooted, dltMap) = dltGraphFromCfg cfg
    domList :: [(CfNode, [CfNode])]
    domList = bimap (dltMap Im.!) ((dltMap Im.!) <$>) <$> f dltRooted

-- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
-- to find dominators. The result is converted back to CfNodes before being returned.
-- Per dom-lt, the complexity is:
-- O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
getDominators :: Cfg a -> Dominators
getDominators = Dominators . domHelper Dlt.dom

getPostDominators :: Cfg a -> PostDominators
getPostDominators = PostDominators . domHelper Dlt.pdom

-- |Check if an edge is a back edge using graph dominators.
-- We assume the dominators include the nodes referenced in the edge.
-- If that assumption is wrong, isBackEdge defaults to False.
isBackEdge :: Dominators -> CfEdge -> Bool
isBackEdge domMap edge =
  case Hm.lookup (edge ^. T.src) (coerce domMap) of
    Nothing -> False
    (Just domNodes) -> Hs.member (edge ^. T.dst) domNodes

fromGraphEdge :: (BranchType, (CfNode, CfNode)) -> CfEdge
fromGraphEdge (bType, (srcNode, dstNode)) = CfEdge srcNode dstNode bType

getBackEdges :: Cfg a -> [CfEdge]
getBackEdges cfg =
  [e | e <- fromGraphEdge <$> G.edges cfg,
       isBackEdge doms e]
    where
      doms :: Dominators
      doms = getDominators cfg
