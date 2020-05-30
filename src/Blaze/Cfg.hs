module Blaze.Cfg
  ( module Exports,
    module Blaze.Cfg,
  )
where

import Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Cfg as Exports
import qualified Data.Graph.Dom as Dlt
import qualified Data.HashMap.Strict as Hm
import qualified Data.IntMap.Strict as Im
import Data.Set as Set

type DltMap = IntMap CfNode

type CfMap = HashMap CfNode Int

buildNodeMap :: Cfg a -> DltMap
buildNodeMap cfg =
  Im.fromList $ zip [0 ..] (Set.toList . G.nodes . _graph $ cfg)

buildDltGraph :: Cfg a -> DltMap -> Dlt.Rooted
buildDltGraph cfg dltMap =
  (cfMap Hm.! _root cfg, Dlt.fromEdges dltEdges)
  where
    cfMap :: CfMap
    cfMap = Hm.fromList $ swap <$> Im.assocs dltMap
    dltEdges :: [Dlt.Edge]
    dltEdges = do
      (_, (src, dst)) <- G.edges . _graph $ cfg
      -- We know there is a corresponding Int for every CfNode
      return (cfMap Hm.! src, cfMap Hm.! dst)

-- | Convert a Blaze CFG to a dom-lt flow graph
dltGraphFromCfg :: Cfg a -> (Dlt.Rooted, DltMap)
dltGraphFromCfg cfg =
  (buildDltGraph cfg dltMap, dltMap)
  where
    dltMap :: DltMap
    dltMap = buildNodeMap cfg

domHelper :: (Dlt.Rooted -> [(Dlt.Node, Dlt.Path)]) -> Cfg a -> [(CfNode, [CfNode])]
domHelper f cfg =
  bimap (dltMap Im.!) ((dltMap Im.!) <$>) <$> f dltRooted
    where 
      dltRooted :: Dlt.Rooted
      dltMap :: DltMap
      (dltRooted, dltMap) = dltGraphFromCfg cfg

-- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
-- to find dominators. The result is converted back to CfNodes before being returned.
-- Per dom-lt, the complexity is:
-- O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
getDominators :: Cfg a -> [(CfNode, [CfNode])]
getDominators = domHelper Dlt.dom

getPostDominators :: Cfg a -> [(CfNode, [CfNode])]
getPostDominators = domHelper Dlt.pdom