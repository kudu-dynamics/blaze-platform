module Blaze.Cfg
  ( module Exports,
    module Blaze.Cfg,
  )
where

import qualified Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Cfg as Exports
import qualified Data.Graph.Dom as Dlt
import qualified Data.HashMap.Strict as Hm
import qualified Data.HashSet as Hs
import qualified Data.IntMap.Strict as Im
import qualified Data.Set as Set

type DltMap a = IntMap (CfNode a)

type CfMap a = HashMap (CfNode a) Int

buildNodeMap :: Ord a => Cfg a -> DltMap a
buildNodeMap cfg =
  Im.fromList $ zip [0 ..] (Set.toList . G.nodes . view #graph $ cfg)

buildAdjMap :: [Dlt.Node] -> [Dlt.Edge] -> IntMap [Dlt.Node]
buildAdjMap ns =
  foldl' mergeEdges initialAdjMap
  where
    initialAdjMap :: IntMap [Dlt.Node]
    initialAdjMap = Im.fromList $ (,[]) <$> ns
    mergeEdges :: IntMap [Dlt.Node] -> Dlt.Edge -> IntMap [Dlt.Node]
    mergeEdges acc e =
      Im.adjust (snd e :) (fst e) acc

-- | Build a graph for use with Data.Graph.Dom for finding dominators
--  and post-dominators.
--  Note that we use unchecked HashMap lookups (!) as we know the
--  entries must be present. That is, we know there is a corresponding
--  Int for every CfNode.
buildDltGraph ::
  forall a.
  (Hashable a, Ord a) =>
  Cfg a ->
  DltMap a ->
  Dlt.Rooted
buildDltGraph cfg dltMap =
  -- NB: Must use 'fromAdj' since 'fromEdges' will not include nodes
  -- that don't have outgoing edges.
  (cfMap Hm.! view #root cfg, Dlt.fromAdj dltAdj)
 where
  cfMap :: CfMap a
  cfMap = Hm.fromList $ swap <$> Im.assocs dltMap
  dltNodes :: [Dlt.Node]
  dltNodes = (cfMap Hm.!) <$> (Set.toList . G.nodes . view #graph $ cfg)
  dltEdges :: [Dlt.Edge]
  dltEdges = do
    (_, (src_, dst_)) <- G.edges . view #graph $ cfg
    return (cfMap Hm.! src_, cfMap Hm.! dst_)
  dltAdj :: [(Dlt.Node, [Dlt.Node])]
  dltAdj = Im.toList $ buildAdjMap dltNodes dltEdges

-- | Convert a Blaze CFG to a dom-lt flow graph
dltGraphFromCfg :: forall a. (Hashable a, Ord a) => 
  Cfg a -> (Dlt.Rooted, DltMap a)
dltGraphFromCfg cfg =
  (buildDltGraph cfg dltMap, dltMap)
  where
    dltMap :: DltMap a
    dltMap = buildNodeMap cfg

domHelper ::
  forall a.
  (Hashable a, Ord a) =>
  (Dlt.Rooted -> [(Dlt.Node, Dlt.Path)]) ->
  Cfg a ->
  HashMap (CfNode a) (HashSet (CfNode a))
domHelper f cfg =
  Hm.fromList . ((Hs.fromList <$>) <$>) $ domList
 where
  dltRooted :: Dlt.Rooted
  dltMap :: DltMap a
  (dltRooted, dltMap) = dltGraphFromCfg cfg
  domList :: [(CfNode a, [CfNode a])]
  domList = bimap (dltMap Im.!) ((dltMap Im.!) <$>) <$> f dltRooted

-- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
-- to find dominators. The result is converted back to CfNodes before being returned.
-- Per dom-lt, the complexity is:
-- O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
getDominators :: (Hashable a, Ord a) => Cfg a -> Dominators a
getDominators = Dominators . domHelper Dlt.dom

getPostDominators :: (Hashable a, Ord a) => Cfg a -> PostDominators a
getPostDominators = PostDominators . domHelper Dlt.pdom
