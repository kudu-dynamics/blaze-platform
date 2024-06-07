module Flint.Graph where

import Flint.Prelude

import Blaze.Types.Graph (Edge(Edge), OuterNodeDescendants, StrictDescendantsMap(StrictDescendantsMap))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


newtype InterEdgeDescendants innerNode
  = InterEdgeDescendants (HashMap (Edge innerNode) (HashSet innerNode))
  deriving (Eq, Ord, Show, Generic)

instance Hashable x => Semigroup (InterEdgeDescendants x) where
  (<>) (InterEdgeDescendants a) (InterEdgeDescendants b) = InterEdgeDescendants
    $ HashMap.unionWith (\_ _ -> error "Edge should not repeat") a b

instance Hashable x => Monoid (InterEdgeDescendants x) where
  mempty = InterEdgeDescendants HashMap.empty

-- TODO: write tests for this
-- | Just like InterNodeDescendants, but per-edge
calcInterEdgeDescendants
  :: forall outerNode innerNode.
     ( Hashable outerNode
     , Hashable innerNode
     )
  => (innerNode -> Maybe outerNode) -- does node flow to a different outer node? (ie Call node)
  -> StrictDescendantsMap innerNode
  -> OuterNodeDescendants outerNode innerNode
  -> [Edge innerNode]
  -> InterEdgeDescendants innerNode
calcInterEdgeDescendants getTransToOuter (StrictDescendantsMap dmap) outerNodeDescendants =
  foldl' addDescendants mempty
  where
    addDescendants
      :: InterEdgeDescendants innerNode
      -> Edge innerNode
      -> InterEdgeDescendants innerNode
    addDescendants (InterEdgeDescendants m) e@(Edge _ dst) =
      InterEdgeDescendants $ HashMap.insert e descs m
      where
        innerDescs = fromJust $ HashMap.lookup dst dmap
        outerDescs = mapMaybe getTransToOuter . HashSet.toList $ innerDescs
        getOuterNode'sInnerNodes
          = fromMaybe HashSet.empty
          . flip HashMap.lookup outerNodeDescendants 
        nodesFromOuterDescs
          = foldl' (\s outer -> getOuterNode'sInnerNodes outer <> s) HashSet.empty outerDescs
        descs = HashSet.singleton dst <> nodesFromOuterDescs <> innerDescs
