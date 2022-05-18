module Blaze.Graph
  ( module Exports,
    module Blaze.Graph,
  )
where

import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph as Exports hiding (edge, label, src, dst)
import qualified Blaze.Types.Graph.EdgeGraph as Eg
import Blaze.Types.Graph.EdgeGraph (EdgeGraphNode (NodeNode, EdgeNode))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HSet

------- Dominators
getDominatorMapping
  :: forall n g l. (Hashable n, Graph l n g)
  => n
  -> g n
  -> HashMap n (HashSet n)
getDominatorMapping rootNode g = foldl' (flip buildDominatedMapping) HashMap.empty allNodes
  where
    allNodes = reachable rootNode g
    allNodesSet = HashSet.fromList allNodes
    getDominatedBy :: n -> [n]
    getDominatedBy node = HashSet.toList
      . HashSet.difference allNodesSet
      . HashSet.fromList
      . bfsReachable rootNode
      . removeNode node
      $ g
    buildDominatedMapping :: n -> HashMap n (HashSet n) -> HashMap n (HashSet n)
    buildDominatedMapping n m = foldr alterIfNotEqual m $ getDominatedBy n
      where
        alterIfNotEqual :: n -> HashMap n (HashSet n) -> HashMap n (HashSet n)
        alterIfNotEqual n' m'
          | n' == n = m'
          | otherwise = HashMap.alter addOrCreate n' m'
        addOrCreate :: Maybe (HashSet n) -> Maybe (HashSet n)
        addOrCreate Nothing = Just $ HashSet.singleton n
        addOrCreate (Just s) = Just $ HashSet.insert n s

-- | Nodes reachable from n in bfs order, excludes self unless can be reached later
bfsReachable :: (Graph l n g) => n -> g n -> [n]
bfsReachable n g = concat $ bfs [n] g

getDominators :: (Hashable n, Graph l n g) => n -> g n -> Dominators n
getDominators rootNode = Dominators . getDominatorMapping rootNode

getPostDominators_ :: (Hashable n, Graph l n g) => n -> g n -> PostDominators n
getPostDominators_ termNode = PostDominators . getDominatorMapping termNode . G.transpose

-- | Gets all post dominators. If there are multiple terminal nodes,
--   each will point to `dummyTermNode`.
getPostDominators
  :: (Hashable n, Graph l n g)
  => n
  -> l
  -> g n
  -> PostDominators n
getPostDominators dummyTermNode dummyTermEdgeLabel g =
  case HashSet.toList $ G.getTermNodes g of
    [] -> domEmpty
    [x] -> getPostDominators_ x g
    xs -> domRemoveNode dummyTermNode
      . getPostDominators_ dummyTermNode
      $ foldl' (flip f) g xs
      where
        f x = G.addEdge (G.LEdge dummyTermEdgeLabel $ G.Edge x dummyTermNode)

-- | All nodes that can reach node and that can be reached by node.
-- Formally, this is the union of the in-component and the out-component of
-- node n in graph g.
connectedNodes :: (Graph l n g, Hashable n) => n -> g n -> HashSet n
connectedNodes n g = HSet.fromList reachedNodes <> HSet.fromList reachingNodes
  where
    reachedNodes = G.reachable n g
    reachingNodes = G.reachable n $ G.transpose g

connectedNodesAndEdges :: forall l n g g'.
  ( Graph l n g
  , Graph () (EdgeGraphNode l n) g'
  , Hashable n
  , Hashable l
  ) => Proxy g' -> n -> g n -> (HashSet n, HashSet (LEdge l n))
connectedNodesAndEdges _ n g = foldr f (HSet.empty, HSet.empty) cnodes
  where
    g' = Eg.toEdgeGraph g :: g' (EdgeGraphNode l n)
    cnodes = connectedNodes (NodeNode n) g'
    f :: EdgeGraphNode l n -> (HashSet n, HashSet (LEdge l n)) -> (HashSet n, HashSet (LEdge l n))
    f (NodeNode x) (nodes', edges') = (HSet.insert x nodes', edges')
    f (EdgeNode e) (nodes', edges') = (nodes', HSet.insert e edges')
