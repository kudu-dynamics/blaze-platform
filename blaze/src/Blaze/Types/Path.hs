{-# LANGUAGE DefaultSignatures #-}
module Blaze.Types.Path where

import Blaze.Prelude
import Blaze.Types.Graph (LEdge(LEdge), Edge(Edge), Graph, GraphConstruct)


class IsPath l n p | p -> l where
  root :: p n -> n
  end :: p n -> n
  succ :: n -> p n -> Maybe n
  pred :: n -> p n -> Maybe n

  -- | Returns edge from n to succ
  succEdge :: n -> p n -> Maybe (LEdge l n)
  -- | Returns edge from pred to n
  predEdge :: n -> p n -> Maybe (LEdge l n)

  nodes :: p n -> HashSet n

  hasNode :: n -> p n -> Bool

  -- | Converts path to (starting node, graph), where graph contains a linear path
  toPathGraph :: (Graph l n g, GraphConstruct l n g) => p n -> (n, g n)

  -- | Converts to list of nodes, ordered according to position in the path
  toNodeList :: p n -> NonEmpty n
  -- | Converts a path to a list of edges, ordered according to position in the path
  -- Result is: `(root node, [edge])`
  toEdgeList :: p n -> (n, [LEdge l n])
  
  -- | Replaces node `n` in the first path with the second path.
  -- Assumes that inner and outer path nodes are unique.
  expandNode :: n -> p n -> p n -> p n

  -- | Appends second path to the end of first path.
  -- Assumes that first and second path nodes are unique.
  -- Edge lable `l` is used between last node of p1 and first of p2
  append :: p n -> l -> p n -> p n

  -- | Connects two paths through common end/start node.
  -- where last node of path 1 == first node of path 2
  connect :: p n -> p n -> Maybe (p n)

  -- | Removes path after n, but keeps n
  removeAfterNode :: n -> p n -> p n
  -- | Removes path before n, but keeps n
  removeBeforeNode :: n -> p n -> p n

  -- | Drops max(n, len(p) - 1)  nodes from start of path
  drop :: Word64 -> p n -> p n

class PathConstruct l n p | p -> l where
  -- | From a graph with a single path. Fails if g is empty or has multiple paths
  -- First arg is starting node.
  -- Starting node is passed in to possibly reduce redundant computation.
  -- Fails if starting node has preds
  fromPathGraph :: Graph l n g => n -> g n -> Maybe (p n)

  -- | Creates a path from a list of edges. n is start node.
  -- If the list of edges is empty, a singleton path is created.
  -- Returns Nothing if the edge list forms a graph with multiple paths.
  fromEdges :: n -> [LEdge l n] -> Maybe (p n)

  build :: PathBuilder l n -> p n

  default build :: PathBuilder l n -> p n
  build p = fromMaybe (error "Cannot build path with duplicate node")
    $ fromEdges startNode_ edges_
    where
      (startNode_, edges_) = mkLEdges (p ^. #halfEdges) (p ^. #endNode) []
      mkLEdges [] n acc = (n, acc)
      mkLEdges ((a, lbl):es) b acc = mkLEdges es a $ LEdge lbl (Edge a b) : acc

start :: n -> PathBuilder l n
start n = PathBuilder n []

type HalfEdge l n = (n, l)

data PathBuilder l n = PathBuilder
  { endNode :: n
  , halfEdges :: [HalfEdge l n]
  } deriving (Eq, Ord, Show, Generic)

(|-) :: [HalfEdge l n] -> n -> PathBuilder l n
(|-) es n = PathBuilder n es
infixl 7 |-

(-|) :: PathBuilder l n -> l -> [HalfEdge l n]
(-|) p l = (p ^. #endNode, l) : p ^. #halfEdges
infixl 7 -|
