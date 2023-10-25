{- HLINT ignore "Use if" -}

module Blaze.Types.Path.Alga where

import Blaze.Prelude hiding (pred)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Graph.Alga as AlgaGraph
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (Identifiable, NodeId, getNodeId, LEdge(LEdge), Edge(Edge))
import qualified Blaze.Types.Path as P
import Blaze.Types.Path (IsPath, PathConstruct)
import Blaze.Util (getMemoized)

import qualified Data.List.NonEmpty as NE

-- | A graph implementation that is build atop the Alga graph library.
-- The 'l' type specifies the edge label, the 'i' type specifies the 
-- node idenitifier, and the 'n' type specifies the node.
data AlgaPath l i n = AlgaPath
  -- TODO: We can move to AdjacencyIntMap if we assert NodeId is an Int
  { rootNode :: NodeId i
  , graph :: AlgaGraph l i n    
  } deriving (Generic, Show, Ord, Eq, Functor, Foldable, Traversable)

-- | Call this if you want to map over the nodes and change the node ids.
safeMap
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     , Show a
     )
  => (a -> b)
  -> AlgaPath l i a
  -> AlgaPath l j b
safeMap f p = AlgaPath newRootNodeId . AlgaGraph.safeMap f $ p ^. #graph
  where
    newRootNodeId = getNodeId . f . P.root $ p

safeTraverse_
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     , Monad m
     , Show a
     )
  => (a -> m b)
  -> AlgaPath l i a
  -> StateT (HashMap a b) m (AlgaPath l j b)
safeTraverse_ f p = do
  let oldRoot = P.root p
  newRoot <- getMemoized f oldRoot
  newGraph <- AlgaGraph.safeTraverse_ f $ p ^. #graph
  return $ AlgaPath (getNodeId newRoot) newGraph

safeTraverse
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     , Monad m
     , Show a
     )
  => (a -> m b)
  -> AlgaPath l i a
  -> m (AlgaPath l j b)
safeTraverse f p = evalStateT (safeTraverse_ f p) HashMap.empty

unsafeTraverse
  :: ( Hashable a
     , Hashable b
     , Hashable i
     , Hashable j
     , Ord i
     , Ord j
     , Identifiable a i
     , Identifiable b j
     , Applicative f
     , Show a
     )
  => (a -> f b)
  -> AlgaPath l i a
  -> f (AlgaPath l j b)
unsafeTraverse f p = AlgaPath <$> newRootNodeId <*> AlgaGraph.unsafeTraverse f (p ^. #graph)
  where
    newRootNodeId = getNodeId <$> f (P.root p)

instance (NFData l, NFData i, NFData n) => NFData (AlgaPath l i n)

instance
  (Ord i, Hashable i, Hashable n, Identifiable n i) =>
  PathConstruct l n (AlgaPath l i)
  where

  fromPathGraph rootNode_ g_ = do
    ensureLinearPathGraph
    return $ AlgaPath
      { rootNode = getNodeId rootNode_
      , graph = g
      }
    where
      g = G.convertGraph g_
      -- | This fails if any node as multiple succs, if root has pred, or
      -- if there is a loop.
      ensureLinearPathGraph = do
        ensureRootExistsAndHasNoPreds
        ensureLinearWithoutLoops HashSet.empty rootNode_

      ensureRootExistsAndHasNoPreds =
        bool mzero (return ())
        $ HashSet.null (G.preds rootNode_ g)
        && G.hasNode rootNode_ g

      ensureLinearWithoutLoops visited n = case HashSet.toList (G.succs n g) of
        [] -> return () -- end of path reached
        [m] -> case HashSet.member m visited of
          True -> mzero
          False -> ensureLinearWithoutLoops (HashSet.insert m visited) m 
        _ -> mzero

  fromEdges startNode [] = P.fromPathGraph startNode (G.fromNode startNode :: AlgaGraph l i n)
  fromEdges startNode es = P.fromPathGraph startNode (G.fromEdges es :: AlgaGraph l i n)
    
instance
  (Show n, Ord i, Hashable i, Hashable n, Identifiable n i) =>
  IsPath l n (AlgaPath l i)
  where

  root p = unsafeGetNode (p ^. #rootNode) p

  end p = loopUntilLast (P.root p)
    where
      loopUntilLast a = maybe a loopUntilLast $ P.succ a p

  succ a p = case HashSet.toList . G.succs a $ p ^. #graph of
    [] -> Nothing
    [b] -> Just b
    xs -> error $ "Internal path graph has node with multiple succs:\n " <> cs (pshow a) <> "\n\n has these succs:\n" <> cs (pshow xs)
    
  pred b p = case HashSet.toList . G.preds b $ p ^. #graph of
    [] -> Nothing
    [a] -> Just a
    _ -> error "Internal path graph has node with multiple preds"

  succEdge a p = do
    b <- P.succ a p
    l <- HashMap.lookup (G.Edge (getNodeId a) (getNodeId b)) $ p ^. #graph . #edgeMap
    return . LEdge l $ Edge a b

  predEdge b p = do
    a <- P.pred b p
    l <- HashMap.lookup (G.Edge (getNodeId a) (getNodeId b)) $ p ^. #graph . #edgeMap
    return . LEdge l $ Edge a b

  hasNode n = G.hasNode n . view #graph

  nodes = G.nodes . view #graph
                 
  toPathGraph p =
    ( fromJust . HashMap.lookup (p ^. #rootNode) $ p ^. #graph . #nodeMap
    , G.convertGraph $ p ^. #graph
    )

  toNodeList p = followNodes $ unsafeGetNode (p ^. #rootNode) p
    where
      followNodes a = case P.succ a p of
        Nothing -> NE.singleton a
        Just b -> NE.cons a $ followNodes b

  toEdgeList p = ( root_
                 , followEdges root_
                 )
    where
      root_ = unsafeGetNode (p ^. #rootNode) p
      followEdges a = case P.succEdge a p of
        Nothing -> []
        Just e@(LEdge _ (Edge _ b)) -> e : followEdges b

  expandNode n pOuter pInner
    | not (P.hasNode n pOuter) = error "Node not found"
    | otherwise = AlgaPath
      { rootNode = case pedge of
          -- replaced node is root, so replace with inner root node
          Nothing -> pInner ^. #rootNode
          Just _ -> pOuter ^. #rootNode
      , graph = g
      }
    where
      pedge = P.predEdge n pOuter
      sedge = P.succEdge n pOuter
      linkStart (LEdge l (Edge a b))
        = G.addEdge (LEdge l (Edge a (P.root pInner)))
        . G.removeEdge (Edge a b)
      linkEnd (LEdge l (Edge a b))
        = G.addEdge (LEdge l (Edge (P.end pInner) b))
        . G.removeEdge (Edge a b)
      g = maybe identity linkStart pedge
          . maybe identity linkEnd sedge
          . G.addEdges (G.edges $ pInner ^. #graph)
          . G.removeNode n
          $ pOuter ^. #graph

  append p1 lbl p2 = AlgaPath
    { rootNode = p1 ^. #rootNode
    , graph = G.addEdge (LEdge lbl (Edge (P.end p1) (P.root p2)))
              . G.addEdges (G.edges $ p2 ^. #graph)
              $ p1 ^. #graph
    }

  removeAfterNode n p = p & #graph %~ followAndRemove (P.succ n p)
    where
      followAndRemove Nothing g = g
      followAndRemove (Just x) g = followAndRemove (P.succ x p)
        $ G.removeNode x g -- removing nodes removes edges with nodes in AlgaGraph

  removeBeforeNode n p = AlgaPath
    { rootNode = G.getNodeId n
    , graph = backtrackAndRemove (P.pred n p) $ p ^. #graph
    }
    where
      backtrackAndRemove Nothing g = g
      backtrackAndRemove (Just x) g = backtrackAndRemove (P.pred x p)
        $ G.removeNode x g -- removing nodes removes edges with nodes in AlgaGraph

getNode :: Hashable i => NodeId i -> AlgaPath l i n -> Maybe n
getNode n p = HashMap.lookup n $ p ^. #graph . #nodeMap

unsafeGetNode :: Hashable i => NodeId i -> AlgaPath l i n -> n
unsafeGetNode n = fromJust . getNode n
