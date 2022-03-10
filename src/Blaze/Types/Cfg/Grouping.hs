{- | GroupingCfg is a Cfg that can contain groups of nodes as a single block
This module provides functionality for grouping, ungrouping, and regrouping.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Blaze.Types.Cfg.Grouping where

import Blaze.Prelude

import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph (AlgaGraph))

import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Bitraversable (bifor)
import Control.Lens (set)


data GroupingNode a = GroupingNode
  { termNode :: CfNode a
  , grouping :: Cfg a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data CfNode a
  = BasicBlock (Cfg.BasicBlockNode a)
  | Call (Cfg.CallNode a)
  | EnterFunc (Cfg.EnterFuncNode a)
  | LeaveFunc (Cfg.LeaveFuncNode a)
  | Grouping (GroupingNode a)
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data CfEdge a = CfEdge
  { src :: CfNode a
  , dst :: CfNode a
  , branchType :: Cfg.BranchType
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

fromCfNode :: Cfg.CfNode a -> CfNode a
fromCfNode (Cfg.BasicBlock n) = BasicBlock n
fromCfNode (Cfg.Call n) = Call n
fromCfNode (Cfg.EnterFunc n) = EnterFunc n
fromCfNode (Cfg.LeaveFunc n) = LeaveFunc n

toCfNode :: CfNode a -> Maybe (Cfg.CfNode a)
toCfNode (BasicBlock n) = Just (Cfg.BasicBlock n)
toCfNode (Call n) = Just (Cfg.Call n)
toCfNode (EnterFunc n) = Just (Cfg.EnterFunc n)
toCfNode (LeaveFunc n) = Just (Cfg.LeaveFunc n)
toCfNode (Grouping _) = Nothing

fromCfEdge :: Cfg.CfEdge a -> CfEdge a
fromCfEdge (Cfg.CfEdge s d bt) = CfEdge (fromCfNode s) (fromCfNode d) bt

toCfEdge :: CfEdge a -> Maybe (Cfg.CfEdge a)
toCfEdge (CfEdge s d bt) = Cfg.CfEdge <$> toCfNode s <*> toCfNode d <*> Just bt

fromLEdge :: G.LEdge Cfg.BranchType (CfNode a) -> CfEdge a
fromLEdge (G.LEdge bt (G.Edge s d)) = CfEdge s d bt

toLEdge :: CfEdge a -> G.LEdge Cfg.BranchType (CfNode a)
toLEdge (CfEdge a b bt) = G.LEdge bt (G.Edge a b)

type ControlFlowGraph a = AlgaGraph Cfg.BranchType (CfNode a) (CfNode ())

asIdNode :: CfNode a -> CfNode ()
asIdNode = void

asIdEdge :: CfEdge a -> CfEdge ()
asIdEdge = void

data Cfg a = Cfg
  { graph :: ControlFlowGraph a
  , root :: CfNode a
  }
  deriving (Eq, Ord, Show, Generic)

instance Functor Cfg where
  fmap f cfg = Cfg
    { graph = G.mapAttrs (fmap f) $ cfg ^. #graph
    , root = f <$> (cfg ^. #root)
    }

instance Foldable Cfg where
  foldMap f = G.foldMapAttrs g . view #graph
    where
      g = \case
        Grouping x -> foldMap f $ x ^. #grouping
        BasicBlock x -> f $ x ^. #nodeData
        Call x -> f $ x ^. #nodeData
        EnterFunc x -> f $ x ^. #nodeData
        LeaveFunc x -> f $ x ^. #nodeData

instance Traversable Cfg where
  traverse f cfg = Cfg
    <$> G.traverseAttrs (traverse f) (cfg ^. #graph)
    <*> traverse f (cfg ^. #root)

instance Hashable a => Hashable (Cfg a) where
  hashWithSalt n = hashWithSalt n . toTransport
  hash = hash . toTransport

instance ToJSON a => ToJSON (Cfg a) where
 toJSON = toJSON . toTransport

instance (Eq a, Hashable a, FromJSON a) => FromJSON (Cfg a) where
 parseJSON = fmap fromTransport . parseJSON

mkCfg :: (Hashable a, Eq a) => CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
mkCfg root' rest es =
  Cfg
    { graph = mkControlFlowGraph root' rest es
    , root = root'
    }

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: forall a .(Hashable a, Eq a)
                   => CfNode a
                   -> [CfNode a]
                   -> [CfEdge a]
                   -> ControlFlowGraph a
mkControlFlowGraph root' ns es = G.addNodesWithAttrs attrList
  . G.fromEdges
  $ fmap asIdNode . toLEdge <$> es
  where
    nodesInEdges = concatMap (\e -> [e ^. #src, e ^. #dst]) es
    allNodes :: [CfNode a]
    allNodes = HashSet.toList . HashSet.fromList $ (root' : ns) <> nodesInEdges

    attrList :: [(CfNode (), CfNode a)]
    attrList = (\n -> (asIdNode n, n)) <$> allNodes

-- | CfgTransport is an intermediary type for Hashable, FromJSON, and ToJSON instances
data CfgTransport a = CfgTransport
  { transportEdges :: [CfEdge ()]
  , transportRoot :: CfNode ()
  , transportNodes :: [(CfNode (), CfNode a)]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

fromCfg :: Cfg.Cfg a -> Cfg a
fromCfg (Cfg.Cfg (AlgaGraph adjs edata ndata) root_) =
  Cfg
    (AlgaGraph
      (AM.gmap fromCfNode adjs)
      (HashMap.mapKeys (fmap fromCfNode) edata)
      (HashMap.map fromCfNode . HashMap.mapKeys fromCfNode $ ndata))
    (fromCfNode root_)

toCfg :: Cfg a -> Cfg.Cfg a
toCfg cfg@(Cfg graph_ root_) =
  case getGroups cfg of
    [] -> fromJust $ toCfg_ cfg
    groups ->
      toCfg $
        foldl'
          (\cfg' gn -> _)
          cfg
          groups
  where
    getGroups :: Cfg a -> [GroupingNode a]
    getGroups (Cfg (AlgaGraph _ _ vs) _) =
      mapMaybe (\case Grouping n -> Just n; _ -> Nothing) $ HashMap.elems vs

terminalNode :: CfNode a -> Cfg.CfNode a
terminalNode (BasicBlock n) = Cfg.BasicBlock n
terminalNode (Call n) = Cfg.Call n
terminalNode (EnterFunc n) = Cfg.EnterFunc n
terminalNode (LeaveFunc n) = Cfg.LeaveFunc n
terminalNode (Grouping (GroupingNode exit _ _)) = terminalNode exit

initialNode :: CfNode a -> Cfg.CfNode a
initialNode (BasicBlock n) = Cfg.BasicBlock n
initialNode (Call n) = Cfg.Call n
initialNode (EnterFunc n) = Cfg.EnterFunc n
initialNode (LeaveFunc n) = Cfg.LeaveFunc n
initialNode (Grouping (GroupingNode _ _ (Cfg _ root_))) = initialNode root_

unfoldGroups :: (Eq a, Hashable a) => Cfg a -> Cfg.Cfg a
unfoldGroups = fromJust . toCfgMaybe . expandAll
  where
    expandAll :: (Eq a, Hashable a) => Cfg a -> Cfg a
    expandAll cfg =
      foldl'
      (\cfg' n ->
         case expandNode n of
           Nothing -> cfg'
           Just (exit, subcfg) -> substNode cfg' n subcfg exit)
      cfg
      (fmap snd . HashMap.toList $ cfg ^. #graph . #nodeAttrMap)

    expandNode :: (Eq a, Hashable a) => CfNode a -> Maybe (CfNode a, Cfg a)
    expandNode =
      \case
        (BasicBlock _) -> Nothing
        (Call _) -> Nothing
        (EnterFunc _) -> Nothing
        (LeaveFunc _) -> Nothing
        (Grouping (GroupingNode exit _ sub)) -> Just (fromCfNode $ terminalNode exit, expandAll sub)

toCfgMaybe :: Cfg a -> Maybe (Cfg.Cfg a)
toCfgMaybe (Cfg (AlgaGraph adjs edata ndata) root_) =
  Cfg.Cfg
    <$> (AlgaGraph
           <$> (AM.edges <$> traverse (\p -> bifor p toCfNode toCfNode) (AM.edgeList adjs))
           <*> (HashMap.fromList <$> traverse (\p -> bifor p (traverse toCfNode) Just) (HashMap.toList edata))
           <*> (HashMap.fromList <$> traverse (\p -> bifor p toCfNode toCfNode) (HashMap.toList ndata)))
    <*> toCfNode root_

toTransport :: forall a. Cfg a -> CfgTransport a
toTransport pcfg = CfgTransport
  { transportEdges = edges'
  , transportRoot = root'
  , transportNodes = nodes'
  }
  where
    root' = void $ pcfg ^. #root

    nodes' :: [(CfNode (), CfNode a)]
    nodes' = HashMap.toList $ pcfg ^. #graph . #nodeAttrMap

    edges' :: [CfEdge ()]
    edges' = fmap fromLEdge . G.edges $ pcfg ^. #graph

fromTransport :: (Eq a, Hashable a) => CfgTransport a -> Cfg a
fromTransport t = mkCfg root' nodes' edges'
  where
    nodeMap = HashMap.fromList $ t ^. #transportNodes

    fullNode = fromJust . flip HashMap.lookup nodeMap

    fullEdge e = CfEdge
      { src = fullNode $ e ^. #src
      , dst = fullNode $ e ^. #dst
      , branchType = e ^. #branchType
      }

    root' = fullNode $ t ^. #transportRoot

    nodes' = snd <$> t ^. #transportNodes

    edges' = fullEdge <$> t ^. #transportEdges

-- -- | Record of the group layouts, including inner groups of groups.
-- type GroupingTree = [GroupSpec]

-- -- | Root dominator and term post-dominator nodes for group, including inner groups.
-- -- root and term are stored as non-group Cfg CfNode so that we don't point to groups
-- data GroupSpec = GroupSpec
--   { groupRoot :: Cfg.CfNode ()
--   , groupTerm :: Cfg.CfNode ()
--   , innerGroups :: GroupingTree


---------------------------------------
-- HERE LIETH COPY/PASTE ABSTRACTION --
---------------------------------------

instance (Hashable a, Eq a) => G.Graph Cfg.BranchType (CfNode a) (CfNode a) (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use mkCfg to construct a CFG."
  fromEdges _ = error "Use mkCfg to construct a CFG."
  succs node g = toFullNodeSet g
    . G.succs (asIdNode node)
    . view #graph
    $ g
  preds node g = toFullNodeSet g
    . G.preds (asIdNode node)
    . view #graph
    $ g
  nodes g = toFullNodeSet g . G.nodes $ g ^. #graph
  edges g = fmap (fmap $ getFullNode g) . G.edges $ g ^. #graph

  getEdgeLabel edge = G.getEdgeLabel (asIdNode <$> edge) . view #graph
  setEdgeLabel label edge cfg = cfg & #graph %~ G.setEdgeLabel label (asIdNode <$> edge)

  -- Sort of pointless. Could just return `node`
  getNodeAttr node = G.getNodeAttr (asIdNode node) . view #graph

  setNodeAttr attr node cfg = cfg
    & #graph %~ G.setNodeAttr attr (asIdNode node)
    & #root %~ updateRoot
    where
      updateRoot oldRoot
        | asIdNode oldRoot == asIdNode node = attr
        | otherwise = oldRoot

  -- useless since `nodes` already returns nodes with their attrs
  getNodeAttrMap cfg = HashMap.fromList
    . fmap f
    . HashMap.toList
    . G.getNodeAttrMap
    $ cfg ^. #graph
    where
      f (_, b) = (b, b)

  removeEdge edge = over #graph $ G.removeEdge (asIdNode <$> edge)
  removeNode node = over #graph $ G.removeNode (asIdNode node)
  addNodes nodes = over #graph $
    G.addNodesWithAttrs (asAttrTuple <$> nodes)
  addNodesWithAttrs nodes = over #graph $
    G.addNodesWithAttrs (over _1 asIdNode <$> nodes)
  addEdge lblEdge = over #graph $
    G.addEdge (asIdNode <$> lblEdge)
    . G.addNodesWithAttrs (asAttrTuple <$> toList lblEdge)

  hasNode node = G.hasNode (asIdNode node) . view #graph
  transpose = over #graph G.transpose
  bfs startNodes cfg = fmap (fmap $ getFullNode cfg)
    . G.bfs (asIdNode <$> startNodes)
    . view #graph
    $ cfg

  -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
  subgraph pred cfg =
    cfg & #graph %~ G.subgraph (pred . getFullNode cfg)

  reachable n cfg = fmap (getFullNode cfg)
    . G.reachable (asIdNode n)
    $ cfg ^. #graph

asAttrTuple :: CfNode a -> (CfNode (), CfNode a)
asAttrTuple x = (asIdNode x, x)

getFullNode' :: ControlFlowGraph a -> CfNode () -> Maybe (CfNode a)
getFullNode' g n = G.getNodeAttr n g

getFullNode :: Cfg a -> CfNode () -> CfNode a
getFullNode g = fromJust . getFullNode' (g ^. #graph)

getFullEdge :: (Hashable a, Eq a) => Cfg a -> G.Edge (CfNode a) -> Maybe (CfEdge a)
getFullEdge cfg e = CfEdge (e ^. #src) (e ^. #dst) <$> G.getEdgeLabel e cfg

toFullNodeSet :: (Hashable a, Eq a) => Cfg a -> HashSet (CfNode ()) -> HashSet (CfNode a)
toFullNodeSet g = HashSet.map $ getFullNode g

-- TODO: move this to Graph class
predEdges :: (Hashable a, Eq a) => CfNode a -> Cfg a -> HashSet (CfEdge a)
predEdges n cfg = HashSet.map (\pred -> fromJust . getFullEdge cfg $ G.Edge pred n)
  . G.preds n
  $ cfg

-- TODO: move this to Graph class
succEdges :: (Hashable a, Eq a) => CfNode a -> Cfg a -> HashSet (CfEdge a)
succEdges n cfg = HashSet.map (fromJust . getFullEdge cfg . G.Edge n)
  . G.succs n
  $ cfg

edges :: (Hashable a, Eq a) => Cfg a -> [CfEdge a]
edges = fmap fromLEdge . G.edges

addEdge :: (Hashable a, Eq a) => CfEdge a -> Cfg a -> Cfg a
addEdge e = G.addEdge $ toLEdge e

addEdges :: (Hashable a, Eq a) => [CfEdge a] -> Cfg a -> Cfg a
addEdges xs cfg = foldl' (flip addEdge) cfg xs

-- | Substitute a node with another CFG.
substNode :: forall a. (Eq a, Hashable a) => Cfg a -> CfNode a -> Cfg a -> CfNode a -> Cfg a
substNode
  outerCfg@(Cfg _ outerRoot)
  node
  innerCfg@(Cfg _ innerRoot)
  exitNode' =
    -- Check if the node we are substituting is the outer CFG's root
    if asIdNode outerRoot /= asIdNode node
       then newCfg & #root .~ outerRoot
       else newCfg & #root .~ innerRoot
   where
    -- TODO: Improve Graph API for fetching edges
    predEdges' :: [CfEdge a]
    predEdges' = HashSet.toList $ predEdges node outerCfg
    succEdges' :: [CfEdge a]
    succEdges' = HashSet.toList $ succEdges node outerCfg

    newPredEdges :: [CfEdge a]
    newPredEdges = set #dst innerRoot <$> predEdges'
    newSuccEdges :: [CfEdge a]
    newSuccEdges = set #src exitNode' <$> succEdges'
    newCfg :: Cfg a
    newCfg =
      G.removeNode node
      . G.addNodes (HashSet.toList $ Cfg.nodes innerCfg)
      . addEdges (edges innerCfg)
      . addEdges newPredEdges
      . addEdges newSuccEdges $ outerCfg
