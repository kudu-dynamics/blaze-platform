{- | GroupingCfg is a Cfg that can contain groups of nodes as a single block
This module provides functionality for grouping, ungrouping, and regrouping.
-}

{-# LANGUAGE ViewPatterns #-}

module Blaze.Types.Cfg.Grouping
  ( module Blaze.Types.Cfg.Grouping
  , module Exports
  , G.succs
  , G.preds
  , G.nodes
  , G.removeNode
  , G.addNodes
  , G.hasNode
  , G.transpose
  , G.bfs
  , G.sinks
  )
where

import Blaze.Prelude

import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Cfg as Exports (BranchType, BasicBlockNode(BasicBlockNode), CallNode(CallNode), EnterFuncNode(EnterFuncNode), LeaveFuncNode(LeaveFuncNode))
import qualified Blaze.Graph as G
import Blaze.Types.Graph (Dominators, PostDominators)
import Blaze.Types.Graph.Alga (AlgaGraph (AlgaGraph))
import Blaze.Types.Pil.Common ( Ctx )
import Blaze.Types.Pil (Stmt)
import Blaze.Util.Spec (mkDummyCtx, mkUuid1)

import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Bitraversable (bifor)
import Control.Lens (set)


type PilNode = CfNode [Stmt]
type PilEdge = CfEdge [Stmt]
type PilCallNode = Cfg.CallNode [Stmt]
type PilBbNode = Cfg.BasicBlockNode [Stmt]
type PilNodeMapEntry = (PilNode, [Stmt])
type PilCfg = Cfg [Stmt]

-- | A node type that represents a "grouped" sub-CFG within a larger CFG
data GroupingNode a = GroupingNode
  { termNode :: CfNode a
  , uuid :: UUID
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

-- | Record of the group layouts, including inner groups of groups.
type GroupingTree = [GroupSpec]

-- | Root dominator and term post-dominator nodes for group, including inner groups.
-- root and term are stored as non-group Cfg CfNode so that we don't point to groups
data GroupSpec = GroupSpec
  { groupRoot :: Cfg.CfNode ()
  , groupTerm :: Cfg.CfNode ()
  , innerGroups :: GroupingTree
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
  deriving anyclass (Hashable)

-- | Translate a flat 'Cfg.CfNode' to its grouped 'CfNode' equivalent
fromCfNode :: Cfg.CfNode a -> CfNode a
fromCfNode (Cfg.BasicBlock n) = BasicBlock n
fromCfNode (Cfg.Call n) = Call n
fromCfNode (Cfg.EnterFunc n) = EnterFunc n
fromCfNode (Cfg.LeaveFunc n) = LeaveFunc n

-- | Translate a grouped 'CfNode' to its flat 'Cfg.CfNode' equivalent, but
-- return 'Nothing' if it is a 'Grouping'
toCfNodeMaybe :: CfNode a -> Maybe (Cfg.CfNode a)
toCfNodeMaybe (BasicBlock n) = Just (Cfg.BasicBlock n)
toCfNodeMaybe (Call n) = Just (Cfg.Call n)
toCfNodeMaybe (EnterFunc n) = Just (Cfg.EnterFunc n)
toCfNodeMaybe (LeaveFunc n) = Just (Cfg.LeaveFunc n)
toCfNodeMaybe (Grouping _) = Nothing

-- | Translate a flat 'Cfg.CfEdge' to its grouped 'CfEdge' equivalent
fromCfEdge :: Cfg.CfEdge a -> CfEdge a
fromCfEdge (Cfg.CfEdge s d bt) = CfEdge (fromCfNode s) (fromCfNode d) bt

-- | Translate a grouped 'CfEdge' to its flat 'Cfg.CfEdge' equivalent, but
-- return 'Nothing' if either of its vertices are a 'Grouping'
toCfEdgeMaybe :: CfEdge a -> Maybe (Cfg.CfEdge a)
toCfEdgeMaybe (CfEdge s d bt) = Cfg.CfEdge <$> toCfNodeMaybe s <*> toCfNodeMaybe d <*> Just bt

fromLEdge :: G.LEdge Cfg.BranchType (CfNode a) -> CfEdge a
fromLEdge (G.LEdge bt (G.Edge s d)) = CfEdge s d bt

toLEdge :: CfEdge a -> G.LEdge Cfg.BranchType (CfNode a)
toLEdge (CfEdge a b bt) = G.LEdge bt (G.Edge a b)

type ControlFlowGraph a = AlgaGraph Cfg.BranchType (CfNode a) (CfNode ())

asIdNode :: CfNode a -> CfNode ()
asIdNode = void

asIdEdge :: CfEdge a -> CfEdge ()
asIdEdge = void

getFullNodeMay :: Cfg a -> CfNode () -> Maybe (CfNode a)
getFullNodeMay g = getFullNode' $ g ^. #graph

getNodeData :: CfNode a -> Maybe a
getNodeData = \case
  BasicBlock x -> Just $ x ^. #nodeData
  Call x -> Just $ x ^. #nodeData
  EnterFunc x -> Just $ x ^. #nodeData
  LeaveFunc x -> Just $ x ^. #nodeData
  Grouping _ -> Nothing

setNodeData :: (Hashable a, Eq a) => a -> CfNode a -> Cfg a -> Cfg a
setNodeData a n = G.setNodeAttr (fmap (const a) n) n

getNodeUUID :: CfNode a -> UUID
getNodeUUID = \case
  BasicBlock x -> x ^. #uuid
  Call x -> x ^. #uuid
  EnterFunc x -> x ^. #uuid
  LeaveFunc x -> x ^. #uuid
  Grouping x -> x ^. #uuid

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

-- | Transform a flat 'Cfg.Cfg' into a grouped 'Cfg' with no 'Grouping' nodes
fromCfg :: Cfg.Cfg a -> Cfg a
fromCfg (Cfg.Cfg (AlgaGraph adjs edata ndata) root_) =
  Cfg
    (AlgaGraph
      (AM.gmap fromCfNode adjs)
      (HashMap.mapKeys (fmap fromCfNode) edata)
      (HashMap.map fromCfNode . HashMap.mapKeys fromCfNode $ ndata))
    (fromCfNode root_)

-- | Find the initial, non-'Grouping' node inside a node @n@, recursively
initialNode ::
  -- | A node @n@
  CfNode a ->
  Cfg.CfNode a
initialNode (BasicBlock n) = Cfg.BasicBlock n
initialNode (Call n) = Cfg.Call n
initialNode (EnterFunc n) = Cfg.EnterFunc n
initialNode (LeaveFunc n) = Cfg.LeaveFunc n
initialNode (Grouping (GroupingNode _ _ (Cfg _ root_))) = initialNode root_

-- | Find the final, non-'Grouping' node inside a node @n@, recursively
terminalNode ::
  -- | A node @n@
  CfNode a ->
  Cfg.CfNode a
terminalNode (BasicBlock n) = Cfg.BasicBlock n
terminalNode (Call n) = Cfg.Call n
terminalNode (EnterFunc n) = Cfg.EnterFunc n
terminalNode (LeaveFunc n) = Cfg.LeaveFunc n
terminalNode (Grouping (GroupingNode exit _ _)) = terminalNode exit

-- | Recursively unfolds all 'Grouping' nodes in the 'Cfg', creating a flat
-- 'Cfg.Cfg'. Also, summarize the recursive structure of 'Grouping' nodes into a
-- nested 'GroupingTree'
unfoldGroups :: (Eq a, Hashable a) => Cfg a -> (Cfg.Cfg a, GroupingTree)
unfoldGroups = first (fromJust . toCfgMaybe) . expandAll
  where
    expandAll :: (Eq a, Hashable a) => Cfg a -> (Cfg a, GroupingTree)
    expandAll cfg =
      second sort $
        foldl'
        (\(cfg', groupSpecs) n ->
          case expandNode n of
            Nothing -> (cfg', groupSpecs)
            Just (exit, subcfg, groupSpec) -> (substNode cfg' n subcfg exit, groupSpec : groupSpecs))
        (cfg, [])
        (fmap snd . HashMap.toList $ cfg ^. #graph . #nodeAttrMap)

    expandNode :: (Eq a, Hashable a) => CfNode a -> Maybe (CfNode a, Cfg a, GroupSpec)
    expandNode =
      \case
        (BasicBlock _) -> Nothing
        (Call _) -> Nothing
        (EnterFunc _) -> Nothing
        (LeaveFunc _) -> Nothing
        n@(Grouping (GroupingNode exit _ sub)) ->
          let (subExpanded, groupingTree) = expandAll sub in
            Just
              ( fromCfNode $ terminalNode exit
              , subExpanded
              , GroupSpec
                  { groupRoot = initialNode (asIdNode n)
                  , groupTerm = terminalNode (asIdNode exit)
                  , innerGroups = groupingTree
                  }
              )

-- | Fold all groups specified in the 'GroupingTree', recursively
foldGroups :: forall a. (Eq a, Hashable a) => Cfg.Cfg a -> GroupingTree -> Cfg a
foldGroups = foldMany . fromCfg
  where
    foldMany :: Cfg a -> GroupingTree -> Cfg a
    foldMany = foldl' foldSubtree
    foldSubtree :: Cfg a -> GroupSpec -> Cfg a
    foldSubtree cfg (GroupSpec enter exit gss) =
      let cfg' = foldMany cfg gss
      in
        foldOneGroup enter exit cfg'

-- | Fold away one group in the CFG
foldOneGroup ::
  forall a.
  (Eq a, Hashable a) =>
  -- | First (enter) node in group
  Cfg.CfNode () ->
  -- | Last (exit) node in group
  Cfg.CfNode () ->
  Cfg a ->
  Cfg a
foldOneGroup enter exit cfg =
  case (enterCand, exitCand) of
    (Just enterFound, Just exitFound) ->
      makeGrouping enterFound exitFound cfg
    (_, _) -> cfg
  where
    enterCand = findCand enter
    exitCand = findCand exit
    findCand :: Cfg.CfNode () -> Maybe (CfNode a)
    findCand (fromCfNode -> n) =
      find ((== getNodeUUID n) . getNodeUUID)
      . HashMap.elems
      $ cfg ^. #graph . #nodeAttrMap


-- | Transforms a grouped 'Cfg' into a flat 'Cfg.Cfg' only if the original 'Cfg'
-- was essentially ungrouped, i.e., it contained no 'Grouping' nodes
toCfgMaybe :: Cfg a -> Maybe (Cfg.Cfg a)
toCfgMaybe (Cfg (AlgaGraph adjs edata ndata) root_) =
  Cfg.Cfg
    <$> (AlgaGraph
           <$> (AM.edges <$> traverse (\p -> bifor p toCfNodeMaybe toCfNodeMaybe) (AM.edgeList adjs))
           <*> (HashMap.fromList <$> traverse (\p -> bifor p (traverse toCfNodeMaybe) Just) (HashMap.toList edata))
           <*> (HashMap.fromList <$> traverse (\p -> bifor p toCfNodeMaybe toCfNodeMaybe) (HashMap.toList ndata)))
    <*> toCfNodeMaybe root_

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
  subgraph pred' cfg =
    cfg & #graph %~ G.subgraph (pred' . getFullNode cfg)

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
predEdges n cfg = HashSet.map (\pred' -> fromJust . getFullEdge cfg $ G.Edge pred' n)
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



------------

mkDummyTermNode :: Ctx -> a -> CfNode a
mkDummyTermNode ctx nodeData 
  = BasicBlock
    $ Cfg.BasicBlockNode
    { ctx = ctx
    , start = 0
    , end = 0
    , uuid = mkUuid1 (0 :: Int)
    , nodeData = nodeData
    }

getPostDominatorsAsIdNodes_ :: CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes_ dummyTermNode dummyBranchType cfg = G.getPostDominators dummyTermNode dummyBranchType (cfg ^. #graph)

getPostDominatorsAsIdNodes :: Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes = getPostDominatorsAsIdNodes_ dummyTermNode Cfg.UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

getPostDominators_ :: (Hashable a, Eq a) => CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode a)
getPostDominators_ dummyTermNode dummyBranchType cfg
  = G.domMap (getFullNode cfg)
  . G.getPostDominators dummyTermNode dummyBranchType
  $ cfg ^. #graph

getPostDominators :: (Hashable a, Eq a) => Cfg a -> PostDominators (CfNode a)
getPostDominators = getPostDominators_ dummyTermNode Cfg.UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

getDominators_ :: Cfg a -> Dominators (CfNode ())
getDominators_ cfg = G.getDominators (asIdNode $ cfg ^. #root) (cfg ^. #graph)

getDominators :: (Hashable a, Eq a) => Cfg a -> Dominators (CfNode a)
getDominators cfg = G.domMap (getFullNode cfg)
  $ G.getDominators (asIdNode $ cfg ^. #root) (cfg ^. #graph)


---------


-- | Returns the set of nodes that could be group terminals, given the start node.
-- A group terminal must be a post dominator of the start node and dominated by it.
getPossibleGroupTerms
  :: forall a. (Hashable a, Eq a)
  => CfNode a
  -- ^ start node
  -> Cfg a
  -> HashSet (CfNode a)
getPossibleGroupTerms startNode cfg = case mpdoms of
  Nothing -> HashSet.empty
  Just pdoms -> HashSet.fromList . filter dominatedByStartNode $ pdoms  
  where
    domMapping = getDominators cfg
    mpdoms :: Maybe [CfNode a]
    mpdoms = fmap HashSet.toList
      . G.domLookup startNode
      $ getPostDominators cfg

    dominatedByStartNode :: CfNode a -> Bool
    dominatedByStartNode candidateTerm = fromMaybe False $ do
      doms <- G.domLookup candidateTerm domMapping
      return $ HashSet.member startNode doms


-- | Gets all nodes dominated by a start node and post-dominated by and end node
findNodesInGroup
  :: (Hashable a, Eq a)
  => CfNode a
  -- ^ start (dominating) node
  -> CfNode a
  -- ^ end (post-dominating) node
  -> Cfg a
  -> HashSet (CfNode a)
findNodesInGroup startNode endNode cfg = HashSet.filter isDoubleDominated . G.nodes $ cfg
  where
    domLookup' :: (G.DominatorMapping m, Eq a, Hashable a)
               => a
               -> m a
               -> HashSet a
    domLookup' n = fromMaybe HashSet.empty . G.domLookup n
    
    domMapping = getDominators cfg
    pdomMapping = getPostDominators cfg

    isDoubleDominated n = HashSet.member startNode (domLookup' n domMapping)
      && HashSet.member endNode (domLookup' n pdomMapping)


extractGroupingNode
  :: forall a. (Eq a, Hashable a)
  => CfNode a
  -> CfNode a
  -> HashSet (CfNode a)
  -> Cfg a
  -> GroupingNode a
extractGroupingNode startNode endNode groupNodes cfg
  = GroupingNode endNode (getNodeUUID startNode)
  $ mkCfg startNode (HashSet.toList nodes') edges'
  where
    allNodes = HashSet.fromList [startNode, endNode] <> groupNodes
    nodes' = HashSet.delete startNode groupNodes
    
    edges' = filter containsOnlyGroupNodes . edges $ cfg

    containsOnlyGroupNodes :: CfEdge a -> Bool
    containsOnlyGroupNodes (CfEdge a b _) =
      HashSet.member a allNodes && HashSet.member b allNodes


-- | Given a start and terminal node for a grouping, this function finds all
-- nodes within the group and sticks them into their own CFG.
makeGrouping
  :: forall a. (Hashable a, Eq a)
  => CfNode a
  -- ^ start node
  -> CfNode a
  -- ^ terminal node
  -> Cfg a
  -> Cfg a
makeGrouping startNode endNode cfg = cfg'
  where
    innerNodes = HashSet.difference (findNodesInGroup startNode endNode cfg) (HashSet.fromList [startNode, endNode])

    allGroupNodes = HashSet.fromList [startNode, endNode] <> innerNodes

    groupNode :: CfNode a
    groupNode = Grouping $ extractGroupingNode startNode endNode innerNodes cfg

    containsGroupNodes :: (Bool -> Bool -> Bool) -> CfEdge a -> Bool
    containsGroupNodes comb (CfEdge a b _) =
      HashSet.member a allGroupNodes `comb` HashSet.member b allGroupNodes

    containsOnlyGroupNodes :: CfEdge a -> Bool
    containsOnlyGroupNodes = containsGroupNodes (&&)

    containsAnyGroupNodes :: CfEdge a -> Bool
    containsAnyGroupNodes = containsGroupNodes (||)

    nonGroupNodes = HashSet.difference (G.nodes cfg) allGroupNodes

    -- edges that aren't inside the group
    nonGroupEdges = filter (not . containsAnyGroupNodes)
      . fmap fromLEdge
      $ G.edges cfg

    -- Both outgoing and incoming edges, where the startNode and endNode
    -- are replaced with the group node. This handles looping groups.
    exteriorGroupEdges :: [CfEdge a]
    exteriorGroupEdges
      = fmap (\(CfEdge a b lbl) -> CfEdge (substStartEnd a) (substStartEnd b) lbl)
      . HashSet.toList
      $ predEdges startNode cfg <> succEdges endNode cfg
      where
        substStartEnd :: CfNode a -> CfNode a
        substStartEnd e = bool e groupNode $ e == startNode || e == endNode

    newEdges :: [CfEdge a]
    newEdges = nonGroupEdges <> exteriorGroupEdges

    cfg' = if cfg ^. #root == groupNode
      then mkCfg groupNode
            (HashSet.toList nonGroupNodes)
            newEdges
      else mkCfg (cfg ^. #root)
            (HashSet.toList $ nonGroupNodes <> HashSet.singleton groupNode)
            newEdges


---------- from Analysis

getNodesContainingAddress :: (Eq a, Hashable a) => Address -> Cfg a -> HashSet (CfNode a)
getNodesContainingAddress addr = HashSet.filter containsAddr . G.nodes
  where
    -- TODO: confirm that bb range is [start, end)
    containsAddr (BasicBlock bb) = bb ^. #start <= addr && addr <= bb ^. #end
    containsAddr (Call n) = n ^. #start == addr
    containsAddr (EnterFunc _) = False
    containsAddr (LeaveFunc _) = False
    containsAddr (Grouping n) = not . HashSet.null . getNodesContainingAddress addr $ n ^. #grouping
