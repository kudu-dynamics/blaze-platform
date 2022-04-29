{- | GroupingCfg is a Cfg that can contain groups of nodes as a single block
This module provides functionality for grouping, ungrouping, and regrouping.
-}

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

import qualified Blaze.Graph as G
import Blaze.Types.Cfg as Exports (
  BasicBlockNode (BasicBlockNode),
  BranchType,
  CallNode (CallNode),
  EnterFuncNode (EnterFuncNode),
  LeaveFuncNode (LeaveFuncNode), CfNode (BasicBlock, Call, EnterFunc, LeaveFunc, Grouping), GroupingNode (GroupingNode), Cfg (Cfg), CfgTransport (CfgTransport), CfEdge (CfEdge), ControlFlowGraph
 )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Graph (Dominators, PostDominators)
import Blaze.Types.Pil.Common (Ctx)
import Blaze.Util.Spec (mkDummyCtx, mkUuid1)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Lens (set)

-- | Record of the group layouts, including inner groups of groups.
type GroupingTree a = [GroupSpec a]

-- | Root dominator and term post-dominator nodes for group, including inner groups.
-- root and term are stored as non-group Cfg CfNode so that we don't point to groups
data GroupSpec a = GroupSpec
  { groupRoot :: CfNode ()
  , groupTerm :: CfNode ()
  , innerGroups :: GroupingTree a
  , groupData :: a
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
  deriving anyclass (Hashable)

-- | Find the initial, non-'Grouping' node inside a node @n@, recursively
initialNode ::
  -- | A node @n@
  CfNode a ->
  Cfg.CfNode a
initialNode (BasicBlock n) = Cfg.BasicBlock n
initialNode (Call n) = Cfg.Call n
initialNode (EnterFunc n) = Cfg.EnterFunc n
initialNode (LeaveFunc n) = Cfg.LeaveFunc n
initialNode (Grouping (GroupingNode _ _ (Cfg _ root_ _nextCtxIndex) _)) = initialNode root_

-- | Find the final, non-'Grouping' node inside a node @n@, recursively
terminalNode ::
  -- | A node @n@
  CfNode a ->
  Cfg.CfNode a
terminalNode (BasicBlock n) = Cfg.BasicBlock n
terminalNode (Call n) = Cfg.Call n
terminalNode (EnterFunc n) = Cfg.EnterFunc n
terminalNode (LeaveFunc n) = Cfg.LeaveFunc n
terminalNode (Grouping (GroupingNode exit _ _ _)) = terminalNode exit

-- | Recursively unfolds all 'Grouping' nodes in the 'Cfg', creating a flat
-- 'Cfg.Cfg'. Also, summarize the recursive structure of 'Grouping' nodes into a
-- nested 'GroupingTree'
unfoldGroups :: forall a. (Ord a, Hashable a) => Cfg a -> (Cfg a, GroupingTree a)
unfoldGroups = expandAll
 where
  expandAll cfg =
    second sort $
      foldl'
        ( \(cfg', groupSpecs) n ->
            case expandNode n of
              Nothing -> (cfg', groupSpecs)
              Just (exit, subcfg, groupSpec) -> (substNode cfg' n subcfg exit, groupSpec : groupSpecs)
        )
        (cfg, [])
        (fmap snd . HashMap.toList $ cfg ^. #graph . #nodeAttrMap)

  expandNode :: CfNode a -> Maybe (CfNode a, Cfg a, GroupSpec a)
  expandNode =
    \case
      (BasicBlock _) -> Nothing
      (Call _) -> Nothing
      (EnterFunc _) -> Nothing
      (LeaveFunc _) -> Nothing
      n@(Grouping (GroupingNode exit _ sub gData)) ->
        let (subExpanded, groupingTree) = expandAll sub
         in Just
              ( terminalNode exit
              , subExpanded
              , GroupSpec
                  { groupRoot = initialNode (Cfg.asIdNode n)
                  , groupTerm = terminalNode (Cfg.asIdNode exit)
                  , innerGroups = groupingTree
                  , groupData = gData
                  }
              )

-- | Fold all groups specified in the 'GroupingTree', recursively
foldGroups :: forall a. (Eq a, Hashable a) => Cfg a -> GroupingTree a -> Cfg a
foldGroups = foldMany
  where
    foldMany :: Cfg a -> GroupingTree a -> Cfg a
    foldMany = foldl' foldSubtree
    foldSubtree :: Cfg a -> GroupSpec a -> Cfg a
    foldSubtree cfg (GroupSpec enter exit gss gdata) =
      let cfg' = foldMany cfg gss
      in
        foldOneGroup enter exit cfg' gdata

-- | Fold away one group in the CFG
foldOneGroup ::
  forall a.
  (Eq a, Hashable a) =>
  -- | First (enter) node in group
  Cfg.CfNode () ->
  -- | Last (exit) node in group
  Cfg.CfNode () ->
  Cfg a ->
  a ->
  Cfg a
foldOneGroup enter exit cfg nData =
  case (enterCand, exitCand) of
    (Just enterFound, Just exitFound) ->
      makeGrouping enterFound exitFound cfg nData
    (_, _) -> cfg
  where
    enterCand = findCand enter
    exitCand = findCand exit
    findCand :: CfNode () -> Maybe (CfNode a)
    findCand n =
      find ((== Cfg.getNodeUUID n) . Cfg.getNodeUUID)
      . HashMap.elems
      $ cfg ^. #graph . #nodeAttrMap

expandGroupingNode :: (Hashable a, Eq a) => GroupingNode a -> Cfg a -> Cfg a
expandGroupingNode n cfg = substNode cfg (Grouping n) (n ^. #grouping) (n ^. #termNode)

-- TODO: Determine if this can be merged with existing graph/node substitution code in other modules
-- | Substitute a node with another CFG.
substNode :: forall a. (Eq a, Hashable a) => Cfg a -> CfNode a -> Cfg a -> CfNode a -> Cfg a
substNode
  outerCfg@(Cfg _ outerRoot _)
  node
  innerCfg@(Cfg _ innerRoot _)
  exitNode' =
    -- Check if the node we are substituting is the outer CFG's root
    if Cfg.asIdNode outerRoot /= Cfg.asIdNode node
       then newCfg & #root .~ outerRoot
       else newCfg & #root .~ innerRoot
   where
    -- TODO: Improve Graph API for fetching edges
    predEdges' :: [CfEdge a]
    predEdges' = HashSet.toList $ Cfg.predEdges node outerCfg
    succEdges' :: [CfEdge a]
    succEdges' = HashSet.toList $ Cfg.succEdges node outerCfg

    newPredEdges :: [CfEdge a]
    newPredEdges = set #dst innerRoot <$> predEdges'
    newSuccEdges :: [CfEdge a]
    newSuccEdges = set #src exitNode' <$> succEdges'
    newCfg :: Cfg a
    newCfg =
      G.removeNode node
      . G.addNodes (HashSet.toList $ Cfg.nodes innerCfg)
      . Cfg.addEdges (Cfg.edges innerCfg)
      . Cfg.addEdges newPredEdges
      . Cfg.addEdges newSuccEdges $ outerCfg



------------

mkDummyTermNode :: Ctx -> a -> CfNode a
mkDummyTermNode ctx d
  = BasicBlock
    $ Cfg.BasicBlockNode
    { ctx = ctx
    , start = 0
    , end = 0
    , uuid = mkUuid1 (0 :: Int)
    , nodeData = d
    }

getPostDominatorsAsIdNodes_ :: CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes_ dummyTermNode dummyBranchType cfg = G.getPostDominators dummyTermNode dummyBranchType (cfg ^. #graph)

getPostDominatorsAsIdNodes :: Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes = getPostDominatorsAsIdNodes_ dummyTermNode Cfg.UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

getPostDominators_ :: (Hashable a, Eq a) => CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode a)
getPostDominators_ dummyTermNode dummyBranchType cfg
  = G.domMap (Cfg.getFullNode cfg)
  . G.getPostDominators dummyTermNode dummyBranchType
  $ cfg ^. #graph

getPostDominators :: (Hashable a, Eq a) => Cfg a -> PostDominators (CfNode a)
getPostDominators = getPostDominators_ dummyTermNode Cfg.UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

getDominators_ :: Cfg a -> Dominators (CfNode ())
getDominators_ cfg = G.getDominators (Cfg.asIdNode $ cfg ^. #root) (cfg ^. #graph)

getDominators :: (Hashable a, Eq a) => Cfg a -> Dominators (CfNode a)
getDominators cfg = G.domMap (Cfg.getFullNode cfg)
  $ G.getDominators (Cfg.asIdNode $ cfg ^. #root) (cfg ^. #graph)


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
  Just pdoms ->
    HashSet.fromList
      . filter (groupIsClosed cfg startNode)
      . filter dominatedByStartNode
      $ pdoms
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

groupIsClosed ::
  (Eq a, Hashable a) =>
  Cfg a ->
  -- | start node
  CfNode a ->
  -- | end node
  CfNode a ->
  Bool
groupIsClosed cfg start end =
  all (\n -> (n == start || predsClosed n) && (n == end || succsClosed n)) g
  where
    g = HashSet.union (HashSet.fromList [start, end]) $ findNodesInGroup start end cfg
    succsClosed n = all (`HashSet.member` g) (G.succs n cfg)
    predsClosed n = all (`HashSet.member` g) (G.preds n cfg)

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

{- HLINT ignore extractGroupingNode "Eta reduce" -}
extractGroupingNode ::
  forall a.
  (Eq a, Hashable a) =>
  CfNode a ->
  CfNode a ->
  HashSet (CfNode a) ->
  Cfg a ->
  a ->
  GroupingNode a
extractGroupingNode startNode endNode groupNodes cfg nData =
  GroupingNode
    endNode
    (Cfg.getNodeUUID startNode)
    (Cfg.mkCfg (cfg ^. #nextCtxIndex) startNode (HashSet.toList nodes') edges')
    nData
 where
  allNodes = HashSet.fromList [startNode, endNode] <> groupNodes
  nodes' = HashSet.delete startNode groupNodes

  edges' = filter containsOnlyGroupNodes . Cfg.edges $ cfg

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
  -> a
  -> Cfg a
makeGrouping startNode endNode cfg nData = cfg'
  where
    innerNodes = HashSet.difference (findNodesInGroup startNode endNode cfg) (HashSet.fromList [startNode, endNode])

    allGroupNodes = HashSet.fromList [startNode, endNode] <> innerNodes

    groupNode :: CfNode a
    groupNode = Grouping $ extractGroupingNode startNode endNode innerNodes cfg nData

    containsGroupNodes :: (Bool -> Bool -> Bool) -> CfEdge a -> Bool
    containsGroupNodes comb (CfEdge a b _) =
      HashSet.member a allGroupNodes `comb` HashSet.member b allGroupNodes

    -- containsOnlyGroupNodes :: CfEdge a -> Bool
    -- containsOnlyGroupNodes = containsGroupNodes (&&)

    containsAnyGroupNodes :: CfEdge a -> Bool
    containsAnyGroupNodes = containsGroupNodes (||)

    srcOutside = containsGroupNodes (\a _ -> not a)
    dstOutside = containsGroupNodes (\_ b -> not b)

    nonGroupNodes = HashSet.difference (G.nodes cfg) allGroupNodes

    -- edges that aren't inside the group
    nonGroupEdges = filter (not . containsAnyGroupNodes)
      . fmap Cfg.fromLEdge
      $ G.edges cfg

    -- Both outgoing and incoming edges, where the startNode and endNode
    -- are replaced with the group node. This handles looping groups.
    exteriorGroupEdges :: [CfEdge a]
    exteriorGroupEdges
      = fmap (\(CfEdge a b lbl) -> CfEdge (substStartEnd a) (substStartEnd b) lbl)
      . HashSet.toList
      $ ( HashSet.filter srcOutside (Cfg.predEdges startNode cfg)
          <>
          HashSet.filter dstOutside (Cfg.succEdges endNode cfg)
        )
      where
        substStartEnd :: CfNode a -> CfNode a
        substStartEnd e = bool e groupNode $ e == startNode || e == endNode

    newEdges :: [CfEdge a]
    newEdges = nonGroupEdges <> exteriorGroupEdges

    nextCtxIndex' = cfg ^. #nextCtxIndex

    cfg' = if cfg ^. #root == startNode
      then Cfg.mkCfg nextCtxIndex' groupNode
            (HashSet.toList nonGroupNodes)
            newEdges
      else Cfg.mkCfg nextCtxIndex' (cfg ^. #root)
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
