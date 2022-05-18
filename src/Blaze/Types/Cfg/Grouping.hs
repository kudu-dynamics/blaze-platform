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
import qualified Blaze.Cfg as Cfg
import qualified Data.HashSet as HashSet

-- | Record of the group layouts, including inner groups of groups.
type GroupingTree a = [GroupSpec a]

-- | Root dominator and term post-dominator nodes for group, including inner groups.
-- root and term are stored as non-group Cfg CfNode so that we don't point to groups.
-- The 'a' type parameter is presumed to be 'Foldable' such that the 'a' values associated
-- with every node in a group can be folded into a single 'groupData' field of 'a'.
data GroupSpec a = GroupSpec
  { groupRoot :: CfNode a
  , groupTerm :: CfNode a
  , innerGroups :: GroupingTree a
  , groupData :: a
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
  deriving anyclass (Hashable)

-- | Find the initial, non-'Grouping' node inside a node @n@, recursively
initialNode ::
  -- | A node @n@
  CfNode a ->
  CfNode a
initialNode (BasicBlock n) = Cfg.BasicBlock n
initialNode (Call n) = Cfg.Call n
initialNode (EnterFunc n) = Cfg.EnterFunc n
initialNode (LeaveFunc n) = Cfg.LeaveFunc n
initialNode (Grouping (GroupingNode _ _ gCfg _)) = initialNode $ Cfg.getRootNode gCfg

-- | Find the final, non-'Grouping' node inside a node @n@, recursively
terminalNode ::
  -- | A node @n@
  CfNode a ->
  CfNode a
terminalNode (BasicBlock n) = Cfg.BasicBlock n
terminalNode (Call n) = Cfg.Call n
terminalNode (EnterFunc n) = Cfg.EnterFunc n
terminalNode (LeaveFunc n) = Cfg.LeaveFunc n
terminalNode (Grouping (GroupingNode exit _ _ _)) = terminalNode exit

-- | Recursively unfolds all 'Grouping' nodes in the 'Cfg', creating a flat
-- 'Cfg'. Also, summarize the recursive structure of 'Grouping' nodes into a
-- nested 'GroupingTree'
unfoldGroups :: forall a. (Ord a, Hashable a) => Cfg (CfNode a) -> (Cfg (CfNode a), GroupingTree a)
unfoldGroups = expandAll
 where
  expandAll cfg =
    second sort $
      foldl'
        ( \(cfg', groupSpecs) n ->
            case expandNode n of
              Nothing -> (cfg', groupSpecs)
              Just (exit, subcfg, groupSpec) -> (Cfg.substNode cfg' n subcfg exit, groupSpec : groupSpecs)
        )
        (cfg, [])
        (G.nodes cfg)

  expandNode :: CfNode a -> Maybe (CfNode a, Cfg (CfNode a), GroupSpec a)
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
                  { groupRoot = initialNode n
                  , groupTerm = terminalNode exit
                  , innerGroups = groupingTree
                  , groupData = gData
                  }
              )

-- | Fold all groups specified in the 'GroupingTree', recursively
foldGroups :: 
  forall a. Hashable a =>
  Cfg (CfNode [a]) -> GroupingTree [a] -> Cfg (CfNode [a])
foldGroups = foldMany
  where
    foldMany :: Cfg (CfNode [a]) -> GroupingTree [a] -> Cfg (CfNode [a])
    foldMany = foldl' foldSubtree
    foldSubtree :: Cfg (CfNode [a]) -> GroupSpec [a] -> Cfg (CfNode [a])
    foldSubtree cfg (GroupSpec enter exit gss gdata) =
      let cfg' = foldMany cfg gss
      in
        foldOneGroup enter exit cfg' gdata

-- | Fold away one group in the CFG
foldOneGroup ::
  forall a.
  Hashable a =>
  -- | First (enter) node in group
  CfNode [a] ->
  -- | Last (exit) node in group
  CfNode [a] ->
  Cfg (CfNode [a]) ->
  [a] ->
  Cfg (CfNode [a])
foldOneGroup enter exit cfg nData =
  case (enterCand, exitCand) of
    (Just enterFound, Just exitFound) ->
      makeGrouping enterFound exitFound cfg nData
    (_, _) -> cfg
  where
    enterCand = Cfg.getNode cfg $ G.getNodeId enter
    exitCand = Cfg.getNode cfg $ G.getNodeId exit

expandGroupingNode :: Hashable a => GroupingNode a -> Cfg (CfNode a) -> Cfg (CfNode a)
expandGroupingNode n cfg = Cfg.substNode cfg (Grouping n) (n ^. #grouping) (n ^. #termNode)

-- | Returns the set of nodes that could be group terminals, given the start node.
-- A group terminal must be a post dominator of the start node and dominated by it.
getPossibleGroupTerms
  :: forall a. Hashable a
  => CfNode [a]
  -- ^ start node
  -> Cfg (CfNode [a])
  -> HashSet (CfNode [a])
getPossibleGroupTerms startNode cfg = case mpdoms of
  Nothing -> HashSet.empty
  Just pdoms ->
    HashSet.fromList
      . filter (groupIsClosed cfg startNode)
      . filter dominatedByStartNode
      $ pdoms
  where
    domMapping = Cfg.getDominators cfg
    mpdoms :: Maybe [CfNode [a]]
    mpdoms = fmap HashSet.toList
      . G.domLookup startNode
      $ Cfg.getPostDominators cfg

    dominatedByStartNode :: CfNode [a] -> Bool
    dominatedByStartNode candidateTerm = fromMaybe False $ do
      doms <- G.domLookup candidateTerm domMapping
      return $ HashSet.member startNode doms

groupIsClosed ::
  Hashable a =>
  Cfg (CfNode [a]) ->
  -- | start node
  CfNode [a] ->
  -- | end node
  CfNode [a] ->
  Bool
groupIsClosed cfg start end =
  all (\n -> (n == start || predsClosed n) && (n == end || succsClosed n)) g
  where
    g = HashSet.union (HashSet.fromList [start, end]) $ findNodesInGroup start end cfg
    succsClosed n = all (`HashSet.member` g) (G.succs n cfg)
    predsClosed n = all (`HashSet.member` g) (G.preds n cfg)

-- | Gets all nodes dominated by a start node and post-dominated by and end node
findNodesInGroup
  :: Hashable a
  => CfNode [a]
  -- ^ start (dominating) node
  -> CfNode [a]
  -- ^ end (post-dominating) node
  -> Cfg (CfNode [a])
  -> HashSet (CfNode [a])
findNodesInGroup startNode endNode cfg = HashSet.filter isDoubleDominated . G.nodes $ cfg
  where
    domLookup' :: (G.DominatorMapping m, Hashable a)
               => a
               -> m a
               -> HashSet a
    domLookup' n = fromMaybe HashSet.empty . G.domLookup n

    domMapping = Cfg.getDominators cfg
    pdomMapping = Cfg.getPostDominators cfg

    isDoubleDominated n = HashSet.member startNode (domLookup' n domMapping)
      && HashSet.member endNode (domLookup' n pdomMapping)

{- HLINT ignore extractGroupingNode "Eta reduce" -}
extractGroupingNode ::
  forall a.
  Hashable a =>
  CfNode a ->
  CfNode a ->
  HashSet (CfNode a) ->
  Cfg (CfNode a) ->
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

  containsOnlyGroupNodes :: CfEdge (CfNode a) -> Bool
  containsOnlyGroupNodes (CfEdge src dst _) =
    HashSet.member src allNodes && HashSet.member dst allNodes

-- | Given a start and terminal node for a grouping, this function finds all
-- nodes within the group and sticks them into their own CFG.
makeGrouping
  :: forall a. Hashable a
  => CfNode [a]
  -- ^ start node
  -> CfNode [a]
  -- ^ terminal node
  -> Cfg (CfNode [a])
  -> [a]
  -> Cfg (CfNode [a])
makeGrouping startNode endNode cfg nData = cfg'
  where
    innerNodes = HashSet.difference (findNodesInGroup startNode endNode cfg) (HashSet.fromList [startNode, endNode])

    allGroupNodes = HashSet.fromList [startNode, endNode] <> innerNodes

    groupNode :: CfNode [a]
    groupNode = Grouping $ extractGroupingNode startNode endNode innerNodes cfg nData

    containsGroupNodes :: (Bool -> Bool -> Bool) -> CfEdge (CfNode [a]) -> Bool
    containsGroupNodes comb (CfEdge src dst _) =
      HashSet.member src allGroupNodes `comb` HashSet.member dst allGroupNodes

    -- containsOnlyGroupNodes :: CfEdge a -> Bool
    -- containsOnlyGroupNodes = containsGroupNodes (&&)

    containsAnyGroupNodes :: CfEdge (CfNode [a]) -> Bool
    containsAnyGroupNodes = containsGroupNodes (||)

    srcOutside = containsGroupNodes (\src _ -> not src)
    dstOutside = containsGroupNodes (\_ dst -> not dst)

    nonGroupNodes = HashSet.difference (G.nodes cfg) allGroupNodes

    -- edges that aren't inside the group
    nonGroupEdges = filter (not . containsAnyGroupNodes)
      . fmap Cfg.fromLEdge
      $ G.edges cfg

    -- Both outgoing and incoming edges, where the startNode and endNode
    -- are replaced with the group node. This handles looping groups.
    exteriorGroupEdges :: [CfEdge (CfNode [a])]
    exteriorGroupEdges
      = fmap (\(CfEdge src dst lbl) -> CfEdge (substStartEnd src) (substStartEnd dst) lbl)
      . HashSet.toList
      $ ( HashSet.filter srcOutside (Cfg.predEdges startNode cfg)
          <>
          HashSet.filter dstOutside (Cfg.succEdges endNode cfg)
        )
      where
        substStartEnd :: CfNode [a] -> CfNode [a]
        substStartEnd e = bool e groupNode $ e == startNode || e == endNode

    newEdges :: [CfEdge (CfNode [a])]
    newEdges = nonGroupEdges <> exteriorGroupEdges

    nextCtxIndex' = cfg ^. #nextCtxIndex
    rootNode = Cfg.getRootNode cfg
    cfg' = if rootNode == startNode
      then Cfg.mkCfg nextCtxIndex' groupNode
            (HashSet.toList nonGroupNodes)
            newEdges
      else Cfg.mkCfg nextCtxIndex' rootNode
            (HashSet.toList $ nonGroupNodes <> HashSet.singleton groupNode)
            newEdges