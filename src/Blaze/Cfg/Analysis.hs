{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude hiding (succ)
import Blaze.Types.Cfg (CfNode (BasicBlock), PilCfg, PilNode, PilEdge, BranchNode, CfEdge(CfEdge))
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt)
import qualified Data.Set as Set
import qualified Data.HashSet as HSet


copyProp :: InterCfg -> InterCfg
copyProp icfg =
  InterCfg $
    foldl'
      (flip $ Cfg.updateNodeData (PA._copyProp copyPropState))
      cfg
      (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap concat . Set.toList .  Cfg.nodes $ cfg
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: InterCfg -> InterCfg
constantProp icfg =
  InterCfg $
    foldl'
      (flip $ Cfg.updateNodeData (PA._constantProp constPropState))
      cfg
      (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap concat . Set.toList .  Cfg.nodes $ cfg
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts


getStmts :: InterCfg -> [[Stmt]]
getStmts (InterCfg cfg) =
  fmap concat . Set.toList $ Cfg.nodes cfg

prune :: InterCfg -> InterCfg
prune icfg =
  removeDeadNodes $ foldl' (flip cutEdge) icfg' deadBranches
 where
  icfg' :: InterCfg
  icfg' = constantProp . copyProp $ icfg
  deadBranches :: [PilEdge]
  deadBranches = getDeadBranches icfg'

getDeadBranches :: InterCfg -> [PilEdge]
getDeadBranches icfg =
  concat $ mapMaybe (getDeadBranchesForNode icfg) branchNodes
 where
  branchNodes :: [BranchNode [Stmt]]
  branchNodes = mapMaybe (Cfg.parseBranchNode Cfg.getNodeData)
    $ Set.toList (G.nodes (unInterCfg icfg))
  getDeadBranchesForNode :: InterCfg -> BranchNode [Stmt] -> Maybe [PilEdge]
  getDeadBranchesForNode _g branchNode = do
    constVal <- Cfg.evalCondition (branchNode ^. #branchCondOp)
    let constBranchType = if constVal then Cfg.TrueBranch else Cfg.FalseBranch
        cfg = unInterCfg icfg
        cfNode = BasicBlock $ branchNode ^. #basicBlock
        succs = Set.toList $ Cfg.succs cfNode cfg
        -- Probably only one since there's likely just a pair of edges,
        -- could change later
        deadSuccs =
          filter
            (\succ -> Just constBranchType /= G.getEdgeLabel (G.Edge cfNode succ) cfg)
            succs
    traverse
      ( \deadSucc ->
          CfEdge cfNode deadSucc <$> G.getEdgeLabel (G.Edge cfNode deadSucc) cfg
      )
      deadSuccs

removeDeadNodes :: InterCfg -> InterCfg
removeDeadNodes icfg =
  InterCfg $ foldl' (flip G.removeNode) (unInterCfg icfg) deadNodes
 where
  deadNodes = getDeadNodes $ unInterCfg icfg

getDeadNodes :: PilCfg -> [PilNode]
getDeadNodes cfg =
  HSet.toList $ HSet.difference origNodes reachableNodes
 where
  origNodes :: HashSet PilNode
  origNodes = HSet.fromList . Set.toList $ G.nodes cfg
  reachableNodes :: HashSet PilNode
  reachableNodes = HSet.fromList . concat $ G.bfs [cfg ^. #root] cfg

cutEdge :: PilEdge -> InterCfg -> InterCfg
cutEdge edge (InterCfg cfg) =
  InterCfg $ Cfg.removeEdge edge cfg
