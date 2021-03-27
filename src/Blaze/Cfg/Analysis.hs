{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude hiding (succ)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt)
import qualified Data.Set as Set
import Blaze.Cfg (
  PilEdgeUnique,
  PilNode,
  evalCondition,
  parseBranchNode,
 )
import Blaze.Types.Graph.Unique (Unique)
import Blaze.Types.Cfg (
  PilCfg,
  BranchNode,
  BranchType (FalseBranch, TrueBranch),
  CfNode (BasicBlock),
 )
import qualified Data.HashSet as HSet


copyProp :: InterCfg -> InterCfg
copyProp icfg =
  InterCfg $
    foldl'
    (flip $ Cfg.updateNode (PA._copyProp copyPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (concatMap concat) . Set.toList . G.nodes $ cfg
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: InterCfg -> InterCfg
constantProp icfg =
  InterCfg $
    foldl'
    (flip $ Cfg.updateNode (PA._constantProp constPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (concatMap concat) . Set.toList . G.nodes $ cfg
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts


getStmts :: InterCfg -> [[Stmt]]
getStmts (InterCfg cfg) =
  fmap (concat . view #node) . Set.toList $ G.nodes cfg

prune :: InterCfg -> InterCfg
prune icfg =
  removeDeadNodes $ foldl' (flip cutEdge) icfg' deadBranches
  where
    icfg' :: InterCfg
    icfg' = constantProp . copyProp $ icfg
    deadBranches :: [PilEdgeUnique]
    deadBranches = getDeadBranches icfg'

getDeadBranches :: InterCfg -> [PilEdgeUnique]
getDeadBranches icfg =
  concat $ mapMaybe (getDeadBranchesForNode icfg) branchNodes
  where
    branchNodes :: [Unique (BranchNode [Stmt])]
    branchNodes =
      mapMaybe (traverse $ parseBranchNode (Just . concat))
      . Set.toList
      . G.nodes
      $ unInterCfg icfg
    getDeadBranchesForNode :: InterCfg -> Unique (BranchNode [Stmt]) -> Maybe [PilEdgeUnique]
    getDeadBranchesForNode _g branchNode = do
      constVal <- evalCondition (branchNode ^. #node . #branchCondOp)
      let constBranchType = if constVal then TrueBranch else FalseBranch
          cfg = unInterCfg icfg
          cfNode :: Unique (CfNode [Stmt])
          cfNode = (`fmap` branchNode) $ BasicBlock . view #basicBlock
          succs = Set.toList $ G.succs cfNode cfg
          -- Probably only one since there's likely just a pair of edges,
          -- could change later
          deadSuccs =
            filter
              (\succ -> Just constBranchType /= G.getEdgeLabel (G.Edge cfNode succ) cfg)
              succs
      traverse
        (\deadSucc -> do
           lbl <- G.getEdgeLabel (G.Edge cfNode deadSucc) cfg
           return $ G.LEdge lbl (G.Edge cfNode deadSucc))
        deadSuccs

removeDeadNodes :: InterCfg -> InterCfg
removeDeadNodes icfg =
  InterCfg $ foldl' (flip G.removeNode) (unInterCfg icfg) deadNodes
 where
  deadNodes = getDeadNodes $ unInterCfg icfg


getDeadNodes :: PilCfg -> [Unique PilNode]
getDeadNodes cfg =
  HSet.toList $ HSet.difference origNodes reachableNodes
 where
  origNodes :: HashSet (Unique PilNode)
  origNodes = HSet.fromList . Set.toList $ G.nodes cfg
  reachableNodes :: HashSet (Unique PilNode)
  reachableNodes = HSet.fromList . concat $ G.bfs [cfg ^. #root] cfg

cutEdge :: PilEdgeUnique -> InterCfg -> InterCfg
cutEdge edge (InterCfg cfg) =
  InterCfg $ G.removeEdge (G.Edge (edge ^. #edge . #src) (edge ^. #edge . #dst)) cfg
