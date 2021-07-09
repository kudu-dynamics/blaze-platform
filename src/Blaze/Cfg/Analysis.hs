{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude hiding (succ)
import Blaze.Types.Cfg (CfNode (BasicBlock), PilCfg, PilNode, PilEdge, BranchNode, CfEdge(CfEdge), Cfg)
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg, unInterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt, PilVar)
import qualified Data.Set as Set
import qualified Data.HashSet as HSet

transformStmts :: ([Stmt] -> [Stmt]) -> InterCfg -> InterCfg
transformStmts f icfg =
  InterCfg $
    foldl'
      (flip $ Cfg.updateNodeData f)
      cfg
      (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg

copyProp :: InterCfg -> InterCfg
copyProp icfg =
  transformStmts (PA._copyProp copyPropState) icfg
 where
  allStmts :: [Stmt]
  allStmts = concat . getStmts $ icfg
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: InterCfg -> InterCfg
constantProp icfg =
  transformStmts (PA._constantProp constPropState) icfg
 where
  allStmts :: [Stmt]
  allStmts = concat . getStmts $ icfg
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts

getStmts :: InterCfg -> [[Stmt]]
getStmts (InterCfg cfg) =
  fmap concat . Set.toList $ Cfg.nodes cfg

fixed :: Eq a => (a -> a) -> a -> a
fixed f x =
  if x == x'
    then x
    else f x'
 where
  x' = f x

-- TODO: Checking for fixed point by running prune means prune is always run at least twice.
--       I.e., prune will always be run one more time than necessary.
--       As an alternative, consider whether we can either identify when an additional prune
--       call is needed or what specific portion of a prune needs to be rerun.
fixedPrune :: InterCfg -> InterCfg
fixedPrune = fixed prune

-- TODO: Generalize this to support all assingment statements
-- | Remove all DefPhi statements where the assigned variable is not used.
removeUnusedPhi :: InterCfg -> InterCfg 
removeUnusedPhi icfg =
  transformStmts (PA.removeUnusedPhi usedVars) icfg
  where
    usedVars :: HashSet PilVar
    usedVars = PA.getRefVars . concat $ getStmts icfg

-- | Reduce all 'DefPhi' statements by removing selected variables from the
-- 'src' list. If the 'src' list is reduced to holding a single item, the
-- 'DefPhi' statement will be transferred to a 'Def' statement.
reducePhi :: HashSet PilVar -> InterCfg -> InterCfg 
reducePhi removedVars =
  transformStmts (PA.reducePhis removedVars)

prune :: InterCfg -> InterCfg
prune icfg =
  removeEmptyBasicBlockNodes'
    . reducePhi removedVars
    . fixed removeUnusedPhi
    . removeNodes deadNodes
    $ icfg''
 where
  removeEmptyBasicBlockNodes' (InterCfg cfg) = InterCfg . removeEmptyBasicBlockNodes $ cfg
  icfg' :: InterCfg
  icfg' = constantProp . copyProp $ icfg
  deadBranches :: [PilEdge]
  deadBranches = getDeadBranches icfg'
  icfg'' :: InterCfg
  icfg'' = foldl' (flip cutEdge) icfg' deadBranches
  -- Need deadNodes to compute removedVars and to actually remove the dead nodes
  deadNodes :: [PilNode]
  deadNodes = getDeadNodes (unInterCfg icfg'')
  removedVars :: HashSet PilVar
  removedVars = PA.getDefinedVars (concatMap concat deadNodes)

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

removeNodes :: [PilNode] -> InterCfg -> InterCfg
removeNodes nodes icfg = 
  InterCfg $ foldl' (flip G.removeNode) (unInterCfg icfg) nodes

removeDeadNodes :: InterCfg -> InterCfg
removeDeadNodes icfg = removeNodes deadNodes icfg
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

removeEmptyBasicBlockNodes :: forall a. Ord a => Cfg [a] -> Cfg [a]
removeEmptyBasicBlockNodes = Cfg.removeNodesBy Cfg.mergeBranchTypeDefault f
  where
    f (Cfg.BasicBlock x) = null $ x ^. #nodeData
    f _ = False
