{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import Blaze.Types.Pil.Analysis (DataDependenceGraph)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude hiding (succ)
import Blaze.Types.Cfg (CfNode (BasicBlock), PilCfg, PilNode, PilEdge, BranchNode, CallNode, CfEdge(CfEdge), Cfg, BranchType)
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg, unInterCfg), unInterCfg, liftInter)
import Blaze.Types.Pil (Stmt, PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Graph (Edge)
import Blaze.Import.CallGraph (CallGraphImporter, getFunctions)
import Blaze.CallGraph (getCallGraph)
import Blaze.Types.Cfg.Analysis
import Blaze.Function (Function)


transformStmts :: ([Stmt] -> [Stmt]) -> InterCfg -> InterCfg
transformStmts f icfg =
  InterCfg $
    foldl'
      (flip $ Cfg.updateNodeData f)
      cfg
      (HashSet.toList $ G.nodes cfg)
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
  fmap concat . HashSet.toList $ Cfg.nodes cfg

fixed :: Eq a => (a -> a) -> a -> a
fixed f x =
  if x == x'
    then x
    else f x'
 where
  x' = f x

-- TODO: Checking for fixed point by running simplify means simplify is always run at least twice.
--       I.e., simplify will always be run one more time than necessary.
--       As an alternative, consider whether we can either identify when an additional simplify
--       call is needed or what specific portion of a simplify needs to be rerun.
fixedSimplify :: InterCfg -> InterCfg
fixedSimplify = fixed simplify

-- TODO: Generalize this to support all assignment statements
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

-- | Simplification helper. This function recurses until reaching a fixed point 
-- for the ICFG or until the 'numItersLeft' argument is zero.
-- NB: This function performs a little extra work in exchange for being less complex.
--     We could check if any 'DefPhi' statements were reduced to 'Def' statements
--     and only recurse if that check passed. In the worst case, we will attempt
--     copy prop and const prop an extra iteration, as well as compute dead branches 
--     and dead nodes and extra iteration. We would also need to check for removed nodes
--     (and edges?)
_simplify :: Int -> InterCfg -> InterCfg
_simplify numItersLeft icfg =
  -- TODO: Do we need to also check if statements were removed 
  --       via copy prop or other statement transforms?
  if icfg == icfg''' || numItersLeft <= 0
    then icfg'''
    else -- Recursing until stmts don't change or no iterations left
      _simplify (numItersLeft - 1) icfg'''
 where
  icfg' :: InterCfg
  icfg' = constantProp . copyProp $ icfg
  deadBranches :: [PilEdge]
  deadBranches = getDeadBranches icfg'
  icfg'' :: InterCfg
  icfg'' = foldl' (flip cutEdge) icfg' deadBranches
  -- Need deadNodes to compute removedVars and to actually remove the dead nodes
  deadNodes :: HashSet PilNode
  deadNodes = getDeadNodes (unInterCfg icfg'')
  removedVars :: HashSet PilVar
  removedVars = PA.getDefinedVars (concatMap concat deadNodes)
  icfg''' :: InterCfg
  icfg''' =
    reducePhi removedVars
      . fixed removeUnusedPhi
      . removeNodes deadNodes
      $ icfg''

simplify :: InterCfg -> InterCfg
simplify = removeEmptyBasicBlockNodes' . _simplify maxIters
 where
  maxIters = 10
  removeEmptyBasicBlockNodes' (InterCfg cfg) = InterCfg . removeEmptyBasicBlockNodes $ cfg

prune :: Edge PilNode -> InterCfg -> InterCfg
prune edge icfg = simplify prunedIcfg
  where
    prunedIcfg :: InterCfg
    prunedIcfg = InterCfg . G.removeEdge edge . unInterCfg $ icfg

parseJumpToPred :: InterCfg -> PilNode -> Maybe PilNode 
parseJumpToPred icfg n = case predNodes of
  [predNode] -> 
    if isJust $ parseJumpTo predNode then
      Just predNode
    else
      Nothing
  _ -> Nothing
  where
    predNodes = HashSet.toList $ Cfg.preds n (unInterCfg icfg)

parseJumpTo :: PilNode -> Maybe (Pil.JumpToOp Pil.Expression)
parseJumpTo n = case n ^? #_BasicBlock . #nodeData of
    Just [Pil.JumpTo jumpToOp] -> Just jumpToOp
    _ -> Nothing

simplifyJumpTo :: [Stmt] -> [Stmt]
simplifyJumpTo xs = case xs of
  [Pil.JumpTo jumpToOp] ->
    [Pil.JumpTo (jumpToOp & #targets .~ [])]
  _ -> xs

-- | Removes all nodes/edges that don't lead to or can't be reached by node.
-- Returns a modified and simplified ICFG.
focus :: PilNode -> InterCfg -> InterCfg
focus focalNode icfg = fromMaybe icfg' $ do
  jumpToPred <- parseJumpToPred icfg' focalNode
  return . InterCfg . Cfg.updateNodeData simplifyJumpTo jumpToPred . unInterCfg $ icfg'
  where
    icfg' = simplify
            . reducePhi removedVars
            . liftInter (Cfg.removeEdges deadEdges)
            . removeNodes deadNodes
            $ icfg
    (InterCfg cfg) = icfg
    -- Need deadNodes to compute removedVars and to actually remove the dead nodes
    cnodes :: HashSet PilNode
    cedges :: HashSet (G.LEdge BranchType PilNode)
    (cnodes, cedges) = G.connectedNodesAndEdges
                       (Proxy :: Proxy (AlgaGraph () () (G.EdgeGraphNode BranchType PilNode)))
                       focalNode
                       cfg

    deadNodes :: HashSet PilNode
    deadNodes = HashSet.difference (Cfg.nodes cfg) cnodes
    deadEdges :: [PilEdge]
    deadEdges = fmap Cfg.fromLEdge
      . HashSet.toList
      . HashSet.difference (HashSet.fromList $ G.edges cfg)
      $ cedges
    removedVars :: HashSet PilVar
    removedVars = PA.getDefinedVars (concatMap concat deadNodes)

getDeadBranches :: InterCfg -> [PilEdge]
getDeadBranches icfg =
  concat $ mapMaybe (getDeadBranchesForNode icfg) branchNodes
 where
  branchNodes :: [BranchNode [Stmt]]
  branchNodes = mapMaybe (Cfg.parseBranchNode Cfg.getNodeData)
    $ HashSet.toList (G.nodes (unInterCfg icfg))
  getDeadBranchesForNode :: InterCfg -> BranchNode [Stmt] -> Maybe [PilEdge]
  getDeadBranchesForNode _g branchNode = do
    constVal <- Cfg.evalCondition (branchNode ^. #branchCondOp)
    let constBranchType = if constVal then Cfg.TrueBranch else Cfg.FalseBranch
        cfg = unInterCfg icfg
        cfNode = BasicBlock $ branchNode ^. #basicBlock
        succs = HashSet.toList $ Cfg.succs cfNode cfg
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

removeNodes :: HashSet PilNode -> InterCfg -> InterCfg
removeNodes nodes icfg =
  InterCfg $ foldl' (flip G.removeNode) (unInterCfg icfg) nodes

removeDeadNodes :: InterCfg -> InterCfg
removeDeadNodes icfg = removeNodes deadNodes icfg
 where
  deadNodes = getDeadNodes $ unInterCfg icfg

getDeadNodes :: PilCfg -> HashSet PilNode
getDeadNodes cfg =
  HashSet.difference origNodes reachableNodes
 where
  origNodes :: HashSet PilNode
  origNodes = HashSet.fromList . HashSet.toList $ G.nodes cfg
  reachableNodes :: HashSet PilNode
  reachableNodes = HashSet.fromList . concat $ G.bfs [cfg ^. #root] cfg

cutEdge_ :: CfEdge () -> InterCfg -> InterCfg
cutEdge_ edge (InterCfg cfg) = InterCfg . Cfg.removeIdEdge edge $ cfg

cutEdge :: PilEdge -> InterCfg -> InterCfg
cutEdge edge (InterCfg cfg) =
  InterCfg $ Cfg.removeEdge edge cfg

removeEmptyBasicBlockNodes :: forall a. (Hashable a, Eq a) => Cfg [a] -> Cfg [a]
removeEmptyBasicBlockNodes = Cfg.removeNodesBy Cfg.mergeBranchTypeDefault f
  where
    f (Cfg.BasicBlock x) = null $ x ^. #nodeData
    f _ = False

getNodesContainingAddress :: (Eq a, Hashable a) => Address -> Cfg a -> HashSet (CfNode a)
getNodesContainingAddress addr = HashSet.filter containsAddr . G.nodes
  where
    -- TODO: confirm that bb range is [start, end)
    containsAddr (Cfg.BasicBlock bb) = bb ^. #start <= addr && addr <= bb ^. #end
    containsAddr (Cfg.Call n) = n ^. #start == addr
    containsAddr (Cfg.EnterFunc _) = False
    containsAddr (Cfg.LeaveFunc _) = False

------------- Call Node rating --------------

getCallNodeRatingCtx :: CallGraphImporter a => a -> IO CallNodeRatingCtx
getCallNodeRatingCtx imp = do
  funcs <- getFunctions imp
  cg <- getCallGraph imp funcs
  let dmap = G.calcDescendantsDistanceMap cg
  return $ CallNodeRatingCtx cg dmap

getCallNodeRatings
  :: (Hashable a, Eq a)
  => CallNodeRatingCtx
  -> Target
  -> Cfg a
  -> HashMap (CallNode a) CallNodeRating
getCallNodeRatings ctx tgt cfg =
  HashMap.fromList . fmap (toSnd $ getCallNodeRating ctx tgt) $ callNodes
  where
    callNodes = mapMaybe isCallNode . HashSet.toList . G.nodes $ cfg

    isCallNode (Cfg.Call x) = Just x
    isCallNode _ = Nothing

-- | Returns a Call Node rating between 0 and 1, where higher scores are better
-- for reaching the target.
-- The score will be 0 if target cannot be reached, and at least 0.5 if it can
-- be reached through the callgraph.
-- The range (0, 0.5) is reserved for callnodes that might reach target
-- through indirect calls.
getCallNodeRating :: CallNodeRatingCtx -> Target -> CallNode a -> CallNodeRating
getCallNodeRating ctx tgt call = case call ^. #callDest of
  Pil.CallFunc func -> getCallFuncRating ctx tgt func
  _ -> Unreachable

getCallFuncRating :: CallNodeRatingCtx -> Target -> Function -> CallNodeRating
getCallFuncRating ctx tgt srcFunc =
  maybe Unreachable (Reachable . (**2) . normalize) shortestPath
  where
    maxPathLength = 6 :: Int
    normalize :: Int -> Double
    normalize 0 = 1.0
    normalize n = 1.0
                  - fromIntegral (min maxPathLength n)
                  / fromIntegral maxPathLength
    dstFunc = tgt ^. #function
    
    shortestPath = G.getDescendantDistance (ctx ^. #descendantsDistanceMap) srcFunc dstFunc

getDataDependenceGraph :: Cfg [Stmt] -> DataDependenceGraph
getDataDependenceGraph = PA.getDataDependenceGraph . Cfg.gatherCfgData
