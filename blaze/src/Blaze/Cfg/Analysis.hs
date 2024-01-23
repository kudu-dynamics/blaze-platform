{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import Blaze.Types.Pil.Analysis (DataDependenceGraph)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude hiding (to, succ)
import Blaze.Types.Cfg (CfNode (BasicBlock, Call, EnterFunc, LeaveFunc, Grouping), PilCfg, PilNode, PilEdge, BranchNode, CallNode, CfEdge(CfEdge), Cfg, BranchType)
-- import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg, unInterCfg), unInterCfg, liftInter)
import Blaze.Types.Pil (Stmt, PilVar)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Graph (Edge)
import Blaze.Import.CallGraph (CallGraphImporter, getFunctions)
import Blaze.CallGraph (getCallGraph)
import Blaze.Types.Cfg.Analysis
import Control.Lens (to, _last)


transformStmts :: ([Stmt] -> [Stmt]) -> PilCfg -> PilCfg
transformStmts f cfg =
  foldl' (flip $ G.updateNode (Cfg.updateNodeData f))
         cfg
         (HashSet.toList $ G.nodes cfg)

copyProp :: PilCfg -> PilCfg
copyProp cfg =
  transformStmts (PA._copyProp copyPropState) cfg
 where
  allStmts :: [Stmt]
  allStmts = concat . getStmts $ cfg
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: PilCfg -> PilCfg
constantProp cfg =
  transformStmts (PA._constantProp constPropState) cfg
 where
  allStmts :: [Stmt]
  allStmts = concat . getStmts $ cfg
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts

getStmts :: PilCfg -> [[Stmt]]
getStmts cfg =
  fmap Cfg.getNodeData (HashSet.toList $ Cfg.nodes cfg)

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
fixedSimplify :: PilCfg -> PilCfg
fixedSimplify = fixed simplify

-- TODO: Generalize this to support all assignment statements
-- | Remove all DefPhi statements where the assigned variable is not used.
removeUnusedPhi :: PilCfg -> PilCfg
removeUnusedPhi cfg =
  transformStmts (PA.removeUnusedPhi usedVars) cfg
  where
    usedVars :: HashSet PilVar
    usedVars = PA.getRefVars . concat $ getStmts cfg

-- | Reduce all 'DefPhi' statements by removing selected variables from the
-- 'src' list. If the 'src' list is reduced to holding a single item, the
-- 'DefPhi' statement will be transferred to a 'Def' statement.
reducePhi :: HashSet PilVar -> PilCfg -> PilCfg
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
_simplify :: Int -> PilCfg -> PilCfg
_simplify numItersLeft cfg =
  -- TODO: Do we need to also check if statements were removed
  --       via copy prop or other statement transforms?
  if cfg == cfg''' || numItersLeft <= 0
    then cfg'''
    else -- Recursing until stmts don't change or no iterations left
      _simplify (numItersLeft - 1) cfg'''
 where
  cfg' :: PilCfg
  cfg' = constantProp . copyProp $ cfg
  deadBranches :: [PilEdge]
  deadBranches = getDeadBranches cfg'
  cfg'' :: PilCfg
  cfg'' = foldl' (flip Cfg.removeEdge) cfg' deadBranches
  -- Need deadNodes to compute removedVars and to actually remove the dead nodes
  deadNodes :: HashSet PilNode
  deadNodes = getDeadNodes cfg''
  removedVars :: HashSet PilVar
  removedVars = PA.getDefinedVars (concatMap Cfg.getNodeData deadNodes)
  cfg''' :: PilCfg
  cfg''' =
    reducePhi removedVars
      . fixed removeUnusedPhi
      . removeNodes deadNodes
      $ cfg''

simplify :: PilCfg -> PilCfg
simplify = removeEmptyBasicBlockNodes . _simplify maxIters
 where
  maxIters = 10

prune :: Edge PilNode -> PilCfg -> PilCfg
prune edge cfg = simplify $ G.removeEdge edge cfg

-- | Returns the node's predeccessor, if it has exactly one, and otherwise
-- 'Nothing'
onlyPred :: PilCfg -> PilNode -> Maybe PilNode
onlyPred cfg n = case predNodes of
  [predNode] -> Just predNode
  _ -> Nothing
  where
    predNodes = HashSet.toList $ Cfg.preds n cfg

-- | Fixes the predecessor of the focal node of a focus operation so that, if
-- its last statement is a 'JumpTo', all of its 'targets' are removed, except
-- for the focal node's address, if any
fixupJumpToPred :: PilNode -> PilCfg -> PilCfg
fixupJumpToPred node cfg = fromMaybe cfg $ do
  pred_ <- onlyPred cfg node
  addr <- node ^? (#_BasicBlock . #start <> #_Call . #start)
  let filterAddr ns = if addr `elem` ns then [addr] else []
  return . G.updateNode (Cfg.updateNodeData (_last . #_JumpTo . #targets %~ filterAddr)) pred_ $ cfg

-- | Removes all nodes/edges that don't lead to or can't be reached by node.
-- Returns a modified and simplified ICFG.
focus :: PilNode -> PilCfg -> PilCfg
focus focalNode cfg =
  fixupJumpToPred focalNode
  . simplify
  . reducePhi removedVars
  . Cfg.removeEdges deadEdges
  . removeNodes deadNodes
  $ cfg
  where
    -- Need deadNodes to compute removedVars and to actually remove the dead nodes
    cnodes :: HashSet PilNode
    cedges :: HashSet (G.LEdge BranchType PilNode)
    (cnodes, cedges) = G.connectedNodesAndEdges
                       (Proxy :: Proxy (AlgaGraph () Int))
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
    removedVars = PA.getDefinedVars (concatMap Cfg.getNodeData deadNodes)

-- TODO: refactor with regular prune
-- | Like `focus` but doesn't call `simplify`
focus_ :: PilNode -> PilCfg -> PilCfg
focus_ focalNode cfg =
  fixupJumpToPred focalNode
  . reducePhi removedVars
  . Cfg.removeEdges deadEdges
  . removeNodes deadNodes
  $ cfg
  where
    -- Need deadNodes to compute removedVars and to actually remove the dead nodes
    cnodes :: HashSet PilNode
    cedges :: HashSet (G.LEdge BranchType PilNode)
    (cnodes, cedges) = G.connectedNodesAndEdges
                       (Proxy :: Proxy (AlgaGraph () Int))
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
    removedVars = PA.getDefinedVars (concatMap Cfg.getNodeData deadNodes)


getDeadBranches :: PilCfg -> [PilEdge]
getDeadBranches cfg =
  concat $ mapMaybe (getDeadBranchesForNode cfg) branchNodes
 where
  branchNodes :: [BranchNode [Stmt]]
  branchNodes = mapMaybe (Cfg.parseBranchNode Cfg.getNodeData)
    $ HashSet.toList (G.nodes cfg)
  getDeadBranchesForNode :: PilCfg -> BranchNode [Stmt] -> Maybe [PilEdge]
  getDeadBranchesForNode _g branchNode = do
    constVal <- Cfg.evalCondition (branchNode ^. #branchCondOp)
    let constBranchType = if constVal then Cfg.TrueBranch else Cfg.FalseBranch
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

removeNodes :: HashSet PilNode -> PilCfg -> PilCfg
removeNodes nodes cfg =
  foldl' (flip G.removeNode) cfg nodes

removeDeadNodes :: PilCfg -> PilCfg
removeDeadNodes cfg = removeNodes deadNodes cfg
 where
  deadNodes = getDeadNodes cfg

getDeadNodes :: PilCfg -> HashSet PilNode
getDeadNodes cfg =
  HashSet.difference origNodes reachableNodes
 where
  origNodes :: HashSet PilNode
  origNodes = HashSet.fromList . HashSet.toList $ G.nodes cfg
  reachableNodes :: HashSet PilNode
  reachableNodes = HashSet.fromList . concat $ G.bfs [Cfg.getRootNode cfg] cfg

removeEmptyBasicBlockNodes ::
  forall f a.
  (Hashable (f a), Foldable f) =>
  Cfg (CfNode (f a)) ->
  Cfg (CfNode (f a))
removeEmptyBasicBlockNodes = Cfg.removeNodesBy Cfg.mergeBranchTypeDefault isEmpty
 where
  isEmpty :: CfNode (f a) -> Bool
  isEmpty (Cfg.BasicBlock x) = null $ x ^. #nodeData
  isEmpty _ = False

getNodesContainingAddress :: Hashable a => Address -> Cfg (CfNode a) -> HashSet (CfNode a)
getNodesContainingAddress addr = HashSet.filter containsAddr . G.nodes
  where
    containsAddr (BasicBlock bb) = bb ^. #start <= addr && addr <= bb ^. #end
    containsAddr (Call n) = n ^. #start == addr
    containsAddr (EnterFunc _) = False
    containsAddr (LeaveFunc _) = False
    containsAddr (Grouping n) = not . HashSet.null . getNodesContainingAddress addr $ n ^. #grouping

------------- Call Node rating --------------

getCallNodeRatingCtx :: CallGraphImporter a => a -> IO CallNodeRatingCtx
getCallNodeRatingCtx imp = do
  funcs <- getFunctions imp
  cg <- getCallGraph imp funcs
  let dmap = G.calcDescendantsDistanceMap cg
  return $ CallNodeRatingCtx cg dmap

-- | Returns Call Node ratings between 0 and 1, where higher scores are better
-- for reaching the target.
getCallNodeRatings
  :: Hashable a
  => CallNodeRatingCtx
  -> Target
  -> Cfg (CfNode a)
  -> HashMap (CallNode a) CallNodeRating
getCallNodeRatings ctx tgt cfg =
  HashMap.map (maybe Unreachable (Reachable . metric)) distances
  where
    metric :: Int -> Double
    metric = (** 2) . ((/) `on` fromIntegral) shortest . max 1
    distances = getCallNodeDistances ctx tgt callNodes
    shortest = minimum . catMaybes $ HashMap.elems distances
    callNodes = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfg

getCallNodeDistances
  :: Hashable a
  => CallNodeRatingCtx
  -> Target
  -> [CallNode a]
  -> HashMap (CallNode a) (Maybe Int)
getCallNodeDistances ctx tgt =
  HashMap.fromList
    . mapMaybe
        (\callNode ->
            callNode ^? #callDest . #_CallFunc . to (\src -> (callNode, shortestPath src)))
  where
    dstFunc = tgt ^. #function
    shortestPath src = G.getDescendantDistance (ctx ^. #descendantsDistanceMap) src dstFunc

getDataDependenceGraph :: PilCfg -> DataDependenceGraph
getDataDependenceGraph = PA.getDataDependenceGraph . Cfg.gatherCfgData
