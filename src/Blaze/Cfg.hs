{-# LANGUAGE RankNTypes #-}

module Blaze.Cfg (
  module Cfg,
  module Blaze.Cfg,
) where

import Blaze.Graph (Dominators, PostDominators)
import qualified Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Pil (BranchCondOp, Expression, Statement (Exit, NoRet), Stmt, Ctx)
import Blaze.Types.Cfg hiding (nodes)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Spec (mkDummyCtx, mkDummyTermNode)
import Control.Lens (set)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.HashSet as HSet
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Blaze.Pil.Analysis (getVarsFromExpr)


{- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
 to find dominators. The result is converted back to CfNodes before being returned.
 Per dom-lt, the complexity is:
 O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
-}
getDominators_ :: Cfg a -> Dominators (CfNode ())
getDominators_ cfg = G.getDominators (asIdNode $ cfg ^. #root) (cfg ^. #graph)

getDominators :: (Hashable a, Eq a) => Cfg a -> Dominators (CfNode a)
getDominators cfg = G.domMap (getFullNode cfg)
  $ G.getDominators (asIdNode $ cfg ^. #root) (cfg ^. #graph)

getPostDominatorsAsIdNodes_ :: CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes_ dummyTermNode dummyBranchType cfg = G.getPostDominators dummyTermNode dummyBranchType (cfg ^. #graph)

getPostDominatorsAsIdNodes :: Cfg a -> PostDominators (CfNode ())
getPostDominatorsAsIdNodes = getPostDominatorsAsIdNodes_ dummyTermNode UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

getPostDominators_ :: (Hashable a, Eq a) => CfNode () -> BranchType -> Cfg a -> PostDominators (CfNode a)
getPostDominators_ dummyTermNode dummyBranchType cfg
  = G.domMap (getFullNode cfg)
  . G.getPostDominators dummyTermNode dummyBranchType
  $ cfg ^. #graph

getPostDominators :: (Hashable a, Eq a) => Cfg a -> PostDominators (CfNode a)
getPostDominators = getPostDominators_ dummyTermNode UnconditionalBranch
  where
    dummyTermNode = mkDummyTermNode (mkDummyCtx 0) ()

lastStmtFrom :: (HasField' "nodeData" n [Stmt]) => n -> Maybe Stmt
lastStmtFrom = lastMay . view #nodeData

parseReturnNode :: BasicBlockNode [Stmt] -> Maybe (ReturnNode [Stmt])
parseReturnNode node = do
  retOp_ <- lastStmtFrom node >>= preview #_Ret
  return $ ReturnNode node retOp_

parseExitNode :: BasicBlockNode [Stmt] -> Maybe (ExitNode [Stmt])
parseExitNode node = do
  lastStmt <- lastStmtFrom node
  case lastStmt of
    Exit -> return $ ExitNode node
    _ -> Nothing

parseNoRetNode :: BasicBlockNode [Stmt] -> Maybe (NoRetNode [Stmt])
parseNoRetNode node = do
  lastStmt <- lastStmtFrom node
  case lastStmt of
    NoRet -> return $ NoRetNode node
    _ -> Nothing

parseTerminalNode :: CfNode [Stmt] -> Maybe (TerminalNode [Stmt])
parseTerminalNode node = do
  bb <- node ^? #_BasicBlock
  (TermRet <$> parseReturnNode bb)
    <|> (TermExit <$> parseExitNode bb)
    <|> (TermNoRet <$> parseNoRetNode bb)

parseBranchNode
  :: (CfNode [Stmt] -> [Stmt])
  -> CfNode [Stmt]
  -> Maybe (BranchNode [Stmt])
parseBranchNode getStmts node = do
  bb <- node ^? #_BasicBlock
  lastStmt <- lastMay $ getStmts node
  case lastStmt of
    Pil.BranchCond op -> Just $ BranchNode bb op
    _ -> Nothing

{- |Get all terminal blocks. An error is raised if a CFG does not contain at least
 one terminal block.
-}
getTerminalBlocks :: PilCfg -> NonEmpty (TerminalNode [Stmt])
getTerminalBlocks cfg =
  -- TODO: We may want to add a 'FuncCfg' type so that arbitrary CFGs that may not correspond
  --       to an entire function do not cause errors.
  if List.null nodes
    then error "CFGs should always have at least one terminal basic block."
    else NEList.fromList nodes
 where
  nodes :: [TerminalNode [Stmt]]
  nodes =
    mapMaybe
      parseTerminalNode
      (HashSet.toList $ Cfg.nodes cfg)

-- |Get all the return expressions.
getRetExprs :: PilCfg -> [Expression]
getRetExprs cfg =
  view (#retOp . #value) <$> retBlocks
 where
  retBlocks :: [ReturnNode [Stmt]]
  retBlocks = mapMaybe (preview #_TermRet) (NEList.toList $ getTerminalBlocks cfg)

evalCondition :: BranchCondOp Expression -> Maybe Bool
evalCondition bn = case bn ^. #cond . #op of
  Pil.CONST_BOOL x -> Just $ x ^. #constant
  Pil.CONST x -> Just . (/= 0) $ x ^. #constant
  Pil.CMP_E x -> intBinOp x (==)
  Pil.CMP_NE x -> intBinOp x (/=)
  Pil.CMP_SGE x -> intBinOp x (>=)
  Pil.CMP_SGT x -> intBinOp x (>)
  Pil.CMP_SLE x -> intBinOp x (<=)
  Pil.CMP_SLT x -> intBinOp x (<)
  Pil.CMP_UGE x -> intBinOp x (>=)
  Pil.CMP_UGT x -> intBinOp x (>)
  Pil.CMP_ULE x -> intBinOp x (<=)
  Pil.CMP_ULT x -> intBinOp x (<)
  _ -> Nothing
 where
  intBinOp ::
    ( HasField' "left" x Expression
    , HasField' "right" x Expression
    ) =>
    x ->
    (forall a. (Hashable a, Ord a) => a -> a -> Bool) ->
    Maybe Bool
  intBinOp x p =
    p <$> getConstArg (x ^. #left)
      <*> getConstArg (x ^. #right)

  getConstArg :: Expression -> Maybe Int64
  getConstArg x = x ^? #op . #_CONST . #constant

getOutBranchingType :: (Eq a, Hashable a) => CfNode a -> Cfg a -> Maybe BranchingType
getOutBranchingType n cfg = case outBranches of
  [(TrueBranch, tedge), (FalseBranch, fedge)] ->
    Just . Undecided $ UndecidedIfBranches { falseEdge = fedge, trueEdge = tedge }
  [(FalseBranch, fedge), (TrueBranch, tedge)] ->
    Just . Undecided $ UndecidedIfBranches { falseEdge = fedge, trueEdge = tedge }
  [(TrueBranch, tedge)] ->
    Just $ OnlyTrue tedge
  [(FalseBranch, fedge)] ->
    Just $ OnlyFalse fedge
  _ -> Nothing
  where
    outBranches :: [(BranchType, CfEdge ())]
    outBranches =
      fmap (\e -> (e ^. #branchType, Cfg.asIdEdge e))
      . HashSet.toList
      $ Cfg.succEdges n cfg


-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getBranchCondNode
  :: (Eq a, Hashable a)
  => (a -> (Maybe Int, Statement b))
  -> CfNode [a]
  -> Cfg [a]
  -> Maybe (BranchCond b)
getBranchCondNode extractIndexStmt n cfg = mcond <*> getOutBranchingType n cfg  
  where
    mcond = lastMay (Cfg.getNodeData n) >>= (\case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ BranchCond i x
      _ -> Nothing)
      . extractIndexStmt

getBranchCondNodes
  :: (Hashable a, Eq a)
  => (a -> (Maybe Int, Statement b))
  -> Cfg [a]
  -> [BranchCond b]
getBranchCondNodes extractIndexStmt typedCfg = mapMaybe (flip (getBranchCondNode extractIndexStmt) typedCfg)
  . HashSet.toList
  . G.nodes
  $ typedCfg

stmtCtxs :: Stmt -> HashSet Ctx
stmtCtxs = foldMap (HSet.fromList . mapMaybe (view #ctx) . HSet.toList . getVarsFromExpr)

nodeCtxs :: CfNode [Stmt] -> HashSet Ctx
nodeCtxs = foldMap (foldMap stmtCtxs)

getCtxIndices :: PilCfg -> Bimap Int Ctx
getCtxIndices cfg = Bimap.fromList $ zip [0..] ctxs
  where
    ctxs = sortUnique . foldMap nodeCtxs . G.nodes $ cfg
    sortUnique = sort . HSet.toList

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
    predEdges' = HashSet.toList $ Cfg.predEdges node outerCfg
    succEdges' :: [CfEdge a]
    succEdges' = HashSet.toList $ Cfg.succEdges node outerCfg

    newPredEdges :: [CfEdge a]
    newPredEdges = set #dst innerRoot <$> predEdges'
    newSuccEdges :: [CfEdge a]
    newSuccEdges = set #src exitNode' <$> succEdges'
    newCfg :: Cfg a
    newCfg =
      Cfg.removeNode node
      . Cfg.addNodes (HashSet.toList $ Cfg.nodes innerCfg)
      . Cfg.addEdges (Cfg.edges innerCfg)
      . Cfg.addEdges newPredEdges
      . Cfg.addEdges newSuccEdges $ outerCfg

findNodeByUUID :: forall a. (Eq a, Hashable a) => UUID -> Cfg a -> Maybe (CfNode a)
findNodeByUUID id cfg = case filter ((== id) . getNodeUUID) . HashSet.toList . G.nodes $ cfg of
  [] -> Nothing
  [x] -> Just x
  (x:_xs) -> Just x -- Should never happen. Maybe print warning?
