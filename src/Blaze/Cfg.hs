{-# LANGUAGE RankNTypes #-}

module Blaze.Cfg (
  module Exports,
  module Blaze.Cfg,
) where

import Blaze.Graph (Dominators, PostDominators, Identifiable, NodeId)
import qualified Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Cfg as Exports
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Pil (BranchCondOp, Expression, PilVar, Statement (Exit, NoRet), Stmt, Ctx)
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
import qualified Blaze.Pil.Analysis as PilA
import qualified Blaze.Types.Graph.Alga as Ag

-- | Finds all dominators for a CFG.
getDominators :: (Hashable n, Identifiable n UUID) => Cfg n -> Dominators n
getDominators cfg = G.getDominators (getRootNode cfg) (cfg ^. #graph)

getPostDominators_ :: (Hashable n, Identifiable n UUID) => n -> BranchType -> Cfg n -> PostDominators n
getPostDominators_ dummyTermNode dummyBranchType cfg
  = G.getPostDominators dummyTermNode dummyBranchType $ cfg ^. #graph

getPostDominators ::
  Hashable (CfNode [a]) =>
  Cfg (CfNode [a]) ->
  PostDominators (CfNode [a])
getPostDominators = getPostDominators_ dummyTermNode UnconditionalBranch
 where
  dummyTermNode = mkDummyTermNode (mkDummyCtx 0) []

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
  if List.null termNodes
    then error "CFGs should always have at least one terminal basic block."
    else NEList.fromList termNodes
 where
  termNodes :: [TerminalNode [Stmt]]
  termNodes =
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

getOutBranchingType :: forall a. Hashable a => CfNode a -> Cfg (CfNode a) -> Maybe (BranchingType (CfNode a))
getOutBranchingType n cfg = case outBranches of
  [(TrueBranch, tedge), (FalseBranch, fedge)] ->
    Just . Undecided $ UndecidedIfBranches{falseEdge = fedge, trueEdge = tedge}
  [(FalseBranch, fedge), (TrueBranch, tedge)] ->
    Just . Undecided $ UndecidedIfBranches{falseEdge = fedge, trueEdge = tedge}
  [(TrueBranch, tedge)] ->
    Just $ OnlyTrue tedge
  [(FalseBranch, fedge)] ->
    Just $ OnlyFalse fedge
  _ -> Nothing
 where
  outBranches :: [(BranchType, CfEdge (CfNode a))]
  outBranches =
    fmap (\e -> (e ^. #branchType, e)) . HashSet.toList $ succEdges n cfg

{- | If the node is a conditional if-node, and one of the branches has been removed,
  this returns which branch remains (True or False) and the conditional expr.
  TODO: Rerwrite this for clarity
-}
getBranchCondNode ::
  forall a b.
  Hashable a =>
  (a -> (Maybe Int, Statement b)) ->
  CfNode [a] ->
  Cfg (CfNode [a]) ->
  Maybe (BranchCond (CfNode [a]) b)
getBranchCondNode extractIndexStmt n cfg = mcond <*> getOutBranchingType n cfg
 where
  mcond =
    lastMay (getNodeData n)
      >>= ( \case
              (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ BranchCond i x
              _ -> Nothing
          )
        . extractIndexStmt

getBranchCondNodes
  :: (Hashable a, Identifiable (CfNode [a]) UUID)
  => (a -> (Maybe Int, Statement b))
  -> Cfg (CfNode [a])
  -> [BranchCond (CfNode [a]) b]
getBranchCondNodes extractIndexStmt typedCfg = mapMaybe (flip (getBranchCondNode extractIndexStmt) typedCfg)
  . HashSet.toList
  . G.nodes
  $ typedCfg

-- | Substitute a node with another CFG.
substNode ::
  forall a.
  (Hashable a, Identifiable (CfNode a) UUID) =>
  Cfg (CfNode a) ->
  CfNode a ->
  Cfg (CfNode a) ->
  CfNode a ->
  Cfg (CfNode a)
substNode
  outerCfg@(Cfg _ outerRootId _)
  node
  innerCfg@(Cfg _ innerRootId _)
  exitNode' =
    -- Check if the node we are substituting is the outer CFG's root
    if outerRootId == G.getNodeId node
      then newCfg & #rootId .~ innerRootId
      else newCfg & #rootId .~ outerRootId
   where
    -- TODO: Improve Graph API for fetching edges
    predEdges' :: [CfEdge (CfNode a)]
    predEdges' = HashSet.toList $ predEdges node outerCfg
    succEdges' :: [CfEdge (CfNode a)]
    succEdges' = HashSet.toList $ succEdges node outerCfg
    innerRoot :: CfNode a
    innerRoot = getRootNode innerCfg
    newPredEdges :: [CfEdge (CfNode a)]
    newPredEdges = set #dst innerRoot <$> predEdges'
    newSuccEdges :: [CfEdge (CfNode a)]
    newSuccEdges = set #src exitNode' <$> succEdges'
    newCfg :: Cfg (CfNode a)
    newCfg =
      addNodes (HashSet.toList $ Cfg.nodes innerCfg)
        . addEdges (edges innerCfg)
        . addEdges newPredEdges
        . addEdges newSuccEdges
        -- Removal of the node should happen first as the root node
        -- of the inner CFG shares the UUID with the removed node.
        . removeNode node 
        $ outerCfg


findNodeByUUID ::
  (Hashable a, Identifiable (CfNode [a]) UUID) =>
  UUID ->
  Cfg (CfNode [a]) ->
  Maybe (CfNode [a])
findNodeByUUID id cfg = case filter ((== id) . getNodeUUID) . HashSet.toList . G.nodes $ cfg of
  [] -> Nothing
  [x] -> Just x
  (x : _xs) -> Just x -- Should never happen. Maybe print warning?

stmtCtxs :: Stmt -> HashSet Ctx
stmtCtxs = foldMap (HSet.fromList . mapMaybe (view #ctx) . HSet.toList . getVarsFromExpr)

nodeCtxs :: CfNode [Stmt] -> HashSet Ctx
nodeCtxs = foldMap $ foldMap stmtCtxs

getCtxIndices :: PilCfg -> Bimap Int Ctx
getCtxIndices cfg = Bimap.fromList . fmap asTupleCtx $ ctxs
  where
    asTupleCtx :: Ctx -> (Int, Ctx)
    asTupleCtx ctx' = (fromIntegral $ ctx' ^. #ctxId, ctx')
    ctxs = HSet.toList . foldMap nodeCtxs . G.nodes $ cfg

substVars :: (PilVar -> PilVar) -> PilCfg -> PilCfg
substVars f = fmap (updateNodeData (PilA.substVars f))

gatherCfgData ::
  (Hashable stmt, Identifiable (CfNode [stmt]) UUID) =>
  Cfg (CfNode [stmt]) ->
  [stmt]
gatherCfgData cfg = concatMap getNodeData (HashSet.toList . G.nodes $ cfg)

-- | A convenience function that uses 'fromJust' to extract the root
-- node from a 'Maybe n'.
getRootNode :: Cfg n -> n
getRootNode cfg = fromJust $ Ag.getNode (cfg ^. #graph) (cfg ^. #rootId)

-- | A convenience function that looks up a node by an ID.
getNode :: Cfg n -> NodeId UUID -> Maybe n
getNode cfg = Ag.getNode (cfg ^. #graph)

