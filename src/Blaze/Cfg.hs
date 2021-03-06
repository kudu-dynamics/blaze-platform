{-# LANGUAGE RankNTypes #-}

module Blaze.Cfg (
  module Exports,
  module Blaze.Cfg,
) where

import qualified Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Cfg as Exports
import Blaze.Types.Pil (BranchCondOp, Expression, Statement (BranchCond, Exit), Stmt)
import qualified Blaze.Types.Pil as Pil
import Control.Lens (preview)
import qualified Data.Graph.Dom as Dlt
import qualified Data.HashMap.Strict as Hm
import qualified Data.HashSet as Hs
import qualified Data.IntMap.Strict as Im
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set

type DltMap a = IntMap (CfNode a)

type CfMap a = HashMap (CfNode a) Int

buildNodeMap :: Ord a => Cfg a -> DltMap a
buildNodeMap cfg =
  Im.fromList $ zip [0 ..] (Set.toList . G.nodes . view #graph $ cfg)

buildAdjMap :: [Dlt.Node] -> [Dlt.Edge] -> IntMap [Dlt.Node]
buildAdjMap ns =
  foldl' mergeEdges initialAdjMap
 where
  initialAdjMap :: IntMap [Dlt.Node]
  initialAdjMap = Im.fromList $ (,[]) <$> ns
  mergeEdges :: IntMap [Dlt.Node] -> Dlt.Edge -> IntMap [Dlt.Node]
  mergeEdges acc e =
    Im.adjust (snd e :) (fst e) acc

{- | Build a graph for use with Data.Graph.Dom for finding dominators
  and post-dominators.
  Note that we use unchecked HashMap lookups (!) as we know the
  entries must be present. That is, we know there is a corresponding
  Int for every CfNode.
-}
buildDltGraph ::
  forall a.
  (Hashable a, Ord a) =>
  Cfg a ->
  DltMap a ->
  Dlt.Rooted
buildDltGraph cfg dltMap =
  -- NB: Must use 'fromAdj' since 'fromEdges' will not include nodes
  -- that don't have outgoing edges.
  (cfMap Hm.! view #root cfg, Dlt.fromAdj dltAdj)
 where
  cfMap :: CfMap a
  cfMap = Hm.fromList $ swap <$> Im.assocs dltMap
  dltNodes :: [Dlt.Node]
  dltNodes = (cfMap Hm.!) <$> (Set.toList . G.nodes . view #graph $ cfg)
  dltEdges :: [Dlt.Edge]
  dltEdges = do
    (_, (src_, dst_)) <- G.edges . view #graph $ cfg
    return (cfMap Hm.! src_, cfMap Hm.! dst_)
  dltAdj :: [(Dlt.Node, [Dlt.Node])]
  dltAdj = Im.toList $ buildAdjMap dltNodes dltEdges

-- | Convert a Blaze CFG to a dom-lt flow graph
dltGraphFromCfg ::
  forall a.
  (Hashable a, Ord a) =>
  Cfg a ->
  (Dlt.Rooted, DltMap a)
dltGraphFromCfg cfg =
  (buildDltGraph cfg dltMap, dltMap)
 where
  dltMap :: DltMap a
  dltMap = buildNodeMap cfg

domHelper ::
  forall a.
  (Hashable a, Ord a) =>
  (Dlt.Rooted -> [(Dlt.Node, Dlt.Path)]) ->
  Cfg a ->
  HashMap (CfNode a) (HashSet (CfNode a))
domHelper f cfg =
  Hm.fromList . ((Hs.fromList <$>) <$>) $ domList
 where
  dltRooted :: Dlt.Rooted
  dltMap :: DltMap a
  (dltRooted, dltMap) = dltGraphFromCfg cfg
  domList :: [(CfNode a, [CfNode a])]
  domList = bimap (dltMap Im.!) ((dltMap Im.!) <$>) <$> f dltRooted

{- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
 to find dominators. The result is converted back to CfNodes before being returned.
 Per dom-lt, the complexity is:
 O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
-}
getDominators :: (Hashable a, Ord a) => Cfg a -> Dominators a
getDominators = Dominators . domHelper Dlt.dom

getPostDominators :: (Hashable a, Ord a) => Cfg a -> PostDominators a
getPostDominators = PostDominators . domHelper Dlt.pdom

lastStmtFrom :: (HasField' "nodeData" n [Stmt]) => n -> Maybe Stmt
lastStmtFrom = lastMay . view (field' @"nodeData")

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

parseTailCallNode :: BasicBlockNode [Stmt] -> Maybe (TailCallNode [Stmt])
parseTailCallNode node = do
  tailCallOp_ <- lastStmtFrom node >>= preview #_TailCall
  return $ TailCallNode node tailCallOp_

parseTerminalNode :: CfNode [Stmt] -> Maybe (TerminalNode [Stmt])
parseTerminalNode node = do
  bb <- node ^? #_BasicBlock
  (TermRet <$> parseReturnNode bb)
    <|> (TermExit <$> parseExitNode bb)
    <|> (TermTailCall <$> parseTailCallNode bb)

parseBranchNode ::
  ( CfNode [Stmt] -> Maybe [Stmt]
  ) ->
  CfNode [Stmt] ->
  Maybe (BranchNode [Stmt])
parseBranchNode getStmts node = do
  bb <- node ^? #_BasicBlock
  lastStmt <- lastMay =<< getStmts node
  case lastStmt of
    BranchCond op -> Just $ BranchNode bb op
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
      (Set.toList $ G.nodes (cfg ^. #graph))

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
    (forall a. Ord a => a -> a -> Bool) ->
    Maybe Bool
  intBinOp x p =
    p <$> getConstArg (x ^. #left)
      <*> getConstArg (x ^. #right)

  getConstArg :: Expression -> Maybe Int64
  getConstArg x = x ^? #op . #_CONST . #constant
