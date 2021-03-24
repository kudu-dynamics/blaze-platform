module Blaze.Cfg.Interprocedural (
  module Blaze.Cfg.Interprocedural,
  module Exports,
) where

import Blaze.Prelude hiding (Symbol, sym)

import Blaze.Cfg
import Blaze.Function (FuncParamInfo (FuncParamInfo, FuncVarArgInfo), Function)
import qualified Blaze.Graph as G
import Blaze.Types.Cfg.Interprocedural as Exports
import Blaze.Types.Graph.Unique (Unique, mkUnique)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (
  CallDest (CallFunc),
  CallStatement,
  Ctx,
  DefOp (DefOp),
  DefPhiOp (DefPhiOp),
  Expression,
  PilVar (PilVar),
  Statement (Def, DefPhi),
  Stmt,
  Symbol,
  mkCallStatement,
 )
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import Control.Lens.Setter (set)
import Blaze.Graph (LEdge(LEdge), Edge(Edge))

getCallTarget :: CallStatement -> Maybe Function
getCallTarget callStmt = do
  let callDest = callStmt ^. #callOp . #dest
  case callDest of
    CallFunc fun -> Just fun
    _ -> Nothing

getCallStmt :: PilCallNode -> Maybe CallStatement
getCallStmt node = do
  stmt <- headMay (node ^. #nodeData)
  mkCallStatement stmt

mkParamVar :: Ctx -> FuncParamInfo -> PilVar
mkParamVar ctx funcParam =
  PilVar sym (Just ctx)
 where
  sym :: Symbol
  sym = case funcParam of
    FuncParamInfo paramInfo -> coerce $ paramInfo ^. #name
    FuncVarArgInfo paramInfo -> coerce $ paramInfo ^. #name

-- | Create a node that indicates a context switch between two function contexts.
mkEnterFuncNode :: Ctx -> Ctx -> CallStatement -> EnterFuncNode [Stmt]
mkEnterFuncNode outerCtx calleeCtx callStmt =
  EnterFuncNode outerCtx calleeCtx stmts
 where
  paramVars :: [PilVar]
  paramVars = mkParamVar calleeCtx <$> calleeCtx ^. #func . #params
  stmts :: [Stmt]
  stmts = Def . uncurry DefOp <$> zip paramVars (callStmt ^. #args)

{- |Generate a defintion for the result variable in the outer (caller) context
where the a phi function containing variables that reference all the possible
return expressions from potentially multiple return sites in the callee are listed.
Since a return statement may return an expression, we first ensure a variable referencing
the expressions is available by generating one for each expression. This may end up
introducing spurious variable copies which can be reduced with copy propagation.
-}
mkLeaveFuncNode :: Ctx -> Ctx -> CallStatement -> [Expression] -> LeaveFuncNode [Stmt]
mkLeaveFuncNode outerCtx calleeCtx callStmt retExprs =
  LeaveFuncNode calleeCtx outerCtx stmts
 where
  resultVar :: Maybe PilVar
  resultVar = callStmt ^. #resultVar
  retVars :: [(PilVar, Expression)]
  retVars = generateVars calleeCtx "retVar_" 0 retExprs
  retDefs :: [Stmt]
  retDefs = Def . uncurry DefOp <$> retVars
  resultDef :: Maybe Stmt
  resultDef = DefPhi <$> (DefPhiOp <$> resultVar <*> Just (fst <$> retVars))
  stmts :: [Stmt]
  stmts = case resultDef of
    Just stmt -> retDefs ++ [stmt]
    Nothing -> []

generateVars :: Ctx -> Text -> Int -> [Expression] -> [(PilVar, Expression)]
generateVars ctx baseName id (expr : rest) =
  (PilVar (baseName <> show id) (Just ctx), expr) : generateVars ctx baseName (id + 1) rest
generateVars _ _ _ [] = []

{- | Expand a call by substituting a call node with the CFG corresponding to the
 call destination.
-}
expandCall ::
  Ctx ->
  Ctx ->
  InterCfg ->
  Unique PilCallNode ->
  Builder a (Maybe InterCfg)
expandCall callerCtx calleeCtx icfg callNode = do
  getCfg_ <- use #getCfg
  case getCallStmt $ callNode ^. #node of
    Just callStmt ->
      -- TODO: CallNode should provide the call statement from a record field
      case getCallTarget callStmt of
        Just targetFunc -> do
          -- result <- liftIO $ getCfg_ ctxIndex targetFunc
          result <- liftIO $ getCfg_ (calleeCtx ^. #ctxIndex) targetFunc
          case result of
            Just (ImportResult targetCfg _) -> do
              (targetCfg', leaveFunc) <- expandCall_ callerCtx calleeCtx callStmt targetCfg
              return . Just $ substNode icfg (Call <$> callNode) (InterCfg targetCfg') leaveFunc
            Nothing -> return Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing

-- TODO: Change `MonadIO` to be some UUID generator mtl constraint
expandCall_ ::
  (MonadIO m) =>
  Ctx ->
  Ctx ->
  CallStatement ->
  PilCfg ->
  m (PilCfg, Unique PilNode)
expandCall_ callerCtx calleeCtx callStmt targetCfg = do
  enterFuncNode <- mkUnique enterFunc
  leaveFuncNode <- mkUnique leaveFunc
  return ( targetCfg' enterFuncNode leaveFuncNode
         , leaveFuncNode)
  -- Connect the enter and leave function nodes to the targetCfg
  where
    enterFunc :: PilNode
    enterFunc = EnterFunc $ mkEnterFuncNode callerCtx calleeCtx callStmt
    leaveFunc :: PilNode
    leaveFunc = LeaveFunc $ mkLeaveFuncNode callerCtx calleeCtx callStmt (getRetExprs targetCfg)
    prevRoot :: Unique PilNode
    prevRoot = targetCfg ^. #root
    retNodes :: [Unique (ReturnNode [Stmt])]
    retNodes = getRetNodes targetCfg
    linkRetNodes :: Unique PilNode -> PilCfg -> Unique (ReturnNode [Stmt]) -> PilCfg
    linkRetNodes leaveFuncNode cfg retNode =
      G.addEdge
      (G.LEdge UnconditionalBranch (G.Edge (fmap (\n -> BasicBlock $ n ^. #basicBlock) retNode) leaveFuncNode))
      cfg
    targetCfg' :: Unique PilNode -> Unique PilNode -> PilCfg
    targetCfg' enterFuncNode leaveFuncNode = 
      foldl'
        (linkRetNodes leaveFuncNode)
        ( G.addEdge (LEdge UnconditionalBranch (Edge enterFuncNode prevRoot))
          $ targetCfg & #root .~ enterFuncNode
        )
        retNodes

getRetNodes :: PilCfg -> [Unique (ReturnNode [Stmt])]
getRetNodes cfg = 
  mapMaybe (traverse (^? #_TermRet)) $ NEList.toList (getTerminalBlocks cfg)

-- | Substitute a node with another interprocedural CFG.
substNode :: InterCfg -> Unique PilNode -> InterCfg -> Unique PilNode -> InterCfg
substNode
  (InterCfg (Cfg outerGraph outerRoot))
  node
  (InterCfg (Cfg innerGraph innerRoot))
  exitNode =
    -- Check if the node we are substituting is the outer CFG's root
    if outerRoot ^. #node /= node ^. #node
      then InterCfg (Cfg graph_ outerRoot)
      else InterCfg (Cfg graph_ innerRoot)
   where
    -- TODO: Improve Graph API for fetching edges
    predEdges :: [LEdge BranchType (Unique PilNode)]
    predEdges = filter (\(LEdge _ (Edge _ dest)) -> dest == node) $ G.edges outerGraph
    succEdges :: [LEdge BranchType (Unique PilNode)]
    succEdges = filter (\(LEdge _ (Edge source _)) -> source == node) $ G.edges outerGraph
    newPredEdges :: [LEdge BranchType (Unique PilNode)]
    newPredEdges = set (#edge . #dst) innerRoot <$> predEdges
    newSuccEdges :: [LEdge BranchType (Unique PilNode)]
    newSuccEdges = set (#edge . #src) exitNode <$> succEdges
    graph_ :: ControlFlowGraph [Stmt]
    graph_ = 
      G.removeNode node
      . G.addNodes (Set.toList $ G.nodes innerGraph)
      . G.addEdges (G.edges innerGraph) 
      . G.addEdges newPredEdges
      . G.addEdges newSuccEdges $ outerGraph
