module Blaze.Cfg.Interprocedural (
  module Blaze.Cfg.Interprocedural,
  module Exports,
) where

import Blaze.Prelude hiding (Symbol, sym)

import Blaze.Cfg hiding (BasicBlockNode (ctx), CallNode (ctx), callDest, substNode)
import qualified Blaze.Cfg as Cfg
import Blaze.Function (FuncParamInfo (FuncParamInfo, FuncVarArgInfo), Function)
import Blaze.Types.Cfg.Interprocedural as Exports
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
import qualified Blaze.Graph as G


getCallTargetFunction :: CallDest a -> Maybe Function
getCallTargetFunction = \case
  CallFunc fun -> Just fun
  _ -> Nothing

getCallStmt :: PilCallNode -> Maybe CallStatement
getCallStmt node = do
  stmt <- lastMay (node ^. #nodeData)
  mkCallStatement stmt

getParamSym :: FuncParamInfo -> Symbol
getParamSym = \case
  FuncParamInfo paramInfo -> coerce $ paramInfo ^. #name
  FuncVarArgInfo paramInfo -> coerce $ paramInfo ^. #name

-- | Create a node that indicates a context switch between two function contexts.
mkEnterFuncNode :: UUID -> Ctx -> Ctx -> CallStatement -> EnterFuncNode [Stmt]
mkEnterFuncNode uuid' outerCtx calleeCtx callStmt =
  EnterFuncNode outerCtx calleeCtx uuid' stmts
  where
    mkParamVar :: (FuncParamInfo, Expression) -> (PilVar, Expression)
    mkParamVar (pinfo, x) =
      ( PilVar (fromByteBased $ x ^. #size) (Just calleeCtx) (getParamSym pinfo)
      , x )
    paramVarsWithExpressions :: [(PilVar, Expression)]
    paramVarsWithExpressions = mkParamVar
      <$> zip (calleeCtx ^. #func . #params) (callStmt ^. #args)
    stmts :: [Stmt]
    stmts = Def . uncurry DefOp <$> paramVarsWithExpressions

{- |Generate a defintion for the result variable in the outer (caller) context
where the a phi function containing variables that reference all the possible
return expressions from potentially multiple return sites in the callee are listed.
Since a return statement may return an expression, we first ensure a variable referencing
the expressions is available by generating one for each expression. This may end up
introducing spurious variable copies which can be reduced with copy propagation.
-}
mkLeaveFuncNode :: UUID -> Ctx -> Ctx -> CallStatement -> [Expression] -> LeaveFuncNode [Stmt]
mkLeaveFuncNode uuid' outerCtx calleeCtx callStmt retExprs =
  LeaveFuncNode calleeCtx outerCtx uuid' stmts
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
generateVars ctx baseName id (expr : rest)
  = (PilVar (fromByteBased $ expr ^. #size) (Just ctx) $ baseName <> show id, expr)
  : generateVars ctx baseName (id + 1) rest
generateVars _ _ _ [] = []

data ExpandCallError = NotCallStatement [Stmt]
                     | CallDestNotFunction (CallDest ())
                     | FailedToCreateCfg Function
                     deriving (Eq, Ord, Show, Generic)

getTargetFunc :: PilCallNode -> Maybe Function
getTargetFunc callNode = getCallTargetFunction $ callNode ^. #callDest

{- | Expand a call by substituting a call node with the CFG corresponding to the
 call destination.
-}
expandCall ::
  PilCfg ->
  PilCallNode ->
  Function ->

  Builder a (Either ExpandCallError PilCfg)
expandCall callerCfg callNode targetFunc = do
  getCfg_ <- use #getCfg
  -- ctxId <- getNextCtxIndex
  runExceptT $ do
    callStmt <-
      liftMaybe (NotCallStatement $ callNode ^. #nodeData) $
        getCallStmt callNode
    (ImportResult targetCtx _ targetCfg) <-
      liftMaybeIO (FailedToCreateCfg targetFunc) $
        getCfg_ targetFunc (callerCfg ^. #nextCtxIndex)
    leaveFuncUUID <- liftIO randomIO

    return . Cfg.incNextCtxIndex $ expandCall_
      callerCfg
      callNode
      callStmt
      targetCfg
      targetCtx
      leaveFuncUUID

expandCall_ ::
  PilCfg ->
  PilCallNode ->
  CallStatement ->
  PilCfg ->
  Ctx ->
  UUID ->
  PilCfg
expandCall_
  callerCfg
  callNode
  callStmt
  targetCfg
  targetCtx
  leaveFuncUUID =
    Cfg.substNode callerCfg (Call callNode) wrappedTargetCfg leaveFunc
    where
      callerCtx = callNode ^. #ctx
      (WrappedTargetCfg wrappedTargetCfg leaveFunc) =
        wrapTargetCfg
          (callNode ^. #uuid)
          leaveFuncUUID
          callerCtx
          targetCtx
          callStmt
          targetCfg

data WrappedTargetCfg = WrappedTargetCfg
  { wrappedCfg :: PilCfg
  , exitNode :: PilNode
  }
  deriving (Eq, Show, Generic)

wrapTargetCfg ::
  UUID ->
  UUID ->
  Ctx ->
  Ctx ->
  CallStatement ->
  PilCfg ->
  WrappedTargetCfg
wrapTargetCfg enterFuncUUID leaveFuncUUID callerCtx calleeCtx callStmt targetCfg =
  WrappedTargetCfg (targetCfg' enterFunc leaveFunc) leaveFunc
  where
    enterFunc =
      EnterFunc $
        mkEnterFuncNode enterFuncUUID callerCtx calleeCtx callStmt
    leaveFunc =
      LeaveFunc
        . mkLeaveFuncNode leaveFuncUUID callerCtx calleeCtx callStmt
        $ getRetExprs targetCfg
    -- Connect the enter and leave function nodes to the targetCfg
    prevRoot = Cfg.getRootNode targetCfg
    retNodes = getRetNodes targetCfg
    linkRetNodes :: PilNode -> PilCfg -> ReturnNode [Stmt] -> PilCfg
    linkRetNodes leaveFunc' cfg retNode =
      Cfg.addEdge
        (CfEdge (BasicBlock $ retNode ^. #basicBlock) leaveFunc' UnconditionalBranch)
        cfg
    targetCfg' :: PilNode -> PilNode -> PilCfg
    targetCfg' enterFunc' leaveFunc' =
      foldl'
        (linkRetNodes leaveFunc')
        (Cfg.addEdge (CfEdge enterFunc' prevRoot UnconditionalBranch) $
           targetCfg & #rootId .~ G.getNodeId enterFunc')
        retNodes

getRetNodes :: PilCfg -> [ReturnNode [Stmt]]
getRetNodes cfg =
  mapMaybe (\tn -> tn ^? #_TermRet) $ NEList.toList (getTerminalBlocks cfg)
