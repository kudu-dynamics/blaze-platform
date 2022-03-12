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


getCallTargetFunction :: CallDest a -> Maybe Function
getCallTargetFunction = \case
  CallFunc fun -> Just fun
  _ -> Nothing

getCallStmt :: PilCallNode -> Maybe CallStatement
getCallStmt node = do
  stmt <- lastMay (node ^. #nodeData)
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
mkEnterFuncNode :: UUID -> Ctx -> Ctx -> CallStatement -> EnterFuncNode [Stmt]
mkEnterFuncNode uuid' outerCtx calleeCtx callStmt =
  EnterFuncNode outerCtx calleeCtx uuid' stmts
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
generateVars ctx baseName id (expr : rest) =
  (PilVar (baseName <> show id) (Just ctx), expr) : generateVars ctx baseName (id + 1) rest
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
  InterCfg ->
  PilCallNode ->
  Function ->
  Builder a (Either ExpandCallError InterCfg)
expandCall callerCfg callNode targetFunc = do
  getCfg_ <- use #getCfg
  -- ctxId <- getNextCtxIndex
  runExceptT $ do
    callStmt <-
      liftMaybe (NotCallStatement $ callNode ^. #nodeData) $
        getCallStmt callNode
    (ImportResult targetCtx targetCfg _) <-
      liftMaybeIO (FailedToCreateCfg targetFunc) $
        getCfg_ targetFunc
    leaveFuncUUID <- liftIO randomIO
    return $
      expandCall_
        callerCfg
        callNode
        callStmt
        (InterCfg targetCfg)
        targetCtx
        leaveFuncUUID

expandCall_ ::
  InterCfg ->
  PilCallNode ->
  CallStatement ->
  InterCfg ->
  Ctx ->
  UUID ->
  InterCfg
expandCall_
  (InterCfg callerCfg)
  callNode
  callStmt
  (InterCfg targetCfg)
  targetCtx
  leaveFuncUUID =
    substNode (InterCfg callerCfg) (Call callNode) (InterCfg wrappedTargetCfg) leaveFunc
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

    prevRoot = targetCfg ^. #root
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
           targetCfg & #root .~ enterFunc')
        retNodes

getRetNodes :: PilCfg -> [ReturnNode [Stmt]]
getRetNodes cfg = 
  mapMaybe (\tn -> tn ^? #_TermRet) $ NEList.toList (getTerminalBlocks cfg)

-- | Substitute a node with another interprocedural CFG.
substNode :: InterCfg -> PilNode -> InterCfg -> PilNode -> InterCfg
substNode (InterCfg outerCfg) node (InterCfg innerCfg) exitNode' =
  InterCfg $ Cfg.substNode outerCfg node innerCfg exitNode'
