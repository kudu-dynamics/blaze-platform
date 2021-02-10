module Blaze.Import.Source.BinaryNinja.Pil.Path where

import Binja.Function (Function)
import qualified Binja.Function as BNFunc
import Binja.Core (BNBinaryView)
import qualified Binja.MLIL as MLIL
import Blaze.Pil as Pil
import qualified Binja.Variable as BNVar
-- import qualified Blaze.Pil as Pil
import Blaze.Prelude hiding (sym)
import Blaze.Types.Function (CallSite)
import qualified Binja.Core as BN
import qualified Blaze.Types.Function as Func
import qualified Blaze.Import.Source.BinaryNinja.Pil as Pil
import Blaze.Types.Path
  ( AbstractCallNode,
    CallNode,
    ConditionNode,
    Node
      ( AbstractCall,
        Call,
        Condition,
        Ret,
        SubBlock
      ),
    RetNode,
    SubBlockNode,
  )
import qualified Blaze.Types.Path as Path
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil
  ( Ctx,
    CtxIndex,
    Stmt
  )
import qualified Blaze.Types.Pil as Pil
import qualified Data.List.NonEmpty as NE
import Blaze.Import.Source.BinaryNinja.Pil (Converter)
import qualified Blaze.Types.CallGraph as CG
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as BNCG

-- convert path to [Pil]

-- updates ctx if Function is new
-- TODO: WHy use this?
-- I don't think the ctx can change without a Call node
maybeUpdateCtx :: CG.Function -> Converter ()
maybeUpdateCtx fn = do
  cctx <- get
  when (fn /= cctx ^. #ctx . #func) . enterNewCtx $ fn

enterNewCtx :: CG.Function -> Converter ()
enterNewCtx fn = do
  #ctxMaxIdx %= incIndex
  i <- use #ctxMaxIdx
  outerCtx <- use #ctx
  
  let innerCtx = newCtx i outerCtx
  #ctx .= innerCtx
  pushCtx innerCtx

  where
    newCtx :: CtxIndex -> Ctx -> Ctx
    newCtx i ctx = ctx & #func .~ fn
                       & #ctxIndex .~ i
    incIndex :: CtxIndex -> CtxIndex
    incIndex = (+ 1)
    pushCtx :: Ctx -> Converter ()
    pushCtx ctx = #ctxStack %= NE.cons ctx

retCtx :: Converter ()
retCtx = do
  _innerCtx <- popCtx
  outerCtx <- peekCtx
  #ctx .= outerCtx
  where
    popCtx :: Converter Ctx
    popCtx = do
      stack <- use #ctxStack
      let (_innerCtx, mStack) = NE.uncons stack
      case mStack of 
        Just stack' -> do
          #ctxStack .= stack'
          return _innerCtx
        Nothing -> error "The converter stack should never be empty."
    peekCtx :: Converter Ctx
    peekCtx = do
      stack <- use #ctxStack
      return $ NE.head stack

peekPrevCtx :: Converter (Maybe Ctx)
peekPrevCtx = do
  stack <- use #ctxStack
  return $ headMay . NE.tail $ stack

convertBinjaFunc :: Function -> Converter CG.Function
convertBinjaFunc func' = do
  bv <- use #binaryView
  liftIO $ BNCG.convertFunction bv func'

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  func' <- convertBinjaFunc $ sb ^. #func
  maybeUpdateCtx func'
  instrs <- liftIO $ do
    mlilFunc <- BNFunc.getMLILSSAFunction $ sb ^. #func
    mapM (MLIL.instruction mlilFunc) [(sb ^. #start) .. (sb ^. #end - 1)]
  Pil.convertInstrs instrs

convertConditionNode :: ConditionNode -> Converter [Stmt]
convertConditionNode n = do
  expr <- Pil.convertExpr $ n ^. #condition
  return . (:[]) . Pil.Constraint . Pil.ConstraintOp $
    if n ^. #trueOrFalseBranch
    then expr
    else Pil.Expression (expr ^. #size) (Pil.NOT . Pil.NotOp $ expr)

convertAbstractCallNode :: AbstractCallNode -> Converter [Stmt]
convertAbstractCallNode n = do
  Pil.convertCallInstruction $ n ^. #callSite . #callInstr

-- TODO: Check this earlier in the conversion process? 
getCallDestFunc :: CallSite -> Function
getCallDestFunc x = case x ^. #callDest of
  (Func.DestFunc f) -> f
  _ -> error "Only calls to known functions may be expanded."

defSymbol :: Pil.Symbol -> Pil.Expression -> Converter Stmt
defSymbol sym expr = do
  ctx <- use #ctx
  -- TODO: Sort out use of mapsTo when defining the PilVar
  let pilVar = Pil.PilVar sym (Just ctx)
  return $ Pil.Def (Pil.DefOp pilVar expr)

defPilVar :: Pil.PilVar -> Pil.Expression -> Stmt
defPilVar pilVar expr = Pil.Def (Pil.DefOp pilVar expr)

createParamSymbol :: Int -> BNVar.Variable -> Pil.Symbol
createParamSymbol version var =
  coerce name <> "#" <> show version
    where
      name :: Text
      name = var ^. BNVar.name

convertCallNode :: CallNode -> Converter [Stmt]
convertCallNode n = do
  let destFunc = getCallDestFunc $ n ^. #callSite
      callInstr = n ^. (#callSite . #callInstr)
  -- The argument expressions should be converted in the caller's context.
  -- Convert before entering the new callee's context.
  cgDestFunc <- convertBinjaFunc destFunc
  argExprs <- traverse Pil.convertExpr (callInstr ^. #params)
  enterNewCtx cgDestFunc
  ctx <- use #ctx
  params <- liftIO $ BNVar.getFunctionParameterVariables destFunc
  let paramSyms = createParamSymbol 0 <$> params
  defs <- zipWithM defSymbol paramSyms argExprs
  return $ (Pil.EnterContext . Pil.EnterContextOp $ ctx) : defs

getRetVals_ :: SubBlockNode -> Converter [Pil.Expression]
getRetVals_ node = do
  mlilFunc <- liftIO $ BNFunc.getMLILSSAFunction $ node ^. #func
  lastInstr <- liftIO $ MLIL.instruction mlilFunc $ node ^. #end - 1
  case lastInstr ^? (MLIL.op . MLIL._RET) of
    (Just retOp) ->
      traverse Pil.convertExpr (retOp ^. MLIL.src)
    Nothing ->
      error "Missing required return instruction."

getRetVals :: RetNode -> Converter [Pil.Expression]
getRetVals retNode = do
  path <- use #path
  case Path.pred (Ret retNode) path >>= (^? #_SubBlock) of
    (Just prevNode) ->
      getRetVals_ prevNode
    Nothing ->
      error "RetNode not preceded by a SubBlockNode."

isPathStmt :: Stmt -> Bool
isPathStmt stmt =
  case stmt of
    Pil.BranchCond _ -> False 
    Pil.UnimplInstr _ -> False
    _ -> True

convertRetNode :: RetNode -> Converter [Stmt]
convertRetNode node = do
  leavingCtx <- use #ctx
  retVals <- getRetVals node
  retCtx
  returningCtx <- use #ctx
  resultVars <- traverse Pil.convertToPilVarAndLog (node ^. #callSite . #callInstr . #outputDest)
  let defs = zipWith defPilVar resultVars retVals
  return $ Pil.ExitContext (Pil.ExitContextOp leavingCtx returningCtx) : defs

convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
convertNode (Condition x) = convertConditionNode x
convertNode (AbstractCall x) = convertAbstractCallNode x
convertNode (Call x) = convertCallNode x
convertNode (Ret x) = convertRetNode x
convertNode _ = return [] -- TODO

convertNodes :: [Node] -> Converter [Stmt]
convertNodes = fmap concat . traverse convertNode

convertPath_ :: AlgaPath -> Converter [Stmt]
convertPath_ = convertNodes . Path.toList

convertPath :: BNBinaryView -> AlgaPath -> IO [Stmt]
convertPath bv path = case Path.startFunction path of
  Nothing -> return []
  Just func' -> do
    cgFunc <- liftIO $ BNCG.convertFunction bv func'
    addrSize' <- BN.getViewAddressSize bv
    let st = Pil.mkConverterState bv Pil.knownFuncDefs addrSize' cgFunc path
    stmts <- fst <$> Pil.runConverter (convertPath_ path) st
    return $ filter isPathStmt stmts
