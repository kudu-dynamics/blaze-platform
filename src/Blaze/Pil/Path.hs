module Blaze.Pil.Path where

import Binja.Function (Function)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as MLIL
import qualified Binja.Variable as BNVar
import qualified Blaze.Pil as Pil
import Blaze.Prelude
import Blaze.Types.Function (CallSite)
import qualified Blaze.Types.Function as Func
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
  ( Converter,
    ConverterState (ConverterState),
    Ctx (Ctx),
    CtxIndex,
    Stmt,
    runConverter,
  )
import qualified Blaze.Types.Pil as Pil
import Data.Coerce (coerce)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

-- convert path to [Pil]

-- updates ctx if Function is new
-- TODO: WHy use this?
-- I don't think the ctx can change without a Call node
maybeUpdateCtx :: Function -> Converter ()
maybeUpdateCtx fn = do
  cctx <- get
  when (fn /= cctx ^. Pil.ctx . Pil.func) . enterNewCtx $ fn

enterNewCtx :: Function -> Converter ()
enterNewCtx fn = do
  Pil.ctxMaxIdx %= incIndex
  i <- use Pil.ctxMaxIdx
  outerCtx <- use Pil.ctx
  
  let innerCtx = newCtx i outerCtx
  Pil.ctx .= innerCtx
  pushCtx innerCtx

  where
    newCtx :: CtxIndex -> Ctx -> Ctx
    newCtx i ctx = ctx & Pil.func .~ fn
                       & Pil.ctxIndex .~ i
    incIndex :: CtxIndex -> CtxIndex
    incIndex = (+ 1)
    pushCtx :: Ctx -> Converter ()
    pushCtx ctx = Pil.ctxStack %= NE.cons ctx

retCtx :: Converter ()
retCtx = do
  _innerCtx <- popCtx
  outerCtx <- peekCtx
  Pil.ctx .= outerCtx
  where
    popCtx :: Converter Ctx
    popCtx = do
      stack <- use Pil.ctxStack
      let (_innerCtx, mStack) = NE.uncons stack
      case mStack of 
        Just stack' -> do
          Pil.ctxStack .= stack'
          return _innerCtx
        Nothing -> error "The converter stack should never be empty."
    peekCtx :: Converter Ctx
    peekCtx = do
      stack <- use Pil.ctxStack
      return $ NE.head stack

peekPrevCtx :: Converter (Maybe Ctx)
peekPrevCtx = do
  stack <- use Pil.ctxStack
  return $ headMay . NE.tail $ stack

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  maybeUpdateCtx $ sb ^. Path.func
  instrs <- liftIO $ do
    mlilFunc <- BNFunc.getMLILSSAFunction $ sb ^. Path.func
    mapM (MLIL.instruction mlilFunc) [(sb ^. Path.start) .. (sb ^. Path.end - 1)]
  Pil.convertInstrs instrs

convertConditionNode :: ConditionNode -> Converter [Stmt]
convertConditionNode n = do
  ctx <- use Pil.ctx
  let expr = Pil.convertExpr ctx $ n ^. Path.condition
  return . (:[]) . Pil.Constraint . Pil.ConstraintOp $
    if n ^. Path.trueOrFalseBranch
    then expr
    else Pil.Expression (expr ^. Pil.size) (Pil.NOT . Pil.NotOp $ expr)

convertAbstractCallNode :: AbstractCallNode -> Converter [Stmt]
convertAbstractCallNode n = do
  ctx <- use Pil.ctx
  Pil.convertCallInstruction ctx (n ^. Path.callSite . Func.callInstr)

-- TODO: Check this earlier in the conversion process? 
getCallDestFunc :: CallSite -> Function
getCallDestFunc x = case x ^. Func.callDest of
  (Func.DestFunc f) -> f
  _ -> error "Only calls to known functions may be expanded."

paramArgDef :: Pil.Symbol -> Pil.Expression -> Converter Stmt
paramArgDef param arg = do
  ctx <- use Pil.ctx
  -- TODO: Sort out use of mapsTo when defining the PilVar
  let pilVar = Pil.PilVar param (Just ctx) HS.empty
  return $ Pil.Def (Pil.DefOp pilVar arg)

createParamSymbol :: Int -> BNVar.Variable -> Pil.Symbol
createParamSymbol version var =
  coerce name <> "#" <> show version
    where
      name :: Text
      name = var ^. BNVar.name

convertCallNode :: CallNode -> Converter [Stmt]
convertCallNode n = do
  let destFunc =set getCallDestFunc $ n ^. Path.callSite
  enterNewCtx destFunc
  ctx <- use Pil.ctx
  mPrevCtx <- peekPrevCtx
  let prevCtx = case mPrevCtx of 
                  Nothing -> error "No previous context found."
                  Just prevCtx_ -> prevCtx_
  params <- liftIO $ BNVar.getFunctionParameterVariables destFunc
  let callInstr = n ^. (Path.callSite . Func.callInstr)
      argExprs = Pil.convertExpr prevCtx <$> callInstr ^. Func.params
      paramSyms = createParamSymbol 0 <$> params
  defs <- zipWithM paramArgDef paramSyms argExprs
  return $ (Pil.EnterContext . Pil.EnterContextOp $ ctx) : defs

convertRetNode :: RetNode -> Converter [Stmt]
convertRetNode _ = do
  leavingCtx <- use Pil.ctx
  retCtx
  returningCtx <- use Pil.ctx
  return [ Pil.ExitContext $ Pil.ExitContextOp leavingCtx returningCtx ]

convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
convertNode (Condition x) = convertConditionNode x
convertNode (AbstractCall x) = convertAbstractCallNode x
convertNode (Call x) = convertCallNode x
convertNode (Ret x) = convertRetNode x
convertNode _ = return [] -- TODO

convertNodes :: [Node] -> Converter [Stmt]
convertNodes = fmap concat . traverse convertNode

createStartCtx :: Function -> Ctx
createStartCtx func = Ctx func 0

createStartConverterState :: AlgaPath -> Function -> ConverterState
createStartConverterState path func = 
  ConverterState path (startCtx ^. Pil.ctxIndex) (startCtx :| []) startCtx []
    where 
      startCtx :: Ctx
      startCtx = createStartCtx func

convertPath :: Function -> AlgaPath -> IO [Stmt]
convertPath startFunc path =
  fmap (concat . fst) . flip runConverter (createStartConverterState path startFunc) . traverse convertNode . Path.toList $ path
