module Blaze.Pil.Path where

import           Blaze.Prelude

import           Binja.Function                    ( Function )

import qualified Binja.Function       as HFunction
import qualified Binja.MLIL           as MLIL
import qualified Blaze.Pil            as Pil
import Blaze.Types.Function (CallSite)
import qualified Blaze.Types.Function as Func
import           Blaze.Types.Path                  ( AbstractCallNode
                                                   , CallNode
                                                   , RetNode
                                                   , ConditionNode
                                                   , Node( AbstractCall
                                                         , Condition
                                                         , SubBlock
                                                         , Call
                                                         , Ret
                                                         )
                                                   , Path
                                                   , SubBlockNode
                                                   )
import qualified Blaze.Types.Path     as Path
import           Blaze.Types.Pil                   ( ConverterCtx( ConverterCtx )
                                                   , SimpleCtx(SimpleCtx)
                                                   , Ctx( Ctx )
                                                   , CtxIndex
                                                   , Stmt
                                                   , Converter
                                                   , runConverter
                                                   )
import qualified Blaze.Types.Pil      as Pil
import qualified Data.HashSet         as HSet
import qualified Data.List.NonEmpty   as NE

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

getSimpleCtx :: Converter SimpleCtx
getSimpleCtx = do
  ctx <- view Pil.ctx <$> get
  return $ SimpleCtx (ctx ^. Pil.func) (ctx ^. Pil.ctxIndex)

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  maybeUpdateCtx $ sb ^. Path.func
  instrs <- liftIO $ do
    mlilFunc <- HFunction.getMLILSSAFunction $ sb ^. Path.func
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
  liftIO $ Pil.convertCallInstruction ctx (n ^. Path.callSite . Func.callInstr)

-- TODO: Check this earlier in the conversion process? 
getCallDestFunc :: CallSite -> Function
getCallDestFunc x = case x ^. Func.callDest of
  (Func.DestFunc f) -> f
  _ -> error "Only calls to known functions may be expanded."

convertCallNode :: CallNode -> Converter [Stmt]
convertCallNode n = do
  enterNewCtx . getCallDestFunc $ n ^. Path.callSite
  sCtx <- getSimpleCtx
  return [ Pil.EnterContext . Pil.EnterContextOp $ sCtx ]

convertRetNode :: RetNode -> Converter [Stmt]
convertRetNode _ = do
  leavingCtx <- getSimpleCtx
  retCtx
  returningCtx <- getSimpleCtx
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
createStartCtx func = Ctx func 0 HSet.empty

createStartConverterCtx :: Function -> ConverterCtx
createStartConverterCtx func = 
  ConverterCtx (startCtx ^. Pil.ctxIndex) (startCtx :| []) startCtx
    where 
      startCtx :: Ctx
      startCtx = createStartCtx func

convertPath :: Path p => Function -> p -> IO [Stmt]
convertPath startFunc =
  fmap (concat . fst) . flip runConverter (createStartConverterCtx startFunc) . traverse convertNode . Path.toList
