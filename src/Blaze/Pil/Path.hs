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
                                                   , TypeEnv( TypeEnv )
                                                   , runConverter
                                                   )
import qualified Blaze.Types.Pil      as Pil
import qualified Data.HashMap.Strict  as HMap
import qualified Data.HashSet         as HSet

-- convert path to [Pil]

-- updates ctx if Function is new
-- TODO: WHy use this?
-- I don't think the ctx can change without a Call node
maybeUpdateCtx :: Function -> Converter ()
maybeUpdateCtx fn = do
  cctx <- get
  when (Just fn /= cctx ^. Pil.ctx . Pil.func) . enterNewCtx $ Just fn

enterNewCtx :: Maybe Function -> Converter ()
enterNewCtx fn = do
  Pil.ctxIndexCounter %= incIndex
  i <- use Pil.ctxIndexCounter
  Pil.ctx %= newCtx i
  where
    newCtx i ctx = ctx & Pil.func .~ fn
                       & Pil.ctxIndex .~ i
    incIndex :: Maybe CtxIndex -> Maybe CtxIndex
    incIndex Nothing = Just 0
    incIndex (Just n) = Just (n + 1)

retCtxTo :: Maybe Function -> Converter ()
retCtxTo fn = do
  Pil.ctxIndexCounter %= fmap (subtract 1)
  i <- use Pil.ctxIndexCounter
  Pil.ctx %= newCtx i
  where
    newCtx i ctx = ctx & Pil.func .~ fn
                       & Pil.ctxIndex .~ i

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

getCallDestFunc :: CallSite -> Maybe Function
getCallDestFunc x = case x ^. Func.callDest of
  (Func.DestFunc f) -> Just f
  _ -> Nothing

convertCallNode :: CallNode -> Converter [Stmt]
convertCallNode n = do
  enterNewCtx . getCallDestFunc $ n ^. Path.callSite
  sCtx <- getSimpleCtx
  return [ Pil.EnterContext . Pil.EnterContextOp $ sCtx ]

convertRetNode :: RetNode -> Converter [Stmt]
convertRetNode n = do
  leavingCtx <- getSimpleCtx
  retCtxTo . Just $ n ^. Path.callSite . Func.caller
  returningCtx <- getSimpleCtx
  return [ Pil.ExitContext $ Pil.ExitContextOp leavingCtx returningCtx ]
--   enterNewCtx $ n ^. Path.func
--   sCtx <- getSimpleCtx
--   return [Pil.EnterContext . Pil.EnterContextOp $ sCtx]


convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
convertNode (Condition x) = convertConditionNode x
convertNode (AbstractCall x) = convertAbstractCallNode x
convertNode (Call x) = convertCallNode x
convertNode (Ret x) = convertRetNode x
convertNode _ = return [] -- TODO


convertNodes :: [Node] -> Converter [Stmt]
convertNodes = fmap concat . traverse convertNode

startCtx :: Ctx
startCtx = Ctx Nothing Nothing HSet.empty (TypeEnv HMap.empty)

startConverterCtx :: ConverterCtx
startConverterCtx = ConverterCtx Nothing startCtx

--- TODO: Keep track of ctx-index separately from Ctx
--- always increment on every new function Ctx change
convertPath :: Path p => p -> IO [Stmt]
convertPath =
  fmap (concat . fst) . flip runConverter startConverterCtx . traverse convertNode . Path.toList
