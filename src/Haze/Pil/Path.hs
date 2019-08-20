{-# LANGUAGE TemplateHaskell #-}
module Haze.Pil.Path where

import Haze.Prelude
import Haze.Types.Path ( Path
                       , Node(SubBlock, Condition, Call, Ret, AbstractPath, AbstractCall)
                       , SubBlockNode
                       , ConditionNode
                       , CallNode
                       , AbstractPathNode
                       , RetNode
                       )
import Hinja.Function (Function)
import qualified Hinja.MLIL as MLIL
import qualified Haze.Types.Path as Path
import qualified Hinja.Function as Function
import Haze.Types.Pil ( Statement
                      , Expression
                      , Ctx(Ctx)
                      , ConverterCtx(ConverterCtx)
                      , Stmt
                      , CtxIndex
                      )
import qualified Haze.Types.Pil as Pil
import qualified Haze.Pil as Pil
import Haze.Types.Pil (Converter, runConverter)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- convert path to [Pil]

-- updates ctx if Function is new
maybeUpdateCtx :: Function -> Converter ()
maybeUpdateCtx fn = do
  cctx <- get
  when (Just fn /= cctx ^. Pil.ctx . Pil.func) $ enterNewCtx fn

enterNewCtx :: Function -> Converter ()
enterNewCtx fn = do
  Pil.ctxIndexCounter %= incIndex
  i <- use Pil.ctxIndexCounter
  Pil.ctx %= newCtx i
  where
    newCtx i ctx = ctx & Pil.func .~ (Just fn)
                       & Pil.ctxIndex .~ i
    incIndex :: Maybe CtxIndex -> Maybe CtxIndex
    incIndex Nothing = Just 1
    incIndex (Just n) = Just (n + 1)

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  maybeUpdateCtx $ sb ^. Path.func
  instrs <- liftIO $ do
    mlilFunc <- Function.getMLILSSAFunction $ sb ^. Path.func
    mapM (MLIL.instruction mlilFunc) [(sb ^. Path.start) .. (sb ^. Path.end - 1)]
  flip Pil.convertInstrs instrs <$> use Pil.ctx

convertConditionNode :: ConditionNode -> Converter [Stmt]
convertConditionNode n = do
  ctx <- use Pil.ctx
  case Pil.convertExpr ctx $ n ^. Path.condition of
    Nothing -> return []
    Just expr -> return . (:[]) . Pil.Constraint . Pil.ConstraintOp $
      case n ^. Path.trueOrFalseBranch of
        True -> expr
        False -> Pil.Expression (expr ^. Pil.size) (Pil.NOT . Pil.NotOp $ expr)

convertAbstractCallNode :: AbstractCallNode -> Converter [Stmt]
convertAbstractCallNode n = return []

-- convertCallNode :: CallNode -> Converter [Stmt]
-- convertCallNode n = do
--   calleeCtx <- get
  
--   return []

convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
convertNode (Condition x) = convertConditionNode x
convertNode (AbstractCall x) convertAbstractCallNode x
--convertNode (CallNode x) = convertCallNode x
convertNode _ = return [] -- TODO


convertNodes :: [Node] -> Converter [Stmt]
convertNodes ((Call cn) : (AbstractPath apn) : (Ret rn) : xs) = do
  stmts <- convertAbstractCallTrio cn apn rn
  (stmts <>) <$> convertNodes xs
convertNodes xs = fmap concat . traverse convertNode $ xs

startCtx :: Ctx
startCtx = Ctx Nothing Nothing Set.empty Map.empty

startConverterCtx :: ConverterCtx
startConverterCtx = ConverterCtx Nothing startCtx

--- TODO: Keep track of ctx-index separately from Ctx
--- always increment on every new function Ctx change
convertPath :: Path p => p -> IO [Stmt]
convertPath =
  fmap (concat . fst) . flip runConverter startConverterCtx . traverse convertNode . Path.toList


