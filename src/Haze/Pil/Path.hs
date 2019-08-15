{-# LANGUAGE TemplateHaskell #-}
module Haze.Pil.Path where

import Haze.Prelude
import Haze.Types.Path ( Path
                       , Node(SubBlock, Condition)
                       , SubBlockNode
                       , ConditionNode
                       )
import Hinja.Function (Function)
import qualified Hinja.MLIL as MLIL
import qualified Haze.Types.Path as Path
import qualified Hinja.Function as Function
import Haze.Types.Pil ( Statement
                      , Expression
                      , Ctx(Ctx)
                      , Stmt
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
  ctx <- get
  when (Just fn /= ctx ^. Pil.func) $ updateCtx fn

updateCtx :: Function -> Converter ()
updateCtx fn = do
  Pil.func .= Just fn
  Pil.ctxIndex %= g
  where
    g Nothing = Just 1
    g (Just n) = Just (n + 1)

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  maybeUpdateCtx $ sb ^. Path.func
  instrs <- liftIO $ do
    mlilFunc <- Function.getMLILSSAFunction $ sb ^. Path.func
    mapM (MLIL.instruction mlilFunc) [(sb ^. Path.start) .. (sb ^. Path.end - 1)]
  flip Pil.convertInstrs instrs <$> get

-- convertConditionNode :: ConditionNode -> Converter [Stmt]
-- convertConditionNode n = do
--   ctx <- get
--   case Pil.convertExpr ctx $ n ^. Path.condition of
--     Nothing -> return []
--     Just expr -> return . (:[]) . Pil.Constraint . Pil.ConstraintOp $
--       case n ^. Path.trueOrFalseBranch of
--         True -> expr
--         False -> Pil.Expression (expr ^. Pil.size) $ Pil.NOT expr
      
--   putText "Looks like a condition node."
--   return []


convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
--convertNode (Condition x) = convertConditionNode x
convertNode _ = return [] -- TODO

startCtx :: Ctx
startCtx = Ctx Nothing Nothing Set.empty Map.empty

convertPath :: Path p => p -> IO [Stmt]
convertPath =
  fmap (concat . fst) . flip runConverter startCtx . traverse convertNode . Path.toList


