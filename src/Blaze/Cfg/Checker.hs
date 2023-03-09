module Blaze.Cfg.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Pil.Checker as Checker
import Blaze.Types.Pil (Ctx, Stmt)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Checker (TypeReport, ConstraintGenError, SymTypedStmt)
import Blaze.Cfg (getRootNode, gatherCfgData, getCfgCtx)
import Blaze.Types.Cfg (Cfg, CfNode)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Pil.Analysis as Analysis


-- | Checks indexed statements pulled from a CFG.
-- First performs analysis pass to make field accesses explicit
-- then removes unused phi vars, which otherwise would cause type inference
-- to somtimes equate unrelated vars.
checkFromCfg :: Maybe Ctx -> [(Int, Stmt)] -> Either ConstraintGenError TypeReport
checkFromCfg mRootCtx cfgStmts =
  checkIndexedStmts mRootCtx . removeUnusedPhi $
    zip (fmap fst cfgStmts) (Analysis.substAddrs $ fmap snd cfgStmts)

checkCfg :: Cfg (CfNode [Stmt])
         -> Either ConstraintGenError
            ( Cfg (CfNode [(Int, Stmt)])
            , Cfg (CfNode [(Int, SymTypedStmt)])
            , TypeReport
            )
checkCfg cfg = case checkFromCfg (Just $ getCfgCtx cfg) indexedStmts' of
  Left err -> Left err
  Right tr -> Right (cfg', typedCfg, tr)
    where
      stmtMap :: HashMap Int SymTypedStmt
      stmtMap = HashMap.fromList $ tr ^. #symTypedStmts
      typedCfg :: Cfg (CfNode [(Int, SymTypedStmt)])
      typedCfg = fmap replaceStmts cfg'
      replaceStmts :: CfNode [(Int, Stmt)] -> CfNode [(Int, SymTypedStmt)]
      replaceStmts = fmap lookupStmts
      lookupStmts :: [(Int, Stmt)] -> [(Int, SymTypedStmt)]
      -- NB: Some statements may have been removed from checkFromCfg.
      lookupStmts = mapMaybe (\(i, _) -> (i,) <$> HashMap.lookup i stmtMap)
  where
    indexedStmts = gatherCfgData cfg'
    indexedStmts' = zip (fst <$> indexedStmts) (prep $ snd <$> indexedStmts)
    prep = Analysis.substAddrs
    cfg' = indexedCfg cfg

indexedCfg :: forall f a. Traversable f => Cfg (CfNode (f a)) -> Cfg (CfNode (f (Int, a)))
indexedCfg cfg = evalState ((traverse . traverse . traverse) index cfg) 0
  where
    index :: a -> State Int (Int, a)
    index x = do
      n <- get
      modify (+1)
      return (n, x)
