module Blaze.Cfg.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Pil.Checker as Checker
import qualified Blaze.Types.Pil.Checker as Ch
import Blaze.Types.Pil ( Expression, Statement, Stmt )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Checker (TypeReport, ConstraintGenError)
import Blaze.Types.Cfg (Cfg)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Graph as G
import qualified Blaze.Pil.Analysis as Analysis


gatherCfgData :: Hashable a => Cfg [a] -> [a]
gatherCfgData = concatMap concat . HashMap.elems . G.getNodeAttrMap

-- | Checks indexed statements pulled from a CFG.
-- First performs analysis pass to make field accesses explicit
-- then removes unused phi vars, which otherwise would cause type inference
-- to somtimes equate unrelated vars.
checkFromCfg :: [(Int, Stmt)] -> Either ConstraintGenError TypeReport
checkFromCfg cfgStmts =
  checkIndexedStmts . removeUnusedPhi $
    zip (fmap fst cfgStmts) (Analysis.substAddrs $ fmap snd cfgStmts)

checkCfg :: Cfg [Statement Expression]
         -> Either ConstraintGenError
            ( Cfg [(Int, Statement Expression)]
            , Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe Ch.DeepSymType)))]
            , TypeReport
            )
checkCfg cfg = case checkFromCfg indexedStmts' of
  Left err -> Left err
  Right tr -> Right (cfg', typedCfg, tr)
    where
      stmtMap :: HashMap Int (Statement (Ch.InfoExpression (Ch.SymInfo, Maybe Ch.DeepSymType)))
      stmtMap = HashMap.fromList $ tr ^. #symTypeStmts
      typedCfg :: Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe Ch.DeepSymType)))]
      typedCfg = Cfg.mapAttrs replaceStmts cfg'

      replaceStmts = mapMaybe (\(i, _) -> (i,) <$> HashMap.lookup i stmtMap)
  where
    indexedStmts = gatherCfgData cfg'
    indexedStmts' = zip (fst <$> indexedStmts) (prep $ snd <$> indexedStmts)
    prep = Analysis.substAddrs
    cfg' = indexedCfg cfg

indexedCfg :: Cfg [a] -> Cfg [(Int, a)]
indexedCfg cfg = flip evalState 0 $ Cfg.traverseAttrs (traverse f) cfg
  where
    f :: a -> State Int (Int, a)
    f x = do
      n <- get
      modify (+1)
      return (n, x)
