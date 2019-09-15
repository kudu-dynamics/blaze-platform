module Blaze.Pil.Analysis where

import Blaze.Prelude
import Blaze.Types.Pil (Stmt, PilVar, Statement (Def), Expression, ExprOp)
import qualified Blaze.Types.Pil as Pil
import qualified Data.Set as Set

getDefinedVars_ :: Stmt -> [PilVar]
getDefinedVars_ (Def d) = [d ^. Pil.var]
getDefinedVars_ _ = []

getDefinedVars :: [Stmt] -> [PilVar]
getDefinedVars = concatMap getDefinedVars_

getVarsFromExpr :: Expression -> [PilVar]
getVarsFromExpr e = case e ^. Pil.op of
  (Pil.VAR vop) -> [vop ^. Pil.src]
  x -> concatMap getVarsFromExpr x

getVars_ :: Stmt -> [PilVar]
getVars_ = concatMap getVarsFromExpr

getVars :: [Stmt] -> [PilVar]
getVars = concatMap getVars_

getFreeVars :: [Stmt] -> Set PilVar
getFreeVars xs = Set.difference allVars defined
  where
    defined = Set.fromList $ getDefinedVars xs
    allVars = Set.fromList $ getVars xs
