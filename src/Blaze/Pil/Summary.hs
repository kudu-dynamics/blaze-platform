module Blaze.Pil.Summary (
    module Blaze.Pil.Summary,
) where

import Blaze.Prelude
import Blaze.Types.Pil.Summary
import Blaze.Pil (
    ExprOp (LOAD, VAR),
    Expression (Expression),
    LoadOp (LoadOp),
    PilVar,
    Statement (Store, Ret),
    Stmt,
    VarOp (VarOp), RetOp (RetOp),
 )
import Blaze.Pil.Analysis (LoadExpr (LoadExpr), findLoads, getFreeVars)
import Blaze.Pil.Analysis.Path (simplifyVars)
import Data.HashSet qualified as HashSet

isArgLoad :: HashSet PilVar -> LoadExpr -> Bool
isArgLoad inputArgs (LoadExpr (Expression _ (LOAD (LoadOp (Expression _ (VAR (VarOp var))))))) =
    HashSet.member var inputArgs
isArgLoad _ _ = False

isStore :: Stmt -> Bool
isStore (Store _) = True
isStore _ = False

getRetVal :: Stmt -> Maybe Expression
getRetVal (Ret (RetOp x)) = Just x
getRetVal _ = Nothing

fromStmts :: [Stmt] -> CodeSummary
fromStmts stmts =
    let stmts' = simplifyVars stmts
        inputVars = HashSet.toList $ getFreeVars stmts'
     in CodeSummary
            { inputVars = inputVars
            , inputLoads = filter (not . isArgLoad (HashSet.fromList inputVars)) (concatMap findLoads stmts')
            , results = mapMaybe getRetVal stmts'
            , effects = filter isStore stmts'
            }
