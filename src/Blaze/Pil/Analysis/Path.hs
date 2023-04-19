{-| Analyses for [Stmt]'s that come from a linear Path --}

module Blaze.Pil.Analysis.Path
  ( module Blaze.Pil.Analysis.Path
  ) where

import Blaze.Prelude

import qualified Blaze.Pil.Analysis as PA
import Blaze.Types.Pil (Stmt, Expression, PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HashMap


-- | Helper for simplifyVars
simplifyVars_ :: Int -> [Stmt] -> [Stmt]
simplifyVars_ itersLeft stmts
  -- | itersLeft <= 0 = error "No fixed point reached in simplifyVars_"
  | itersLeft <= 0 = stmts
  | stmts == stmts'' = stmts
  | otherwise = simplifyVars_ (itersLeft - 1) stmts''
  where stmts' :: [Stmt]
        stmts' = PA.fixedRemoveUnusedDef . PA.fixedRemoveUnusedPhi .
          PA.copyProp . PA.constantProp . PA.copyPropMem $ stmts
        stmts'' :: [Stmt]
        stmts'' = PA.reducePhis (PA.getFreeVars stmts') stmts'

-- | Copy propagation, constant propagation, and DefPhi reduction
simplifyVars :: [Stmt] -> [Stmt]
-- TODO: Move the `not . isNopStore` filtering to a more appropriate location
simplifyVars = simplifyVars_ 10 . filter (not . PA.isNopStore)

substVarExprsInStmt :: HashMap PilVar Expression -> Stmt -> Stmt
substVarExprsInStmt m = PA.substVarExpr_ $ flip HashMap.lookup m

-- | Propagates whole expressions assigned to variables to variable uses.
-- The point of this is twofold:
-- 1. Human readability.
-- 2. We might want to isolate function input constraints such that the only
--    vars used in those constraints are input vars and global mem loads.
--    This would be useful in a limited model checker that either cannot `execute`
--    full Stmt paths with stores, loads, and calls, or it can, but uses the
--    simpler constraints to narrow down the search space before checking a more
--    complete model.
--
-- Should not copy expressions containing a LOAD expr.
-- For example:
--   y = [x]
--   ?: (y > 100)
--   [x] = z
--   return (y + [x])
-- We don't want `return ([x] + [x])` because the first `[x]` is a free var
-- and the second is `z`.
--
-- You could subst `[x]` into `?: (y > 100)`, but you'd still have to leave `y`
-- around for later usage, so I think it's better just to rely on `PA.memoryTransform`,
-- which versions each store as a var.
-- This assumes `PA.memoryTransform` has already been called.
expandVars_ :: HashMap PilVar Expression -> [Stmt] -> [Stmt]
expandVars_  _ [] = []
expandVars_ varSubstMap (stmt:stmts) = case Pil.mkCallStatement stmt of
  -- | Don't add anything to subst map if it's a call statement.
  --   But subst over the args.
  Just _ -> (substAll <$> stmt) : expandVars_ varSubstMap stmts

  -- | Handle the non-call statements.
  Nothing ->  case stmt of
    Pil.Def (Pil.DefOp _ (Pil.Expression _ (Pil.LOAD _))) -> defaultProp
    Pil.Def (Pil.DefOp pv expr) -> expandVars_ varSubstMap' stmts
      where
        expr' = substAll expr
        varSubstMap' = HashMap.insert pv expr' varSubstMap

    Pil.Constraint _ -> defaultProp

    Pil.Store (Pil.StoreOp addr expr) -> stmt' : expandVars_ varSubstMap stmts
      where
        stmt' = Pil.Store (Pil.StoreOp addr' expr')
        addr' = substAll addr
        expr' = substAll expr

    Pil.UnimplInstr _ -> defaultProp

    -- Removes mem from subst map because we don't know what the unimpl instr did to it
    Pil.UnimplMem (Pil.UnimplMemOp addr) -> stmt' : expandVars_ varSubstMap stmts
      where
        stmt' = Pil.UnimplMem (Pil.UnimplMemOp addr')
        addr' = substAll addr

    Pil.Undef -> defaultProp
    Pil.Nop -> defaultProp
    Pil.Annotation _ -> defaultProp
    Pil.EnterContext _ -> defaultProp
    Pil.ExitContext _ -> defaultProp

    Pil.Call _ -> error "This stmt should have been caught by mkCallStatement"

    Pil.DefPhi _ -> defaultProp -- rely on other analyses to remove unused phi
    Pil.DefMemPhi _ -> defaultProp
    Pil.BranchCond _ -> defaultProp
    Pil.Jump _  -> defaultProp
    Pil.JumpTo _  -> defaultProp
    Pil.Ret _ -> defaultProp
    Pil.NoRet -> defaultProp
    Pil.Exit -> defaultProp
    Pil.TailCall _ -> error "This stmt should have been caught by mkCallStatement"

  where
    defaultProp :: [Stmt]
    defaultProp = (substAll <$> stmt) : expandVars_ varSubstMap stmts

    substAll :: Expression -> Expression
    substAll = PA.substVarExprInExpr (`HashMap.lookup` varSubstMap)

expandVars :: [Stmt] -> [Stmt]
expandVars = expandVars_ HashMap.empty
