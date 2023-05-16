{-| Analyses for [Stmt]'s that come from a linear Path --}

module Blaze.Pil.Analysis.Path
  ( module Blaze.Pil.Analysis.Path
  ) where

import Blaze.Prelude

import qualified Blaze.Pil.Analysis as PA
import Blaze.Pil (Stmt, Expression, PilVar)
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Analysis (untilFixedPoint)
import qualified Data.HashMap.Strict as HashMap


-- | Helper for simplifyVars
simplifyVars_ :: Int -> [Stmt] -> [Stmt]
simplifyVars_ = untilFixedPoint Nothing $ \stmts ->
  let stmts' = PA.fixedRemoveUnusedDef
               . PA.fixedRemoveUnusedPhi
               . PA.copyProp
               . PA.constantProp
               . PA.copyPropMem
               $ stmts
  in
    PA.reducePhis (PA.getFreeVars stmts') stmts'

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


-- | This fully subst variables with their equivalent expressions.
-- It also substitutes mem loads where possible, and eliminates unused
-- var def statements.
-- The goal is eliminate non-arg vars and non-global mem accesses.
-- Don't use this on partial paths through a funcion and expect to
-- later append other paths to it, because var def elimination might ruin linking.
-- TODO: once we know which mem addresses are global, we can eliminate the internal
--       store stmts.
aggressiveExpand :: [Stmt] -> [Stmt]
aggressiveExpand = aggressiveExpand_ HashMap.empty HashMap.empty . zip [0..]

type Addr = Expression

type StmtIndex = Int
type VarSubstMap = HashMap PilVar (StmtIndex, Expression)

aggressiveExpand_
  :: HashMap PilVar (StmtIndex, Expression)
  -> HashMap Addr Expression
  -> [(StmtIndex, Stmt)]
  -> [Stmt]
aggressiveExpand_ _ _ [] = []
aggressiveExpand_ varSubstMap memSubstMap ((stmtIndex, stmt):stmts) = case Pil.mkCallStatement stmt of
  -- | If it's a call statement, remove args from mem subst map because the call
  -- could affect them.
  -- Also, don't add anything to varSubstMap because function calls aren't necessarily pure
  -- so we can't subst the calls in multiple places.
  Just call ->
    stmt' : aggressiveExpand_ varSubstMap (removeFromMemSubstMap args) stmts
      where
        args = substAll <$> call ^. #args
        stmt' = substAll <$> stmt

  -- | Handle the non-call statements.
  Nothing ->  case stmt of
    Pil.Def (Pil.DefOp pv expr) -> aggressiveExpand_ varSubstMap' memSubstMap stmts
      where
        expr' = substAll expr
        varSubstMap' = HashMap.insert pv (stmtIndex, expr') varSubstMap

    Pil.Constraint _ -> defaultProp

    Pil.Store (Pil.StoreOp addr expr) -> stmt' : aggressiveExpand_ varSubstMap memSubstMap' stmts
      where
        stmt' = Pil.Store (Pil.StoreOp addr' expr')
        addr' = substAll addr
        expr' = substAll expr
        memSubstMap' = HashMap.insert addr' expr' memSubstMap

    Pil.UnimplInstr _ -> defaultProp

    -- Removes mem from subst map because we don't know what the unimpl instr did to it
    Pil.UnimplMem (Pil.UnimplMemOp addr) -> stmt' : aggressiveExpand_ varSubstMap (removeFromMemSubstMap [addr']) stmts
      where
        stmt' = Pil.UnimplMem (Pil.UnimplMemOp addr')
        addr' = substAll addr

    Pil.Undef -> defaultProp
    Pil.Nop -> defaultProp
    Pil.Annotation _ -> defaultProp
    Pil.EnterContext _ -> defaultProp
    Pil.ExitContext _ -> defaultProp

    Pil.Call _ -> error "This stmt should have been caught by mkCallStatement"

    Pil.DefPhi (Pil.DefPhiOp dest src) -> case mapMaybe (\v -> (v,) <$> isPreviouslyDefined v) src of
      [] -> aggressiveExpand_ varSubstMap memSubstMap stmts
      [(_v, (_si, expr))] -> aggressiveExpand_
                        (HashMap.insert dest (stmtIndex, expr) varSubstMap)
                        memSubstMap
                        stmts
      varExprs -> aggressiveExpand_ varSubstMap' memSubstMap stmts
        where
          (_latestVar, (_si, expr)) = maximumBy (\a b-> compare (fst a) (fst b)) varExprs
          varSubstMap' = HashMap.insert dest (stmtIndex, expr) varSubstMap
      where
        isPreviouslyDefined :: PilVar -> Maybe (StmtIndex, Expression)
        isPreviouslyDefined pv
          | pv == dest = Nothing
          | otherwise = HashMap.lookup pv varSubstMap

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
    defaultProp = (substAll <$> stmt) : aggressiveExpand_ varSubstMap memSubstMap stmts

    substAll :: Expression -> Expression
    substAll = substMem . substVars

    -- | Substitutes any pre-defined vars with their exprs.
    substVars :: Expression -> Expression
    substVars = substVars' -- PA.substVarExprInExpr (`HashMap.lookup` varSubstMap)

    -- | Substitutes any pre-defined vars with their exprs.
    substVars' :: Expression -> Expression
    substVars' = PA.substExprInExpr f
      where
        f :: Expression -> Maybe Expression
        f (Pil.Expression sz expr) = case expr of
          Pil.VAR (Pil.VarOp v) -> snd <$> HashMap.lookup v varSubstMap
          Pil.VAR_FIELD (Pil.VarFieldOp v off) -> Just
            . Pil.Expression sz
            . Pil.Extract
            $ Pil.ExtractOp x off
            where
              x = maybe (Pil.Expression (coerce $ v ^. #size) (Pil.VAR (Pil.VarOp v))) snd
                  $ HashMap.lookup v varSubstMap
          _ -> Nothing


    -- | Substitutes any loads with previous stores to that location.
    -- Be sure to call this after running `substVars` on the expr.
    substMem :: Expression -> Expression
    substMem = PA.substExprInExpr substLoad
      where
        substLoad :: Expression -> Maybe Expression
        substLoad (Pil.Expression _ (Pil.LOAD (Pil.LoadOp x))) = HashMap.lookup x memSubstMap
        substLoad _ = Nothing

    removeFromMemSubstMap :: [Expression] -> HashMap Addr Expression
    removeFromMemSubstMap = foldl' (flip HashMap.delete) memSubstMap
