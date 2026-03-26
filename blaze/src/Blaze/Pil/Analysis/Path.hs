{-| Analyses for [Stmt]'s that come from a linear Path --}

module Blaze.Pil.Analysis.Path
  ( module Blaze.Pil.Analysis.Path
  ) where

import Blaze.Prelude

import qualified Blaze.Pil.Analysis as PA
import Blaze.Pil (Stmt, Expression, PilVar)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Expression (IsExpression(..))
import Blaze.Util.Analysis (untilFixedPoint)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe (unsafePerformIO)

-- | Extract call args from a Statement, if it's a call-like statement.
getCallArgs :: Pil.Statement expr -> Maybe [expr]
getCallArgs = \case
  Pil.Call (Pil.CallOp _ args) -> Just args
  Pil.TailCall (Pil.TailCallOp _ args _) -> Just args
  _ -> Nothing

-- | Check if an ExprOp is a CALL
isCallOp :: Pil.ExprOp expr -> Bool
isCallOp (Pil.CALL _) = True
isCallOp _ = False

-- | Extract args from a CALL ExprOp
getCallArgsFromOp :: Pil.ExprOp expr -> [expr]
getCallArgsFromOp (Pil.CALL (Pil.CallOp _ args)) = args
getCallArgsFromOp _ = []


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
expandVars_ varSubstMap (stmt@(Pil.Stmt stmtAddr statement):stmts) = case Pil.mkCallStatement stmt of
  -- | Don't add anything to subst map if it's a call statement.
  --   But subst over the args.
  Just _ -> (substAll <$> stmt) : expandVars_ varSubstMap stmts

  -- | Handle the non-call statements.
  Nothing ->  case statement of
    Pil.Def (Pil.DefOp _ (Pil.Expression _ (Pil.LOAD _))) -> defaultProp
    Pil.Def (Pil.DefOp pv expr) -> expandVars_ varSubstMap' stmts
      where
        expr' = substAll expr
        varSubstMap' = HashMap.insert pv expr' varSubstMap

    Pil.Constraint _ -> defaultProp

    Pil.Store (Pil.StoreOp addr expr) -> stmt' : expandVars_ varSubstMap stmts
      where
        stmt' = Pil.Stmt stmtAddr $ Pil.Store (Pil.StoreOp addr' expr')
        addr' = substAll addr
        expr' = substAll expr

    Pil.UnimplInstr _ -> defaultProp

    -- Removes mem from subst map because we don't know what the unimpl instr did to it
    Pil.UnimplMem (Pil.UnimplMemOp addr) -> stmt' : expandVars_ varSubstMap stmts
      where
        stmt' = Pil.Stmt stmtAddr $ Pil.UnimplMem (Pil.UnimplMemOp addr')
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
-- | Collapse nested ARRAY_ADDR and FIELD_ADDR operations that accumulate
-- during memory substitution (e.g. when fputc increments stdout's buffer
-- pointer repeatedly, creating ptr[1][1][1]... instead of ptr[3]).
simplifyArrayAddr :: IsExpression expr => expr -> expr
simplifyArrayAddr = PA.substExprInExpr collapse
  where
    collapse expr = case getExprOp expr of
      Pil.ARRAY_ADDR (Pil.ArrayAddrOp base idx stride)
        | Pil.ARRAY_ADDR (Pil.ArrayAddrOp innerBase innerIdx innerStride) <- getExprOp base
        , stride == innerStride
        -> Just $ mkExprLike expr
             (Pil.ARRAY_ADDR (Pil.ArrayAddrOp innerBase (addExprs innerIdx idx) stride))
      Pil.FIELD_ADDR (Pil.FieldAddrOp base off)
        | Pil.FIELD_ADDR (Pil.FieldAddrOp innerBase innerOff) <- getExprOp base
        -> Just $ mkExprLike expr
             (Pil.FIELD_ADDR (Pil.FieldAddrOp innerBase (innerOff + off)))
      _ -> Nothing

addExprs :: IsExpression expr => expr -> expr -> expr
addExprs a b = case (getExprOp a, getExprOp b) of
  (Pil.CONST (Pil.ConstOp x), Pil.CONST (Pil.ConstOp y))
    -> mkExprLike b (Pil.CONST (Pil.ConstOp (x + y)))
  _ -> mkExprLike a (Pil.ADD (Pil.AddOp a b))

-- Because this substitutes loads, we might erase possible TOCTOU patterns
-- TODO: once we know which mem addresses are global, we can eliminate the internal
--       store stmts.
aggressiveExpand :: [Stmt] -> [Stmt]
aggressiveExpand = aggressiveExpand_

-- | Generic aggressiveExpand that works on any expression/statement type satisfying IsExpression.
-- The stmt type must be an AddressableStatement parameterized by the expression type.
aggressiveExpand_
  :: forall expr. (IsExpression expr, Show (Pil.AddressableStatement expr))
  => [Pil.AddressableStatement expr] -> [Pil.AddressableStatement expr]
aggressiveExpand_ = view #processed . aggressiveExpand__

type StmtIndex = Int

data AggressiveExpandState expr = AggressiveExpandState
  { usedVars :: HashSet PilVar
  , varSubstMap :: HashMap PilVar (StmtIndex, expr)
  , memSubstMap :: HashMap expr expr
  , processed :: [Pil.AddressableStatement expr]
  } deriving (Generic)

aggressiveExpand__
  :: forall expr. (IsExpression expr, Show (Pil.AddressableStatement expr))
  => [Pil.AddressableStatement expr]
  -> AggressiveExpandState expr
aggressiveExpand__ = over #processed reverse
  . foldl' (flip aggressiveExpand'_)
      (AggressiveExpandState HashSet.empty HashMap.empty HashMap.empty [])
  . zip [0..]

-- | version meant to be used with foldl'
aggressiveExpand'_
  :: forall expr. (IsExpression expr, Show (Pil.AddressableStatement expr))
  => (StmtIndex, Pil.AddressableStatement expr)
  -> AggressiveExpandState expr
  -> AggressiveExpandState expr
aggressiveExpand'_ (stmtIndex, stmt@(Pil.Stmt stmtAddr statement)) AggressiveExpandState{usedVars, varSubstMap, memSubstMap, processed} = case getCallArgs statement of
  -- | If it's a call statement, remove args from mem subst map because the call
  -- could affect them.
  -- Also, don't add anything to varSubstMap because function calls aren't necessarily pure
  -- so we can't subst the calls in multiple places.
  Just callArgs -> AggressiveExpandState
               usedVars'
               varSubstMap
               (removeFromMemSubstMap args)
               processed'
      where
        usedVars' = addUsedFromStmt stmt'
        args = substAll <$> callArgs
        stmt' = substAll <$> stmt
        processed' = stmt':processed

  -- | Handle the non-call statements.
  Nothing ->  case statement of
    Pil.Def (Pil.DefOp pv expr)
      | isCallOp (getExprOp expr) -> AggressiveExpandState
              -- Call assigned to var: treat like a call (don't inline)
              (addUsedFromStmt (substAll <$> stmt))
              varSubstMap
              (removeFromMemSubstMap (substAll <$> getCallArgsFromOp (getExprOp expr)))
              ((substAll <$> stmt):processed)
      | otherwise -> AggressiveExpandState
              usedVars'
              varSubstMap'
              memSubstMap
              processed
      where
        usedVars' = HashSet.insert pv $ addUsedFromExpr expr'
        expr' = substAll expr
        varSubstMap' = HashMap.insert pv (stmtIndex, expr') varSubstMap

    Pil.Constraint _ -> defaultProp

    Pil.Store (Pil.StoreOp addr expr) -> AggressiveExpandState
              usedVars'
              varSubstMap
              memSubstMap'
              (stmt':processed)
      where
        usedVars' = addUsedFromStmt stmt'
        stmt' = Pil.Stmt stmtAddr $ Pil.Store (Pil.StoreOp addr' expr')
        addr' = substAll addr
        expr' = substAll expr
        memSubstMap' = HashMap.insert addr' expr' memSubstMap

    Pil.UnimplInstr _ -> defaultProp

    -- Removes mem from subst map because we don't know what the unimpl instr did to it
    Pil.UnimplMem (Pil.UnimplMemOp addr) -> AggressiveExpandState
      usedVars'
      varSubstMap
      (removeFromMemSubstMap [addr'])
      (stmt':processed)
      where
        usedVars' = addUsedFromStmt stmt'
        stmt' = Pil.Stmt stmtAddr $ Pil.UnimplMem (Pil.UnimplMemOp addr')
        addr' = substAll addr

    Pil.Undef -> defaultProp
    Pil.Nop -> defaultProp
    Pil.Annotation _ -> defaultProp
    Pil.EnterContext _ -> defaultProp
    Pil.ExitContext _ -> defaultProp

    Pil.Call _ -> error "This stmt should have been caught by mkCallStatement"

    Pil.DefPhi (Pil.DefPhiOp dest src) -> case mapMaybe (\v -> (v,) <$> isPreviouslyDefined v) src of
      [] -> case mapMaybe (\v -> bool Nothing (Just v) $ HashSet.member v usedVars) src of
        [] -> AggressiveExpandState usedVars varSubstMap memSubstMap processed
        [v] -> handleSingleVar v
        -- TODO: does this ever happen? make it warning instead
        (v:_) -> unsafePerformIO $ do
          warn $ "Path has PHI where multiple src vars have been used already in path: " <> show stmt
          return $ handleSingleVar v
        where
          handleSingleVar v = AggressiveExpandState usedVars varSubstMap' memSubstMap processed
            where
              expr = liftVar v
              varSubstMap' = HashMap.insert dest (stmtIndex, expr) varSubstMap
      [(_v, (_si, expr))] -> AggressiveExpandState usedVars varSubstMap' memSubstMap processed
        where
          varSubstMap' = HashMap.insert dest (stmtIndex, expr) varSubstMap
      varExprs -> AggressiveExpandState usedVars varSubstMap' memSubstMap processed
        where
          (_latestVar, (_si, expr)) = maximumBy (\a b-> compare (fst a) (fst b)) varExprs
          varSubstMap' = HashMap.insert dest (stmtIndex, expr) varSubstMap
      where
        isPreviouslyDefined :: PilVar -> Maybe (StmtIndex, expr)
        isPreviouslyDefined pv
          | pv == dest = Nothing
          | otherwise = HashMap.lookup pv varSubstMap

    Pil.DefMemPhi _ -> defaultProp
    Pil.BranchCond _ -> defaultProp
    Pil.Jump _  -> simplyDrop
    Pil.JumpTo _  -> simplyDrop
    Pil.Ret _ -> defaultProp
    Pil.NoRet -> defaultProp
    Pil.Exit -> defaultProp
    Pil.TailCall _ -> error "This stmt should have been caught by mkCallStatement"
  where
    simplyDrop = AggressiveExpandState usedVars varSubstMap memSubstMap processed
    defaultProp = let stmt' = substAll <$> stmt
                      usedVars' = addUsedFromStmt stmt'
                  in
                    AggressiveExpandState usedVars' varSubstMap memSubstMap (stmt':processed)

    substAll :: expr -> expr
    substAll = simplifyArrayAddr . substMem . substVars

    substVars :: expr -> expr
    substVars = PA.substExprInExpr f
      where
        f :: expr -> Maybe expr
        f e = case getExprOp e of
          Pil.VAR (Pil.VarOp v) -> snd <$> HashMap.lookup v varSubstMap
          Pil.VAR_FIELD (Pil.VarFieldOp v off) -> Just
            $ mkExprLike e
            $ Pil.Extract
            $ Pil.ExtractOp x off
            where
              x = maybe (liftVar v) snd $ HashMap.lookup v varSubstMap
          _ -> Nothing

    substMem :: expr -> expr
    substMem = PA.substExprInExpr substLoad
      where
        substLoad :: expr -> Maybe expr
        substLoad e = case getExprOp e of
          Pil.LOAD (Pil.LoadOp x) -> HashMap.lookup x memSubstMap
          _ -> Nothing

    removeFromMemSubstMap :: [expr] -> HashMap expr expr
    removeFromMemSubstMap = foldl' (flip HashMap.delete) memSubstMap

    addUsedFromStmt :: Pil.AddressableStatement expr -> HashSet PilVar
    addUsedFromStmt = HashSet.union usedVars . PA.getVarsFromStmt_
    addUsedFromExpr :: expr -> HashSet PilVar
    addUsedFromExpr = HashSet.union usedVars . HashSet.fromList . PA.getVarsFromExpr_'
