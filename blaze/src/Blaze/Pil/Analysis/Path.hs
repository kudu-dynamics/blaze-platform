{-| Analyses for [Stmt]'s that come from a linear Path --}

module Blaze.Pil.Analysis.Path
  ( module Blaze.Pil.Analysis.Path
  ) where

import Blaze.Prelude

import qualified Blaze.Pil.Analysis as PA
import qualified Blaze.Pil.Construct as C
import Blaze.Pil (Stmt, Expression(..), PilVar)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Expression (IsExpression(..))
import Blaze.Util.Analysis (untilFixedPoint)

import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Numeric
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
aggressiveExpand = aggressiveExpand_ . promoteStackLocals

-- | Generic aggressiveExpand that works on any expression/statement type satisfying IsExpression.
-- The stmt type must be an AddressableStatement parameterized by the expression type.
aggressiveExpand_
  :: forall expr. (IsExpression expr, Show (Pil.AddressableStatement expr))
  => [Pil.AddressableStatement expr] -> [Pil.AddressableStatement expr]
aggressiveExpand_ = view #processed . aggressiveExpand__

-- | Maximum unfolded node count for expressions in the substitution maps.
-- When copy-propagation creates expressions that exceed this count, they are
-- not inlined (Defs are kept as named variables; Stores are not cached for
-- future LOAD substitution). This prevents exponential blowup from shared
-- sub-expressions: e.g. in a heap allocator, @alloc_size@ is read, masked,
-- used to compute a neighbor address, then written back — each cycle doubles
-- the unfolded tree because the same sub-expression appears in two operands.
maxSubstExprNodes :: Int
maxSubstExprNodes = 64

-- | Count nodes in the unfolded expression tree, with a budget that stops
-- traversal early. Returns the count up to @budget+1@.
-- Even with Haskell sharing (DAG), this counts the unfolded tree because
-- each shared child is recursed into independently from each parent.
exprNodeCount :: IsExpression expr => Int -> expr -> Int
exprNodeCount budget e
  | budget <= 0 = 1
  | otherwise =
      let children = F.toList (getExprOp e)
      in fst $ foldl' (\(!acc, !remaining) child ->
           let count = exprNodeCount remaining child
           in (acc + count, remaining - count))
         (1, budget - 1) children

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
      | exprNodeCount maxSubstExprNodes expr' > maxSubstExprNodes ->
              -- Expression too large to inline: keep the Def in output as a named
              -- variable. This prevents exponential blowup when a variable whose
              -- definition is already large gets used in multiple operands.
              -- Remove any previous binding for pv so later uses see the variable
              -- name, not a stale old value.
              AggressiveExpandState
              (HashSet.insert pv $ addUsedFromExpr expr')
              (HashMap.delete pv varSubstMap)
              memSubstMap
              (Pil.Stmt stmtAddr (Pil.Def (Pil.DefOp pv expr')) : processed)
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
        -- Don't cache expressions whose unfolded tree exceeds the node limit.
        -- This prevents exponential blowup from repeated store-load cycles
        -- on the same memory location (e.g. heap allocator alloc_size field).
        -- Remove any previous entry for addr' so later loads see the raw LOAD,
        -- not a stale old value.
        memSubstMap' = if exprNodeCount maxSubstExprNodes expr' > maxSubstExprNodes
                       then HashMap.delete addr' memSubstMap
                       else HashMap.insert addr' expr' memSubstMap

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

-- --------------------------------------------------------------------------
-- mem2reg: promote non-escaped stack slots from Store/Load to Def/Var
-- --------------------------------------------------------------------------

-- | Promote non-escaped stack slots from Store/Load through STACK_ADDR
-- back to Def/Var form. A slot is "escaped" if its STACK_ADDR appears
-- anywhere other than as the immediate address of a Store or Load.
-- Slots with inconsistent access widths are also excluded.
promoteStackLocals :: [Stmt] -> [Stmt]
promoteStackLocals stmts
  | HashSet.null promotable = stmts
  | otherwise = rewriteStmts promotable varMap stmts
  where
    escaped = findEscapedOffsets stmts
    allOffsets = findAllStackAddrOffsets stmts
    mixedWidth = findMixedWidthOffsets stmts
    promotable = allOffsets `HashSet.difference` escaped `HashSet.difference` mixedWidth
    varMap = mkStackVarMap promotable stmts

-- | Find all StackOffset values that appear in STACK_ADDR expressions.
findAllStackAddrOffsets :: [Stmt] -> HashSet Pil.StackOffset
findAllStackAddrOffsets = foldMap (foldMap collectFromExpr)
  where
    collectFromExpr :: Expression -> HashSet Pil.StackOffset
    collectFromExpr (Expression _ op) = case op of
      Pil.STACK_ADDR off -> HashSet.singleton off
      other -> foldMap collectFromExpr other

-- | Find stack offsets that are "escaped" — their STACK_ADDR appears in
-- a context other than the immediate address of a Store or Load.
findEscapedOffsets :: [Stmt] -> HashSet Pil.StackOffset
findEscapedOffsets = foldMap goStmt
  where
    goStmt :: Stmt -> HashSet Pil.StackOffset
    goStmt (Pil.Stmt _ stmt) = case stmt of
      Pil.Store (Pil.StoreOp addr value) ->
        case addr ^. #op of
          -- Store [STACK_ADDR K] value — addr is safe, but scan value for escapes
          Pil.STACK_ADDR _ -> escapedInExpr value
          -- Store [other] value — scan both addr and value
          _ -> escapedInExpr addr <> escapedInExpr value
      -- For all other statement types, scan child expressions
      other -> F.foldMap escapedInExpr other

    -- | A STACK_ADDR inside a LOAD is safe (it's just reading the slot).
    -- A bare STACK_ADDR anywhere else means the address escapes.
    escapedInExpr :: Expression -> HashSet Pil.StackOffset
    escapedInExpr (Expression _ op) = case op of
      Pil.STACK_ADDR off -> HashSet.singleton off
      Pil.LOAD (Pil.LoadOp src) -> case src ^. #op of
        Pil.STACK_ADDR _ -> mempty
        _ -> escapedInExpr src
      other -> foldMap escapedInExpr other

-- | Find stack offsets that are accessed at multiple different widths.
-- A byte store plus a word load to the same offset must not be promoted.
findMixedWidthOffsets :: [Stmt] -> HashSet Pil.StackOffset
findMixedWidthOffsets stmts = HashMap.keysSet . HashMap.filter (> 1) $ widthCounts
  where
    widthCounts :: HashMap Pil.StackOffset Int
    widthCounts = HashMap.map HashSet.size $ foldl' collectWidths HashMap.empty stmts

    collectWidths :: HashMap Pil.StackOffset (HashSet (Pil.Size Expression)) -> Stmt -> HashMap Pil.StackOffset (HashSet (Pil.Size Expression))
    collectWidths acc (Pil.Stmt _ stmt) = case stmt of
      Pil.Store (Pil.StoreOp addr value)
        | Pil.STACK_ADDR off <- addr ^. #op
        -> HashMap.insertWith (<>) off (HashSet.singleton $ value ^. #size) acc'
        where acc' = foldl' collectFromExpr acc stmt
      _ -> foldl' collectFromExpr acc stmt

    collectFromExpr :: HashMap Pil.StackOffset (HashSet (Pil.Size Expression)) -> Expression -> HashMap Pil.StackOffset (HashSet (Pil.Size Expression))
    collectFromExpr acc (Expression sz op) = case op of
      Pil.LOAD (Pil.LoadOp (Expression _ (Pil.STACK_ADDR off)))
        -> foldl' collectFromExpr (HashMap.insertWith (<>) off (HashSet.singleton sz) acc) op
      _ -> foldl' collectFromExpr acc op

-- | Build a map from promotable StackOffset to the PilVar that will represent it.
-- Uses the first Store's value size to determine the variable width.
mkStackVarMap :: HashSet Pil.StackOffset -> [Stmt] -> HashMap Pil.StackOffset PilVar
mkStackVarMap promotable = foldl' go HashMap.empty
  where
    go :: HashMap Pil.StackOffset PilVar -> Stmt -> HashMap Pil.StackOffset PilVar
    go acc (Pil.Stmt _ stmt) = case stmt of
      Pil.Store (Pil.StoreOp addr value)
        | Pil.STACK_ADDR off <- addr ^. #op
        , HashSet.member off promotable
        , not (HashMap.member off acc)
        -> HashMap.insert off (mkStackPilVar off (value ^. #size) Nothing) acc
      _ -> acc

mkStackPilVar :: Pil.StackOffset -> Pil.Size Expression -> Maybe Pil.SSAVersion -> PilVar
mkStackPilVar stackOff dataSize ver = C.pilVar__
  (C.getPilVarSize dataSize)
  (Just $ stackOff ^. #ctx)
  ver
  (stackVarName off)
  (off >= 0)  -- positive offset = stack-passed argument
  (Pil.StackMemory . fromIntegral $ off)
  where off = stackOff ^. #offset

stackVarName :: ByteOffset -> Pil.Symbol
stackVarName (ByteOffset n) =
  (if n < 0 then "var_" else "arg_") <> cs (Numeric.showHex (abs n) "")

-- | Rewrite statements with sequential SSA versions per offset:
--   Store [STACK_ADDR K] v → Def var_K_vN = v  (bumps version)
--   Load [STACK_ADDR K]   → VAR var_K_vN       (uses current version)
--   DefPhi with StackMemory vars → rewritten to current version
rewriteStmts :: HashSet Pil.StackOffset -> HashMap Pil.StackOffset PilVar -> [Stmt] -> [Stmt]
rewriteStmts promotable baseVarMap stmts = reverse . view #rsOut $ foldl' go initState stmts
  where
    initState = RewriteState HashMap.empty []

    go :: RewriteState -> Stmt -> RewriteState
    go st (Pil.Stmt addr stmt) = case stmt of
      Pil.Store (Pil.StoreOp storeAddr value)
        | Pil.STACK_ADDR off <- storeAddr ^. #op
        , HashSet.member off promotable
        , Just basePv <- HashMap.lookup off baseVarMap
        -> let (pv, st') = bumpVersion off basePv st
           in st' & #rsOut %~ (Pil.Stmt addr (Pil.Def . Pil.DefOp pv $ rewriteExpr st' value) :)
      Pil.DefPhi (Pil.DefPhiOp dest srcs)
        | Just pv <- rewritePhiVar st dest
        -> st & #rsOut %~ (Pil.Stmt addr (Pil.DefPhi . Pil.DefPhiOp pv $ map (\s -> fromMaybe s (rewritePhiVar st s)) srcs) :)
      other -> st & #rsOut %~ (Pil.Stmt addr (fmap (rewriteExpr st) other) :)

    bumpVersion :: Pil.StackOffset -> PilVar -> RewriteState -> (PilVar, RewriteState)
    bumpVersion off basePv st =
      let ver = maybe 1 (+ 1) $ HashMap.lookup off (st ^. #rsVersions)
          pv = basePv & #version ?~ ver
          st' = st & #rsVersions %~ HashMap.insert off ver
      in (pv, st')

    currentPv :: RewriteState -> Pil.StackOffset -> Maybe PilVar
    currentPv st off = do
      basePv <- HashMap.lookup off baseVarMap
      let ver = HashMap.lookup off (st ^. #rsVersions)
      pure $ basePv & #version .~ ver

    -- | Map a PilVar with StackMemory location to its promoted counterpart.
    -- Reconstructs the full StackOffset (ctx + offset) from the PilVar's
    -- own fields so that multi-context paths don't collide.
    rewritePhiVar :: RewriteState -> PilVar -> Maybe PilVar
    rewritePhiVar st pv = do
      ctx <- pv ^. #ctx
      off <- case pv ^. #location of
        Pil.StackMemory o -> Just o
        _ -> Nothing
      currentPv st (Pil.StackOffset ctx (fromIntegral off))

    rewriteExpr :: RewriteState -> Expression -> Expression
    rewriteExpr st (Expression sz op) = case op of
      Pil.LOAD (Pil.LoadOp src)
        | Pil.STACK_ADDR off <- src ^. #op
        , HashSet.member off promotable
        , Just pv <- currentPv st off
        -> Expression sz . Pil.VAR $ Pil.VarOp pv
      other -> Expression sz $ fmap (rewriteExpr st) other

data RewriteState = RewriteState
  { rsVersions :: HashMap Pil.StackOffset Pil.SSAVersion
  , rsOut :: [Stmt]
  } deriving (Generic)
