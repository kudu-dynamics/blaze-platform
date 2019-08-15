module Haze.Pil where

import Haze.Prelude hiding (Symbol, Type)
import Haze.Types.Pil
import qualified Hinja.MLIL as MLIL
import qualified Data.Set as Set
import qualified Hinja.Variable as Variable
import qualified Haze.Types.Pil as Pil
import qualified Hinja.Function as Function

typeWidthToOperationSize :: Variable.TypeWidth -> MLIL.OperationSize
typeWidthToOperationSize (Variable.TypeWidth n) = MLIL.OperationSize n

convertExpr :: Ctx -> MLIL.Expression t -> Maybe Expression
convertExpr ctx = convertMExpression . convertExpr_ ctx

convertExpr_ :: Ctx -> MLIL.Expression t -> MExpression
convertExpr_ ctx e = MExpression
  { _size = e ^. size
  , _op = convertExprOp ctx $ e ^. op
  }

convertExprOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> Maybe (ExprOp MExpression)
convertExprOp ctx mop =
  case mop of
    (MLIL.ADC x) -> Just . ADC $ f x
    (MLIL.ADD x) -> Just . ADD $ f x
    (MLIL.ADDRESS_OF x) -> Just . ADDRESS_OF $ f x
    (MLIL.ADDRESS_OF_FIELD x) -> Just . ADDRESS_OF_FIELD $ f x
    (MLIL.ADD_OVERFLOW x) -> Just . ADD_OVERFLOW $ f x
    (MLIL.AND x) -> Just . AND $ f x
    (MLIL.ASR x) -> Just . ASR $ f x
    (MLIL.BOOL_TO_INT x) -> Just . BOOL_TO_INT $ f x
    (MLIL.CEIL x) -> Just . CEIL $ f x
    (MLIL.CMP_E x) -> Just . CMP_E $ f x
    (MLIL.CMP_NE x) -> Just . CMP_NE $ f x
    (MLIL.CMP_SGE x) -> Just . CMP_SGE $ f x
    (MLIL.CMP_SGT x) -> Just . CMP_SGT $ f x
    (MLIL.CMP_SLE x) -> Just . CMP_SLE $ f x
    (MLIL.CMP_SLT x) -> Just . CMP_SLT $ f x
    (MLIL.CMP_UGE x) -> Just . CMP_UGE $ f x
    (MLIL.CMP_UGT x) -> Just . CMP_UGT $ f x
    (MLIL.CMP_ULE x) -> Just . CMP_ULE $ f x
    (MLIL.CMP_ULT x) -> Just . CMP_ULT $ f x
    (MLIL.CONST x) -> Just . CONST $ f x
    (MLIL.CONST_PTR x) -> Just . CONST_PTR $ f x
    (MLIL.DIVS x) -> Just . DIVS $ f x
    (MLIL.DIVS_DP x) -> Just . DIVS_DP $ f x
    (MLIL.DIVU x) -> Just . DIVU $ f x
    (MLIL.DIVU_DP x) -> Just . DIVU_DP $ f x
    (MLIL.FABS x) -> Just . FABS $ f x
    (MLIL.FADD x) -> Just . FADD $ f x
    (MLIL.FCMP_E x) -> Just . FCMP_E $ f x
    (MLIL.FCMP_GE x) -> Just . FCMP_GE $ f x
    (MLIL.FCMP_GT x) -> Just . FCMP_GT $ f x
    (MLIL.FCMP_LE x) -> Just . FCMP_LE $ f x
    (MLIL.FCMP_LT x) -> Just . FCMP_LT $ f x
    (MLIL.FCMP_NE x) -> Just . FCMP_NE $ f x
    (MLIL.FCMP_O x) -> Just . FCMP_O $ f x
    (MLIL.FCMP_UO x) -> Just . FCMP_UO $ f x
    (MLIL.FDIV x) -> Just . FDIV $ f x
    (MLIL.FLOAT_CONST x) -> Just . FLOAT_CONST $ f x
    (MLIL.FLOAT_CONV x) -> Just . FLOAT_CONV $ f x
    (MLIL.FLOAT_TO_INT x) -> Just . FLOAT_TO_INT $ f x
    (MLIL.FLOOR x) -> Just . FLOOR $ f x
    (MLIL.FMUL x) -> Just . FMUL $ f x
    (MLIL.FNEG x) -> Just . FNEG $ f x
    (MLIL.FSQRT x) -> Just . FSQRT $ f x
    (MLIL.FSUB x) -> Just . FSUB $ f x
    (MLIL.FTRUNC x) -> Just . FTRUNC $ f x
    (MLIL.IMPORT x) -> Just . IMPORT $ f x
    (MLIL.INT_TO_FLOAT x) -> Just . INT_TO_FLOAT $ f x
    (MLIL.LOAD x) -> Just . LOAD $ f x
    (MLIL.LOAD_SSA x) -> Just . LOAD_SSA $ f x
    (MLIL.LOAD_STRUCT x) -> Just . LOAD_STRUCT $ f x
    (MLIL.LOAD_STRUCT_SSA x) -> Just . LOAD_STRUCT_SSA $ f x
    (MLIL.LOW_PART x) -> Just . LOW_PART $ f x
    (MLIL.LSL x) -> Just . LSL $ f x
    (MLIL.LSR x) -> Just . LSR $ f x
    (MLIL.MODS x) -> Just . MODS $ f x
    (MLIL.MODS_DP x) -> Just . MODS_DP $ f x
    (MLIL.MODU x) -> Just . MODU $ f x
    (MLIL.MODU_DP x) -> Just . MODU_DP $ f x
    (MLIL.MUL x) -> Just . MUL $ f x
    (MLIL.MULS_DP x) -> Just . MULS_DP $ f x
    (MLIL.MULU_DP x) -> Just . MULU_DP $ f x
    (MLIL.NEG x) -> Just . NEG $ f x
    (MLIL.NOT x) -> Just . NOT $ f x
    (MLIL.OR x) -> Just . OR $ f x
    (MLIL.RLC x) -> Just . RLC $ f x
    (MLIL.ROL x) -> Just . ROL $ f x
    (MLIL.ROR x) -> Just . ROR $ f x
    (MLIL.ROUND_TO_INT x) -> Just . ROUND_TO_INT $ f x
    (MLIL.RRC x) -> Just . RRC $ f x
    (MLIL.SBB x) -> Just . SBB $ f x
    (MLIL.SUB x) -> Just . SUB $ f x
    (MLIL.SX x) -> Just . SX $ f x
    (MLIL.TEST_BIT x) -> Just . TEST_BIT $ f x
    (MLIL.UNIMPL) -> Just UNIMPL
--    (MLIL.VAR x) -> Just . VarOp expr)
    (MLIL.VAR_ALIASED x) -> Just . VAR_ALIASED
      $ VarAliasedOp (convertToPilVar ctx $ x ^. Pil.src)
    (MLIL.VAR_ALIASED_FIELD x) -> Just . VAR_ALIASED_FIELD
      $ VarAliasedFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)      
--    (MLIL.VAR_FIELD x) -> Just . VarFieldOp expr)
    (MLIL.VAR_PHI x) -> Just . VAR_PHI
      $ VarPhiOp (convertToPilVar ctx $ x ^. MLIL.dest) (fmap (convertToPilVar ctx) $ x ^. MLIL.src)
--    (MLIL.VAR_SPLIT x) -> Just . VarSplitOp expr)
    (MLIL.VAR_SPLIT_SSA x) -> Just . VAR_SPLIT
      $ VarSplitOp (convertToPilVar ctx $ x ^. MLIL.high) (convertToPilVar ctx $ x ^. MLIL.low)
    (MLIL.VAR_SSA x) -> Just . VAR
      $ VarOp (convertToPilVar ctx $ x ^. MLIL.src)
    (MLIL.VAR_SSA_FIELD x) -> Just . VAR_FIELD
      $ VarFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)
    (MLIL.XOR x) -> Just . XOR $ f x
    (MLIL.ZX x) -> Just . ZX $ f x
    _ -> Nothing
  where
    f :: Functor m => m (MLIL.Expression t) -> m MExpression
    f = fmap $ convertExpr_ ctx

getSymbol :: MLIL.SSAVariable -> Symbol
getSymbol v = (v ^. MLIL.var . Variable.name) <> "#" <> (show $ v ^. MLIL.version)

convertToPilVar :: Ctx -> MLIL.SSAVariable -> PilVar
convertToPilVar ctx v = PilVar
  (getSymbol v)
  (ctx ^. func)
  (ctx ^. ctxIndex)
  (Set.singleton $ SSAVariableRef v (ctx ^. func) (ctx ^. ctxIndex))

-- does an MEXpression and its children have all `Just` op fields?
isMExpressionComplete :: MExpression -> Bool
isMExpressionComplete mexpr = case mexpr ^. op of
  Nothing -> False
  Just exop -> foldr (\mexpr' b -> isMExpressionComplete mexpr' && b) True exop

convertMExpression :: MExpression -> Maybe Expression
convertMExpression mexpr = bool Nothing (Just $ convertCompleteMExpression mexpr)
  $ isMExpressionComplete mexpr
  where
    convertCompleteMExpression :: MExpression -> Expression
    convertCompleteMExpression mexpr' = Expression
      { _size = mexpr' ^. size
      , _op = fmap convertCompleteMExpression . fromJust $ mexpr' ^. op
      }

convertInstrOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> Maybe [Statement Expression]
convertInstrOp ctx op' = case op' of
  (MLIL.CALL_OUTPUT_SSA _) -> Nothing

  (MLIL.CALL_PARAM_SSA _) -> Nothing

  -- TODO
  (MLIL.CALL_SSA _) -> Nothing

  (MLIL.CALL_UNTYPED_SSA _) -> Nothing

  (MLIL.SET_VAR_SSA x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.dest) expr]

  -- TODO: Need some way to merge with previous version and include offset
  (MLIL.SET_VAR_SSA_FIELD x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.prev . MLIL.dest) expr]

  (MLIL.SET_VAR_SPLIT_SSA x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [ Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.high) expr
           , Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.low) expr
           ]

  -- TODO: Need to use :prev?
  (MLIL.SET_VAR_ALIASED x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.prev . MLIL.dest) expr]

  -- TODO: Need to find an example
  -- not in Dive-Logger
  (MLIL.SET_VAR_ALIASED_FIELD _) -> pure []

  (MLIL.STORE_SSA x) -> do
    exprSrc <- convertExpr ctx (x ^. MLIL.src)
    exprDest <- convertExpr ctx (x ^. MLIL.dest)
    return [Store $ StoreOp exprDest exprSrc]

  -- TODO: Need to find an example
  -- not in Dive-Logger
  (MLIL.STORE_STRUCT_SSA _) -> pure []

  (MLIL.VAR_PHI x) -> do
    latestVar <- headMay
                 . filter (flip Set.member $ ctx ^. definedVars)
                 . fmap (convertToPilVar ctx)
                 . sortOn ((*(-1)) . view MLIL.version)
                 $ x ^. MLIL.src
    vt <- x ^. MLIL.dest . MLIL.var . Variable.varType
    return [Def . DefOp (convertToPilVar ctx $ x ^. MLIL.dest)
             $ Expression (typeWidthToOperationSize $ vt ^. Variable.width)
             (VAR $ VarOp latestVar)
           ]

  MLIL.UNIMPL -> Just [UnimplInstr]

  (MLIL.UNIMPL_MEM x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [UnimplMem $ UnimplMemOp expr]

  MLIL.UNDEF -> Just [Undef]

  MLIL.NOP -> Just [Nop]

  _ -> Nothing
