module Blaze.Pil where

import           Blaze.Prelude                    hiding ( Symbol
                                                         , Type
                                                         )

import           Binja.Function                          ( Function )
import qualified Binja.Function       as Function
import qualified Binja.MLIL           as MLIL
import qualified Binja.Variable       as Variable
import           Blaze.Types.Function                    ( CallInstruction )
import qualified Blaze.Types.Function as Function
import           Blaze.Types.Pil                         ( CallDest
                                                         , Ctx
                                                         , DefOp( DefOp )
                                                         , ExprOp
                                                         , Expression( Expression )
                                                         , MExpression( MExpression )
                                                         , PilVar( PilVar )
                                                         , SSAVariableRef( SSAVariableRef )
                                                         , Statement( Def
                                                                    , Nop
                                                                    , Store
                                                                    , Undef
                                                                    , UnimplInstr
                                                                    , UnimplMem
                                                                    )
                                                         , Stmt
                                                         , StoreOp( StoreOp )
                                                         , Symbol
                                                         , UnimplMemOp( UnimplMemOp )
                                                         )
import qualified Blaze.Types.Pil      as Pil
import qualified Data.Set             as Set

typeWidthToOperationSize :: Variable.TypeWidth -> MLIL.OperationSize
typeWidthToOperationSize (Variable.TypeWidth n) = MLIL.OperationSize n

convertExpr :: Ctx -> MLIL.Expression t -> Maybe Expression
convertExpr ctx = convertMExpression . convertExpr_ ctx

convertExpr_ :: Ctx -> MLIL.Expression t -> MExpression
convertExpr_ ctx e = MExpression (e ^. Pil.size) (convertExprOp ctx $ e ^. Pil.op)

convertExprOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> Maybe (ExprOp MExpression)
convertExprOp ctx mop =
  case mop of
    (MLIL.ADC x) -> Just . Pil.ADC $ f x
    (MLIL.ADD x) -> Just . Pil.ADD $ f x
    (MLIL.ADDRESS_OF x) -> Just . Pil.ADDRESS_OF $ f x
    (MLIL.ADDRESS_OF_FIELD x) -> Just . Pil.ADDRESS_OF_FIELD $ f x
    (MLIL.ADD_OVERFLOW x) -> Just . Pil.ADD_OVERFLOW $ f x
    (MLIL.AND x) -> Just . Pil.AND $ f x
    (MLIL.ASR x) -> Just . Pil.ASR $ f x
    (MLIL.BOOL_TO_INT x) -> Just . Pil.BOOL_TO_INT $ f x
    (MLIL.CEIL x) -> Just . Pil.CEIL $ f x
    (MLIL.CMP_E x) -> Just . Pil.CMP_E $ f x
    (MLIL.CMP_NE x) -> Just . Pil.CMP_NE $ f x
    (MLIL.CMP_SGE x) -> Just . Pil.CMP_SGE $ f x
    (MLIL.CMP_SGT x) -> Just . Pil.CMP_SGT $ f x
    (MLIL.CMP_SLE x) -> Just . Pil.CMP_SLE $ f x
    (MLIL.CMP_SLT x) -> Just . Pil.CMP_SLT $ f x
    (MLIL.CMP_UGE x) -> Just . Pil.CMP_UGE $ f x
    (MLIL.CMP_UGT x) -> Just . Pil.CMP_UGT $ f x
    (MLIL.CMP_ULE x) -> Just . Pil.CMP_ULE $ f x
    (MLIL.CMP_ULT x) -> Just . Pil.CMP_ULT $ f x
    (MLIL.CONST x) -> Just . Pil.CONST $ f x
    (MLIL.CONST_PTR x) -> Just . Pil.CONST_PTR $ f x
    (MLIL.DIVS x) -> Just . Pil.DIVS $ f x
    (MLIL.DIVS_DP x) -> Just . Pil.DIVS_DP $ f x
    (MLIL.DIVU x) -> Just . Pil.DIVU $ f x
    (MLIL.DIVU_DP x) -> Just . Pil.DIVU_DP $ f x
    (MLIL.FABS x) -> Just . Pil.FABS $ f x
    (MLIL.FADD x) -> Just . Pil.FADD $ f x
    (MLIL.FCMP_E x) -> Just . Pil.FCMP_E $ f x
    (MLIL.FCMP_GE x) -> Just . Pil.FCMP_GE $ f x
    (MLIL.FCMP_GT x) -> Just . Pil.FCMP_GT $ f x
    (MLIL.FCMP_LE x) -> Just . Pil.FCMP_LE $ f x
    (MLIL.FCMP_LT x) -> Just . Pil.FCMP_LT $ f x
    (MLIL.FCMP_NE x) -> Just . Pil.FCMP_NE $ f x
    (MLIL.FCMP_O x) -> Just . Pil.FCMP_O $ f x
    (MLIL.FCMP_UO x) -> Just . Pil.FCMP_UO $ f x
    (MLIL.FDIV x) -> Just . Pil.FDIV $ f x
    (MLIL.FLOAT_CONST x) -> Just . Pil.FLOAT_CONST $ f x
    (MLIL.FLOAT_CONV x) -> Just . Pil.FLOAT_CONV $ f x
    (MLIL.FLOAT_TO_INT x) -> Just . Pil.FLOAT_TO_INT $ f x
    (MLIL.FLOOR x) -> Just . Pil.FLOOR $ f x
    (MLIL.FMUL x) -> Just . Pil.FMUL $ f x
    (MLIL.FNEG x) -> Just . Pil.FNEG $ f x
    (MLIL.FSQRT x) -> Just . Pil.FSQRT $ f x
    (MLIL.FSUB x) -> Just . Pil.FSUB $ f x
    (MLIL.FTRUNC x) -> Just . Pil.FTRUNC $ f x
    (MLIL.IMPORT x) -> Just . Pil.IMPORT $ f x
    (MLIL.INT_TO_FLOAT x) -> Just . Pil.INT_TO_FLOAT $ f x
    (MLIL.LOAD x) -> Just . Pil.LOAD $ f x
    (MLIL.LOAD_SSA x) -> Just . Pil.LOAD_SSA $ f x
    (MLIL.LOAD_STRUCT x) -> Just . Pil.LOAD_STRUCT $ f x
    (MLIL.LOAD_STRUCT_SSA x) -> Just . Pil.LOAD_STRUCT_SSA $ f x
    (MLIL.LOW_PART x) -> Just . Pil.LOW_PART $ f x
    (MLIL.LSL x) -> Just . Pil.LSL $ f x
    (MLIL.LSR x) -> Just . Pil.LSR $ f x
    (MLIL.MODS x) -> Just . Pil.MODS $ f x
    (MLIL.MODS_DP x) -> Just . Pil.MODS_DP $ f x
    (MLIL.MODU x) -> Just . Pil.MODU $ f x
    (MLIL.MODU_DP x) -> Just . Pil.MODU_DP $ f x
    (MLIL.MUL x) -> Just . Pil.MUL $ f x
    (MLIL.MULS_DP x) -> Just . Pil.MULS_DP $ f x
    (MLIL.MULU_DP x) -> Just . Pil.MULU_DP $ f x
    (MLIL.NEG x) -> Just . Pil.NEG $ f x
    (MLIL.NOT x) -> Just . Pil.NOT $ f x
    (MLIL.OR x) -> Just . Pil.OR $ f x
    (MLIL.RLC x) -> Just . Pil.RLC $ f x
    (MLIL.ROL x) -> Just . Pil.ROL $ f x
    (MLIL.ROR x) -> Just . Pil.ROR $ f x
    (MLIL.ROUND_TO_INT x) -> Just . Pil.ROUND_TO_INT $ f x
    (MLIL.RRC x) -> Just . Pil.RRC $ f x
    (MLIL.SBB x) -> Just . Pil.SBB $ f x
    (MLIL.SUB x) -> Just . Pil.SUB $ f x
    (MLIL.SX x) -> Just . Pil.SX $ f x
    (MLIL.TEST_BIT x) -> Just . Pil.TEST_BIT $ f x
    (MLIL.UNIMPL) -> Just Pil.UNIMPL
--    (MLIL.VAR x) -> Just . VarOp expr)
    (MLIL.VAR_ALIASED x) -> Just . Pil.VAR_ALIASED
      $ Pil.VarAliasedOp (convertToPilVar ctx $ x ^. Pil.src)
    (MLIL.VAR_ALIASED_FIELD x) -> Just . Pil.VAR_ALIASED_FIELD
      $ Pil.VarAliasedFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)      
--    (MLIL.VAR_FIELD x) -> Just . VarFieldOp expr)
    (MLIL.VAR_PHI x) -> Just . Pil.VAR_PHI
      $ Pil.VarPhiOp (convertToPilVar ctx $ x ^. MLIL.dest) (fmap (convertToPilVar ctx) $ x ^. MLIL.src)
--    (MLIL.VAR_SPLIT x) -> Just . VarSplitOp expr)
    (MLIL.VAR_SPLIT_SSA x) -> Just . Pil.VAR_SPLIT
      $ Pil.VarSplitOp (convertToPilVar ctx $ x ^. MLIL.high) (convertToPilVar ctx $ x ^. MLIL.low)
    (MLIL.VAR_SSA x) -> Just . Pil.VAR
      $ Pil.VarOp (convertToPilVar ctx $ x ^. MLIL.src)
    (MLIL.VAR_SSA_FIELD x) -> Just . Pil.VAR_FIELD
      $ Pil.VarFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)
    (MLIL.XOR x) -> Just . Pil.XOR $ f x
    (MLIL.ZX x) -> Just . Pil.ZX $ f x
    _ -> Nothing
  where
    f :: Functor m => m (MLIL.Expression t) -> m MExpression
    f = fmap $ convertExpr_ ctx

getSymbol :: MLIL.SSAVariable -> Symbol
getSymbol v = (v ^. MLIL.var . Variable.name) <> "#" <> (show $ v ^. MLIL.version)

convertToPilVar :: Ctx -> MLIL.SSAVariable -> PilVar
convertToPilVar ctx v = PilVar
  (getSymbol v)
  (ctx ^. Pil.func)
  (ctx ^. Pil.ctxIndex)
  (Set.singleton $ SSAVariableRef v (ctx ^. Pil.func) (ctx ^. Pil.ctxIndex))

-- does an MEXpression and its children have all `Just` op fields?
isMExpressionComplete :: MExpression -> Bool
isMExpressionComplete mexpr = case mexpr ^. Pil.op of
  Nothing -> False
  Just exop -> foldr (\mexpr' b -> isMExpressionComplete mexpr' && b) True exop

convertMExpression :: MExpression -> Maybe Expression
convertMExpression mexpr = bool Nothing (Just $ convertCompleteMExpression mexpr)
  $ isMExpressionComplete mexpr
  where
    convertCompleteMExpression :: MExpression -> Expression
    convertCompleteMExpression mexpr' =
      Expression (mexpr' ^. Pil.size)
      (fmap convertCompleteMExpression . fromJust $ mexpr' ^. Pil.op)

convertInstrOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> [Statement Expression]
convertInstrOp ctx op' = maybe [] identity $ case op' of
  (MLIL.CALL_OUTPUT_SSA _) -> Nothing

  (MLIL.CALL_PARAM_SSA _) -> Nothing

  -- TODO
  -- output field points to CallOutputSSA
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
                 . filter (flip Set.member $ ctx ^. Pil.definedVars)
                 . fmap (convertToPilVar ctx)
                 . sortOn ((*(-1)) . view MLIL.version)
                 $ x ^. MLIL.src
    vt <- x ^. MLIL.dest . MLIL.var . Variable.varType
    return [Def . DefOp (convertToPilVar ctx $ x ^. MLIL.dest)
             $ Expression (typeWidthToOperationSize $ vt ^. Variable.width)
             (Pil.VAR $ Pil.VarOp latestVar)
           ]

  MLIL.UNIMPL -> Just [UnimplInstr]

  (MLIL.UNIMPL_MEM x) -> do
    expr <- convertExpr ctx (x ^. MLIL.src)
    return [UnimplMem $ UnimplMemOp expr]

  MLIL.UNDEF -> Just [Undef]

  MLIL.NOP -> Just [Nop]

  _ -> Nothing


convertInstr :: Ctx -> MLIL.Instruction t -> [Stmt]
convertInstr ctx = convertInstrOp ctx . view MLIL.op

convertInstrs :: Ctx -> [MLIL.Instruction t] -> [Stmt]
convertInstrs ctx = concatMap $ convertInstr ctx

getCallDestFunctionName :: Function -> CallDest expr -> IO (Maybe Text)
getCallDestFunctionName ctxfn (Pil.CallConstPtr op) = do
  bv <- Function.getFunctionDataBinaryView ctxfn
  mfn <- Function.getFunctionStartingAt bv Nothing
    . Function.Address . fromIntegral $ op ^. Pil.constant
  return $ view Function.name <$> mfn
getCallDestFunctionName _ _ = return Nothing

convertCallInstruction :: Ctx -> CallInstruction -> IO [Stmt]
convertCallInstruction ctx c = case cond of
  Nothing -> return []
  Just ([], _) -> return []
  Just ((dest:_), target) -> do
    mname <- maybe (return Nothing) (flip getCallDestFunctionName target)
             $ (ctx ^. Pil.func)
    let callExpr = Expression (c ^. Function.size)
          . Pil.CALL . Pil.CallOp target mname . mapMaybe (convertExpr ctx)
          $ c ^. Function.params
    return [Def $ DefOp (convertToPilVar ctx dest) callExpr]
  where
    cond = (,) <$> (c ^. Function.outputDest)
               <*> (Pil.getCallDest <$>
                    (c ^. Function.dest >>= convertExpr ctx))
