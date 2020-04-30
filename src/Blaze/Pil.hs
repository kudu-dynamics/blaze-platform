module Blaze.Pil where

import Binja.Function (Function)
import qualified Binja.Function as Function
import qualified Binja.MLIL as MLIL
import qualified Binja.Variable as Variable
import Blaze.Prelude hiding
  ( Symbol,
    Type,
  )
import Blaze.Types.Function (CallInstruction)
import qualified Blaze.Types.Function as Function
import Blaze.Types.Pil
  ( CallDest,
    Ctx,
    DefOp (DefOp),
    ExprOp,
    Expression (Expression),
    PilVar (PilVar),
    SSAVariableRef (SSAVariableRef),
    Statement
      ( Def,
        Nop,
        Store,
        Undef,
        UnimplInstr,
        UnimplMem
      ),
    Stmt,
    StoreOp (StoreOp),
    Symbol,
    UnimplMemOp (UnimplMemOp),
  )
import qualified Blaze.Types.Pil as Pil
import Data.BinaryAnalysis (Address (Address))
import qualified Data.HashSet as HSet
import qualified Data.Text as Text


typeWidthToOperationSize :: Variable.TypeWidth -> MLIL.OperationSize
typeWidthToOperationSize (Variable.TypeWidth n) = MLIL.OperationSize n

convertExpr :: Ctx -> MLIL.Expression t -> Expression
convertExpr ctx x = Expression (x ^. Pil.size) (convertExprOp ctx $ x ^. Pil.op)

convertExprOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> ExprOp Expression
convertExprOp ctx mop =
  case mop of
    (MLIL.ADC x) -> Pil.ADC $ f x
    (MLIL.ADD x) -> Pil.ADD $ f x
    (MLIL.ADDRESS_OF x) -> Pil.ADDRESS_OF $ f x
    (MLIL.ADDRESS_OF_FIELD x) -> Pil.ADDRESS_OF_FIELD $ f x
    (MLIL.ADD_OVERFLOW x) -> Pil.ADD_OVERFLOW $ f x
    (MLIL.AND x) -> Pil.AND $ f x
    (MLIL.ASR x) -> Pil.ASR $ f x
    (MLIL.BOOL_TO_INT x) -> Pil.BOOL_TO_INT $ f x
    (MLIL.CEIL x) -> Pil.CEIL $ f x
    (MLIL.CMP_E x) -> Pil.CMP_E $ f x
    (MLIL.CMP_NE x) -> Pil.CMP_NE $ f x
    (MLIL.CMP_SGE x) -> Pil.CMP_SGE $ f x
    (MLIL.CMP_SGT x) -> Pil.CMP_SGT $ f x
    (MLIL.CMP_SLE x) -> Pil.CMP_SLE $ f x
    (MLIL.CMP_SLT x) -> Pil.CMP_SLT $ f x
    (MLIL.CMP_UGE x) -> Pil.CMP_UGE $ f x
    (MLIL.CMP_UGT x) -> Pil.CMP_UGT $ f x
    (MLIL.CMP_ULE x) -> Pil.CMP_ULE $ f x
    (MLIL.CMP_ULT x) -> Pil.CMP_ULT $ f x
    (MLIL.CONST x) -> Pil.CONST $ f x
    (MLIL.CONST_PTR x) -> Pil.CONST_PTR $ f x
    (MLIL.DIVS x) -> Pil.DIVS $ f x
    (MLIL.DIVS_DP x) -> Pil.DIVS_DP $ f x
    (MLIL.DIVU x) -> Pil.DIVU $ f x
    (MLIL.DIVU_DP x) -> Pil.DIVU_DP $ f x
    (MLIL.FABS x) -> Pil.FABS $ f x
    (MLIL.FADD x) -> Pil.FADD $ f x
    (MLIL.FCMP_E x) -> Pil.FCMP_E $ f x
    (MLIL.FCMP_GE x) -> Pil.FCMP_GE $ f x
    (MLIL.FCMP_GT x) -> Pil.FCMP_GT $ f x
    (MLIL.FCMP_LE x) -> Pil.FCMP_LE $ f x
    (MLIL.FCMP_LT x) -> Pil.FCMP_LT $ f x
    (MLIL.FCMP_NE x) -> Pil.FCMP_NE $ f x
    (MLIL.FCMP_O x) -> Pil.FCMP_O $ f x
    (MLIL.FCMP_UO x) -> Pil.FCMP_UO $ f x
    (MLIL.FDIV x) -> Pil.FDIV $ f x
    (MLIL.FLOAT_CONST x) -> Pil.FLOAT_CONST $ f x
    (MLIL.FLOAT_CONV x) -> Pil.FLOAT_CONV $ f x
    (MLIL.FLOAT_TO_INT x) -> Pil.FLOAT_TO_INT $ f x
    (MLIL.FLOOR x) -> Pil.FLOOR $ f x
    (MLIL.FMUL x) -> Pil.FMUL $ f x
    (MLIL.FNEG x) -> Pil.FNEG $ f x
    (MLIL.FSQRT x) -> Pil.FSQRT $ f x
    (MLIL.FSUB x) -> Pil.FSUB $ f x
    (MLIL.FTRUNC x) -> Pil.FTRUNC $ f x
    (MLIL.IMPORT x) -> Pil.IMPORT $ f x
    (MLIL.INT_TO_FLOAT x) -> Pil.INT_TO_FLOAT $ f x
    (MLIL.LOAD x) -> Pil.LOAD $ f x
    (MLIL.LOAD_SSA x) -> Pil.LOAD . Pil.LoadOp . convertExpr ctx $ x ^. MLIL.src
    (MLIL.LOAD_STRUCT x) -> Pil.LOAD . Pil.LoadOp
      $ addConstToExpr (convertExpr ctx $ x ^. MLIL.src) (x ^. MLIL.offset)
    (MLIL.LOAD_STRUCT_SSA x) -> Pil.LOAD . Pil.LoadOp
      $ addConstToExpr (convertExpr ctx $ x ^. MLIL.src) (x ^. MLIL.offset)
    (MLIL.LOW_PART x) -> Pil.LOW_PART $ f x
    (MLIL.LSL x) -> Pil.LSL $ f x
    (MLIL.LSR x) -> Pil.LSR $ f x
    (MLIL.MODS x) -> Pil.MODS $ f x
    (MLIL.MODS_DP x) -> Pil.MODS_DP $ f x
    (MLIL.MODU x) -> Pil.MODU $ f x
    (MLIL.MODU_DP x) -> Pil.MODU_DP $ f x
    (MLIL.MUL x) -> Pil.MUL $ f x
    (MLIL.MULS_DP x) -> Pil.MULS_DP $ f x
    (MLIL.MULU_DP x) -> Pil.MULU_DP $ f x
    (MLIL.NEG x) -> Pil.NEG $ f x
    (MLIL.NOT x) -> Pil.NOT $ f x
    (MLIL.OR x) -> Pil.OR $ f x
    (MLIL.RLC x) -> Pil.RLC $ f x
    (MLIL.ROL x) -> Pil.ROL $ f x
    (MLIL.ROR x) -> Pil.ROR $ f x
    (MLIL.ROUND_TO_INT x) -> Pil.ROUND_TO_INT $ f x
    (MLIL.RRC x) -> Pil.RRC $ f x
    (MLIL.SBB x) -> Pil.SBB $ f x
    (MLIL.SUB x) -> Pil.SUB $ f x
    (MLIL.SX x) -> Pil.SX $ f x
    (MLIL.TEST_BIT x) -> Pil.TEST_BIT $ f x
    MLIL.UNIMPL -> Pil.UNIMPL "UNIMPL"
--    (MLIL.VAR x) -> VarOp expr)
    (MLIL.VAR_ALIASED x) -> Pil.VAR_ALIASED
      . Pil.VarAliasedOp . convertToPilVar ctx $ x ^. Pil.src
    (MLIL.VAR_ALIASED_FIELD x) -> Pil.VAR_ALIASED_FIELD
      $ Pil.VarAliasedFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)      
--    (MLIL.VAR_FIELD x) -> VarFieldOp expr)
    (MLIL.VAR_PHI x) -> Pil.VAR_PHI
      $ Pil.VarPhiOp (convertToPilVar ctx $ x ^. MLIL.dest) (fmap (convertToPilVar ctx) $ x ^. MLIL.src)
--    (MLIL.VAR_SPLIT x) -> VarSplitOp expr)
    (MLIL.VAR_SPLIT_SSA x) -> Pil.VAR_SPLIT
      $ Pil.VarSplitOp (convertToPilVar ctx $ x ^. MLIL.high) (convertToPilVar ctx $ x ^. MLIL.low)
    (MLIL.VAR_SSA x) -> Pil.VAR
      $ Pil.VarOp (convertToPilVar ctx $ x ^. MLIL.src)
    (MLIL.VAR_SSA_FIELD x) -> Pil.VAR_FIELD
      $ Pil.VarFieldOp (convertToPilVar ctx $ x ^. MLIL.src) (x ^. MLIL.offset)
    (MLIL.XOR x) -> Pil.XOR $ f x
    (MLIL.ZX x) -> Pil.ZX $ f x
    x -> Pil.UNIMPL $ Text.take 20 (show x) <> "..."
  where
    f :: Functor m => m (MLIL.Expression t) -> m Expression
    f = fmap $ convertExpr ctx

getSymbol :: MLIL.SSAVariable -> Symbol
getSymbol v = (v ^. MLIL.var . Variable.name) <> "#" <> show (v ^. MLIL.version)

convertToPilVar :: Ctx -> MLIL.SSAVariable -> PilVar
convertToPilVar ctx v = PilVar
  (getSymbol v)
  (ctx ^. Pil.func)
  (ctx ^. Pil.ctxIndex)
  (HSet.singleton $ SSAVariableRef v (ctx ^. Pil.func) (ctx ^. Pil.ctxIndex))


addConstToExpr :: Expression -> Int64 -> Expression
addConstToExpr expr@(Expression size _) n = Expression size addOp
  where
    addOp = Pil.ADD $ Pil.AddOp expr const'
    const' = Expression size . Pil.CONST $ Pil.ConstOp n

convertInstrOp :: Ctx -> MLIL.Operation (MLIL.Expression t) -> [Statement Expression]
convertInstrOp ctx op' = maybe [] identity $ case op' of
  (MLIL.CALL_OUTPUT_SSA _) -> Nothing

  (MLIL.CALL_PARAM_SSA _) -> Nothing

  -- TODO
  -- output field points to CallOutputSSA
  (MLIL.CALL_SSA _) -> Nothing

  (MLIL.CALL_UNTYPED_SSA _) -> Nothing

  (MLIL.SET_VAR_SSA x) -> do
    expr <- Just $ convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.dest) expr]

  -- TODO: Need some way to merge with previous version and include offset
  (MLIL.SET_VAR_SSA_FIELD x) -> do
    expr <- Just $ convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.prev . MLIL.dest) expr]

  (MLIL.SET_VAR_SPLIT_SSA x) -> do
    expr@(Expression size _) <- Just $ convertExpr ctx (x ^. MLIL.src)
    let halfSize = size `div` 2
        highExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr (fromIntegral halfSize)
        lowExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr 0
    return [ Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.high) highExpr
           , Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.low) lowExpr
           ]

  -- TODO: Need to use prev.src?
  (MLIL.SET_VAR_ALIASED x) -> do
    expr <- Just $ convertExpr ctx (x ^. MLIL.src)
    return [Def $ DefOp (convertToPilVar ctx $ x ^. MLIL.prev . MLIL.dest) expr]

  -- TODO: Need to find an example
  -- not in Dive-Logger
  (MLIL.SET_VAR_ALIASED_FIELD _) -> pure []

  (MLIL.STORE_SSA x) -> do
    exprSrc <- Just $ convertExpr ctx (x ^. MLIL.src)
    exprDest <- Just $ convertExpr ctx (x ^. MLIL.dest)
    return [Store $ StoreOp exprDest exprSrc]

  (MLIL.STORE_STRUCT_SSA x) -> Just [Store $ StoreOp exprDest exprSrc]
    where
      exprDest = addConstToExpr (convertExpr ctx (x ^. MLIL.dest)) (x ^. MLIL.offset)
      exprSrc = convertExpr ctx (x ^. MLIL.src)

  (MLIL.VAR_PHI x) -> do
    latestVar <- headMay
                 . filter (flip HSet.member $ ctx ^. Pil.definedVars)
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
    expr <- Just $ convertExpr ctx (x ^. MLIL.src)
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
    . Address . fromIntegral $ op ^. Pil.constant
  return $ view Function.name <$> mfn
getCallDestFunctionName _ _ = return Nothing

-- TODO: Does this fail on system calls?
convertCallInstruction :: Ctx -> CallInstruction -> IO [Stmt]
convertCallInstruction ctx c = case cond of
  Nothing -> return []
  Just ([], _) -> return []
  Just ((dest:_), target) -> do
    mname <- maybe (return Nothing) 
                   (`getCallDestFunctionName` target)
                   (ctx ^. Pil.func)
    let callExpr = Expression (c ^. Function.size)
          . Pil.CALL . Pil.CallOp target mname . fmap (convertExpr ctx)
          $ c ^. Function.params
    return [Def $ DefOp (convertToPilVar ctx dest) callExpr]
  where
    cond = (,) <$> (c ^. Function.outputDest)
               <*> (Pil.getCallDest <$>
                    (c ^. Function.dest >>= Just . convertExpr ctx))
