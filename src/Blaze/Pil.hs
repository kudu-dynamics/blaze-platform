module Blaze.Pil where

import Binja.Function (Function)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as MLIL
import qualified Binja.Variable as BNVar
import Binja.Variable (Variable)
import Blaze.Pil.Analysis (getAllSyms)
import qualified Blaze.Types.Path.AlgaPath as AlgaPath
import Blaze.Types.Pil.Analysis (symbolGenerator)
import Blaze.Prelude hiding
  ( Symbol,
    Type,
  )
import Blaze.Types.Function
  ( Access (In, Out),
    CallInstruction,
    FuncInfo,
    FuncParamInfo (FuncParamInfo, FuncVarArgInfo),
    ParamInfo (ParamInfo),
    ResultInfo (ResultInfo),
    mkFuncInfo,
  )
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil
  ( CallDest,
    CallOp (CallOp),
    Ctx,
    DefOp (DefOp),
    Expression (Expression),
    PilVar (PilVar),
    SSAVariableRef (SSAVariableRef),
    Statement
      ( Call,
        Def,
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
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HSet
import qualified Data.Text as Text
import Binja.Function (MLILSSAFunction)

type F = MLILSSAFunction

--binja's default size is 0
--but that seems like it would mess up arithmetic
--with size 8, it will be easy to cast to smaller width if needed
defaultStackLocalAddrSize :: Pil.OperationSize
defaultStackLocalAddrSize = 8

-- maybe used for set_var_ssa_field
defaultVarSize :: Pil.OperationSize
defaultVarSize = 8

varToStackLocalAddr :: Ctx -> Variable -> Expression
varToStackLocalAddr ctx v =
  Expression defaultStackLocalAddrSize
    . Pil.STACK_LOCAL_ADDR
    . Pil.StackLocalAddrOp
    . Pil.StackOffset ctx
    . fromIntegral
    $ v ^. BNVar.storage

-- should we throw an error if (v ^. BNVar.sourceType /= StackVariableSourceType)?

typeWidthToOperationSize :: BNVar.TypeWidth -> MLIL.OperationSize
typeWidthToOperationSize (BNVar.TypeWidth n) = MLIL.OperationSize n

mkFieldOffsetExprAddr :: Pil.Expression -> Int64 -> Pil.Expression
mkFieldOffsetExprAddr addrExpr offset =
  Pil.Expression
    (addrExpr ^. MLIL.size)
    ( Pil.FIELD_ADDR . Pil.FieldAddrOp addrExpr
        . fromIntegral
        $ offset
    )

convertExpr :: MLIL.Expression t -> Pil.Converter Expression
convertExpr expr = do
  ctx <- use Pil.ctx
  case expr ^. MLIL.op of
    (MLIL.ADC x) -> mkExpr . Pil.ADC <$> f x
    (MLIL.ADD x) -> mkExpr . Pil.ADD <$> f x
    (MLIL.ADDRESS_OF x) -> return $ varToStackLocalAddr ctx (x ^. MLIL.src)
    (MLIL.ADDRESS_OF_FIELD x) ->
      return $ mkExpr . Pil.FIELD_ADDR
        . Pil.FieldAddrOp (varToStackLocalAddr ctx $ x ^. MLIL.src)
        . fromIntegral
        $ x ^. MLIL.offset
    (MLIL.ADD_OVERFLOW x) -> mkExpr . Pil.ADD_OVERFLOW <$> f x
    (MLIL.AND x) -> mkExpr . Pil.AND <$> f x
    (MLIL.ASR x) -> mkExpr . Pil.ASR <$> f x
    (MLIL.BOOL_TO_INT x) -> mkExpr . Pil.BOOL_TO_INT <$> f x
    (MLIL.CEIL x) -> mkExpr . Pil.CEIL <$> f x
    (MLIL.CMP_E x) -> mkExpr . Pil.CMP_E <$> f x
    (MLIL.CMP_NE x) -> mkExpr . Pil.CMP_NE <$> f x
    (MLIL.CMP_SGE x) -> mkExpr . Pil.CMP_SGE <$> f x
    (MLIL.CMP_SGT x) -> mkExpr . Pil.CMP_SGT <$> f x
    (MLIL.CMP_SLE x) -> mkExpr . Pil.CMP_SLE <$> f x
    (MLIL.CMP_SLT x) -> mkExpr . Pil.CMP_SLT <$> f x
    (MLIL.CMP_UGE x) -> mkExpr . Pil.CMP_UGE <$> f x
    (MLIL.CMP_UGT x) -> mkExpr . Pil.CMP_UGT <$> f x
    (MLIL.CMP_ULE x) -> mkExpr . Pil.CMP_ULE <$> f x
    (MLIL.CMP_ULT x) -> mkExpr . Pil.CMP_ULT <$> f x
    (MLIL.CONST x) -> mkExpr . Pil.CONST <$> f x
    (MLIL.CONST_PTR x) -> mkExpr . Pil.CONST_PTR <$> f x
    (MLIL.DIVS x) -> mkExpr . Pil.DIVS <$> f x
    (MLIL.DIVS_DP x) -> mkExpr . Pil.DIVS_DP <$> f x
    (MLIL.DIVU x) -> mkExpr . Pil.DIVU <$> f x
    (MLIL.DIVU_DP x) -> mkExpr . Pil.DIVU_DP <$> f x
    (MLIL.FABS x) -> mkExpr . Pil.FABS <$> f x
    (MLIL.FADD x) -> mkExpr . Pil.FADD <$> f x
    (MLIL.FCMP_E x) -> mkExpr . Pil.FCMP_E <$> f x
    (MLIL.FCMP_GE x) -> mkExpr . Pil.FCMP_GE <$> f x
    (MLIL.FCMP_GT x) -> mkExpr . Pil.FCMP_GT <$> f x
    (MLIL.FCMP_LE x) -> mkExpr . Pil.FCMP_LE <$> f x
    (MLIL.FCMP_LT x) -> mkExpr . Pil.FCMP_LT <$> f x
    (MLIL.FCMP_NE x) -> mkExpr . Pil.FCMP_NE <$> f x
    (MLIL.FCMP_O x) -> mkExpr . Pil.FCMP_O <$> f x
    (MLIL.FCMP_UO x) -> mkExpr . Pil.FCMP_UO <$> f x
    (MLIL.FDIV x) -> mkExpr . Pil.FDIV <$> f x
    (MLIL.FLOAT_CONST x) -> mkExpr . Pil.FLOAT_CONST <$> f x
    (MLIL.FLOAT_CONV x) -> mkExpr . Pil.FLOAT_CONV <$> f x
    (MLIL.FLOAT_TO_INT x) -> mkExpr . Pil.FLOAT_TO_INT <$> f x
    (MLIL.FLOOR x) -> mkExpr . Pil.FLOOR <$> f x
    (MLIL.FMUL x) -> mkExpr . Pil.FMUL <$> f x
    (MLIL.FNEG x) -> mkExpr . Pil.FNEG <$> f x
    (MLIL.FSQRT x) -> mkExpr . Pil.FSQRT <$> f x
    (MLIL.FSUB x) -> mkExpr . Pil.FSUB <$> f x
    (MLIL.FTRUNC x) -> mkExpr . Pil.FTRUNC <$> f x
    (MLIL.IMPORT x) -> mkExpr . Pil.IMPORT <$> f x
    (MLIL.INT_TO_FLOAT x) -> mkExpr . Pil.INT_TO_FLOAT <$> f x
    (MLIL.LOAD x) -> mkExpr . Pil.LOAD <$> f x
    (MLIL.LOAD_SSA x) -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $ srcExpr
    (MLIL.LOAD_STRUCT x) -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $
        mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    (MLIL.LOAD_STRUCT_SSA x) -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $
        mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    (MLIL.LOW_PART x) -> mkExpr . Pil.LOW_PART <$> f x
    (MLIL.LSL x) -> mkExpr . Pil.LSL <$> f x
    (MLIL.LSR x) -> mkExpr . Pil.LSR <$> f x
    (MLIL.MODS x) -> mkExpr . Pil.MODS <$> f x
    (MLIL.MODS_DP x) -> mkExpr . Pil.MODS_DP <$> f x
    (MLIL.MODU x) -> mkExpr . Pil.MODU <$> f x
    (MLIL.MODU_DP x) -> mkExpr . Pil.MODU_DP <$> f x
    (MLIL.MUL x) -> mkExpr . Pil.MUL <$> f x

    -- NOTE: binja gets these return sizes wrong
    (MLIL.MULS_DP x) -> Expression (2 * expr ^. Pil.size) . Pil.MULS_DP <$> f x
    (MLIL.MULU_DP x) -> Expression (2 * expr ^. Pil.size) . Pil.MULU_DP <$> f x

    (MLIL.NEG x) -> mkExpr . Pil.NEG <$> f x
    (MLIL.NOT x) -> mkExpr . Pil.NOT <$> f x
    (MLIL.OR x) -> mkExpr . Pil.OR <$> f x
    (MLIL.RLC x) -> mkExpr . Pil.RLC <$> f x
    (MLIL.ROL x) -> mkExpr . Pil.ROL <$> f x
    (MLIL.ROR x) -> mkExpr . Pil.ROR <$> f x
    (MLIL.ROUND_TO_INT x) -> mkExpr . Pil.ROUND_TO_INT <$> f x
    (MLIL.RRC x) -> mkExpr . Pil.RRC <$> f x
    (MLIL.SBB x) -> mkExpr . Pil.SBB <$> f x
    (MLIL.SUB x) -> mkExpr . Pil.SUB <$> f x
    (MLIL.SX x) -> mkExpr . Pil.SX <$> f x
    (MLIL.TEST_BIT x) -> mkExpr . Pil.TEST_BIT <$> f x
    MLIL.UNIMPL -> return $ mkExpr $ Pil.UNIMPL "UNIMPL"
    --    (MLIL.VAR x) -> VarOp expr)
    (MLIL.VAR_ALIASED x) -> return $ mkExpr . Pil.LOAD . Pil.LoadOp $ addrExpr
      where
        addrExpr = varToStackLocalAddr ctx (x ^. MLIL.src . MLIL.var)
    (MLIL.VAR_ALIASED_FIELD x) ->
      return $ mkExpr . Pil.LOAD . Pil.LoadOp
        . mkFieldOffsetExprAddr addrExpr
        $ x ^. MLIL.offset
      where
        addrExpr = varToStackLocalAddr ctx (x ^. MLIL.src . MLIL.var)
    (MLIL.VAR_SPLIT_SSA x) -> do
      highVar <- convertToPilVarAndLog $ x ^. MLIL.high
      lowVar <- convertToPilVarAndLog $ x ^. MLIL.low
      -- return $ mkExpr . Pil.VAR_SPLIT $ Pil.VarSplitOp highVar lowVar
      -- NOTE: Binja gets the return size wrong. use above if they fix it
      return $ Expression (2 * expr ^. Pil.size)
        . Pil.VAR_SPLIT $ Pil.VarSplitOp highVar lowVar
    (MLIL.VAR_SSA x) -> do
      srcVar <- convertToPilVarAndLog $ x ^. MLIL.src
      return $ mkExpr . Pil.VAR $ Pil.VarOp srcVar
    (MLIL.VAR_SSA_FIELD x) -> do
      srcVar <- convertToPilVarAndLog $ x ^. MLIL.src
      return $ mkExpr . Pil.VAR_FIELD $
        Pil.VarFieldOp srcVar (ByteOffset $ x ^. MLIL.offset)
    (MLIL.XOR x) -> mkExpr . Pil.XOR <$> f x
    (MLIL.ZX x) -> mkExpr . Pil.ZX <$> f x
    x -> return $ mkExpr . Pil.UNIMPL $ Text.take 20 (show x) <> "..."
    where
      f :: Traversable m => m (MLIL.Expression t) -> Pil.Converter (m Expression)
      f = traverse convertExpr
      mkExpr :: Pil.ExprOp Expression -> Expression
      mkExpr = Expression (expr ^. Pil.size)

getSymbol :: MLIL.SSAVariable -> Symbol
getSymbol v = (v ^. MLIL.var . BNVar.name) <> "#" <> show (v ^. MLIL.version)

convertToPilVar :: Ctx -> MLIL.SSAVariable -> PilVar
convertToPilVar ctx v = PilVar
  (getSymbol v)
  (Just ctx)
  (HSet.singleton $ SSAVariableRef v (ctx ^. Pil.func) (ctx ^. Pil.ctxIndex))

convertToPilVarAndLog :: MLIL.SSAVariable -> Pil.Converter PilVar
convertToPilVarAndLog v = do
  ctx <- use Pil.ctx
  let pvar =
        PilVar
          (getSymbol v)
          (Just ctx)
          (HSet.singleton $ SSAVariableRef v (ctx ^. Pil.func) (ctx ^. Pil.ctxIndex))
  Pil.usedVars %= HSet.insert pvar
  return pvar

addConstToExpr :: Expression -> Int64 -> Expression
addConstToExpr expr@(Expression size _) n = Expression size addOp
  where
    addOp = Pil.ADD $ Pil.AddOp expr const'
    const' = Expression size . Pil.CONST $ Pil.ConstOp n

-- |Find the first candidate var in the list.
-- The list contains vars cons'd on as they are encountered in processing a path.
-- The var last defined is at the head of the list.
-- If none of the candidate vars appear in the list, return 'Nothing'.
getLastDefined :: [PilVar] -> HashSet PilVar -> Maybe PilVar
getLastDefined orderedVars candidateVars =
  headMay [v | v <- orderedVars, HSet.member v candidateVars]

convertInstrOp :: MLIL.Operation (MLIL.Expression t) -> Pil.Converter [Statement Expression]
convertInstrOp op' = do
  ctx <- use Pil.ctx
  case op' of
    (MLIL.SET_VAR_SSA x) -> do
      pvar <- convertToPilVarAndLog $ x ^. MLIL.dest
      expr <- convertExpr (x ^. MLIL.src)
      Pil.definedVars %= (pvar :)
      return [Def $ DefOp pvar expr]
    -- TODO: Need some way to merge with previous version and include offset
    (MLIL.SET_VAR_SSA_FIELD x) -> do
      pvarDest <- convertToPilVarAndLog destVar
      pvarSrc <- convertToPilVarAndLog srcVar
      chunkExpr <- convertExpr (x ^. MLIL.src)
      let updateVarExpr =
            Expression varSize . Pil.UPDATE_VAR $
              Pil.UpdateVarOp pvarSrc off chunkExpr
      Pil.definedVars %= (pvarDest :)
      return [Def $ DefOp pvarDest updateVarExpr]
      where
        destVar = x ^. MLIL.prev . MLIL.dest
        srcVar = x ^. MLIL.prev . MLIL.src
        off = fromIntegral $ x ^. MLIL.offset
        getVarWidth v = v ^? MLIL.var . BNVar.varType . _Just . BNVar.width
        varSize =
          maybe defaultVarSize fromIntegral $
            getVarWidth destVar <|> getVarWidth srcVar
    (MLIL.SET_VAR_SPLIT_SSA x) -> do
      pvarHigh <- convertToPilVarAndLog $ x ^. MLIL.high
      pvarLow <- convertToPilVarAndLog $ x ^. MLIL.low
      expr@(Expression size _) <- convertExpr (x ^. MLIL.src)
      let halfSize = size `div` 2
          highExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr (fromIntegral halfSize)
          lowExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr 0
      Pil.definedVars %= (pvarHigh :)
      Pil.definedVars %= (pvarLow :)
      return
        [ Def $ DefOp pvarHigh highExpr,
          Def $ DefOp pvarLow lowExpr
        ]
    -- note: prev.src and prev.dest are the same except memory ssa version
    (MLIL.SET_VAR_ALIASED x) -> do
      expr <- convertExpr (x ^. MLIL.src)
      return [Store $ StoreOp addrExpr expr]
      where
        addrExpr = varToStackLocalAddr ctx (x ^. MLIL.prev . MLIL.src . MLIL.var)
    (MLIL.SET_VAR_ALIASED_FIELD x) -> do
      srcExpr <- convertExpr (x ^. MLIL.src)
      return [Store $ StoreOp destAddrExpr srcExpr]
      where
        addrExpr = varToStackLocalAddr ctx (x ^. MLIL.prev . MLIL.src . MLIL.var)
        destAddrExpr =
          mkFieldOffsetExprAddr addrExpr $
            x ^. MLIL.offset
    (MLIL.STORE_SSA x) -> do
      exprSrc <- convertExpr (x ^. MLIL.src)
      exprDest <- convertExpr (x ^. MLIL.dest)
      return [Store $ StoreOp exprDest exprSrc]
    (MLIL.STORE_STRUCT_SSA x) -> do
      destExpr <- (`mkFieldOffsetExprAddr` (x ^. MLIL.offset)) <$> convertExpr (x ^. MLIL.dest)
      srcExpr <- convertExpr (x ^. MLIL.src)
      return [Store $ StoreOp destExpr srcExpr]
    -- TODO: How should we organize handling path-sensitive vs -insensitive conversions of phi nodes?
    --       Consider introducing a PIL phi instruction here and using Pil.Analysis to resolve for paths during simplification
    --       phase.
    (MLIL.VAR_PHI x) -> do
      defVars <- use Pil.definedVars
      let srcVars = HSet.fromList . fmap (convertToPilVar ctx) $ x ^. MLIL.src
          latestVar = getLastDefined defVars srcVars
      case latestVar of
        Nothing -> return []
        Just lVar -> do
          -- By definition, the lVar is already defined and doesn't need to be logged again
          pvar <- convertToPilVarAndLog $ x ^. MLIL.dest
          let vt = fromJust $ x ^. MLIL.dest . MLIL.var . BNVar.varType
          Pil.definedVars %= (pvar :)
          -- TODO: This _should_ be redundant. If a PilVar is in the defined list, it should also be 
          --       in the used vars set. Consider removing and/or finding a better way to enforce
          --       this expectation.
          Pil.usedVars %= HSet.insert lVar
          return
            [ Def . DefOp pvar $
                Expression
                  (typeWidthToOperationSize $ vt ^. BNVar.width)
                  (Pil.VAR $ Pil.VarOp lVar)
            ]
    MLIL.UNIMPL -> return [UnimplInstr "UNIMPL"]
    (MLIL.UNIMPL_MEM x) -> do
      expr <- convertExpr (x ^. MLIL.src)
      return [UnimplMem $ UnimplMemOp expr]
    MLIL.UNDEF -> return [Undef]
    MLIL.NOP -> return [Nop]
    _ -> return []

-- | intercepts VAR_PHI and splits out into separate defs
-- i.e. if x = {a, b, c} then x = a, x = b, x = c
-- used for type checking whole function
convertInstrOpSplitPhi :: MLIL.Operation (MLIL.Expression t) -> Pil.Converter [Statement Expression]
convertInstrOpSplitPhi = \case
  (MLIL.VAR_PHI x) -> do
    vdest <- convertToPilVarAndLog $ x ^. MLIL.dest
    mapM (f vdest) $ x ^. MLIL.src
      where
        f vdest v = do
          pv <- convertToPilVarAndLog v
          -- doesn't seem safe
          let vt = fromJust $ x ^. MLIL.dest . MLIL.var . BNVar.varType
          return . Def . DefOp vdest $
            Expression (typeWidthToOperationSize $ vt ^. BNVar.width)
                       (Pil.VAR $ Pil.VarOp pv)
  x -> convertInstrOp x
    
convertInstr :: MLIL.Instruction t -> Pil.Converter [Stmt]
convertInstr = convertInstrOp . view MLIL.op

convertInstrSplitPhi :: MLIL.Instruction F -> Pil.Converter [Stmt]
convertInstrSplitPhi instr = case Func.toCallInstruction instr of
  Nothing -> convertInstrOpSplitPhi . view MLIL.op $ instr
  Just x -> convertCallInstruction x

convertInstrs :: [MLIL.Instruction t] -> Pil.Converter [Stmt]
convertInstrs = concatMapM convertInstr

convertInstrsSplitPhi :: [MLIL.Instruction F] -> Pil.Converter [Stmt]
convertInstrsSplitPhi = concatMapM convertInstrSplitPhi

getCallDestFunctionName :: Function -> CallDest expr -> IO (Maybe Text)
getCallDestFunctionName ctxfn (Pil.CallConstPtr op) = do
  bv <- BNFunc.getFunctionDataBinaryView ctxfn
  mfn <-
    BNFunc.getFunctionStartingAt bv Nothing
      . Address
      . fromIntegral
      $ op ^. Pil.constant :: IO (Maybe Function)
  return $ view BNFunc.name <$> (mfn :: Maybe Function)
getCallDestFunctionName _ _ = return Nothing

-- |Generate store statements that correspond to any arguments used 
-- for output for a called function.
genCallOutputStores :: [FuncParamInfo] -> [Expression] -> [Stmt]
genCallOutputStores paramInfos params =
  uncurry mkStore <$> zip outArgs exprVars
  where
    maybeOutParam :: FuncParamInfo -> Expression -> Maybe Expression
    maybeOutParam pInfo expr = do
      access <- pInfo ^? Func._FuncParamInfo . Func.access
      if access == Func.Out
        then return expr
        else Nothing
    outArgs :: [Expression]
    outArgs = mapMaybe (uncurry maybeOutParam) . zip paramInfos $ params
    mkStore :: Expression -> PilVar -> Stmt
    mkStore argExpr freeVar =
      Pil.Store $
        Pil.StoreOp
          argExpr
          (Expression (argExpr ^. Pil.size) (Pil.VAR (Pil.VarOp freeVar)))
    -- TODO: Need to actually find the used defined vars and exclude them
    exprVars :: [PilVar]
    exprVars = (\x -> PilVar x Nothing HSet.empty) <$> symbolGenerator (getAllSyms [])

-- TODO: How to deal with BN functions the report multiple return values? Multi-variable def?
convertCallInstruction :: CallInstruction -> Pil.Converter [Stmt]
convertCallInstruction c = do
  ctx <- use Pil.ctx
  -- TODO: Better handling of possible Nothing value
  target <- Pil.getCallDest <$> convertExpr (fromJust (c ^. Func.dest))
  params <- sequence [convertExpr p | p <- c ^. Func.params]
  mname <- liftIO $ getCallDestFunctionName (ctx ^. Pil.func) target
  funcDefs <- use Pil.knownFuncs
  let outStores = getOutStores mname funcDefs params
  
  -- The size of a function call is always 0 according to BN. Need to look at result var types to get
  -- actual size. This is done below when handling the case for a return value.
  let callExpr = Expression (c ^. Func.size) . Pil.CALL $ Pil.CallOp target mname params
  case c ^. Func.outputDest of
    -- TODO: Try to merge Nothing and an empty list, consider changing outputDest to NonEmpty
    [] -> return $ Call (CallOp target mname params) : outStores
    (dest : _) -> do
      dest' <- convertToPilVarAndLog dest
          -- TODO: Make this safe. We currently bail if there's no type provided.
          --       Change MediumLevelILInsutrction._size to be Maybe OperationSize
      let resultSize = dest ^?! MLIL.var . BNVar.varType . _Just . BNVar.width
          opSize = typeWidthToOperationSize resultSize
      return $ Def (DefOp dest' (callExpr & Pil.size .~ opSize)) : outStores
  where 
    getOutStores :: Maybe Text -> HashMap Text FuncInfo -> [Expression] -> [Stmt]
    getOutStores mname funcInfos args =
      case mname >>= (`HM.lookup` funcInfos) of
        Nothing -> []
        (Just funcInfo) ->
          genCallOutputStores (funcInfo ^. Func.params) args

-- | Gets all PIL statements contained in a function.
-- the "Int" is the original MLIL_SSA InstructionIndex
fromFunction' :: Function -> Pil.Converter [(Int, Stmt)]
fromFunction' func = do
  mlilFunc <- liftIO $ BNFunc.getMLILSSAFunction func
  mlilInstrs <- liftIO $ MLIL.fromFunction mlilFunc
  concatMapM f $ zip [0..] mlilInstrs
  where
    f (mlilIndex, mlilInstr) =
      fmap (mlilIndex,) <$> convertInstrSplitPhi mlilInstr
  
fromFunction :: Function -> IO [(Int, Stmt)]
fromFunction func = fmap fst . flip Pil.runConverter st . fromFunction' $ func
  where
    ctx' = Pil.Ctx func 0
    st = Pil.ConverterState
      { _path = AlgaPath.empty
      , _ctxMaxIdx = 0
      , _ctxStack = ctx' :| []
      , _ctx = ctx'
      , _definedVars = []
      , _usedVars = HSet.empty
      , _knownFuncs = knownFuncDefs
      }

isDirectCall :: CallOp Expression -> Bool
isDirectCall c = case c ^. Pil.dest of
  (Pil.CallConstPtr _) -> True
  _ -> False

-- TODO: Move to external file/module of definitions
knownFuncDefs :: HashMap Text FuncInfo
knownFuncDefs =
  HM.fromList
    [ ( "asprintf",
        mkFuncInfo
          "asprintf"
          "asprintf"
          [ FuncParamInfo $ ParamInfo "ret" Out,
            FuncParamInfo $ ParamInfo "fmt" In,
            FuncVarArgInfo
          ]
          (ResultInfo "result")
      ),
      ( "cgc_sprintf",
        mkFuncInfo
          "cgc_sprintf"
          "cgc_sprintf"
          [ FuncParamInfo $ ParamInfo "ret" Out,
            FuncParamInfo $ ParamInfo "fmt" In,
            FuncVarArgInfo
          ]
          (ResultInfo "result")
      )
    ]
