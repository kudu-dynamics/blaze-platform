{-# LANGUAGE TemplateHaskell #-}

module Blaze.Import.Source.BinaryNinja.Pil where

import Blaze.Prelude hiding (Symbol)

import qualified Blaze.Types.Function as Func
import qualified Data.BinaryAnalysis as BA
import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as MLIL
import Blaze.Import.Pil
import Blaze.Types.Function (FuncInfo)
import Blaze.Types.Pil
  ( CallDest,
    CallOp (CallOp),
    Ctx(Ctx),
    CtxIndex(CtxIndex),
    DefOp (DefOp),
    DefPhiOp (DefPhiOp),
    Expression (Expression),
    PilVar (PilVar),
    Statement
      ( Call,
        Def,
        DefPhi,
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

import Blaze.Types.Path.AlgaPath (AlgaPath)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil as Pil
import Blaze.Import.Source.BinaryNinja.Types
import qualified Blaze.Types.CallGraph as CG
import Blaze.Import.Source.BinaryNinja.CallGraph (getCallDestAddr)
import qualified Data.HashMap.Strict as HMap
import qualified Binja.Variable as BNVar
import Binja.Variable (Variable)
import Blaze.Types.Function (CallInstruction)
import Blaze.Pil.Analysis (getAllSyms)
import Blaze.Types.Pil.Analysis (symbolGenerator)
import qualified Data.HashSet as HSet
import qualified Data.Text as Text
import Blaze.Import.Source.BinaryNinja.Types ( SSAVariableRef(SSAVariableRef)
                                             , F
                                             , ConverterState
                                             )
import Blaze.Pil (getLastDefined)
import Blaze.Util.GenericConv (GConv, gconv)

-- $(makeFieldsNoPrefix ''ConverterState)

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: StateT ConverterState IO a}
  deriving (Functor) 
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO)

createStartCtx :: CG.Function -> Ctx
createStartCtx func = Ctx func 0

-- TODO: Consider moving Blaze.Pil.knownFuncDefs to this module and use that instead of
--       accepting a map from the user.
mkConverterState :: HashMap Text FuncInfo -> AddressWidth -> CG.Function -> AlgaPath -> ConverterState
mkConverterState knownFuncDefs addrSize_ f p =
  ConverterState
    p
    (startCtx ^. #ctxIndex)
    (startCtx :| [])
    startCtx
    []
    HSet.empty
    HMap.empty
    knownFuncDefs
    addrSize_
    (BA.bits addrSize_)
 where
  startCtx :: Ctx
  startCtx = createStartCtx f

runConverter :: Converter a -> ConverterState -> IO (a, ConverterState)
runConverter m s = flip runStateT s $ _runConverter m

convert :: ConverterState -> Converter a -> IO a
convert s m = fst <$> runConverter m s

addessWidthToOperationSize :: AddressWidth -> Pil.OperationSize
addessWidthToOperationSize (AddressWidth bits) =
  Pil.OperationSize . BA.toBytes $ bits

-- TODO: Should we throw an error if (v ^. BNVar.sourceType /= StackVariableSourceType)?
varToStackLocalAddr :: Variable -> Converter Expression
varToStackLocalAddr v = do
  ctx <- use #ctx
  addrSize <- addessWidthToOperationSize <$> use #addrSize
  return $ Expression addrSize
    . Pil.STACK_LOCAL_ADDR
    . Pil.StackLocalAddrOp
    . Pil.StackOffset ctx
    . fromIntegral
    $ v ^. BNVar.storage

toPilOpSize :: MLIL.OperationSize -> Pil.OperationSize
toPilOpSize = coerce

typeWidthToOperationSize :: BNVar.TypeWidth -> Pil.OperationSize
typeWidthToOperationSize (BNVar.TypeWidth n) = Pil.OperationSize n

convertExpr :: MLIL.Expression t -> Converter Expression
convertExpr expr = do
  ctx <- use #ctx
  case expr ^. MLIL.op of
    (MLIL.ADC x) -> mkExpr . Pil.ADC <$> f x
    (MLIL.ADD x) -> mkExpr . Pil.ADD <$> f x
    (MLIL.ADDRESS_OF x) -> varToStackLocalAddr (x ^. MLIL.src)
    (MLIL.ADDRESS_OF_FIELD x) -> do
      stackAddr <- varToStackLocalAddr $ x ^. MLIL.src
      return $ mkExpr . Pil.FIELD_ADDR
        . Pil.FieldAddrOp stackAddr
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
    (MLIL.FLOAT_CONST x) -> mkExpr . Pil.CONST_FLOAT <$> f x
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
        Pil.mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    (MLIL.LOAD_STRUCT_SSA x) -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $
        Pil.mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    (MLIL.LOW_PART x) -> mkExpr . Pil.LOW_PART <$> f x
    (MLIL.LSL x) -> mkExpr . Pil.LSL <$> f x
    (MLIL.LSR x) -> mkExpr . Pil.LSR <$> f x
    (MLIL.MODS x) -> mkExpr . Pil.MODS <$> f x
    (MLIL.MODS_DP x) -> mkExpr . Pil.MODS_DP <$> f x
    (MLIL.MODU x) -> mkExpr . Pil.MODU <$> f x
    (MLIL.MODU_DP x) -> mkExpr . Pil.MODU_DP <$> f x
    (MLIL.MUL x) -> mkExpr . Pil.MUL <$> f x

    -- NOTE: binja gets these return sizes wrong
    (MLIL.MULS_DP x) -> Expression (toPilOpSize $ 2 * expr ^. MLIL.size)
                        . Pil.MULS_DP <$> f x
    (MLIL.MULU_DP x) -> Expression (toPilOpSize $ 2 * expr ^. MLIL.size)
                        . Pil.MULU_DP <$> f x

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
    (MLIL.VAR_ALIASED x) -> mkExpr . Pil.LOAD . Pil.LoadOp
      <$> varToStackLocalAddr (x ^. MLIL.src . MLIL.var)
    (MLIL.VAR_ALIASED_FIELD x) -> do
      addrExpr <- varToStackLocalAddr (x ^. MLIL.src . MLIL.var)
      return $ mkExpr . Pil.LOAD . Pil.LoadOp
        . Pil.mkFieldOffsetExprAddr addrExpr
        $ x ^. MLIL.offset
    (MLIL.VAR_SPLIT_SSA x) -> do
      highVar <- convertToPilVarAndLog $ x ^. MLIL.high
      lowVar <- convertToPilVarAndLog $ x ^. MLIL.low
      -- return $ mkExpr . Pil.VAR_JOIN $ Pil.VarJoinOp highVar lowVar
      -- NOTE: Binja gets the return size wrong. use above if they fix it
      return $ Expression (toPilOpSize $ 2 * expr ^. MLIL.size)
        . Pil.VAR_JOIN $ Pil.VarJoinOp highVar lowVar
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
      -- f :: Traversable m => m (MLIL.Expression t) -> Converter (m Expression)
      f :: forall m m' t.
           ( Traversable m
           , Generic (m Expression)
           , Generic (m' Expression)
           , GConv (Rep (m Expression)) (Rep (m' Expression))
           )
        => m (MLIL.Expression t)
        -> Converter (m' Expression)
      f = fmap gconv . traverse convertExpr
      mkExpr :: Pil.ExprOp Expression -> Expression
      mkExpr = Expression (toPilOpSize $ expr ^. MLIL.size)

getSymbol :: MLIL.SSAVariable -> Symbol
getSymbol v = (v ^. MLIL.var . BNVar.name) <> "#" <> show (v ^. MLIL.version)

convertToPilVar :: MLIL.SSAVariable -> Converter PilVar
convertToPilVar v = do
  ctx <- use #ctx
  bnfunc <- undefined $ ctx ^. #func
  let sourceVar = SSAVariableRef v bnfunc (ctx ^. #ctxIndex)
      pilVar = PilVar (getSymbol v) (Just ctx)
  #sourceVars %= HMap.insert pilVar sourceVar
  return pilVar

convertToPilVarAndLog :: MLIL.SSAVariable -> Converter PilVar
convertToPilVarAndLog v = do
  pvar <- convertToPilVar v
  #usedVars %= HSet.insert pvar
  return pvar

convertInstrOp :: MLIL.Operation (MLIL.Expression t) -> Converter [Statement Expression]
convertInstrOp op' = do
  ctx <- use #ctx
  defVarSize <- use #defaultVarSize
  case op' of
    (MLIL.SET_VAR_SSA x) -> do
      pvar <- convertToPilVarAndLog $ x ^. MLIL.dest
      expr <- convertExpr (x ^. MLIL.src)
      #definedVars %= (pvar :)
      return [Def $ DefOp pvar expr]
    -- TODO: Need some way to merge with previous version and include offset
    (MLIL.SET_VAR_SSA_FIELD x) -> do
      pvarDest <- convertToPilVarAndLog destVar
      pvarSrc <- convertToPilVarAndLog srcVar
      chunkExpr <- convertExpr (x ^. MLIL.src)
      let updateVarExpr =
            Expression varSize . Pil.UPDATE_VAR $
              Pil.UpdateVarOp pvarSrc off chunkExpr
      #definedVars %= (pvarDest :)
      return [Def $ DefOp pvarDest updateVarExpr]
      where
        destVar = x ^. MLIL.prev . MLIL.dest
        srcVar = x ^. MLIL.prev . MLIL.src
        off = fromIntegral $ x ^. MLIL.offset
        getVarWidth v = v ^? MLIL.var . BNVar.varType . _Just . BNVar.width
        varSize =
          maybe (Pil.OperationSize . toBytes $ defVarSize) fromIntegral $
            getVarWidth destVar <|> getVarWidth srcVar
    (MLIL.SET_VAR_SPLIT_SSA x) -> do
      pvarHigh <- convertToPilVarAndLog $ x ^. MLIL.high
      pvarLow <- convertToPilVarAndLog $ x ^. MLIL.low
      expr@(Expression size _) <- convertExpr (x ^. MLIL.src)
      let halfSize = size `div` 2
          highExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr (fromIntegral halfSize)
          lowExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr 0
      #definedVars %= (pvarHigh :)
      #definedVars %= (pvarLow :)
      return
        [ Def $ DefOp pvarHigh highExpr,
          Def $ DefOp pvarLow lowExpr
        ]
    -- note: prev.src and prev.dest are the same except memory ssa version
    (MLIL.SET_VAR_ALIASED x) -> do
      expr <- convertExpr (x ^. MLIL.src)
      addrExpr <- varToStackLocalAddr (x ^. MLIL.prev . MLIL.src . MLIL.var)
      return [Store $ StoreOp addrExpr expr]
    (MLIL.SET_VAR_ALIASED_FIELD x) -> do
      srcExpr <- convertExpr (x ^. MLIL.src)
      addrExpr <- varToStackLocalAddr (x ^. MLIL.prev . MLIL.src . MLIL.var)
      let destAddrExpr = Pil.mkFieldOffsetExprAddr addrExpr
                         $ x ^. MLIL.offset

      return [Store $ StoreOp destAddrExpr srcExpr]
    (MLIL.STORE_SSA x) -> do
      exprSrc <- convertExpr (x ^. MLIL.src)
      exprDest <- convertExpr (x ^. MLIL.dest)
      return [Store $ StoreOp exprDest exprSrc]
    (MLIL.STORE_STRUCT_SSA x) -> do
      destExpr <- (`Pil.mkFieldOffsetExprAddr` (x ^. MLIL.offset)) <$> convertExpr (x ^. MLIL.dest)
      srcExpr <- convertExpr (x ^. MLIL.src)
      return [Store $ StoreOp destExpr srcExpr]
    -- TODO: How should we organize handling path-sensitive vs -insensitive conversions of phi nodes?
    --       Consider introducing a PIL phi instruction here and using Pil.Analysis to resolve for paths during simplification
    --       phase.
    (MLIL.VAR_PHI x) -> do
      defVars <- use #definedVars
      -- Not using all the phi vars, so don't need to log them all
      srcVars <- HSet.fromList <$> traverse convertToPilVar (x ^. MLIL.src)
      let latestVar = getLastDefined defVars srcVars
      case latestVar of
        Nothing -> return []
        Just lVar -> do
          -- By definition, the lVar is already defined and doesn't need to be logged again
          pvar <- convertToPilVarAndLog $ x ^. MLIL.dest
          let vt = fromJust $ x ^. MLIL.dest . MLIL.var . BNVar.varType
          #definedVars %= (pvar :)
          -- TODO: This _should_ be redundant. If a PilVar is in the defined list, it should also be 
          --       in the used vars set. Consider removing and/or finding a better way to enforce
          --       this expectation.
          #usedVars %= HSet.insert lVar
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

-- | intercepts VAR_PHI and converts it to PIL DefPhi
-- todo: rename
convertInstrOpSplitPhi :: MLIL.Operation (MLIL.Expression t) -> Converter [Statement Expression]
convertInstrOpSplitPhi = \case
  (MLIL.VAR_PHI x) -> do
    vdest <- convertToPilVarAndLog $ x ^. MLIL.dest
    srcs <- mapM convertToPilVarAndLog $ x ^. MLIL.src
    return [DefPhi $ DefPhiOp vdest srcs]
  x -> convertInstrOp x
    
convertInstr :: MLIL.Instruction t -> Converter [Stmt]
convertInstr = convertInstrOp . view MLIL.op

convertInstrSplitPhi :: MLIL.Instruction F -> Converter [Stmt]
convertInstrSplitPhi instr = case Func.toCallInstruction instr of
  Nothing -> convertInstrOpSplitPhi . view MLIL.op $ instr
  Just x -> convertCallInstruction x

convertInstrs :: [MLIL.Instruction t] -> Converter [Stmt]
convertInstrs = concatMapM convertInstr

convertInstrsSplitPhi :: [MLIL.Instruction F] -> Converter [Stmt]
convertInstrsSplitPhi = concatMapM convertInstrSplitPhi

getCallDestFunctionName :: BNFunc.Function -> CallDest expr -> IO (Maybe Text)
getCallDestFunctionName ctxfn (Pil.CallConstPtr op) = do
  bv <- BNFunc.getFunctionDataBinaryView ctxfn
  mfn <-
    BNFunc.getFunctionStartingAt bv Nothing
      . Address
      . fromIntegral
      $ op ^. #constant :: IO (Maybe BNFunc.Function)
  return $ view BNFunc.name <$> (mfn :: Maybe BNFunc.Function)
getCallDestFunctionName _ _ = return Nothing

-- TODO: How to deal with BN functions the report multiple return values? Multi-variable def?
convertCallInstruction :: CallInstruction -> Converter [Stmt]
convertCallInstruction c = do
  -- TODO: Better handling of possible Nothing value

  target <- Pil.mkCallDest <$> convertExpr (fromJust (c ^. #dest))
  params <- sequence [convertExpr p | p <- c ^. #params]
  
  ctx <- use #ctx
  bnfunc <- undefined $ ctx ^. #func
  mname <- liftIO $ getCallDestFunctionName bnfunc target

  -- getFuncName_ <- use #getFuncName
  -- -- TODO: Is there a way to write this without case matching?
  -- mname <- liftIO $ case getCallDestAddr c of
  --   -- Just destAddr -> (view CG.name <$>) <$> _ destAddr
  --   Just destAddr -> getFuncName_ destAddr
  --   Nothing -> return Nothing

  funcDefs <- use #knownFuncs
  let outStores = getOutStores mname funcDefs params
  -- The size of a function call is always 0 according to BN. Need to look at result var types to get
  -- actual size. This is done below when handling the case for a return value.
  let callExpr = Expression (toPilOpSize $ c ^. #size) . Pil.CALL
                 $ Pil.CallOp target mname params
  case c ^. #outputDest of
    -- TODO: Try to merge Nothing and an empty list, consider changing outputDest to NonEmpty
    [] -> return $ Call (CallOp target mname params) : outStores
    (dest : _) -> do
      dest' <- convertToPilVarAndLog dest
          -- TODO: Make this safe. We currently bail if there's no type provided.
          --       Change MediumLevelILInsutrction._size to be Maybe OperationSize
      let resultSize = dest ^?! MLIL.var . BNVar.varType . _Just . BNVar.width
          opSize = typeWidthToOperationSize resultSize
      return $ Def (DefOp dest' (callExpr & #size .~ opSize)) : outStores
  where 
    getOutStores :: Maybe Text -> HashMap Text FuncInfo -> [Expression] -> [Stmt]
    getOutStores mname funcInfos args =
      case mname >>= (`HMap.lookup` funcInfos) of
        Nothing -> []
        (Just funcInfo) ->
          Pil.genCallOutputStores (funcInfo ^. #params) args

-- | Gets all PIL statements contained in a function.
-- the "Int" is the original MLIL_SSA InstructionIndex
convertFunction :: Function -> Converter [(Int, Stmt)]
convertFunction func = do
  mlilFunc <- liftIO $ BNFunc.getMLILSSAFunction func
  mlilInstrs <- liftIO $ MLIL.fromFunction mlilFunc
  concatMapM f $ zip [0..] mlilInstrs
  where
    f (mlilIndex, mlilInstr) =
      fmap (mlilIndex,) <$> convertInstrSplitPhi mlilInstr

