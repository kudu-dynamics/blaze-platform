module Blaze.Import.Source.BinaryNinja.Pil where

import Blaze.Prelude hiding (Symbol)

import qualified Binja.BasicBlock as BB
import Binja.Core (BNBinaryView)
import qualified Binja.Core as BN
import Binja.Function (Function)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as MLIL
import qualified Binja.View as BNView
import qualified Blaze.Pil as Pil
import qualified Data.BinaryAnalysis as BA
import qualified Prelude as P
import Blaze.Types.Pil
    ( BranchCondOp(BranchCondOp),
      CallOp(CallOp),
      TailCallOp(TailCallOp),
      Ctx(Ctx),
      DefOp(DefOp),
      DefPhiOp(DefPhiOp),
      Expression(Expression),
      MappedStmt,
      MappedStatement(MappedStatement),
      PilVar(PilVar),
      RetOp(RetOp),
      Statement(BranchCond, Call, Def, DefPhi, Nop, Ret, Store, Undef,
                UnimplInstr, UnimplMem, NoRet, TailCall, Jump, JumpTo),
      Stmt,
      StoreOp(StoreOp),
      Symbol,
      UnimplMemOp(UnimplMemOp),
      Ctx,
      CtxId,
      Expression,
      PilVar, JumpOp (JumpOp), JumpToOp (JumpToOp) )

import Binja.Variable (Variable)
import qualified Binja.Variable as BNVar
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as BNCG
import Blaze.Import.Source.BinaryNinja.Types
import qualified Blaze.Types.Function as Func
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as Text

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: StateT ConverterState IO a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO)

-- TODO: Conversions sometimes occur without need for
--       a path. Identify and refactor appropriately.
data ConverterState = ConverterState
  { -- | The current context should be on the top of the stack.
    -- I.e., the stack should never be empty.
    ctxStack :: NonEmpty Ctx
    -- | The current context
  , ctx :: Ctx
    -- | Currently known defined PilVars for all contexts.
    -- This is assumed to be ordered by most recently defined first.
    -- TODO: Can we safeguard for overwriting/colliding with already used PilVars?
    --       This could happen for synthesized PilVars with a Nothing context.
  , definedVars :: [PilVar]
    -- | All PilVars referenced for all contexts.
    -- This differs from _definedVars, as order is not preserved and referenced,
    -- but undefined, PilVars are included
  , usedVars :: HashSet PilVar
    -- TODO: This is fixed to BN MLIL SSA variables here, but will be generalized
    --       when moving to a PilImporter instance.
    -- TODO: Does this need to be a set or just a single variable?

    -- | A mapping of PilVars to the a variable from the import source.
  , sourceVars :: HashMap PilVar SSAVariableRef
    -- | Map of known functions with parameter access information
  , knownFuncs :: HashMap Text Func.FuncInfo
    -- | Address size based on target platform
  , addrSize :: AddressWidth
    -- | Default variable size, usually based on platform default
  , defaultVarSize :: Bits
  , binaryView :: BN.BNBinaryView
  }
  deriving (Eq, Show, Generic)

-- TODO: Consider moving Blaze.Pil.knownFuncDefs to this module and use that instead of
--       accepting a map from the user.
mkConverterState :: BNBinaryView -> CtxId -> HashMap Text Func.FuncInfo -> AddressWidth -> Func.Function -> ConverterState
mkConverterState bv startCtxId knownFuncDefs_ addrSize_ f =
  ConverterState
    (startCtx :| [])
    startCtx
    []
    HSet.empty
    HMap.empty
    knownFuncDefs_
    addrSize_
    (BA.bits addrSize_)
    bv
 where
  startCtx :: Ctx
  startCtx = Ctx f startCtxId

runConverter :: Converter a -> ConverterState -> IO (a, ConverterState)
runConverter m = runStateT $ _runConverter m

convert :: ConverterState -> Converter a -> IO a
convert s m = fst <$> runConverter m s

addessWidthToOperationSize :: AddressWidth -> Pil.Size Expression
addessWidthToOperationSize (AddressWidth bits) =
  Pil.Size . BA.toBytes $ bits

-- TODO: Should we throw an error if (v ^. BNVar.sourceType /= StackVariableSourceType)?
varToStackLocalAddr :: Variable -> Converter Expression
varToStackLocalAddr v = do
  ctx' <- use #ctx
  addrSize' <- addessWidthToOperationSize <$> use #addrSize
  return $ Expression addrSize'
    . Pil.STACK_LOCAL_ADDR
    . Pil.StackLocalAddrOp
    . Pil.StackOffset ctx'
    . fromIntegral
    $ v ^. BNVar.storage

toPilOpSize :: MLIL.OperationSize -> Pil.Size Expression
toPilOpSize = coerce

typeWidthToSize :: BNVar.TypeWidth -> Pil.Size a
typeWidthToSize (BNVar.TypeWidth n) = Pil.Size n

convertExpr :: MLIL.Expression t -> Converter Expression
convertExpr expr = do
  case expr ^. MLIL.op of
    MLIL.ADC (MLIL.AdcOp in0 in1 in2) -> triOp Pil.ADC Pil.AdcOp in0 in1 in2
    MLIL.ADD (MLIL.AddOp in0 in1) -> binOp Pil.ADD Pil.AddOp in0 in1
    MLIL.ADDRESS_OF x -> varToStackLocalAddr (x ^. MLIL.src)
    MLIL.ADDRESS_OF_FIELD x -> do
      stackAddr <- varToStackLocalAddr $ x ^. MLIL.src
      return $ mkExpr . Pil.FIELD_ADDR
        . Pil.FieldAddrOp stackAddr
        . fromIntegral
        $ x ^. MLIL.offset
    MLIL.ADD_OVERFLOW (MLIL.AddOverflowOp in0 in1) -> binOp Pil.ADD_WILL_OVERFLOW Pil.AddWillOverflowOp in0 in1
    MLIL.AND (MLIL.AndOp in0 in1) -> binOp Pil.AND Pil.AndOp in0 in1
    MLIL.ASR (MLIL.AsrOp in0 in1) -> binOp Pil.ASR Pil.AsrOp in0 in1
    MLIL.BOOL_TO_INT (MLIL.BoolToIntOp in0) -> unOp Pil.BOOL_TO_INT Pil.BoolToIntOp in0
    MLIL.CEIL (MLIL.CeilOp in0) -> unOp Pil.CEIL Pil.CeilOp in0
    MLIL.CMP_E (MLIL.CmpEOp in0 in1) -> binOp Pil.CMP_E Pil.CmpEOp in0 in1
    MLIL.CMP_NE (MLIL.CmpNeOp in0 in1) -> binOp Pil.CMP_NE Pil.CmpNeOp in0 in1
    MLIL.CMP_SGE (MLIL.CmpSgeOp in0 in1) -> binOp Pil.CMP_SGE Pil.CmpSgeOp in0 in1
    MLIL.CMP_SGT (MLIL.CmpSgtOp in0 in1) -> binOp Pil.CMP_SGT Pil.CmpSgtOp in0 in1
    MLIL.CMP_SLE (MLIL.CmpSleOp in0 in1) -> binOp Pil.CMP_SLE Pil.CmpSleOp in0 in1
    MLIL.CMP_SLT (MLIL.CmpSltOp in0 in1) -> binOp Pil.CMP_SLT Pil.CmpSltOp in0 in1
    MLIL.CMP_UGE (MLIL.CmpUgeOp in0 in1) -> binOp Pil.CMP_UGE Pil.CmpUgeOp in0 in1
    MLIL.CMP_UGT (MLIL.CmpUgtOp in0 in1) -> binOp Pil.CMP_UGT Pil.CmpUgtOp in0 in1
    MLIL.CMP_ULE (MLIL.CmpUleOp in0 in1) -> binOp Pil.CMP_ULE Pil.CmpUleOp in0 in1
    MLIL.CMP_ULT (MLIL.CmpUltOp in0 in1) -> binOp Pil.CMP_ULT Pil.CmpUltOp in0 in1
    MLIL.CONST (MLIL.ConstOp x) -> mkConstStrOrFuncPtr addr def
      where
        addr = fromIntegral x
        def = pure . mkExpr . Pil.CONST $ Pil.ConstOp x
    MLIL.CONST_DATA (MLIL.ConstDataOp x) -> mkConstStrOrFuncPtr addr def
      where
        addr = fromIntegral x
        def = pure . mkExpr . Pil.CONST $ Pil.ConstOp x
    MLIL.CONST_PTR (MLIL.ConstPtrOp x) -> mkConstStrOrFuncPtr addr def
      where
        addr = fromIntegral x
        def = pure . mkExpr . Pil.CONST_PTR $ Pil.ConstPtrOp x
    MLIL.DIVS (MLIL.DivsOp in0 in1) -> binOp Pil.DIVS Pil.DivsOp in0 in1
    MLIL.DIVS_DP (MLIL.DivsDpOp in0 in1) -> binOp Pil.DIVS_DP Pil.DivsDpOp in0 in1
    MLIL.DIVU (MLIL.DivuOp in0 in1) -> binOp Pil.DIVU Pil.DivuOp in0 in1
    MLIL.DIVU_DP (MLIL.DivuDpOp in0 in1) -> binOp Pil.DIVU_DP Pil.DivuDpOp in0 in1
    MLIL.EXTERN_PTR x -> mkExpr . Pil.ExternPtr . Pil.ExternPtrOp addr off
      <$> getSymbolAtAddress addr
      where
        addr = fromIntegral $ x ^. MLIL.constant
        off = fromIntegral $ x ^. MLIL.offset
    MLIL.FABS (MLIL.FabsOp in0) -> unOp Pil.FABS Pil.FabsOp in0
    MLIL.FADD (MLIL.FaddOp in0 in1) -> binOp Pil.FADD Pil.FaddOp in0 in1
    MLIL.FCMP_E (MLIL.FcmpEOp in0 in1) -> binOp Pil.FCMP_E Pil.FcmpEOp in0 in1
    MLIL.FCMP_GE (MLIL.FcmpGeOp in0 in1) -> binOp Pil.FCMP_GE Pil.FcmpGeOp in0 in1
    MLIL.FCMP_GT (MLIL.FcmpGtOp in0 in1) -> binOp Pil.FCMP_GT Pil.FcmpGtOp in0 in1
    MLIL.FCMP_LE (MLIL.FcmpLeOp in0 in1) -> binOp Pil.FCMP_LE Pil.FcmpLeOp in0 in1
    MLIL.FCMP_LT (MLIL.FcmpLtOp in0 in1) -> binOp Pil.FCMP_LT Pil.FcmpLtOp in0 in1
    MLIL.FCMP_NE (MLIL.FcmpNeOp in0 in1) -> binOp Pil.FCMP_NE Pil.FcmpNeOp in0 in1
    MLIL.FCMP_O (MLIL.FcmpOOp in0 in1) -> binOp Pil.FCMP_O Pil.FcmpOOp in0 in1
    MLIL.FCMP_UO (MLIL.FcmpUoOp in0 in1) -> binOp Pil.FCMP_UO Pil.FcmpUoOp in0 in1
    MLIL.FDIV (MLIL.FdivOp in0 in1) -> binOp Pil.FDIV Pil.FdivOp in0 in1
    MLIL.FLOAT_CONST (MLIL.FloatConstOp in0) -> pure . mkExpr . Pil.CONST_FLOAT $ Pil.ConstFloatOp in0
    MLIL.FLOAT_CONV (MLIL.FloatConvOp in0) -> unOp Pil.FLOAT_CONV Pil.FloatConvOp in0
    MLIL.FLOAT_TO_INT (MLIL.FloatToIntOp in0) -> unOp Pil.FLOAT_TO_INT Pil.FloatToIntOp in0
    MLIL.FLOOR (MLIL.FloorOp in0) -> unOp Pil.FLOOR Pil.FloorOp in0
    MLIL.FMUL (MLIL.FmulOp in0 in1) -> binOp Pil.FMUL Pil.FmulOp in0 in1
    MLIL.FNEG (MLIL.FnegOp in0) -> unOp Pil.FNEG Pil.FnegOp in0
    MLIL.FSQRT (MLIL.FsqrtOp in0) -> unOp Pil.FSQRT Pil.FsqrtOp in0
    MLIL.FSUB (MLIL.FsubOp in0 in1) -> binOp Pil.FSUB Pil.FsubOp in0 in1
    MLIL.FTRUNC (MLIL.FtruncOp in0) -> unOp Pil.FTRUNC Pil.FtruncOp in0
    MLIL.IMPORT (MLIL.ImportOp in0) -> pure . mkExpr . Pil.IMPORT $ Pil.ImportOp in0
    MLIL.INT_TO_FLOAT (MLIL.IntToFloatOp in0) -> unOp Pil.INT_TO_FLOAT Pil.IntToFloatOp in0
    MLIL.LOAD (MLIL.LoadOp in0) -> unOp Pil.LOAD Pil.LoadOp in0
    MLIL.LOAD_SSA x -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $ srcExpr
    MLIL.LOAD_STRUCT x -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $
        Pil.mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    MLIL.LOAD_STRUCT_SSA x -> do
      srcExpr <- convertExpr $ x ^. MLIL.src
      return $ mkExpr . Pil.LOAD . Pil.LoadOp $
        Pil.mkFieldOffsetExprAddr srcExpr (x ^. MLIL.offset)
    MLIL.LOW_PART (MLIL.LowPartOp in0) -> unOp Pil.LOW_PART Pil.LowPartOp in0
    MLIL.LSL (MLIL.LslOp in0 in1) -> binOp Pil.LSL Pil.LslOp in0 in1
    MLIL.LSR (MLIL.LsrOp in0 in1) -> binOp Pil.LSR Pil.LsrOp in0 in1
    MLIL.MODS (MLIL.ModsOp in0 in1) -> binOp Pil.MODS Pil.ModsOp in0 in1
    MLIL.MODS_DP (MLIL.ModsDpOp in0 in1) -> binOp Pil.MODS_DP Pil.ModsDpOp in0 in1
    MLIL.MODU (MLIL.ModuOp in0 in1) -> binOp Pil.MODU Pil.ModuOp in0 in1
    MLIL.MODU_DP (MLIL.ModuDpOp in0 in1) -> binOp Pil.MODU_DP Pil.ModuDpOp in0 in1
    MLIL.MUL (MLIL.MulOp in0 in1) -> binOp Pil.MUL Pil.MulOp in0 in1

    -- -- NOTE: binja gets these return sizes wrong
    MLIL.MULS_DP (MLIL.MulsDpOp in0 in1) ->
      Expression (toPilOpSize $ 2 * expr ^. MLIL.size) . Pil.MULS_DP
      <$> (Pil.MulsDpOp <$> convertExpr in0 <*> convertExpr in1)

    MLIL.MULU_DP (MLIL.MuluDpOp in0 in1) ->
      Expression (toPilOpSize $ 2 * expr ^. MLIL.size) . Pil.MULU_DP
      <$> (Pil.MuluDpOp <$> convertExpr in0 <*> convertExpr in1)

    MLIL.NEG (MLIL.NegOp in0) -> unOp Pil.NEG Pil.NegOp in0
    MLIL.NOT (MLIL.NotOp in0) -> unOp Pil.NOT Pil.NotOp in0
    MLIL.OR (MLIL.OrOp in0 in1) -> binOp Pil.OR Pil.OrOp in0 in1
    MLIL.RLC (MLIL.RlcOp in0 in1 in2) -> triOp Pil.RLC Pil.RlcOp in0 in1 in2
    MLIL.ROL (MLIL.RolOp in0 in1) -> binOp Pil.ROL Pil.RolOp in0 in1
    MLIL.ROR (MLIL.RorOp in0 in1) -> binOp Pil.ROR Pil.RorOp in0 in1
    MLIL.ROUND_TO_INT (MLIL.RoundToIntOp in0) -> unOp Pil.ROUND_TO_INT Pil.RoundToIntOp in0
    MLIL.RRC (MLIL.RrcOp in0 in1 in2) -> triOp Pil.RRC Pil.RrcOp in0 in1 in2
    MLIL.SBB (MLIL.SbbOp in0 in1 in2) -> triOp Pil.SBB Pil.SbbOp in0 in1 in2
    MLIL.SUB (MLIL.SubOp in0 in1) -> binOp Pil.SUB Pil.SubOp in0 in1
    MLIL.SX (MLIL.SxOp in0) -> unOp Pil.SX Pil.SxOp in0
    MLIL.TEST_BIT (MLIL.TestBitOp in0 in1) -> binOp Pil.TEST_BIT Pil.TestBitOp in0 in1
    MLIL.UNIMPL -> return $ mkExpr $ Pil.UNIMPL "UNIMPL"
    MLIL.UNIMPL_MEM _ -> unimpl
    MLIL.VAR_ALIASED x -> mkExpr . Pil.LOAD . Pil.LoadOp
      <$> varToStackLocalAddr (x ^. MLIL.src . MLIL.var)
    MLIL.VAR_ALIASED_FIELD x -> do
      addrExpr <- varToStackLocalAddr (x ^. MLIL.src . MLIL.var)
      return $ mkExpr . Pil.LOAD . Pil.LoadOp
        . Pil.mkFieldOffsetExprAddr addrExpr
        $ x ^. MLIL.offset
    MLIL.VAR_SPLIT_SSA x -> do
      highVar <- convertToPilVarAndLog $ x ^. MLIL.high
      lowVar <- convertToPilVarAndLog $ x ^. MLIL.low
      -- return $ mkExpr . Pil.VAR_JOIN $ Pil.VarJoinOp highVar lowVar
      -- NOTE: Binja gets the return size wrong. use above if they fix it
      return $ Expression (toPilOpSize $ 2 * expr ^. MLIL.size)
        . Pil.VAR_JOIN $ Pil.VarJoinOp highVar lowVar
    MLIL.VAR_SSA x -> do
      srcVar <- convertToPilVarAndLog $ x ^. MLIL.src
      return $ mkExpr . Pil.VAR $ Pil.VarOp srcVar
    MLIL.VAR_SSA_FIELD x -> do
      srcVar <- convertToPilVarAndLog $ x ^. MLIL.src
      return $ mkExpr . Pil.VAR_FIELD $
        Pil.VarFieldOp srcVar (ByteOffset $ x ^. MLIL.offset)
    MLIL.XOR (MLIL.XorOp in0 in1) -> binOp Pil.XOR Pil.XorOp in0 in1
    MLIL.ZX (MLIL.ZxOp in0) -> unOp Pil.ZX Pil.ZxOp in0

    MLIL.INTRINSIC _ -> unimpl
    MLIL.INTRINSIC_SSA _ -> unimpl

    MLIL.MEM_PHI _ -> unimpl
    MLIL.VAR _ -> unimpl
    MLIL.VAR_FIELD _ -> unimpl
    MLIL.VAR_PHI _ -> unimpl
    MLIL.VAR_SPLIT _ -> unimpl

    MLIL.SET_VAR _ -> unimpl
    MLIL.SET_VAR_SSA _ -> unimpl
    MLIL.SET_VAR_FIELD _ -> unimpl
    MLIL.SET_VAR_SSA_FIELD _ -> unimpl
    MLIL.SET_VAR_SPLIT _ -> unimpl
    MLIL.SET_VAR_SPLIT_SSA _ -> unimpl
    MLIL.SET_VAR_ALIASED _ -> unimpl
    MLIL.SET_VAR_ALIASED_FIELD _ -> unimpl

    MLIL.BP -> unimpl
    MLIL.CALL _ -> unimpl
    MLIL.CALL_OUTPUT _ -> unimpl
    MLIL.CALL_OUTPUT_SSA _ -> unimpl
    MLIL.CALL_PARAM _ -> unimpl
    MLIL.CALL_PARAM_SSA _ -> unimpl
    MLIL.CALL_SSA _ -> unimpl
    MLIL.CALL_UNTYPED _ -> unimpl
    MLIL.CALL_UNTYPED_SSA _ -> unimpl
    MLIL.FREE_VAR_SLOT _ -> unimpl
    MLIL.FREE_VAR_SLOT_SSA _ -> unimpl
    MLIL.GOTO _ -> unimpl
    MLIL.IF _ -> unimpl
    MLIL.JUMP _ -> unimpl
    MLIL.JUMP_TO _ -> unimpl
    MLIL.NOP -> unimpl
    MLIL.NORET -> unimpl
    MLIL.RET _ -> unimpl
    MLIL.RET_HINT _ -> unimpl
    MLIL.STORE _ -> unimpl
    MLIL.STORE_SSA _ -> unimpl
    MLIL.STORE_STRUCT _ -> unimpl
    MLIL.STORE_STRUCT_SSA _ -> unimpl
    MLIL.SYSCALL _ -> unimpl
    MLIL.SYSCALL_SSA _ -> unimpl
    MLIL.SYSCALL_UNTYPED _ -> unimpl
    MLIL.SYSCALL_UNTYPED_SSA _ -> unimpl
    MLIL.TAILCALL _ -> unimpl
    MLIL.TAILCALL_SSA _ -> unimpl
    MLIL.TAILCALL_UNTYPED _ -> unimpl
    MLIL.TAILCALL_UNTYPED_SSA _ -> unimpl
    MLIL.TRAP _ -> unimpl
    MLIL.UNDEF -> unimpl
    where
      unimpl = pure . mkExpr . Pil.UNIMPL $ Text.take 20 (show expr) <> "..."

      mkExpr :: Pil.ExprOp Expression -> Expression
      mkExpr = Expression (toPilOpSize $ expr ^. MLIL.size)

      unOp ::
        forall t a.
        (a -> Pil.ExprOp Expression) ->
        (Expression -> a) ->
        MLIL.Expression t ->
        Converter Expression
      unOp opCons opArgsCons in0 =
        mkExpr . opCons . opArgsCons <$> convertExpr in0

      binOp ::
        forall t a.
        (a -> Pil.ExprOp Expression) ->
        (Expression -> Expression -> a) ->
        MLIL.Expression t ->
        MLIL.Expression t ->
        Converter Expression
      binOp opCons opArgsCons in0 in1 =
        mkExpr . opCons <$> (opArgsCons <$> convertExpr in0 <*> convertExpr in1)

      triOp ::
        forall t a.
        (a -> Pil.ExprOp Expression) ->
        (Expression -> Expression -> Expression -> a) ->
        MLIL.Expression t ->
        MLIL.Expression t ->
        MLIL.Expression t ->
        Converter Expression
      triOp opCons opArgsCons in0 in1 in2 =
        mkExpr . opCons <$> (opArgsCons <$> convertExpr in0 <*> convertExpr in1 <*> convertExpr in2)

      mkConstFuncPtrExpr :: Address -> ExceptT () Converter Expression
      mkConstFuncPtrExpr addr = do
        r <- lift (getConstFuncPtrOp addr)
        case r ^. #symbol of
          Just _ -> return . mkExpr $ Pil.ConstFuncPtr r
          Nothing -> do
            bv <- use #binaryView
            liftIO (BNCG.getFunction bv addr) >>= \case
              Just fn -> return
                . mkExpr
                . Pil.ConstFuncPtr
                . Pil.ConstFuncPtrOp addr
                . Just
                . cs
                $ fn ^. #name
              Nothing -> throwError ()

      mkConstStrExpr :: Address -> ExceptT () Converter Expression
      mkConstStrExpr addr = do
        bv <- use #binaryView
        liftIO (BNView.getStringAtAddress bv addr)
          >>= maybe (throwError ()) (return . mkExpr . Pil.ConstStr . Pil.ConstStrOp)

      mkConstStrOrFuncPtr :: Address -> Converter Expression -> Converter Expression
      mkConstStrOrFuncPtr addr def
        | addr == 0 = def
        | otherwise = runExceptT (mkConstFuncPtrExpr addr <|> mkConstStrExpr addr)
                      >>= either (const def) return

convertToBinjaFunction :: Func.Function -> Converter (Maybe BNFunc.Function)
convertToBinjaFunction cgFunc = do
  bv <- use #binaryView
  liftIO $ BNCG.toBinjaFunction bv cgFunc

unsafeConvertToBinjaFunction :: Func.Function -> Converter BNFunc.Function
unsafeConvertToBinjaFunction cgFunc = convertToBinjaFunction cgFunc >>= maybe err return
  where
    err = P.error $ "Could not convert to Binja Func: " <> show cgFunc

convertToPilVar :: MLIL.SSAVariable -> Converter PilVar
convertToPilVar v = do
  ctx' <- use #ctx
  bnfunc <- unsafeConvertToBinjaFunction $ ctx' ^. #func
  varSize <- case v ^. MLIL.var . BNVar.varType of
    Nothing -> fromByteBased . toBytes <$> use #defaultVarSize
    Just vt -> return . typeWidthToSize $ vt ^. BNVar.width
  let sourceVar = SSAVariableRef v bnfunc (ctx' ^. #ctxId)
      pilVar = PilVar varSize (Just ctx') $ getSymbol (v ^. MLIL.version) (v ^. MLIL.var)
  #sourceVars %= HMap.insert pilVar sourceVar
  return pilVar

convertToPilVarAndLog :: MLIL.SSAVariable -> Converter PilVar
convertToPilVarAndLog v = do
  pvar <- convertToPilVar v
  #usedVars %= HSet.insert pvar
  return pvar

convertInstrOp :: MLIL.Operation (MLIL.Expression t) -> Converter [Statement Expression]
convertInstrOp op' = do
  defVarSize <- use #defaultVarSize
  case op' of
    MLIL.IF x -> do
      condition <- convertExpr (x ^. MLIL.condition)
      return [BranchCond $ BranchCondOp condition]
    MLIL.SET_VAR_SSA x -> do
      pvar <- convertToPilVarAndLog $ x ^. MLIL.dest
      expr <- convertExpr (x ^. MLIL.src)
      #definedVars %= (pvar :)
      return [Def $ DefOp pvar expr]
    -- TODO: Need some way to merge with previous version and include offset
    MLIL.SET_VAR_SSA_FIELD x -> do
      pvarDest <- convertToPilVarAndLog destVar
      pvarSrc <- convertToPilVarAndLog srcVar
      chunkExpr <- convertExpr (x ^. MLIL.src)
      let updateVarExpr =
            Expression varSize . Pil.UPDATE_VAR $
              Pil.UpdateVarOp
              (Pil.Expression (coerce $ pvarSrc ^. #size) (Pil.VAR $ Pil.VarOp pvarSrc))
              off
              chunkExpr
      #definedVars %= (pvarDest :)
      return [Def $ DefOp pvarDest updateVarExpr]
      where
        destVar = x ^. MLIL.prev . MLIL.dest
        srcVar = x ^. MLIL.prev . MLIL.src
        off = fromIntegral $ x ^. MLIL.offset
        getVarWidth v = v ^? MLIL.var . BNVar.varType . _Just . BNVar.width
        varSize =
          maybe (Pil.Size . toBytes $ defVarSize) fromIntegral $
            getVarWidth destVar <|> getVarWidth srcVar
    MLIL.SET_VAR_SPLIT_SSA x -> do
      pvarHigh <- convertToPilVarAndLog $ x ^. MLIL.high
      pvarLow <- convertToPilVarAndLog $ x ^. MLIL.low
      expr@(Expression size' _) <- convertExpr (x ^. MLIL.src)
      let halfSize = size' `div` 2
          highExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr (fromIntegral halfSize)
          lowExpr = Expression halfSize . Pil.Extract $ Pil.ExtractOp expr 0
      #definedVars %= (pvarHigh :)
      #definedVars %= (pvarLow :)
      return
        [ Def $ DefOp pvarHigh highExpr,
          Def $ DefOp pvarLow lowExpr
        ]
    -- note: prev.src and prev.dest are the same except memory ssa version
    MLIL.SET_VAR_ALIASED x -> do
      expr <- convertExpr (x ^. MLIL.src)
      addrExpr <- varToStackLocalAddr (x ^. MLIL.prev . MLIL.src . MLIL.var)
      return [Store $ StoreOp addrExpr expr]
    MLIL.SET_VAR_ALIASED_FIELD x -> do
      srcExpr <- convertExpr (x ^. MLIL.src)
      addrExpr <- varToStackLocalAddr (x ^. MLIL.prev . MLIL.src . MLIL.var)
      let destAddrExpr = Pil.mkFieldOffsetExprAddr addrExpr
                         $ x ^. MLIL.offset

      return [Store $ StoreOp destAddrExpr srcExpr]
    MLIL.STORE_SSA x -> do
      exprSrc <- convertExpr (x ^. MLIL.src)
      exprDest <- convertExpr (x ^. MLIL.dest)
      return [Store $ StoreOp exprDest exprSrc]
    MLIL.STORE_STRUCT_SSA x -> do
      destExpr <- (`Pil.mkFieldOffsetExprAddr` (x ^. MLIL.offset)) <$> convertExpr (x ^. MLIL.dest)
      srcExpr <- convertExpr (x ^. MLIL.src)
      return [Store $ StoreOp destExpr srcExpr]
    -- TODO: How should we organize handling path-sensitive vs -insensitive conversions of phi nodes?
    --       Consider introducing a PIL phi instruction here and using Pil.Analysis to resolve for paths during simplification
    --       phase.
    MLIL.VAR_PHI x -> do
      defVars <- use #definedVars
      -- Not using all the phi vars, so don't need to log them all
      srcVars <- HSet.fromList <$> traverse convertToPilVar (x ^. MLIL.src)
      let latestVar = Pil.getLastDefined defVars srcVars
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
                  (typeWidthToSize $ vt ^. BNVar.width)
                  (Pil.VAR $ Pil.VarOp lVar)
            ]
    MLIL.UNIMPL -> return [UnimplInstr "UNIMPL"]
    MLIL.UNIMPL_MEM x -> do
      expr <- convertExpr (x ^. MLIL.src)
      return [UnimplMem $ UnimplMemOp expr]
    MLIL.UNDEF -> return [Undef]
    MLIL.NOP -> return [Nop]
    MLIL.RET x -> do
      -- TODO: Figure out when/if return ever has multiple values
      expr <- maybe (return $ Expression 0 Pil.UNIT) convertExpr
              $ headMay (x ^. MLIL.src)
      return [Ret $ RetOp expr]
    MLIL.MEM_PHI (MLIL.MemPhiOp _dst _srcs) ->
      -- Temporarily ignore for now, because we don't use them
      -- return [DefMemPhi $ DefMemPhiOp dst srcs]
      return []
    MLIL.NORET -> return [ NoRet ]
    MLIL.GOTO _ -> return []
    MLIL.JUMP x -> do
      destExpr <- convertExpr (x ^. MLIL.dest)
      return [ Jump $ JumpOp destExpr ]
    MLIL.JUMP_TO x -> do
      destExpr <- convertExpr (x ^. MLIL.dest)
      return [ JumpTo $ JumpToOp destExpr (x ^. MLIL.targets)]
    _ -> return [UnimplInstr $ show op']

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
convertInstr instr_ = fmap (Pil.Stmt $ instr_ ^. MLIL.address)
  <$> (convertInstrOp . view MLIL.op $ instr_)

convertInstrSplitPhi :: MLIL.Instruction F -> Converter [Stmt]
convertInstrSplitPhi instr_ = fmap (fmap $ Pil.Stmt (instr_ ^. MLIL.address)) $
  case toCallInstruction instr_ of
    Nothing -> convertInstrOpSplitPhi . view MLIL.op $ instr_
    Just x -> convertCallInstruction x

convertInstrs :: [MLIL.Instruction t] -> Converter [Stmt]
convertInstrs = concatMapM convertInstr

convertInstrsSplitPhi :: [MLIL.Instruction F] -> Converter [Stmt]
convertInstrsSplitPhi = concatMapM convertInstrSplitPhi

isTailCall :: CallOperation -> Bool
isTailCall = \case
  TAILCALL _ -> True
  TAILCALL_SSA _ -> True
  TAILCALL_UNTYPED _ -> True
  TAILCALL_UNTYPED_SSA _ -> True
  _ -> False

getSymbolAtAddress :: Address -> Converter (Maybe Symbol)
getSymbolAtAddress addr = do
  bv <- use #binaryView
  msym <- liftIO $ BNView.getSymbolAtAddress bv addr Nothing
  traverse (fmap cs . liftIO . BN.getSymbolFullName) msym

getConstFuncPtrOp :: Address -> Converter Pil.ConstFuncPtrOp
getConstFuncPtrOp addr = Pil.ConstFuncPtrOp addr <$> getSymbolAtAddress addr

getCallDestFromCallTargetExpr :: Expression -> Converter (Pil.CallDest Expression)
getCallDestFromCallTargetExpr targetExpr = case targetExpr ^. #op of
  (Pil.ExternPtr x) -> return $ Pil.CallExtern x
  (Pil.IMPORT c) -> do
    Pil.CallAddr <$> getConstFuncPtrOp (fromIntegral $ c ^. #constant)
  (Pil.ConstFuncPtr x) -> do
    bv <- use #binaryView
    mfunc <- liftIO $ BNFunc.getFunctionStartingAt bv Nothing $ x ^. #address
    case mfunc of
      Nothing -> return $ Pil.CallAddr x
      Just bnf -> fmap Pil.CallFunc . liftIO $ BNCG.convertFunction bv bnf
  (Pil.CONST_PTR c) -> do
    bv <- use #binaryView
    mfunc <- liftIO $ BNFunc.getFunctionStartingAt bv Nothing
      . fromIntegral
      $ c ^. #constant
    case mfunc of
      Nothing -> fmap Pil.CallAddr
        . getConstFuncPtrOp
        . fromIntegral
        $ c ^. #constant
      Just bnf -> fmap Pil.CallFunc . liftIO $ BNCG.convertFunction bv bnf
  _ -> return $ Pil.CallExpr targetExpr

-- TODO: How to deal with BN functions the report multiple return values? Multi-variable def?
convertCallInstruction :: CallInstruction -> Converter [Statement Expression]
convertCallInstruction ci = do
  -- TODO: Better handling of possible Nothing value
  targetExpr <- convertExpr (fromJust (ci ^. #dest))
  target <- getCallDestFromCallTargetExpr targetExpr
  params' <- sequence [convertExpr p | p <- ci ^. #params]

  let mname = (target ^? #_CallFunc . #name)
              <|> (target ^? #_CallAddr . #symbol . _Just)
  funcDefs <- use #knownFuncs
  let outStores = getOutStores mname funcDefs params'
  -- The size of a function call is always 0 according to BN. Need to look at result var types to get
  -- actual size. This is done below when handling the case for a return value.
  let callOp = CallOp target mname params'
  case (ci ^. #outputDest, isTailCall $ ci ^. #op) of
    -- TODO: Try to merge Nothing and an empty list, consider changing outputDest to NonEmpty
    ([], False) -> return $ Call callOp : outStores
    ([], True) -> return $ TailCall (TailCallOp target mname params' Nothing) : outStores
    (dest' : _, tc) -> do
      dest'' <- convertToPilVarAndLog dest'
        -- TODO: Make this safe. We currently bail if there's no type provided.
        --       Change MediumLevelILInsutrction._size to be Maybe OperationSize
      let resultSize = dest' ^?! MLIL.var . BNVar.varType . _Just . BNVar.width
          opSize = typeWidthToSize resultSize
          callExpr = Expression opSize
                     . Pil.CALL
                     $ Pil.CallOp target mname params'
          callStmt = if tc
            then TailCall . TailCallOp target mname params' $ Just (dest'', opSize)
            else Def . DefOp dest'' $ callExpr
      return $ callStmt : outStores
  where
    getOutStores :: Maybe Text -> HashMap Text Func.FuncInfo -> [Expression] -> [Statement Expression]
    getOutStores mname funcInfos args =
      case mname >>= (`HMap.lookup` funcInfos) of
        Nothing -> []
        (Just funcInfo) ->
          Pil.genCallOutputStores (funcInfo ^. #params) args

-- | Gets all PIL statements contained in a function.
-- the "Int" is the original MLIL_SSA InstructionIndex
convertFunction :: Function -> Converter [MappedStmt (Int, Address)]
convertFunction func' = do
  mlilFunc <- liftIO $ BNFunc.getMLILSSAFunction func'
  mlilInstrs <- liftIO $ MLIL.fromFunction mlilFunc
  concatMapM f $ zip [0..] mlilInstrs
  where
    f (mlilIndex, mlilInstr) =
      fmap (MappedStatement (mlilIndex, mlilInstr ^. MLIL.address)) <$> convertInstrSplitPhi mlilInstr

getFuncStatementsMapped :: BNBinaryView -> Func.Function -> CtxId -> IO [MappedStmt (Int, Address)]
getFuncStatementsMapped bv func' ctxId' = do
  mBnFunc <- BNCG.toBinjaFunction bv func'
  case mBnFunc of
    Nothing -> P.error $ "No function found at " <> show (func' ^. #address)
    Just bnFunc -> do
      addrSize' <- BN.getViewAddressSize bv
      let st = mkConverterState bv ctxId' Pil.knownFuncDefs addrSize' func'
      fst <$> runConverter (convertFunction bnFunc) st

getFuncStatements :: BNBinaryView -> Func.Function -> CtxId -> IO [Stmt]
getFuncStatements bv func' ctxId' = fmap (view #stmt) <$> getFuncStatementsMapped bv func' ctxId'

getDestOp :: CallInstruction -> Maybe (MLIL.Operation (MLIL.Expression F))
getDestOp CallInstruction{dest=Just MLIL.Expression{MLIL._op=op'}} = Just op'
getDestOp _ = Nothing

isDirectCall :: CallInstruction -> Bool
isDirectCall c = case getDestOp c of
  Just (MLIL.CONST_PTR _) -> True
  Just (MLIL.IMPORT _) -> True
  _ -> False

-- createCallSite :: BNBinaryView -> Function -> CallInstruction -> IO CallSite
-- createCallSite bv func c = CallSite func c <$>
--   case c ^. #dest of
--     Just dexpr -> case (dexpr ^. MLIL.op :: MLIL.Operation (MLIL.Expression F)) of
--       (MLIL.CONST_PTR cpOp) ->
--         maybe (Pil.CallAddr addr) Pil.CallFunc <$>
--           BNFunc.getFunctionStartingAt bv Nothing addr
--         where
--           addr :: Address
--           addr = fromIntegral $ cpOp ^. MLIL.constant
--       _ -> return $ Pil.CallExpr dexpr
--     Nothing -> return $ Pil.CallExprs [] --- probably should be a failure

getCallsInFunction :: BNFunc.Function -> IO [CallInstruction]
getCallsInFunction fn = do
  bbs <- BNFunc.getMLILSSAFunction fn >>= BB.getBasicBlocks
  concat <$> traverse callsPerBB bbs
  where
    callsPerBB bb = mapMaybe toCallInstruction <$> MLIL.fromBasicBlock bb

getIndirectCallsInFunction :: BNFunc.Function -> IO [CallInstruction]
getIndirectCallsInFunction fn = do
  calls <- getCallsInFunction fn
  return $ filter (not . isDirectCall) calls

getIndirectCallSites :: [BNFunc.Function] -> IO [(BNFunc.Function, CallInstruction)]
getIndirectCallSites fns = do
  indirectCalls <- traverse getIndirectCallsInFunction fns
  return . getTupleList $ zip fns indirectCalls
  where
    getTupleList :: [(BNFunc.Function, [CallInstruction])] -> [(BNFunc.Function, CallInstruction)]
    getTupleList = concat <$> map (uncurry createTuple)
    createTuple fn (i:xs) = [(fn, i)] <> createTuple fn xs
    createTuple _ [] = []
