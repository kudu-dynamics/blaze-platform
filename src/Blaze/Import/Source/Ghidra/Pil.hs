module Blaze.Import.Source.Ghidra.Pil where

import Blaze.Prelude hiding (Symbol)

import qualified Ghidra.BasicBlock as BB
import Ghidra.State (GhidraState)
import qualified Ghidra.Core as Ghidra
import Ghidra.Types.Function (Function)
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.Types.Pcode as Pcode
import qualified Ghidra.State as GState
import Ghidra.Types.Pcode.Lifted (PcodeOp, Output(Output))
import qualified Ghidra.Types.Pcode.Lifted as P
import qualified Ghidra.Types.Address as GAddr
import qualified Ghidra.Types.Variable as GVar

import qualified Blaze.Pil as Pil
import qualified Data.BinaryAnalysis as BA
import Data.Binary.IEEE754 (wordToDouble)
import qualified Prelude as P
import Blaze.Types.Pil
    ( BranchCondOp(BranchCondOp),
      CallOp(CallOp),
      TailCallOp(TailCallOp),
      Ctx(Ctx),
      DefOp(DefOp),
      DefPhiOp(DefPhiOp),
      Expression(Expression),
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

import Ghidra.Types.Variable (HighVarNode, VarNode, VarType)
import qualified Blaze.Import.Source.Ghidra.CallGraph as GCG
import Blaze.Import.Source.Ghidra.Types (convertAddress)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.GenericConv (GConv, gconv)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as Text
import Unsafe.Coerce (unsafeCoerce)

data ConverterError
  = ExpectedConstButGotAddress GAddr.Address
  | ExpectedAddressButGotConst Int64
  | AddressSpaceTypeCannotBeVar GAddr.Address
  | OutOfChoicesError -- for Alternative instance, Monoid's mempty
  | UnsuportedPcodeOp Text
  | UnsuportedAddressSpace Text
  | ExpectedVarExpr Bytes VarType
  deriving (Eq, Ord, Show, Generic, Hashable)

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
  , defaultPtrSize :: Bytes
    -- | A mapping of PilVars to the a variable from the import source.
  , sourceVars :: HashMap PilVar VarNode
  , ghidraState :: GhidraState
  }
  deriving (Eq, Ord, Show, Generic)

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: ExceptT ConverterError (StateT ConverterState IO) a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO, MonadError ConverterError)

class IsVariable a where
  getSize :: a -> Bytes
  getVarType :: a -> VarType

instance IsVariable HighVarNode where
  getSize = view #size
  getVarType = view #varType

instance IsVariable VarNode where
  getSize = view #size
  getVarType = view #varType

instance IsVariable a => IsVariable (Output a) where
  getSize (P.Output a) = getSize a
  getVarType (P.Output a) = getVarType a

instance IsVariable a => IsVariable (P.Input a) where
  getSize = getSize . view #value
  getVarType = getVarType . view #value


-- -- TODO: Rename IsVariable
-- instance IsVariable Address where
--   getSize = fromIntegral . view $ #space . #ptrSize
--   getVarType = GVar.Addr

data VarNodeType
  = VUnique Int64
  | VReg Int64
  | VStack Int64
  | VRam Int64
  | VConstAddr Int64
  | VExtern Int64
  | VImmediate Int64
  | VOther Text
  deriving (Eq, Ord, Read, Show, Generic)

getVarNodeType :: IsVariable a => a -> VarNodeType
getVarNodeType v = case getVarType v of
  GVar.Const n -> VImmediate n
  GVar.Addr x -> case x ^. #space . #name of
    GAddr.EXTERNAL -> VExtern off
    GAddr.HASH -> VOther "HASH"
    GAddr.Const -> VConstAddr off
    GAddr.Ram -> VRam off
    GAddr.Register -> VReg off
    GAddr.Stack -> VStack off
    GAddr.Unique -> VUnique off
    GAddr.Other t -> VOther t
    where
      off = x ^. #offset


-- The point of these individual funcs instead of a single `VarNode -> Expression`
-- func is so you can choose only the correct possibility and `<|>` them together
getConstIntExpr :: IsVariable a => a -> Maybe Pil.Expression
getConstIntExpr v = case getVarNodeType v of
  VImmediate n -> return . mkExpr v . Pil.CONST . Pil.ConstOp $ n
  _ -> Nothing

getConstPtrExpr :: IsVariable a => a -> Maybe Pil.Expression
getConstPtrExpr v = case getVarNodeType v of
  VImmediate n -> return . mkExpr v . Pil.CONST_PTR . Pil.ConstPtrOp $ n
  VRam n -> return . mkExpr v . Pil.CONST_PTR . Pil.ConstPtrOp $ n
  _ -> Nothing

getExternPtrExpr :: IsVariable a => a -> Maybe Pil.Expression
getExternPtrExpr v = case getVarNodeType v of
  -- TODO: figure out what to put for address and offset of ExternPtrOp
  VExtern n -> return . mkExpr v . Pil.ExternPtr $  Pil.ExternPtrOp 0 (fromIntegral n) Nothing
  _ -> Nothing

getPtrExpr :: IsVariable a => a -> Maybe Pil.Expression
getPtrExpr v = getExternPtrExpr v <|> getConstPtrExpr v

-- | Only returns variables
getVarExpr :: IsVariable a => Ctx -> a -> Maybe Pil.Expression
getVarExpr ctx v = case getVarNodeType v of
  VUnique n -> pv $ "unique" <> show n
  VReg n -> pv $ "reg" <> show n
  VStack n -> pv $ "stack" <> show n
  _ -> Nothing
  where
    pv :: Text -> Maybe Pil.Expression
    pv name = return . mkExpr v . Pil.VAR . Pil.VarOp . PilVar name $ Just ctx

getFloatConstExpr :: IsVariable a => a -> Maybe Pil.Expression
getFloatConstExpr v = case getVarNodeType v of
  VImmediate n ->
    -- TODO: make sure this is the proper way to convert const from ghidra to floats
    -- TODO: see if Ghidra will do this for us, since it's arch dependent.
    return . mkExpr v . Pil.CONST_FLOAT . Pil.ConstFloatOp . wordToDouble . unsafeCoerce $ n
    
  _ -> Nothing

convertAny :: [a -> Maybe Pil.Expression] -> a -> Converter Pil.Expression
convertAny converters a = case asum $ fmap ($ a) converters of
  Nothing -> throwError OutOfChoicesError
  Just x -> return x

convertConstFloatOrVar :: IsVariable a => a -> Converter Pil.Expression
convertConstFloatOrVar v = do
  ctx' <- use #ctx
  convertAny [ getFloatConstExpr
             , getVarExpr ctx'
             ]
    v

-- | Converts a ghidra address into a PilVar.
convertVarAddress :: GAddr.Address -> Converter PilVar
convertVarAddress x = do
  name <- case x ^. #space . #name of
    GAddr.Register -> return $ "reg" <> show (x ^. #offset)
    GAddr.Stack -> return $ "var" <> show (x ^. #offset)
    GAddr.Unique -> return $ "unique" <> show (x ^. #offset)
    _ -> throwError $ AddressSpaceTypeCannotBeVar x
  PilVar name . Just <$> use #ctx

-- | Converts a Ghidra VarNode to be a PilVar, or throws error.
requirePilVar :: IsVariable a => a -> Converter PilVar
requirePilVar v = case getVarType v of
  GVar.Const n -> throwError $ ExpectedAddressButGotConst n
  GVar.Addr x -> convertVarAddress x

mkExpr :: IsVariable a => a -> Pil.ExprOp Pil.Expression -> Pil.Expression
mkExpr v = Pil.Expression (fromIntegral $ getSize v)

mkExpr' :: Pil.OperationSize -> Pil.ExprOp Pil.Expression -> Pil.Expression
mkExpr' sz = Pil.Expression sz

mkExprWithDefaultSize :: Pil.ExprOp Pil.Expression -> Converter Pil.Expression
mkExprWithDefaultSize x = do
  sz <- fromIntegral <$> use #defaultPtrSize
  return $ mkExpr' sz x

mkAddressExpr :: GAddr.Address -> Pil.Expression
mkAddressExpr x = Pil.Expression (fromIntegral $ x ^. #space . #ptrSize) . Pil.CONST_PTR . Pil.ConstPtrOp $ x ^. #offset

-- | Converts a Ghidra var to be a Pil Var expression, or throws error.
requireVarExpr :: IsVariable a => a -> Converter Pil.Expression
requireVarExpr v = mkExpr v . Pil.VAR . Pil.VarOp <$> requirePilVar v

requireConst :: IsVariable a => a -> Converter Int64
requireConst v = case getVarType v of
  GVar.Const n -> return n
  GVar.Addr x -> throwError $ ExpectedConstButGotAddress x

requireConstIntExpr :: IsVariable a => a -> Converter Pil.Expression
requireConstIntExpr v = mkExpr v . Pil.CONST . Pil.ConstOp <$> requireConst v

convertDest :: P.Destination -> Converter Pil.Expression
convertDest (P.Absolute addr) = return $ mkAddressExpr addr
convertDest (P.Relative off) = mkExprWithDefaultSize . Pil.CONST . Pil.ConstOp $ off

callDestFromDest :: P.Destination -> Converter (Pil.CallDest Pil.Expression)
callDestFromDest (P.Relative _off) =
  -- Calling into the pcode for the same instruction doesn't make much sense
  -- so assume it won't happen, even though the docs say its the same as Branch
  error "Got a realative offset. Expected only Absolute calls"
callDestFromDest (P.Absolute addr) = case addr ^. #space . #name of
  GAddr.EXTERNAL -> return . Pil.CallExtern $ Pil.ExternPtrOp 0 (fromIntegral $ addr ^. #offset) Nothing
  GAddr.Ram -> do
    gs <- use #ghidraState
    paddr <- return $ convertAddress addr
    liftIO (GCG.getFunction gs paddr) >>= \case
      Just func -> return . Pil.CallFunc $ func
      Nothing -> return . Pil.CallExpr $ mkAddressExpr addr
  GAddr.Const -> do
    paddr <- return $ convertAddress addr
    return . Pil.CallAddr $ Pil.ConstFuncPtrOp paddr Nothing
  _ -> return . Pil.CallExpr $ mkAddressExpr addr

-- TODO: consolodate the other funcs to use this one
convertVarNodeType :: VarNodeType -> Converter (Pil.ExprOp Pil.Expression)
convertVarNodeType vnt = do
  ctx' <- use #ctx
  let pv name = return . Pil.VAR . Pil.VarOp . PilVar name $ Just ctx'
  case vnt of
    VReg n -> pv $ "reg" <> show n -- TODO: add size
    VStack n -> pv $ "stack" <> show n
    VUnique n -> pv $ "unique" <> show n
    VRam n -> return . Pil.CONST_PTR . Pil.ConstPtrOp $ n
    VConstAddr n -> return . Pil.CONST_PTR . Pil.ConstPtrOp $ n -- TODO needs to know addr space for this to make sense. Pil's CONST_PTR just means it's a immediate that is a pointer, not necessarily that it points into the Constant binary address space.
    VExtern n -> return . Pil.ExternPtr $  Pil.ExternPtrOp 0 (fromIntegral n) Nothing 
    VImmediate n -> return . Pil.CONST . Pil.ConstOp $ n
    VOther t -> throwError $ UnsuportedAddressSpace t

convertVarNode :: IsVariable a => a -> Converter Pil.Expression
convertVarNode v = Pil.Expression (fromIntegral . getSize $ v) <$> convertVarNodeType (getVarNodeType v)
 
convertPcodeOpToPilStmt :: forall a. IsVariable a => PcodeOp a -> Converter Pil.Stmt
convertPcodeOpToPilStmt op = get >>= \st -> case op of
  P.BOOL_AND out in0 in1 -> unsupported "BOOL_AND"
  P.BOOL_NEGATE out in0 -> unsupported "BOOL_NEGATE"
  P.BOOL_OR out in0 in1 -> unsupported "BOOL_OR"
  P.BOOL_XOR out in0 in1 -> unsupported "BOOL_XOR"
  P.BRANCH dest -> Pil.Jump . Pil.JumpOp <$> convertDest (dest ^. #value)
  -- Branch indirect. Var contains offset from current instr.
  -- Offset is in context of current addr space
  -- TODO: maybe should use `JumpTo instrAddr [off]`
  P.BRANCHIND in0 -> Pil.Jump . Pil.JumpOp <$> requireVarExpr (in0 ^. #value)
  P.CALL dest inputs -> do
    cdest <- callDestFromDest $ dest ^. #value
    params <-  mapM convertVarNode . fmap (view #value) $ inputs
    return . Pil.Call $ Pil.CallOp cdest Nothing params
  P.CALLIND in0 inputs -> case getVarExpr (st ^. #ctx) (in0 ^. #value) of
    Nothing -> do
      let v = in0 ^. #value
      throwError $ ExpectedVarExpr (getSize v) (getVarType v)
    Just destVarExpr -> do
      params <- mapM convertVarNode . fmap (view #value) $ inputs
      return . Pil.Call $ Pil.CallOp (Pil.CallExpr destVarExpr) Nothing params
    
  P.CALLOTHER in0 inputs -> unsupported "CALLOTHER" -- Can't find this in the docs
  P.CAST out in0 -> unsupported "CAST"    
  P.CBRANCH _dest in0 -> do
    cond <- convertVarNode $ in0 ^. #value
    -- Ignore the dest for now, as it gets encoded in the CFG edges.
    return . Pil.BranchCond . Pil.BranchCondOp $ cond
  P.COPY out in0 -> do
    destVar <- requirePilVar out
    Pil.Def . Pil.DefOp destVar <$> convertVarNode (in0 ^. #value)
  P.CPOOLREF _out _in0 _in1 _inputs -> unsupported "CPOOLREF"
  P.EXTRACT out in0 in1 -> do -- NOT in docs. guessing `Extract dest src offset
    srcExpr <- convertVarNode in0
    offsetExpr <- requireConst in1
    mkDef out . Pil.Extract $ Pil.ExtractOp srcExpr offsetExpr
  P.FLOAT_ABS out in0 -> mkDef out =<< unFloatOp Pil.FABS Pil.FabsOp in0
  P.FLOAT_ADD out in0 in1 -> mkDef out =<< binFloatOp Pil.FADD Pil.FaddOp in0 in1
  P.FLOAT_CEIL out in0 -> mkDef out =<< unFloatOp Pil.CEIL Pil.CeilOp in0
  P.FLOAT_DIV out in0 in1 -> mkDef out =<< binFloatOp Pil.FDIV Pil.FdivOp in0 in1
  P.FLOAT_EQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_E Pil.FcmpEOp in0 in1
  P.FLOAT_FLOAT2FLOAT out in0 -> mkDef out =<< unFloatOp Pil.FLOAT_CONV Pil.FloatConvOp in0
  P.FLOAT_FLOOR out in0 -> mkDef out =<< unFloatOp Pil.FLOOR Pil.FloorOp in0
  P.FLOAT_INT2FLOAT out in0 -> mkDef out =<< unFloatOp Pil.INT_TO_FLOAT Pil.IntToFloatOp in0
  P.FLOAT_LESS out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_LT Pil.FcmpLtOp in0 in1
  P.FLOAT_LESSEQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_LE Pil.FcmpLeOp in0 in1
  P.FLOAT_MULT out in0 in1 -> mkDef out =<< binFloatOp Pil.FMUL Pil.FmulOp in0 in1
  P.FLOAT_NAN out in0 -> mkDef out =<< binFloatOp Pil.FCMP_UO Pil.FcmpUoOp in0 in0
  P.FLOAT_NEG out in0 -> mkDef out =<< unFloatOp Pil.FNEG Pil.FnegOp in0
  P.FLOAT_NOTEQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_NE Pil.FcmpNeOp in0 in1
  P.FLOAT_ROUND out in0 -> mkDef out =<< unFloatOp Pil.ROUND_TO_INT Pil.RoundToIntOp in0
  P.FLOAT_SQRT out in0 -> mkDef out =<< unFloatOp Pil.FSQRT Pil.FsqrtOp in0
  P.FLOAT_SUB out in0 in1 -> mkDef out =<< binFloatOp Pil.FSUB Pil.FsubOp in0 in1
  P.FLOAT_TRUNC out in0 -> mkDef out =<< unFloatOp Pil.FTRUNC Pil.FtruncOp in0
  P.INDIRECT out in0 in1 -> unsupported "INDIRECT"
  P.INSERT -> unsupported "INSERT" -- not in docs
  P.INT_2COMP out in0 -> mkDef out =<< unIntOp Pil.NEG Pil.NegOp in0
  P.INT_ADD out in0 in1 -> mkDef out =<< binIntOp Pil.ADD Pil.AddOp in0 in1
  P.INT_AND out in0 in1 -> mkDef out =<< binIntOp Pil.AND Pil.AndOp in0 in1
  P.INT_CARRY out in0 in1 -> do
    pv <- requirePilVar out
    addOverExpr <- Pil.Expression (fromIntegral $ getSize in0) <$> binIntOp Pil.ADD_OVERFLOW Pil.AddOverflowOp in0 in1
    let zero = Pil.Expression (fromIntegral $ getSize in0) . Pil.CONST $ Pil.ConstOp 0
        eqZeroExpr = Pil.Expression (fromIntegral $ getSize out) . Pil.CMP_E $ Pil.CmpEOp addOverExpr zero
    return . Pil.Def . Pil.DefOp pv $ eqZeroExpr

  P.INT_DIV out in0 in1 -> mkDef out =<< binIntOp Pil.DIVU Pil.DivuOp in0 in1
  P.INT_EQUAL out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_E Pil.CmpEOp in0 in1
  P.INT_LEFT out in0 in1 -> mkDef out =<< binIntOp Pil.LSL Pil.LslOp in0 in1
  P.INT_LESS out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_ULT Pil.CmpUltOp in0 in1
  P.INT_LESSEQUAL out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_ULE Pil.CmpUleOp in0 in1
  P.INT_MULT out in0 in1 -> mkDef out =<< binIntOp Pil.MUL Pil.MulOp in0 in1
  P.INT_NEGATE out in0 -> mkDef out =<< unIntOp Pil.NOT Pil.NotOp in0
  P.INT_NOTEQUAL out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_NE Pil.CmpNeOp in0 in1
  P.INT_OR out in0 in1 -> mkDef out =<< binIntOp Pil.OR Pil.OrOp in0 in1
  P.INT_REM out in0 in1 -> mkDef out =<< binIntOp Pil.MODU Pil.ModuOp in0 in1
  P.INT_RIGHT out in0 in1 -> mkDef out =<< binIntOp Pil.LSR Pil.LsrOp in0 in1
  P.INT_SBORROW out in0 in1 -> unsupported "SBORROW"
  P.INT_SCARRY out in0 in1 -> unsupported "SCARRY" -- maybe use ADD_OVERFLOW wrapped in (> 0), but what about signedness?
  P.INT_SDIV out in0 in1 -> mkDef out =<< binIntOp Pil.DIVS Pil.DivsOp in0 in1
  P.INT_SEXT out in0 -> mkDef out =<< unIntOp Pil.SX Pil.SxOp in0
  P.INT_SLESS out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_SLT Pil.CmpSltOp in0 in1
  P.INT_SLESSEQUAL out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_SLE Pil.CmpSleOp in0 in1
  P.INT_SREM out in0 in1 -> mkDef out =<< binIntOp Pil.MODS Pil.ModsOp in0 in1
  P.INT_SRIGHT out in0 in1 -> mkDef out =<< binIntOp Pil.ASR Pil.AsrOp in0 in1
  P.INT_SUB out in0 in1 -> mkDef out =<< binIntOp Pil.SUB Pil.SubOp in0 in1
  P.INT_XOR out in0 in1 -> mkDef out =<< binIntOp Pil.XOR Pil.XorOp in0 in1
  P.INT_ZEXT out in0 -> mkDef out =<< unIntOp Pil.ZX Pil.ZxOp in0
  P.LOAD out addrSpace in1 -> undefined
  P.MULTIEQUAL out in0 in1 inputs -> undefined
  P.NEW out in0 inputs -> undefined
  P.PCODE_MAX -> undefined -- unknown
  P.PIECE out high low -> do
    high' <- convertVarNode high
    low' <- convertVarNode low
    let outSize = fromIntegral (getSize out)
        lowSize = low' ^. #size
        highSize = high' ^. #size
    if outSize/= lowSize + highSize
      then error "PIECE operand sizes do not add up to output size" -- make this a ConverterError
      else pure ()
    let highShifted = Pil.LSL $ Pil.LslOp high' (mkExpr _ . Pil.CONST . Pil.ConstOp $ fromIntegral lowSize)
        lowExtended = Pil.ZX . Pil.ZxOp $ low'
        res = Pil.OR $ Pil.OrOp (mkExpr out highShifted) (mkExpr out lowExtended)
    mkDef out res
  P.POPCOUNT out in0 -> undefined
  P.PTRADD out base idx stride -> mkDef out =<< triIntOp Pil.ARRAY_ADDR Pil.ArrayAddrOp base idx stride
  P.PTRSUB out base offset -> mkDef out =<< binIntOp Pil.VAR_FIELD Pil.VarFieldOp base offset
  P.RETURN in0 inputs -> undefined
  P.SEGMENTOP -> undefined -- unknowng
  P.STORE addrSpace in0 in1 -> undefined
  P.SUBPIECE out in0 lowOff -> do
    in0' <- convertVarNode in0
    lowOff' <- case getVarNodeType lowOff of
                 VImmediate n -> pure n
                 _ -> error "SUBPIECE second argument was not IMMEDIATE"
    let shiftedSize = (in0' ^. #size) - fromIntegral lowOff'
        shifted = Pil.Expression shiftedSize . Pil.LSR $ Pil.LsrOp in0' (mkExpr _ . Pil.CONST $ Pil.ConstOp lowOff')
        truncated = Pil.LOW_PART . Pil.LowPartOp $ shifted
    mkDef out truncated
  P.UNIMPLEMENTED -> pure $ Pil.UnimplInstr "unimpl"

  where
    mkDef :: P.Output a -> Pil.ExprOp Pil.Expression -> Converter Pil.Stmt
    mkDef v xop = do
      pv <- requirePilVar v
      return . Pil.Def . Pil.DefOp pv $ Pil.Expression (fromIntegral $ getSize v) xop

    unIntOp :: forall b.
               (b -> Pil.ExprOp Pil.Expression)
             -> (Pil.Expression -> b)
             -> P.Input a
             -> Converter (Pil.ExprOp Pil.Expression)
    unIntOp opCons opArgsCons in0 = do
      a <- convertVarNode in0
      return . opCons $ opArgsCons a

    binIntOp :: forall b.
               (b -> Pil.ExprOp Pil.Expression)
             -> (Pil.Expression -> Pil.Expression -> b)
             -> P.Input a
             -> P.Input a
             -> Converter (Pil.ExprOp Pil.Expression)
    binIntOp opCons opArgsCons in0 in1 = do
      a <- convertVarNode in0
      b <- convertVarNode in1
      return . opCons $ opArgsCons a b

    triIntOp :: forall b.
               (b -> Pil.ExprOp Pil.Expression)
             -> (Pil.Expression -> Pil.Expression -> Pil.Expression -> b)
             -> P.Input a
             -> P.Input a
             -> P.Input a
             -> Converter (Pil.ExprOp Pil.Expression)
    triIntOp opCons opArgsCons in0 in1 in2 = do
      a <- convertVarNode in0
      b <- convertVarNode in1
      c <- convertVarNode in2
      return . opCons $ opArgsCons a b c

    binFloatOp :: forall b.
                  (b -> Pil.ExprOp Pil.Expression)
               -> (Pil.Expression -> Pil.Expression -> b)
               -> P.Input a
               -> P.Input a
               -> Converter (Pil.ExprOp Pil.Expression)
    binFloatOp opCons opArgsCons in0 in1 = do
      a <- convertConstFloatOrVar in0
      b <- convertConstFloatOrVar in1
      return . opCons $ opArgsCons a b

    unFloatOp :: forall b.
                    (b -> Pil.ExprOp Pil.Expression)
                 -> (Pil.Expression -> b)
                 -> P.Input a
                 -> Converter (Pil.ExprOp Pil.Expression)
    unFloatOp opCons opArgsCons in0 = do
      a <- convertConstFloatOrVar in0
      return . opCons $ opArgsCons a
    

    unsupported :: Text -> Converter Pil.Stmt
    unsupported = throwError . UnsuportedPcodeOp
