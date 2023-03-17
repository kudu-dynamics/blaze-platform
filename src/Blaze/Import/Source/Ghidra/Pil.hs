module Blaze.Import.Source.Ghidra.Pil where

import Blaze.Prelude hiding (Symbol)

import Ghidra.Address (getAddressSpaceMap)
import Ghidra.State (GhidraState)
import qualified Ghidra.State as GState
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as P
import qualified Ghidra.Types as J
import qualified Ghidra.Types.Pcode.Lifted as P
import qualified Ghidra.Types.Address as GAddr
import qualified Ghidra.Types.Variable as GVar

import qualified Blaze.Pil.Construct as C
import Data.Binary.IEEE754 (wordToDouble)
import qualified Numeric
import qualified Data.Text as Text
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil
  (
    CtxId,
    Ctx(Ctx),
    Expression (Expression),
    Size,
    PilVar,
  )

import Ghidra.Types.Variable (HighVarNode, VarNode, VarType)
import qualified Blaze.Import.Source.Ghidra.CallGraph as GCG
import Blaze.Import.Source.Ghidra.Types (convertAddress)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function ( Function )

import Unsafe.Coerce (unsafeCoerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE


data ConverterError
  = ExpectedConstButGotAddress GAddr.Address
  | ExpectedAddressButGotConst Int64
  | AddressSpaceTypeCannotBeVar GAddr.Address
  | UnsuportedAddressSpace Text
  -- | Could not recognize the VarNode as a 'PilVar', probably because it
  -- belongs to an address space other than 'unique', 'reg', or stack
  | VarNodeInvalidAsPilVar Bytes VarType
  -- | The sizes of the operands to PIECE did not add up to the size of its output
  | PieceOperandsIncorrectSizes
    { outputSize :: Size Expression
    , highArg :: Expression
    , lowArg :: Expression
    }
  -- | The offset argument (input1) to 'PTRSUB' was not @.isConstant()@
  | PtrsubOffsetNotConstant
    { offset :: VarNodeType
    }
  -- | The result list of 'RETURN' was greater than 1
  | ReturningTooManyResults
  -- | The argument to 'BRANCH' or 'CBRANCH' was a /p-code relative/ offset
  | PcodeRelativeBranch
  deriving (Eq, Ord, Show, Generic, Hashable)

data PCodeOpToPilStmtConversionError =
  PCodeOpToPilStmtConversionError
    { address :: Address
    , function :: Function
    , failedOp :: P.PcodeOp Text
    , conversionError :: ConverterError
    }
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
    -- | A mapping of PilVars to the a variable from the import source.
  , sourceVars :: HashMap PilVar VarNode
  , ghidraState :: GhidraState
  }
  deriving (Eq, Ord, Show, Generic)

mkConverterState :: GhidraState -> Ctx -> ConverterState
mkConverterState gs ctx = ConverterState
  { ctxStack = NE.singleton ctx
  , ctx = ctx
  , definedVars = []
  , usedVars = HashSet.empty
  , sourceVars = HashMap.empty
  , ghidraState = gs
  }

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: StateT ConverterState IO a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO)

runConverter
  :: Converter a
  -> ConverterState
  -> IO (a, ConverterState)
runConverter m s = flip runStateT s $ _runConverter m

class IsVariable a where
  getSize :: a -> Bytes
  getVarType :: a -> VarType

instance IsVariable HighVarNode where
  getSize = view #size
  getVarType = view #varType

instance IsVariable VarNode where
  getSize = view #size
  getVarType = view #varType

instance IsVariable a => IsVariable (P.Output a) where
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
  -- | VConstAddr Int64
  | VExtern Int64
  | VImmediate Int64
  | VOther Text
  deriving (Eq, Ord, Read, Show, Generic, Hashable)

getVarNodeType :: IsVariable a => a -> VarNodeType
getVarNodeType v = case getVarType v of
  GVar.Const n -> VImmediate n
  GVar.Addr x -> case x ^. #space . #name of
    GAddr.EXTERNAL -> VExtern off
    GAddr.HASH -> VOther "HASH"
    GAddr.Const -> error "Got a varnode that was not .isConstant() but whose address space was 'const'"
    GAddr.Ram -> VRam off
    GAddr.Register -> VReg off
    GAddr.Stack -> VStack off
    GAddr.Unique -> VUnique off
    GAddr.Other t -> VOther t
    where
      off = x ^. #offset


-- The point of these individual funcs instead of a single `VarNode -> Expression`
-- func is so you can choose only the correct possibility and `<|>` them together
getConstIntExpr :: IsVariable a => a -> Maybe Expression
getConstIntExpr v = case getVarNodeType v of
  VImmediate n -> return . mkExpr v . Pil.CONST . Pil.ConstOp $ n
  _ -> Nothing

getConstPtrExpr :: IsVariable a => a -> Maybe Expression
getConstPtrExpr v = case getVarNodeType v of
  VImmediate n -> return . mkExpr v . Pil.CONST_PTR . Pil.ConstPtrOp $ n
  VRam n -> return . mkExpr v . Pil.CONST_PTR . Pil.ConstPtrOp $ n
  _ -> Nothing

getExternPtrExpr :: IsVariable a => a -> Maybe Expression
getExternPtrExpr v = case getVarNodeType v of
  -- TODO: figure out what to put for address and offset of ExternPtrOp
  VExtern n -> return . mkExpr v . Pil.ExternPtr $  Pil.ExternPtrOp 0 (fromIntegral n) Nothing
  _ -> Nothing

getPtrExpr :: IsVariable a => a -> Maybe Expression
getPtrExpr v = getExternPtrExpr v <|> getConstPtrExpr v

getFloatConstExpr :: IsVariable a => a -> Maybe Expression
getFloatConstExpr v = case getVarNodeType v of
  VImmediate n ->
    -- TODO: make sure this is the proper way to convert const from ghidra to floats
    -- TODO: see if Ghidra will do this for us, since it's arch dependent.
    return . mkExpr v . Pil.CONST_FLOAT . Pil.ConstFloatOp . wordToDouble . unsafeCoerce $ n
    
  _ -> Nothing

convertConstFloatOrVar :: IsVariable a => a -> ExceptT ConverterError Converter Expression
convertConstFloatOrVar v = do
  case getFloatConstExpr v of
    Just x -> pure x
    Nothing -> varNodeToValueExpr v

mkExpr :: IsVariable a => a -> Pil.ExprOp Expression -> Expression
mkExpr v = Expression (fromIntegral $ getSize v)

mkExpr' :: Pil.Size Expression -> Pil.ExprOp Expression -> Expression
mkExpr' = Expression

mkAddressExpr :: GAddr.Address -> Expression
mkAddressExpr x = Expression (fromIntegral $ x ^. #space . #ptrSize) . Pil.CONST_PTR . Pil.ConstPtrOp $ x ^. #offset

requireConst :: IsVariable a => a -> ExceptT ConverterError Converter Int64
requireConst v = case getVarType v of
  GVar.Const n -> return n
  GVar.Addr x -> throwError $ ExpectedConstButGotAddress x

requireConstIntExpr :: IsVariable a => a -> ExceptT ConverterError Converter Expression
requireConstIntExpr v = mkExpr v . Pil.CONST . Pil.ConstOp <$> requireConst v

convertDest :: P.Destination -> ExceptT ConverterError Converter Expression
convertDest (P.Absolute addr) = return $ mkAddressExpr addr
convertDest (P.Relative _off) = throwError PcodeRelativeBranch

callDestFromDest :: P.Destination -> Converter (Pil.CallDest Expression)
callDestFromDest (P.Relative _off) =
  -- Calling into the pcode for the same instruction doesn't make much sense
  -- so assume it won't happen, even though the docs say its the same as Branch
  error "Got a realative offset. Expected only Absolute calls"
callDestFromDest (P.Absolute addr) = case addr ^. #space . #name of
  GAddr.EXTERNAL -> return . Pil.CallExtern $ Pil.ExternPtrOp 0 (fromIntegral $ addr ^. #offset) Nothing
  GAddr.Ram -> do
    gs <- use #ghidraState
    let paddr = convertAddress addr
    liftIO (GCG.getFunction gs paddr) >>= \case
      Just func -> return . Pil.CallFunc $ func
      Nothing -> return . Pil.CallExpr $ mkAddressExpr addr
  GAddr.Const -> do
    let paddr = convertAddress addr
    return . Pil.CallAddr $ Pil.ConstFuncPtrOp paddr Nothing
  _ -> return . Pil.CallExpr $ mkAddressExpr addr

-- | Converts a Ghidra VarNode to either a 'PilVar' or 'Expression' that
-- represents the __abstract location__ that the VarNode specifies. If a
-- 'PilVar' is returned, then it can be used as the left-hand-side of a 'Def';
-- if an 'Expression' is returned, then it can be used as the left-hand-side of
-- a 'Store'. Throws 'ExpectedAddressButGotConst' if the VarNode was a constant,
-- or 'UnsuportedAddressSpace' if it resides in some custom address space that
-- we don't know how to deal with.
varNodeToReference :: IsVariable a => a -> ExceptT ConverterError Converter (Either PilVar Expression)
varNodeToReference v = do
  ctx' <- use #ctx
  let pv = C.pilVar_ (fromByteBased size) $ Just ctx'
      size :: Bytes = getSize v
      operSize :: Size Expression = Pil.widthToSize $ toBits size
  case getVarNodeType v of
    VReg n -> pure . Left . pv $ "reg_" <> showHex n <> "_" <> showHex size
    VStack n -> pure . Left . pv $ stackVarName n
    VUnique n -> pure . Left . pv $ "unique_" <> showHex n
    -- XXX We might need to scale these by addressable unit size
    VRam n -> pure . Right $ C.constPtr (fromIntegral n :: Word64) operSize
    -- XXX We might need to scale these by addressable unit size
    VExtern n -> pure . Right $ C.externPtr 0 (fromIntegral n :: ByteOffset) Nothing operSize
    VImmediate n -> throwError $ ExpectedAddressButGotConst n
    VOther t -> throwError $ UnsuportedAddressSpace t
  where
    stackVarName n = (if n < 0 then "var_" else "arg_") <> showHex (abs n)
    showHex n = Text.pack $ Numeric.showHex n ""

-- | Like 'varNodeToReference' but throw 'VarNodeInvalidAsPilVar' if an
-- 'Expression' was returned instead of a 'PilVar'
varNodeToPilVar :: IsVariable a => a -> ExceptT ConverterError Converter PilVar
varNodeToPilVar v =
  varNodeToReference v >>= \case
    Left pv -> pure pv
    Right _ -> throwError $ VarNodeInvalidAsPilVar (getSize v) (getVarType v)

-- | Like 'varNodeToReference', but returns either a partially applied 'Store'
-- or a partially applied 'Def'
varNodeToAssignment :: IsVariable a => a -> ExceptT ConverterError Converter (Expression -> Pil.Stmt)
varNodeToAssignment v =
  varNodeToReference v <&> \case
    Left pv -> Pil.Def . Pil.DefOp pv
    Right expr -> Pil.Store . Pil.StoreOp expr

-- | Converts a Ghidra VarNode to an 'Expression' that represents the __value
-- stored at__ the abstract location that the VarNode specifies. Throws
-- 'ExpectedAddressButGotConst' if the VarNode was a constant, or
-- 'UnsuportedAddressSpace' if it resides in some custom address space that we
-- don't know how to deal with.
varNodeToValueExpr :: IsVariable a => a -> ExceptT ConverterError Converter Expression
varNodeToValueExpr v = do
  ctx' <- use #ctx
  let pv = C.pilVar_ (fromByteBased size) $ Just ctx'
      size :: Bytes = getSize v
      operSize :: Size Expression = Pil.widthToSize $ toBits size
  case getVarNodeType v of
    VReg n -> pure $ C.var' (pv $ "reg_" <> showHex n <> "_" <> showHex size) operSize
    VStack n -> pure $ C.var' (pv $ stackVarName n) operSize
    VUnique n -> pure $ C.var' (pv $ "unique_" <> showHex n) operSize
    -- XXX We might need to scale these by addressable unit size
    VRam n -> pure $ C.load (C.constPtr (fromIntegral n :: Word64) (Pil.widthToSize (64 :: Bits))) operSize
    -- XXX We might need to scale these by addressable unit size
    VExtern n -> pure $ C.load (C.externPtr 0 (fromIntegral n :: ByteOffset) Nothing (Pil.widthToSize (64 :: Bits))) operSize
    VImmediate n -> pure $ C.const n operSize
    VOther t -> throwError $ UnsuportedAddressSpace t
  where
    stackVarName n = (if n < 0 then "var_" else "arg_") <> showHex (abs n)
    showHex n = Text.pack $ Numeric.showHex n ""

convertPcodeOpToPilStmt :: forall a. IsVariable a => P.PcodeOp a -> ExceptT ConverterError Converter [Pil.Stmt]
convertPcodeOpToPilStmt = \case
  P.BOOL_AND out in0 in1 -> mkDef out =<< binIntOp Pil.AND Pil.AndOp in0 in1
  P.BOOL_NEGATE out in0 -> mkDef out =<< unIntOp Pil.NOT Pil.NotOp in0
  P.BOOL_OR out in0 in1 -> mkDef out =<< binIntOp Pil.OR Pil.OrOp in0 in1
  P.BOOL_XOR out in0 in1 -> mkDef out =<< binIntOp Pil.XOR Pil.XorOp in0 in1
  -- TODO: Handle p-code relative jumps
  P.BRANCH dest -> pure . Pil.Jump . Pil.JumpOp <$> convertDest (dest ^. #value)
  -- Branch indirect. Var contains offset from current instr.
  -- Offset is in context of current addr space
  -- TODO: maybe should use `JumpTo instrAddr [off]`
  -- P.BRANCHIND in0 -> Pil.Jump . Pil.JumpOp <$> requireVarExpr (in0 ^. #value)
  P.BRANCHIND _in0 -> pure [Pil.UnimplInstr "BRANCHIND"]
  P.CALL dest inputs -> do
    cdest <- lift $ callDestFromDest $ dest ^. #value
    params <-  mapM varNodeToValueExpr . fmap (view #value) $ inputs
    pure . (: []) . Pil.Call $ Pil.CallOp cdest Nothing params
  P.CALLIND in0 inputs -> do
    dest <- varNodeToValueExpr in0
    params <- mapM varNodeToValueExpr . fmap (view #value) $ inputs
    pure . (: []) . Pil.Call $ Pil.CallOp (Pil.CallExpr dest) Nothing params

  -- TODO CALLOTHER is pretty much a black-box
  P.CALLOTHER _in0 _inputs -> pure [Pil.UnimplInstr "CALLOTHER"]
  -- TODO do we want to record CASTs into PIL somehow?
  P.CAST out in0 -> mkDef out . view #op =<< varNodeToValueExpr in0
  P.CBRANCH _dest in0 -> do
    cond <- varNodeToValueExpr $ in0 ^. #value
    -- Ignore the dest for now, as it gets encoded in the CFG edges.
    pure . (: []) . Pil.BranchCond . Pil.BranchCondOp $ cond
  P.COPY out in0 ->
    (: []) <$> (varNodeToAssignment out <*> varNodeToValueExpr (in0 ^. #value))
  P.CPOOLREF _out _in0 _in1 _inputs -> pure [Pil.UnimplInstr "CPOOLREF"]
  P.EXTRACT out in0 in1 -> do -- NOT in docs. guessing `Extract dest src offset
    srcExpr <- varNodeToValueExpr in0
    offsetExpr <- requireConst in1
    mkDef out . Pil.Extract $ Pil.ExtractOp srcExpr offsetExpr
  P.FLOAT_ABS out in0 -> mkDef out =<< unFloatOp Pil.FABS Pil.FabsOp in0
  P.FLOAT_ADD out in0 in1 -> mkDef out =<< binFloatOp Pil.FADD Pil.FaddOp in0 in1
  P.FLOAT_CEIL out in0 -> mkDef out =<< unFloatOp Pil.CEIL Pil.CeilOp in0
  P.FLOAT_DIV out in0 in1 -> mkDef out =<< binFloatOp Pil.FDIV Pil.FdivOp in0 in1
  P.FLOAT_EQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_E Pil.FcmpEOp in0 in1
  P.FLOAT2FLOAT out in0 -> mkDef out =<< unFloatOp Pil.FLOAT_CONV Pil.FloatConvOp in0
  P.FLOAT_FLOOR out in0 -> mkDef out =<< unFloatOp Pil.FLOOR Pil.FloorOp in0
  P.INT2FLOAT out in0 -> mkDef out =<< unFloatOp Pil.INT_TO_FLOAT Pil.IntToFloatOp in0
  P.FLOAT_LESS out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_LT Pil.FcmpLtOp in0 in1
  P.FLOAT_LESSEQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_LE Pil.FcmpLeOp in0 in1
  P.FLOAT_MULT out in0 in1 -> mkDef out =<< binFloatOp Pil.FMUL Pil.FmulOp in0 in1
  P.FLOAT_NAN out in0 -> mkDef out =<< binFloatOp Pil.FCMP_UO Pil.FcmpUoOp in0 in0
  P.FLOAT_NEG out in0 -> mkDef out =<< unFloatOp Pil.FNEG Pil.FnegOp in0
  P.FLOAT_NOTEQUAL out in0 in1 -> mkDef out =<< binFloatOp Pil.FCMP_NE Pil.FcmpNeOp in0 in1
  P.FLOAT_ROUND out in0 -> mkDef out =<< unFloatOp Pil.ROUND_TO_INT Pil.RoundToIntOp in0
  P.FLOAT_SQRT out in0 -> mkDef out =<< unFloatOp Pil.FSQRT Pil.FsqrtOp in0
  P.FLOAT_SUB out in0 in1 -> mkDef out =<< binFloatOp Pil.FSUB Pil.FsubOp in0 in1
  P.TRUNC out in0 -> mkDef out =<< unFloatOp Pil.FTRUNC Pil.FtruncOp in0
  P.INDIRECT _out _in0 _in1 -> pure [Pil.UnimplInstr "INDIRECT"]
  P.INSERT _out _in0 _in1 _position _size -> pure [Pil.UnimplInstr "INSERT"]
  -- P.INSERT out in0 in1 position size -> do
  --   in0' <- requirePilVar in0
  --   in1' <- varNodeToValueExpr in1
  --   let size' = fromIntegral (size ^. #value)
  --       position' = fromIntegral (position ^. #value)
  --       truncated = Pil.Expression size' . Pil.LOW_PART $ Pil.LowPartOp in1'
  --       updated = Pil.UPDATE_VAR $ Pil.UpdateVarOp in0' position' truncated
  --   mkDef out updated
  P.INT_2COMP out in0 -> mkDef out =<< unIntOp Pil.NEG Pil.NegOp in0
  P.INT_ADD out in0 in1 -> mkDef out =<< binIntOp Pil.ADD Pil.AddOp in0 in1
  P.INT_AND out in0 in1 -> mkDef out =<< binIntOp Pil.AND Pil.AndOp in0 in1
  P.INT_CARRY out in0 in1 -> mkDef out =<< binIntOp Pil.ADD_WILL_CARRY Pil.AddWillCarryOp in0 in1
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
  P.INT_SBORROW out in0 in1 -> mkDef out =<< binIntOp Pil.SUB_WILL_OVERFLOW Pil.SubWillOverflowOp in0 in1
  P.INT_SCARRY out in0 in1 -> mkDef out =<< binIntOp Pil.ADD_WILL_OVERFLOW Pil.AddWillOverflowOp in0 in1
  P.INT_SDIV out in0 in1 -> mkDef out =<< binIntOp Pil.DIVS Pil.DivsOp in0 in1
  P.INT_SEXT out in0 -> mkDef out =<< unIntOp Pil.SX Pil.SxOp in0
  P.INT_SLESS out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_SLT Pil.CmpSltOp in0 in1
  P.INT_SLESSEQUAL out in0 in1 -> mkDef out =<< binIntOp Pil.CMP_SLE Pil.CmpSleOp in0 in1
  P.INT_SREM out in0 in1 -> mkDef out =<< binIntOp Pil.MODS Pil.ModsOp in0 in1
  P.INT_SRIGHT out in0 in1 -> mkDef out =<< binIntOp Pil.ASR Pil.AsrOp in0 in1
  P.INT_SUB out in0 in1 -> mkDef out =<< binIntOp Pil.SUB Pil.SubOp in0 in1
  P.INT_XOR out in0 in1 -> mkDef out =<< binIntOp Pil.XOR Pil.XorOp in0 in1
  P.INT_ZEXT out in0 -> mkDef out =<< unIntOp Pil.ZX Pil.ZxOp in0
  P.LOAD out _addrSpace in1 -> do
    offset <- mkExpr in1 <$> unIntOp Pil.LOAD Pil.LoadOp in1
    -- TODO: we need to make use of the address space during the second LOAD. How?
    let target = Pil.LOAD $ Pil.LoadOp offset
    mkDef out target
  P.MULTIEQUAL out in0 in1 rest -> do
    -- TODO: memory phi statements are just silently ignored here
    flip catchError (\_ -> pure []) $ do
      pout <- varNodeToPilVar out
      pins <- traverse varNodeToPilVar (in0:in1:rest)
      pure [Pil.DefPhi . Pil.DefPhiOp pout $ pins]
  P.NEW _out _in0 _inputs -> pure [Pil.UnimplInstr "NEW"]
  P.PCODE_MAX -> pure [Pil.UnimplInstr "PCODE_MAX"]
  P.PIECE out high low -> do
    -- out := (high << low.size) | low
    high' <- varNodeToValueExpr high
    low' <- varNodeToValueExpr low
    let outSize = fromIntegral (getSize out)
        lowSize = low' ^. #size
        highSize = high' ^. #size
    when (outSize /= lowSize + highSize) $
      throwError PieceOperandsIncorrectSizes{outputSize=outSize, highArg=high', lowArg=low'}
    let highShifted = Pil.LSL $ Pil.LslOp high' (Expression 8 . Pil.CONST . Pil.ConstOp $ fromIntegral lowSize)
        lowExtended = Pil.ZX . Pil.ZxOp $ low'
        res = Pil.OR $ Pil.OrOp (mkExpr out highShifted) (mkExpr out lowExtended)
    mkDef out res
  P.POPCOUNT out in0 -> mkDef out =<< unIntOp Pil.POPCNT Pil.PopcntOp in0
  P.PTRADD out base idx stride ->
    let stride' = fromIntegral $ stride ^. #value in
      mkDef out =<< binIntOp Pil.ARRAY_ADDR (\b i -> Pil.ArrayAddrOp b i stride') base idx
  P.PTRSUB out base offset -> do
    -- out := (void *)base + offset
    base' <- varNodeToValueExpr base
    offset' <- case getVarNodeType offset of
                 VImmediate n -> pure n
                 vnt -> throwError $ PtrsubOffsetNotConstant vnt
    mkDef out . Pil.FIELD_ADDR $ Pil.FieldAddrOp base' (fromIntegral offset')
  P.RETURN _ [] -> pure [C.ret C.unit]
  P.RETURN _retAddr [result] -> (: []) . Pil.Ret . Pil.RetOp <$> varNodeToValueExpr result
  P.RETURN _ (_:_:_) -> throwError ReturningTooManyResults
  P.SEGMENTOP -> pure [Pil.UnimplInstr "SEGMENTOP"]
  --- XXX We need to utilize the addrSpace
  P.STORE _addrSpace destOffset in1 -> do
    --- XXX we have to scale this offset by the address space addressable unit size
    destOffset' <- varNodeToValueExpr destOffset
    in1' <- varNodeToValueExpr in1
    -- XXX we need to make use of the address space during the Store. How?
    pure . (: []) . Pil.Store $ Pil.StoreOp destOffset' in1'
  P.SUBPIECE out in0 lowOff -> do
    -- out := (in0 >> lowOff) & ((1 << out.size) - 1)
    in0' <- varNodeToValueExpr in0
    let shiftedSize = (in0' ^. #size) - fromIntegral (lowOff ^. #value)
        shifted = Expression shiftedSize . Pil.LSR $ Pil.LsrOp in0' (Expression 8 . Pil.CONST . Pil.ConstOp $ fromIntegral (lowOff ^. #value))
        truncated = Pil.LOW_PART . Pil.LowPartOp $ shifted
    mkDef out truncated
  P.UNIMPLEMENTED ->
    -- TODO: grab the disassembly for the unimplemented instruction
    pure [Pil.UnimplInstr "unimpl"]

  where
    mkDef :: P.Output a -> Pil.ExprOp Expression -> ExceptT ConverterError Converter [Pil.Stmt]
    mkDef v xop = do
      assignment <- varNodeToAssignment v
      return . (: []) . assignment $ Expression (fromIntegral $ getSize v) xop

    unIntOp :: forall b.
               (b -> Pil.ExprOp Expression)
             -> (Expression -> b)
             -> P.Input a
             -> ExceptT ConverterError Converter (Pil.ExprOp Expression)
    unIntOp opCons opArgsCons in0 = do
      a <- varNodeToValueExpr in0
      return . opCons $ opArgsCons a

    binIntOp :: forall b.
               (b -> Pil.ExprOp Expression)
             -> (Expression -> Expression -> b)
             -> P.Input a
             -> P.Input a
             -> ExceptT ConverterError Converter (Pil.ExprOp Expression)
    binIntOp opCons opArgsCons in0 in1 = do
      a <- varNodeToValueExpr in0
      b <- varNodeToValueExpr in1
      return . opCons $ opArgsCons a b

    binFloatOp :: forall b.
                  (b -> Pil.ExprOp Expression)
               -> (Expression -> Expression -> b)
               -> P.Input a
               -> P.Input a
               -> ExceptT ConverterError Converter (Pil.ExprOp Expression)
    binFloatOp opCons opArgsCons in0 in1 = do
      a <- convertConstFloatOrVar in0
      b <- convertConstFloatOrVar in1
      return . opCons $ opArgsCons a b

    unFloatOp :: forall b.
                    (b -> Pil.ExprOp Expression)
                 -> (Expression -> b)
                 -> P.Input a
                 -> ExceptT ConverterError Converter (Pil.ExprOp Expression)
    unFloatOp opCons opArgsCons in0 = do
      a <- convertConstFloatOrVar in0
      return . opCons $ opArgsCons a
    
getFuncStatementsFromRawPcode :: GhidraState -> Function -> CtxId -> IO [Either ConverterError Pil.Stmt]
getFuncStatementsFromRawPcode gs func ctxId = do
  jfunc <- GCG.toGhidraFunction gs func
  let ctx = Ctx func ctxId
  addrSpaceMap <- getAddressSpaceMap gs
  pcodeOps <- fmap snd <$> P.getRawPcode gs addrSpaceMap jfunc
  convertPcodeOps gs ctx pcodeOps

getFuncStatementsFromHighPcode :: GhidraState -> Function -> CtxId -> IO [Either ConverterError Pil.Stmt]
getFuncStatementsFromHighPcode gs func ctxId = do
  jfunc <- GCG.toGhidraFunction gs func
  hfunc <- GFunc.getHighFunction gs jfunc
  let ctx = Ctx func ctxId
  addrSpaceMap <- getAddressSpaceMap gs
  pcodeOps <- fmap snd <$> P.getHighPcode gs addrSpaceMap hfunc jfunc
  convertPcodeOps gs ctx pcodeOps

convertPcodeOps :: IsVariable a => GhidraState -> Ctx -> [P.PcodeOp a] -> IO [Either ConverterError Pil.Stmt]
convertPcodeOps gs ctx pcodeOps = do
  let cstate = mkConverterState gs ctx
  runConverter (traverse (swallowErrors . convertPcodeOpToPilStmt) pcodeOps) cstate >>= \case
    (stmts, _st) -> return $ concat stmts
  where
    -- tryError :: ExceptT e m a -> ExceptT e m (Either e a)
    -- tryError action = catchError (Right <$> action) (pure . Left)
    swallowErrors :: ExceptT ConverterError Converter [a] -> Converter [Either ConverterError a]
    swallowErrors a = runExceptT a <&> \case
      Left err -> [Left err]
      Right stmts -> Right <$> stmts


getCodeRefStatementsFromRawPcode
  :: GhidraState
  -> CtxId
  -> CodeReference Address
  -> IO [Either ConverterError Pil.Stmt]
getCodeRefStatementsFromRawPcode gs ctxId ref = do
  let ctx = Ctx (ref ^. #function) ctxId
  addrSpaceMap <- getAddressSpaceMap gs
  start <- GState.mkAddress gs $ ref ^. #startIndex
  end <- GState.mkAddress gs $ ref ^. #endIndex
  addrSet <- J.mkAddressSetFromRange start end
  pcodeOps <- fmap snd <$> P.getRawPcode gs addrSpaceMap addrSet
  convertPcodeOps gs ctx pcodeOps

getCodeRefStatementsFromHighPcode
  :: GhidraState
  -> CtxId
  -> CodeReference Address
  -> IO [Either ConverterError Pil.Stmt]
getCodeRefStatementsFromHighPcode gs ctxId ref = do
  jfunc <- GCG.toGhidraFunction gs $ ref ^. #function
  hfunc <- GFunc.getHighFunction gs jfunc
  let ctx = Ctx (ref ^. #function) ctxId
  addrSpaceMap <- getAddressSpaceMap gs
  start <- GState.mkAddress gs $ ref ^. #startIndex
  end <- GState.mkAddress gs $ ref ^. #endIndex
  addrSet <- J.mkAddressSetFromRange start end
  pcodeOps <- fmap snd <$> P.getHighPcode gs addrSpaceMap hfunc addrSet
  convertPcodeOps gs ctx pcodeOps

  

