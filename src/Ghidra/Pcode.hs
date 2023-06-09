{-# LANGUAGE DataKinds #-}
module Ghidra.Pcode (
  module Ghidra.Pcode,
  module Ghidra.Types.Pcode,
)
where

import Ghidra.Prelude hiding (toList, getConst)


import Ghidra.State (GhidraState)
import qualified Language.Java as Java
import Ghidra.Instruction (getInstructions)
import qualified Ghidra.Instruction as Instr
import Ghidra.Util (iteratorToList, isJNull, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Pcode
import qualified Ghidra.Variable as Var
import Ghidra.Variable (VarNode, HighVarNode)
import qualified Ghidra.Types.Pcode.Lifted as L
import Ghidra.Types.Pcode.Lifted (PcodeOp, Input(Input), Output(Output), Destination, LiftPcodeError(..))
import Ghidra.Types.Address (AddressSpace, AddressSpaceMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Foreign.JNI as JNI


getPcode :: J.Instruction -> IO [J.PcodeOp]
getPcode x = Java.call x "getPcode" >>= Java.reify >>= traverse JNI.newGlobalRef

getRawPcodeOps :: J.Addressable a => GhidraState -> a -> IO [(J.Address, J.PcodeOp)]
getRawPcodeOps gs x = do
  instrs <- getInstructions gs x
  flip concatMapM instrs $ \instr -> do
    addr <- Instr.getAddress instr
    fmap (addr,) <$> getPcode instr

getPcodeOpAST :: J.HighFunction -> J.Instruction -> IO [J.PcodeOpAST]
getPcodeOpAST hfunc instr = do
  addr <- J.toAddr instr
  iter :: J.Iterator J.PcodeOpAST <- Java.call hfunc "getPcodeOps" addr >>= JNI.newGlobalRef
  iteratorToList iter

getHighPcodeOps :: J.Addressable a => GhidraState -> J.HighFunction -> a -> IO [(J.Address, J.PcodeOpAST)]
getHighPcodeOps gs hfunc x = do
  instrs <- getInstructions gs x
  flip concatMapM instrs $ \instr -> do
    addr <- Instr.getAddress instr
    fmap (addr,) <$> getPcodeOpAST hfunc instr

getBarePcodeOp :: Coercible a J.PcodeOp => a -> IO BarePcodeOp
getBarePcodeOp x = do
  s :: Text <- Java.call x' "getMnemonic" >>= Java.reify
  case readMaybe s :: Maybe BarePcodeOp of
    Nothing -> error $ "Can't convert pcode op: " <> show s
    Just op -> return op
  where
    x' :: J.PcodeOp
    x' = coerce x

mkBareRawPcodeInstruction :: J.PcodeOp -> IO BareRawPcodeInstruction
mkBareRawPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> (maybeNull <$> (Java.call x "getOutput" >>= JNI.newGlobalRef))
  <*> (Java.call x "getInputs" >>= Java.reify >>= traverse JNI.newGlobalRef)

getHighOutput :: J.PcodeOpAST -> IO (Maybe J.VarNodeAST)
getHighOutput x = do
  vnode :: J.VarNode <- Java.call (coerce x :: J.PcodeOp) "getOutput" >>= JNI.newGlobalRef
  if isJNull vnode then return Nothing else return . Just $ coerce vnode

getHighInputs :: J.PcodeOpAST -> IO [J.VarNodeAST]
getHighInputs x = do
  vnodes :: [J.VarNode] <- Java.call (coerce x :: J.PcodeOp) "getInputs" >>= Java.reify
  return $ coerce <$> vnodes

mkBareHighPcodeInstruction :: J.PcodeOpAST -> IO BareHighPcodeInstruction
mkBareHighPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> getHighOutput x
  -- <*> (maybeNull <$> Java.call x "getOutput")
  <*> getHighInputs x -- (Java.call x "getInputs" >>= Java.reify)

mkHighPcodeInstruction :: BareHighPcodeInstruction -> IO HighPcodeInstruction
mkHighPcodeInstruction = traverse Var.mkHighVarNode

mkRawPcodeInstruction :: BareRawPcodeInstruction -> IO RawPcodeInstruction
mkRawPcodeInstruction = traverse Var.mkVarNode

class CanBeDestination a where
  toDestination :: a -> Destination

instance CanBeDestination Var.VarType where
  toDestination (Var.Const n) = L.Relative n
  toDestination (Var.Addr addr) = L.Absolute addr

instance CanBeDestination VarNode where
  toDestination = toDestination . view #varType

instance CanBeDestination HighVarNode where
  toDestination = toDestination . view #varType

class GetConst a where
  getConst :: a -> Maybe Int64

instance GetConst Var.VarType where
  getConst (Var.Const n) = Just n
  getConst (Var.Addr _) = Nothing

instance GetConst VarNode where
  getConst = getConst . view #varType

instance GetConst HighVarNode where
  getConst = getConst . view #varType

liftPcodeInstruction
  :: forall a. (CanBeDestination a, GetConst a)
  => AddressSpaceMap
  -> PcodeInstruction a
  -> Either LiftPcodeError (PcodeOp a)
liftPcodeInstruction addressSpaceMap x = first (LiftInstructionError (x ^. #op)) $ case x ^. #op of
  BOOL_AND -> binOp L.BOOL_AND
  BOOL_NEGATE -> unOp L.BOOL_NEGATE
  BOOL_OR -> binOp L.BOOL_OR
  BOOL_XOR -> binOp L.BOOL_XOR
  BRANCH -> L.BRANCH <$> inputDest 0
  BRANCHIND -> L.BRANCHIND <$> input 0
  CALL -> L.CALL <$> inputDest 0 <*> inputList 1
  CALLIND -> L.CALLIND <$> input 0 <*> inputList 1
  CALLOTHER -> unknown "CALLOTHER" -- L.CALLOTHER <$> input 0 <*> inputList 1
  CAST -> unOp L.CAST
  CBRANCH -> L.CBRANCH <$> inputDest 0 <*> input 1
  COPY -> unOp L.COPY
  CPOOLREF -> L.CPOOLREF <$> out <*> input 0 <*> input 1 <*> inputList 2
  EXTRACT -> unknown "EXTRACT" -- binOp L.EXTRACT
  FLOAT_ABS -> unOp L.FLOAT_ABS
  FLOAT_ADD -> binOp L.FLOAT_ADD
  FLOAT_CEIL -> unOp L.FLOAT_CEIL
  FLOAT_DIV -> binOp L.FLOAT_DIV
  FLOAT_EQUAL -> binOp L.FLOAT_EQUAL
  FLOAT2FLOAT -> unOp L.FLOAT2FLOAT
  FLOAT_FLOOR -> unOp L.FLOAT_FLOOR
  INT2FLOAT -> unOp L.INT2FLOAT
  FLOAT_LESS -> binOp L.FLOAT_LESS
  FLOAT_LESSEQUAL -> binOp L.FLOAT_LESSEQUAL
  FLOAT_MULT -> binOp L.FLOAT_MULT
  FLOAT_NAN -> unOp L.FLOAT_NAN
  FLOAT_NEG -> unOp L.FLOAT_NEG
  FLOAT_NOTEQUAL -> binOp L.FLOAT_NOTEQUAL
  FLOAT_ROUND -> unOp L.FLOAT_ROUND
  FLOAT_SQRT -> unOp L.FLOAT_SQRT
  FLOAT_SUB -> binOp L.FLOAT_SUB
  TRUNC -> unOp L.TRUNC
  INDIRECT -> binOp L.INDIRECT
  INSERT -> L.INSERT <$> out <*> input 0 <*> input 1 <*> inputConst 2 <*> inputConst 3
  INT_2COMP -> unOp L.INT_2COMP
  INT_ADD -> binOp L.INT_ADD
  INT_AND -> binOp L.INT_AND
  INT_CARRY -> binOp L.INT_CARRY
  INT_DIV -> binOp L.INT_DIV
  INT_EQUAL -> binOp L.INT_EQUAL
  INT_LEFT -> binOp L.INT_LEFT
  INT_LESS -> binOp L.INT_LESS
  INT_LESSEQUAL -> binOp L.INT_LESSEQUAL
  INT_MULT -> binOp L.INT_MULT
  INT_NEGATE -> unOp L.INT_NEGATE
  INT_NOTEQUAL -> binOp L.INT_NOTEQUAL
  INT_OR -> binOp L.INT_OR
  INT_REM -> binOp L.INT_REM
  INT_RIGHT -> binOp L.INT_RIGHT
  INT_SBORROW -> binOp L.INT_SBORROW
  INT_SCARRY -> binOp L.INT_SCARRY
  INT_SDIV -> binOp L.INT_SDIV
  INT_SEXT -> unOp L.INT_SEXT
  INT_SLESS -> binOp L.INT_SLESS
  INT_SLESSEQUAL -> binOp L.INT_SLESSEQUAL
  INT_SREM -> binOp L.INT_SREM
  INT_SRIGHT -> binOp L.INT_SRIGHT
  INT_SUB -> binOp L.INT_SUB
  INT_XOR -> binOp L.INT_XOR
  INT_ZEXT -> unOp L.INT_ZEXT
  LOAD -> L.LOAD <$> out <*> inputAddressSpace 0 <*> input 1
  MULTIEQUAL -> L.MULTIEQUAL <$> out <*> input 0 <*> input 1 <*> inputList 2
  NEW -> L.NEW <$> out <*> input 0 <*> inputList 1
  PCODE_MAX -> unknown "PCODE_MAX"
  PIECE -> binOp L.PIECE
  POPCOUNT -> unOp L.POPCOUNT
  PTRADD -> L.PTRADD <$> out <*> input 0 <*> input 1 <*> inputConst 2
  PTRSUB -> binOp L.PTRSUB
  RETURN -> L.RETURN <$> input 0 <*> inputList 1
  SEGMENTOP -> unknown "SEGMENTOP"
  STORE -> L.STORE <$> inputAddressSpace 0 <*> input 1 <*> input 2
  SUBPIECE -> L.SUBPIECE <$> out <*> input 0 <*> inputConst 1
  UNIMPLEMENTED -> return L.UNIMPLEMENTED
  where
    me = maybeToEither

    out :: Either LiftPcodeError (Output a)
    out = Output <$> me OutputNotFound (x ^. #output)

    input :: Word64 -> Either LiftPcodeError (Input a)
    input n = Input n <$> me (ArgNotFound n) (x ^? #inputs . ix (fromIntegral n ))

    inputList :: Word64 -> Either LiftPcodeError [Input a]
    inputList startingInput = traverse input
      . drop (fromIntegral startingInput)
      . fmap fst
      . zip [0..]
      $ x ^. #inputs

    getConst' = me VarNotConst . getConst

    inputDest :: Word64 -> Either LiftPcodeError (Input Destination)
    inputDest n = fmap toDestination <$> input n

    inputConst :: Word64 -> Either LiftPcodeError (Input Int64)
    inputConst n = input n >>= traverse getConst'

    inputAddressSpace :: Word64 -> Either LiftPcodeError (Input AddressSpace)
    inputAddressSpace n = input n >>= traverse f
      where
        f :: a -> Either LiftPcodeError AddressSpace
        f a = do
          spaceId <- fromIntegral <$> getConst' a
          me (CannotFindAddressSpace n spaceId) $ HashMap.lookup spaceId addressSpaceMap

    binOp :: (Output a -> Input a -> Input a -> PcodeOp a)
          -> Either LiftPcodeError (PcodeOp a)
    binOp constructor = constructor <$> out <*> input 0 <*> input 1

    unOp :: (Output a -> Input a -> PcodeOp a)
         -> Either LiftPcodeError (PcodeOp a)
    unOp constructor = constructor <$> out <*> input 0

    unknown :: forall b. Text -> Either LiftPcodeError b
    unknown = Left . UnknownOp

getRawPcode
  :: J.Addressable a
  => GhidraState
  -> AddressSpaceMap
  -> a
  -> IO [(J.Address, PcodeOp VarNode)]
getRawPcode gs addressSpaceMap a = do
  jpcodes <- getRawPcodeOps gs a
  rawInstrs :: [(J.Address, RawPcodeInstruction)] <- traverse (traverse $ mkRawPcodeInstruction <=< mkBareRawPcodeInstruction) jpcodes
  let liftedInstrs = traverse (liftPcodeInstruction addressSpaceMap) <$> rawInstrs
      (errs, instrs) = foldr separateError ([],[]) liftedInstrs
  case errs of
    [] -> return instrs
    _ -> error $ "Encountered Pcode decoding errors:\n" <> cs (pshow errs)
  where
    separateError (Right x) (errs, instrs) = (errs, x:instrs)
    separateError (Left err) (errs, instrs) = (err:errs, instrs)

-- | Get the high P-Code instructions that are covered by the address range of
-- 'a'. This is known to return results in a possibly incorrect order if a
-- 'PcodeBlockBasic' is passed as 'a'. To get the high P-Code for a
-- 'PcodeBlockBasic', use 'getBlockHighPcode'. Other types might have similar
-- issues.
getHighPcode
  :: J.Addressable a
  => GhidraState
  -> AddressSpaceMap
  -> J.HighFunction
  -> a
  -> IO [(J.Address, PcodeOp HighVarNode)]
getHighPcode gs addressSpaceMap hfn a = do
  jpcodes <- getHighPcodeOps gs hfn a
  highInstrs :: [(J.Address, HighPcodeInstruction)] <- traverse (traverse $ mkHighPcodeInstruction <=< mkBareHighPcodeInstruction) jpcodes
  let liftedInstrs = traverse (liftPcodeInstruction addressSpaceMap) <$> highInstrs
      (errs, instrs) = foldr separateError ([],[]) liftedInstrs
  case errs of
    [] -> return instrs
    _ -> error $ "Encountered Pcode decoding errors:\n" <> cs (pshow errs)
  where
    separateError (Right x) (errs, instrs) = (errs, x:instrs)
    separateError (Left err) (errs, instrs) = (err:errs, instrs)

getBlockHighPcode
  :: AddressSpaceMap
  -> J.PcodeBlockBasic
  -> IO [(J.Address, PcodeOp HighVarNode)]
getBlockHighPcode addressSpaceMap block = do
  ops :: [J.PcodeOpAST] <- iteratorToList =<< Java.call block "getIterator"
  opsWithAddr :: [(J.Address, J.PcodeOpAST)] <-
    forM ops $ \op -> do
      seqnum :: J.SequenceNumber <- Java.call op "getSeqnum"
      addr <- Java.call seqnum "getTarget"
      pure (addr, op)
  highInstrs :: [(J.Address, HighPcodeInstruction)] <- traverse (traverse $ mkHighPcodeInstruction <=< mkBareHighPcodeInstruction) opsWithAddr
  let liftedInstrs = traverse (liftPcodeInstruction addressSpaceMap) <$> highInstrs
      (errs, instrs) = foldr separateError ([],[]) liftedInstrs
  case errs of
    [] -> return instrs
    _ -> error $ "Encountered Pcode decoding errors:\n" <> cs (pshow errs)
  where
    separateError (Right x) (errs, instrs) = (errs, x:instrs)
    separateError (Left err) (errs, instrs) = (err:errs, instrs)
