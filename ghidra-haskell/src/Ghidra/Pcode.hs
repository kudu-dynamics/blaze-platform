{-# LANGUAGE DataKinds #-}
module Ghidra.Pcode (
  module Ghidra.Pcode,
  module Ghidra.Types.Pcode,
)
where

import Ghidra.Prelude hiding (toList, getConst, Const)


import qualified Language.Java as Java
import Ghidra.Instruction (getInstructions)
import qualified Ghidra.Instruction as Instr
import Ghidra.Util (iteratorToList, isJNull, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Pcode
import qualified Ghidra.Variable as Var
import Ghidra.Variable (VarNode, HighVarNode)
import Ghidra.Types.Variable (HighVarNode(HighVarNode), VarType(Const, Addr))
import qualified Ghidra.Types.Pcode.Lifted as L
import Ghidra.Types.Pcode.Lifted (PcodeOp, Input(Input), Output(Output), Destination, LiftPcodeError(..))
import Ghidra.Address (mkAddress, mkAddressSpace, mkAddressFromParts, Address(Address), AddressSpace(AddressSpace), readAddressSpaceName)
import Ghidra.Types.Address (AddressSpaceMap)
import qualified Data.Vector.Storable as VS
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Data.HashMap.Strict as HashMap
import qualified Foreign.JNI as JNI
import Data.IORef (readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap


-- | Map from Ghidra PcodeOp.getOpcode() int to BarePcodeOp.
-- Values match ghidra.program.model.pcode.PcodeOp constants.
opcodeMap :: IntMap.IntMap BarePcodeOp
opcodeMap = IntMap.fromList
  [ (0, UNIMPLEMENTED), (1, COPY), (2, LOAD), (3, STORE)
  , (4, BRANCH), (5, CBRANCH), (6, BRANCHIND)
  , (7, CALL), (8, CALLIND), (9, CALLOTHER), (10, RETURN)
  , (11, INT_EQUAL), (12, INT_NOTEQUAL)
  , (13, INT_SLESS), (14, INT_SLESSEQUAL), (15, INT_LESS), (16, INT_LESSEQUAL)
  , (17, INT_ZEXT), (18, INT_SEXT)
  , (19, INT_ADD), (20, INT_SUB), (21, INT_CARRY), (22, INT_SCARRY), (23, INT_SBORROW)
  , (24, INT_2COMP), (25, INT_NEGATE)
  , (26, INT_XOR), (27, INT_AND), (28, INT_OR)
  , (29, INT_LEFT), (30, INT_RIGHT), (31, INT_SRIGHT)
  , (32, INT_MULT), (33, INT_DIV), (34, INT_SDIV), (35, INT_REM), (36, INT_SREM)
  , (37, BOOL_NEGATE), (38, BOOL_XOR), (39, BOOL_AND), (40, BOOL_OR)
  , (41, FLOAT_EQUAL), (42, FLOAT_NOTEQUAL), (43, FLOAT_LESS), (44, FLOAT_LESSEQUAL)
  -- 45 unused
  , (46, FLOAT_NAN)
  , (47, FLOAT_ADD), (48, FLOAT_DIV), (49, FLOAT_MULT), (50, FLOAT_SUB)
  , (51, FLOAT_NEG), (52, FLOAT_ABS), (53, FLOAT_SQRT)
  , (54, INT2FLOAT), (55, FLOAT2FLOAT), (56, TRUNC)
  , (57, FLOAT_CEIL), (58, FLOAT_FLOOR), (59, FLOAT_ROUND)
  , (60, MULTIEQUAL), (61, INDIRECT), (62, PIECE), (63, SUBPIECE)
  , (64, CAST), (65, PTRADD), (66, PTRSUB)
  , (67, SEGMENTOP), (68, CPOOLREF), (69, NEW)
  , (70, INSERT), (71, EXTRACT), (72, POPCOUNT)
  , (73, PCODE_MAX)  -- LZCOUNT in newer Ghidra
  ]

lookupOpcode :: Int64 -> Maybe BarePcodeOp
lookupOpcode i = IntMap.lookup (fromIntegral i) opcodeMap

getPcode :: J.Instruction -> Ghidra [J.PcodeOp]
getPcode x = runIO $ Java.call x "getPcode" >>= Java.reify >>= traverse JNI.newGlobalRef

getRawPcodeOps :: J.Addressable a => J.ProgramDB -> a -> Ghidra [(Address, J.PcodeOp)]
getRawPcodeOps prg x = do
  instrs <- getInstructions prg x
  flip concatMapM instrs $ \instr -> do
    addr <- Instr.getAddress instr >>= mkAddress
    fmap (addr,) <$> getPcode instr

getPcodeOpAST :: J.HighFunction -> J.Instruction -> Ghidra [J.PcodeOpAST]
getPcodeOpAST hfunc instr = do
  addr <- J.toAddr instr
  iter :: J.Iterator J.PcodeOpAST <- runIO $ Java.call hfunc "getPcodeOps" addr >>= JNI.newGlobalRef
  iteratorToList iter

-- TODO change this to just use the function's "getPcodeOps" method. Will probably break a bunch of tests
getHighPcodeOps :: J.Addressable a => J.ProgramDB -> J.HighFunction -> a -> Ghidra [(Address, J.PcodeOpAST)]
getHighPcodeOps prg hfunc x = do
  instrs <- getInstructions prg x
  flip concatMapM instrs $ \instr -> do
    addr <- Instr.getAddress instr >>= mkAddress
    fmap (addr,) <$> getPcodeOpAST hfunc instr

getBarePcodeOp :: Coercible a J.PcodeOp => a -> Ghidra BarePcodeOp
getBarePcodeOp x = do
  s :: Text <- runIO $ Java.call x' "getMnemonic" >>= Java.reify
  case readMaybe s :: Maybe BarePcodeOp of
    Nothing -> error $ "Can't convert pcode op: " <> show s
    Just op -> return op
  where
    x' :: J.PcodeOp
    x' = coerce x

mkBareRawPcodeInstruction :: J.PcodeOp -> Ghidra BareRawPcodeInstruction
mkBareRawPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> (maybeNull <$> runIO (Java.call x "getOutput" >>= JNI.newGlobalRef))
  <*> runIO (Java.call x "getInputs" >>= Java.reify >>= traverse JNI.newGlobalRef)

getHighOutput :: J.PcodeOpAST -> Ghidra (Maybe J.VarNodeAST)
getHighOutput x = do
  vnode :: J.VarNode <- runIO $ Java.call (coerce x :: J.PcodeOp) "getOutput" >>= JNI.newGlobalRef
  if isJNull vnode then return Nothing else return . Just $ coerce vnode

getHighInputs :: J.PcodeOpAST -> Ghidra [J.VarNodeAST]
getHighInputs x = do
  vnodes :: [J.VarNode] <- runIO $ Java.call (coerce x :: J.PcodeOp) "getInputs" >>= Java.reify
  return $ coerce <$> vnodes

mkBareHighPcodeInstruction :: J.PcodeOpAST -> Ghidra BareHighPcodeInstruction
mkBareHighPcodeInstruction x = PcodeInstruction
  <$> getBarePcodeOp x
  <*> getHighOutput x
  -- <*> (maybeNull <$> Java.call x "getOutput")
  <*> getHighInputs x -- (Java.call x "getInputs" >>= Java.reify)

mkHighPcodeInstruction :: J.ProgramDB -> BareHighPcodeInstruction -> Ghidra HighPcodeInstruction
mkHighPcodeInstruction prog = traverse $ Var.mkHighVarNode prog

mkHighPcodeInstructionCached :: J.ProgramDB -> Var.HighVarCache -> BareHighPcodeInstruction -> Ghidra HighPcodeInstruction
mkHighPcodeInstructionCached prog cache = traverse $ Var.mkHighVarNodeCached prog cache

mkRawPcodeInstruction :: BareRawPcodeInstruction -> Ghidra RawPcodeInstruction
mkRawPcodeInstruction = traverse Var.mkVarNode

class CanBeDestination a where
  toDestination :: a -> Destination

instance CanBeDestination Var.VarType where
  toDestination (Const n) = L.Relative n
  toDestination (Addr {location}) = L.Absolute location

instance CanBeDestination VarNode where
  toDestination = toDestination . view #varType

instance CanBeDestination HighVarNode where
  toDestination = toDestination . view #varType

class GetConst a where
  getConst :: a -> Maybe Int64

instance GetConst Var.VarType where
  getConst (Const n) = Just n
  getConst (Addr {}) = Nothing

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
  CALL -> L.CALL (Output <$> x ^. #output) <$> inputDest 0 <*> inputList 1
  CALLIND -> L.CALLIND (Output <$> x ^. #output) <$> input 0 <*> inputList 1
  -- CALLOTHER -> unknown "CALLOTHER" -- L.CALLOTHER <$> input 0 <*> inputList 1
  CALLOTHER -> L.CALLOTHER <$> input 0 <*> inputList 1
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
  => J.ProgramDB
  -> AddressSpaceMap
  -> a
  -> Ghidra [(Address, PcodeOp VarNode)]
getRawPcode prg addressSpaceMap a = do
  jpcodes <- getRawPcodeOps prg a
  rawInstrs :: [(Address, RawPcodeInstruction)] <- traverse (traverse $ mkRawPcodeInstruction <=< mkBareRawPcodeInstruction) jpcodes
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
  => J.ProgramDB
  -> AddressSpaceMap
  -> J.HighFunction
  -> a
  -> Ghidra [(Address, PcodeOp HighVarNode)]
getHighPcode prg addressSpaceMap hfn a = do
  jpcodes <- getHighPcodeOps prg hfn a
  highInstrs :: [(Address, HighPcodeInstruction)] <- traverse (traverse $ mkHighPcodeInstruction prg <=< mkBareHighPcodeInstruction) jpcodes
  let liftedInstrs = traverse (liftPcodeInstruction addressSpaceMap) <$> highInstrs
      (errs, instrs) = foldr separateError ([],[]) liftedInstrs
  case errs of
    [] -> return instrs
    _ -> error $ "Encountered Pcode decoding errors:\n" <> cs (pshow errs)
  where
    separateError (Right x) (errs, instrs) = (errs, x:instrs)
    separateError (Left err) (errs, instrs) = (err:errs, instrs)

getBlockHighPcode
  :: J.ProgramDB
  -> AddressSpaceMap
  -> J.PcodeBlockBasic
  -> Ghidra [(Address, PcodeOp HighVarNode)]
getBlockHighPcode prog addressSpaceMap block = do
  cache <- Var.newHighVarCache addressSpaceMap
  getBlockHighPcodeCached prog cache addressSpaceMap block

-- | Like 'getBlockHighPcode' but uses an external cache for HighVariable
-- deduplication and NO_ADDRESS caching. Pass the same cache across all blocks
-- in a function for maximum dedup benefit.
getBlockHighPcodeCached
  :: J.ProgramDB
  -> Var.HighVarCache
  -> AddressSpaceMap
  -> J.PcodeBlockBasic
  -> Ghidra [(Address, PcodeOp HighVarNode)]
getBlockHighPcodeCached prog cache addressSpaceMap block = do
  -- ONE JNI call extracts all op + varnode data for the entire block
  packed :: VS.Vector Int64 <- runIO $ Java.callStatic "PcodeHelper" "extractBlockData" block
                                 >>= Java.reify
  let numOps = fromIntegral (packed VS.! 0) :: Int

      -- Resolve address from packed offset+spaceId, caching unknown spaces
      resolveAddr :: Int64 -> Int64 -> Ghidra Address
      resolveAddr off spId = do
        spaceMap <- runIO $ readIORef (Var.addrSpaceMapRef cache)
        case mkAddressFromParts spaceMap off (fromIntegral spId) of
          Just a  -> pure a
          Nothing -> do
            af :: J.AddressFactory <- runIO $ Java.call prog "getAddressFactory" >>= JNI.newGlobalRef
            jSpace :: J.AddressSpace <- runIO $ Java.call af "getAddressSpace" (fromIntegral spId :: Int32) >>= JNI.newGlobalRef
            case maybeNull jSpace of
              Nothing -> do
                -- Unknown address space (e.g. from a null varnode packed as zeros).
                -- Use a fallback address space.
                let fallbackSpace = AddressSpace (fromIntegral spId) 0 0 (readAddressSpaceName $ "unknown_" <> show spId)
                pure $ Address fallbackSpace off
              Just jSpace' -> do
                space <- mkAddressSpace jSpace'
                runIO $ modifyIORef' (Var.addrSpaceMapRef cache) (HashMap.insert (fromIntegral spId) space)
                pure $ Address space off

      -- Parse a single varnode from 9 packed fields at offset i
      parseVN :: Int -> Int -> Int -> Ghidra HighVarNode
      parseVN i opI vnI = do
        let sz        = packed VS.! i
            isConst   = packed VS.! (i + 1)
            addrOff   = packed VS.! (i + 2)
            addrSpId  = packed VS.! (i + 3)
            hasPcAddr = packed VS.! (i + 4)
            pcOff     = packed VS.! (i + 5)
            pcSpId    = packed VS.! (i + 6)
            hasHigh   = packed VS.! (i + 7)
            highHash  = packed VS.! (i + 8)

        varType <- if isConst == 1
          then pure $ Const addrOff
          else Addr <$> resolveAddr addrOff addrSpId
                        <*> (if hasPcAddr == 1 then Just <$> resolveAddr pcOff pcSpId else pure Nothing)

        pcAddress <- if hasPcAddr == 1 then Just <$> resolveAddr pcOff pcSpId else pure Nothing

        mhv' <- if hasHigh == 0
          then pure Nothing
          else do
            cached <- runIO $ readIORef (Var.highVarMapRef cache)
            case IntMap.lookup (fromIntegral highHash) cached of
              Just result -> pure (Just result)
              Nothing -> do
                hv <- runIO $ Java.callStatic "PcodeHelper" "getBlockVarNodeHigh"
                        block (fromIntegral opI :: Int32) (fromIntegral vnI :: Int32)
                case maybeNull hv of
                  Nothing -> pure Nothing
                  Just hv' -> do
                    hvRef <- runIO $ JNI.newGlobalRef hv'
                    result <- Var.mkHighVariable prog hvRef
                    runIO $ modifyIORef' (Var.highVarMapRef cache) (IntMap.insert (fromIntegral highHash) result)
                    pure (Just result)

        pure $ HighVarNode varType (fromIntegral sz) pcAddress mhv'

      -- Parse N consecutive varnodes starting at offset i
      parseVNs :: Int -> Int -> Int -> Int -> Ghidra ([HighVarNode], Int)
      parseVNs i 0 _ _ = pure ([], i)
      parseVNs i n opI vnI = do
        vn <- parseVN i opI vnI
        (rest, finalOff) <- parseVNs (i + 9) (n - 1) opI (vnI + 1)
        pure (vn : rest, finalOff)

      -- Parse all ops from packed array
      go :: Int -> Int -> Int -> Ghidra [(Address, HighPcodeInstruction)]
      go _ 0 _ = pure []
      go off opsLeft opIdx = do
        let seqOff  = packed VS.! off
            seqSpId = packed VS.! (off + 1)
            opcode  = packed VS.! (off + 2)
            hasOut  = packed VS.! (off + 3)

        addr <- resolveAddr seqOff seqSpId
        let bareOp = case lookupOpcode opcode of
              Just op -> op
              Nothing -> error $ "Unknown pcode opcode: " <> show opcode

        let outStart = off + 4
        (mOutput, afterOut) <- if hasOut == 1
          then do
            vn <- parseVN outStart opIdx (-1)
            pure (Just vn, outStart + 9)
          else pure (Nothing, outStart)

        let numInputs = fromIntegral (packed VS.! afterOut) :: Int
        (inputs, afterInputs) <- parseVNs (afterOut + 1) numInputs opIdx 0

        let instr = PcodeInstruction bareOp mOutput inputs
        rest <- go afterInputs (opsLeft - 1) (opIdx + 1)
        pure $ (addr, instr) : rest

  highInstrs <- go 1 numOps 0
  let liftedInstrs = traverse (liftPcodeInstruction addressSpaceMap) <$> highInstrs
      (errs, instrs) = foldr separateError ([],[]) liftedInstrs
  case errs of
    [] -> return instrs
    _ -> error $ "Encountered Pcode decoding errors:\n" <> cs (pshow errs)
  where
    separateError (Right x) (errs, instrs) = (errs, x:instrs)
    separateError (Left err) (errs, instrs) = (err:errs, instrs)

getHighVarNodes :: J.ProgramDB -> J.HighFunction -> Ghidra [HighVarNode]
getHighVarNodes prog hFunc = do
    pcodeOpsIter :: J.Iterator J.PcodeOpAST <- runIO $ Java.call hFunc "getPcodeOps"
    pcodeOps :: [J.PcodeOpAST] <- iteratorToList pcodeOpsIter
    foldM getVarnodeASTs [] pcodeOps
    where
        getVarnodeASTs :: [HighVarNode] -> J.PcodeOpAST -> Ghidra [HighVarNode]
        getVarnodeASTs lst pcodeAST = do
            jOutput <- getHighOutput pcodeAST
            jInputs <- getHighInputs pcodeAST
            inputs <- mapM (Var.mkHighVarNode prog) jInputs
            case jOutput of
              Nothing -> return $ inputs ++ lst
              Just jOutput' -> do
                output <- Var.mkHighVarNode prog jOutput'
                return $ (output : inputs) ++ lst

