module Blaze.Function (
  module Exports,
  createCallSite,
  isDirectCall,
  getCallsInFunction,
  getStmtsForFunction,
  getStmtsForAllFunctions,
  getIndirectCallsInFunction,
  getIndirectCallSites,
) where

import Binja.Core (BNBinaryView)
import qualified Binja.Core as BN
import Binja.Function (
  Function,
  getFunctionStartingAt,
 )
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import Blaze.Types.Function as Exports

import qualified Binja.BasicBlock as BB
import qualified Binja.Function as BNFunc
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Import.Source.BinaryNinja as BNI
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as BNICG
import qualified Blaze.Import.Source.BinaryNinja.Pil as BNPil
import qualified Blaze.Pil as Pil
import qualified Blaze.Types.CallGraph as CG
import qualified Blaze.Types.Path.AlgaPath as AP
import qualified Blaze.Types.Pil as Pil
import qualified Data.Set as Set


getDestOp :: CallInstruction -> Maybe (MLIL.Operation (MLIL.Expression F))
getDestOp CallInstruction{dest=Just MLIL.Expression{MLIL._op=op'}} = Just op'
getDestOp _ = Nothing

isDirectCall :: CallInstruction -> Bool
isDirectCall c = case getDestOp c of
  Just (MLIL.CONST_PTR _) -> True
  Just (MLIL.IMPORT _) -> True
  _ -> False

createCallSite :: BNBinaryView -> Function -> CallInstruction -> IO CallSite
createCallSite bv func c = CallSite func c <$> case c ^. #dest of
  Just dexpr -> case (dexpr ^. MLIL.op :: MLIL.Operation (MLIL.Expression F)) of
    (MLIL.CONST_PTR cpop) -> maybe (DestAddr addr) DestFunc <$>
                              getFunctionStartingAt bv Nothing addr
      where
        addr :: Address
        addr = fromIntegral $ cpop ^. MLIL.constant
    _ -> return $ DestExpr dexpr
  Nothing -> return $ DestColl Set.empty --- probably should be a failure

getStmtsForAllFunctions :: BNImporter -> IO [Pil.Stmt]
getStmtsForAllFunctions imp = do
  let bv = imp ^. #binaryView
  bnFuncs <- BNFunc.getFunctions bv
  funcs <- traverse (BNICG.convertFunction bv) bnFuncs
  concat <$> traverse (uncurry $ getStmtsForFunction imp) (zip funcs bnFuncs)

getStmtsForFunction :: BNImporter -> CG.Function -> BNFunc.Function -> IO [Pil.Stmt]
getStmtsForFunction imp fn bnFn = do
  let bv = imp ^. #binaryView
  instrs <- BNFunc.getMLILSSAFunction bnFn >>= BB.getBasicBlocks >>= traverse MLIL.fromBasicBlock
  -- TODO: Using an empty path since we don't actually have a path. 
  --       How should we refactor this so we can use the same converter/importer 
  --       machinery to import arbitrary instructions?
  --       I.e., the path is only needed when expanding function calls, not 
  --       importing a single function.
  addrSize <- BN.getViewAddressSize bv
  let startConverterState = BNPil.mkConverterState bv Pil.knownFuncDefs addrSize fn AP.empty
  stmts <- traverse ((`BNPil.runConverter` startConverterState)
                     . BNPil.convertInstrs) instrs
  return $ concatMap fst stmts

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
    createTuple fn (i:is) = [(fn, i)] <> createTuple fn is
    createTuple _ [] = []
