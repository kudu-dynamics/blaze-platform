module Blaze.Function
  ( module Exports
  , createCallSite
  , isDirectCall
  , getCallsInFunction
  , getStmtsForFunction
  , getStmtsForAllFunctions
  , getIndirectCallsInFunction
  , getIndirectCallSites
  ) where

import Binja.Core (BNBinaryView)
import qualified Binja.Core as BN
import Binja.Function
  ( Function,
    getFunctionStartingAt
  )
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import Blaze.Types.Function as Exports

import qualified Data.Set as Set
import qualified Binja.BasicBlock as BB
import qualified Binja.Function as Func
import qualified Blaze.Graph as G
import Blaze.Types.Path (PathGraph (PathGraph))
import Blaze.Types.Path.AlgaPath (AlgaPath (AlgaPath))
import qualified Blaze.Pil as Pil
import qualified Blaze.Types.Pil as Pil


getDestOp :: CallInstruction -> Maybe (MLIL.Operation (MLIL.Expression F))
getDestOp CallInstruction{_dest=Just MLIL.Expression{MLIL._op=op'}} = Just op'
getDestOp _ = Nothing

isDirectCall :: CallInstruction -> Bool
isDirectCall c = case getDestOp c of
  Just (MLIL.CONST_PTR _) -> True
  Just (MLIL.IMPORT _) -> True
  _ -> False

createCallSite :: BNBinaryView -> Function -> CallInstruction -> IO CallSite
createCallSite bv func c = CallSite func c <$> case c ^. dest of
  Just dexpr -> case (dexpr ^. MLIL.op :: MLIL.Operation (MLIL.Expression F)) of
    (MLIL.CONST_PTR cpop) -> maybe (DestAddr addr) DestFunc <$>
                              getFunctionStartingAt bv Nothing addr
      where
        addr :: Address
        addr = fromIntegral $ cpop ^. MLIL.constant
    _ -> return $ DestExpr dexpr
  Nothing -> return $ DestColl Set.empty --- probably should be a failure

getStmtsForAllFunctions :: BNBinaryView -> IO [Pil.Stmt]
getStmtsForAllFunctions bv = concat <$> (Func.getFunctions bv >>= traverse (getStmtsForFunction bv))

getStmtsForFunction :: BNBinaryView -> Func.Function -> IO [Pil.Stmt]
getStmtsForFunction bv fn = do
  instrs <- Func.getMLILSSAFunction fn >>= BB.getBasicBlocks >>= traverse MLIL.fromBasicBlock
  -- TODO: Using an empty path since we don't actually have a path. 
  --       How should we refactor this so we can use the same converter/importer 
  --       machinery to import arbitrary instructions?
  --       I.e., the path is only needed when expanding function calls, not 
  --       importing a single function.
  addrSize <- BN.getViewAddressSize bv
  let path = AlgaPath (PathGraph G.empty)
      startConverterState = Pil.createStartConverterState path fn Pil.knownFuncDefs addrSize
  tmp <- traverse ((`Pil.runConverter` startConverterState) . Pil.convertInstrs) instrs
  return $ concatMap fst tmp

getCallsInFunction :: Func.Function -> IO [CallInstruction]
getCallsInFunction fn = do
  bbs <- Func.getMLILSSAFunction fn >>= BB.getBasicBlocks
  concat <$> traverse callsPerBB bbs
  where
    callsPerBB bb = mapMaybe toCallInstruction <$> MLIL.fromBasicBlock bb

getIndirectCallsInFunction :: Func.Function -> IO [CallInstruction]
getIndirectCallsInFunction fn = do
  calls <- getCallsInFunction fn
  return $ filter (not . isDirectCall) calls

getIndirectCallSites :: [Func.Function] -> IO [(Func.Function, CallInstruction)]
getIndirectCallSites fns = do
  indirectCalls <- traverse getIndirectCallsInFunction fns
  return . getTupleList $ zip fns indirectCalls
  where
    getTupleList :: [(Func.Function, [CallInstruction])] -> [(Func.Function, CallInstruction)]
    getTupleList = concat <$> map (uncurry createTuple)
    createTuple fn (i:is) = [(fn, i)] <> createTuple fn is
    createTuple _ [] = []