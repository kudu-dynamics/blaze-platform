module Blaze.Function
  ( module Exports
  , createCallSite
  , isDirectCall
  , getStmtsForFunction
  , getStmtsForAllFunctions
  ) where

import Binja.Core (BNBinaryView)
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
import Blaze.Pil (convertInstrs)
import qualified Blaze.Pil.Path as BPP
import qualified Blaze.Types.Pil as Pil

getDestOp :: CallInstruction -> Maybe (MLIL.Operation (MLIL.Expression F))
getDestOp CallInstruction{_dest=Just MLIL.Expression{MLIL._op=op'}} = Just op'
getDestOp _ = Nothing

isDirectCall :: CallInstruction -> Bool
isDirectCall c = case getDestOp c of
  Just (MLIL.CONST_PTR _) -> True
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
getStmtsForAllFunctions bv = concat <$> (Func.getFunctions bv >>= traverse getStmtsForFunction)

getStmtsForFunction :: Func.Function -> IO [Pil.Stmt]
getStmtsForFunction fn = do
  instrs <- Func.getMLILSSAFunction fn >>= BB.getBasicBlocks >>= traverse MLIL.fromBasicBlock
  -- TODO: Using an empty path since we don't actually have a path. 
  --       How should we refactor this so we can use the same converter/importer 
  --       machinery to import arbitrary instructions?
  --       I.e., the path is only needed when expanding function calls, not 
  --       importing a single function.
  let path = AlgaPath (PathGraph G.empty)
  tmp <- traverse ((`Pil.runConverter` BPP.createStartConverterState path fn) . convertInstrs) instrs
  return $ concatMap fst tmp
