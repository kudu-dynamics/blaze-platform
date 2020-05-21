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
import Blaze.Pil (convertInstrs)
import qualified Blaze.Pil.Path as BPP
import qualified Blaze.Types.Pil as Pil
import Data.BinaryAnalysis (Address)

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
  tmp <- traverse ((`Pil.runConverter` BPP.startConverterCtx) . convertInstrs) instrs
  return $ concatMap fst tmp