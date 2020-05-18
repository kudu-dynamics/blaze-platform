{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Blaze.Function
  ( module Exports
  , createCallSite
  , isDirectCall
  ) where

import Binja.Core (BNBinaryView)
import Binja.Function
  ( Function,
    getFunctionStartingAt,
  )
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import Blaze.Types.Function as Exports

import qualified Data.Set as Set
import qualified Binja.BasicBlock as BB
import qualified Binja.Core as BN
import qualified Binja.Function as Func
import qualified Binja.View as V
import Blaze.Pil (convertInstrs)
import qualified Blaze.Pil.Path as BPP
import qualified Blaze.Types.Pil as Pil
import Data.BinaryAnalysis (Address, Address (Address))
import qualified Data.HashSet as HSet
import Data.Text (pack)


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

getVtableStores :: BNBinaryView -> [Pil.Stmt] -> IO [Pil.Stmt]
getVtableStores bv stmts = map fst <$> filterM (isVtable bv . snd) storeConst
  where
    storeConst =
      [ (storeStmt, toIntegralSized vptrCandidate)
        | storeStmt@(Pil.Store (Pil.StoreOp _ (Pil.Expression _ (Pil.CONST (Pil.ConstOp vptrCandidate))))) <- stmts
      ]

isVtable :: BNBinaryView -> Maybe Word64 -> IO Bool
isVtable _ Nothing =
  return False
isVtable bv addr = do
  fns <- Func.getFunctions bv
  let functionAddrs = HSet.fromList $ map (^. Func.start) fns
  readr <- V.getDefaultReader bv
  _ <- BN.seekBinaryReader readr (fromJust addr)
  val <- BN.read64 readr
  case val of
    Nothing -> return False
    _ ->
      return $
        HSet.member
          (Address $ fromJust $ toIntegralSized $ fromJust val)
          functionAddrs

getStmtsForAllFunctions :: BNBinaryView -> IO [Pil.Stmt]
getStmtsForAllFunctions bv = concat <$> (Func.getFunctions bv >>= mapM getStmtsForFunction)

getStmtsForFunction :: Func.Function -> IO [Pil.Stmt]
getStmtsForFunction fn = do
  instrs <- Func.getMLILSSAFunction fn >>= BB.getBasicBlocks >>= mapM MLIL.fromBasicBlock
  tmp <- mapM ((`Pil.runConverter` BPP.startConverterCtx) . convertInstrs) instrs
  return $ concatMap fst tmp

getSectionTypeOfAddr :: BNBinaryView -> Address -> IO Text
getSectionTypeOfAddr bv addr = pack <$> (head <$> BN.getSectionsAt bv addr >>= BN.getSectionType)
