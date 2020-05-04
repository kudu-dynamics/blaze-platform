module Blaze.CallGraph
  ( module Exports,
    getCallGraph,
  )
where

import Binja.Core (BNBinaryView, getFunctionsContaining, getLLILInstructionIndexAtAddress)
import Binja.Function as BNFunc
import qualified Binja.Function as Func
import qualified Binja.MLIL as MLIL
import qualified Binja.Reference as Ref
import Binja.Reference (ReferenceSource, getCodeReferences)
import Blaze.Function (CallInstruction, toCallInstruction)
import Blaze.Prelude
import Blaze.Types.CallGraph as Exports
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (Graph)
import qualified Data.Set as Set
import qualified Streamly.Prelude as S

getCallInstruction :: BNFunc.Function -> ReferenceSource -> IO (Maybe CallInstruction)
getCallInstruction caller ref = do
  llilFunc <- Func.getLLILFunction caller
  llilIndex <- getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
  mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
  mlilFunc <- Func.getMLILFunction caller
  mlilSSAFunc <- Func.getMLILSSAFunction caller
  mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
  toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex

getCallGraphEdges ::
  StreamingIO t m =>
  BNBinaryView ->
  [BNFunc.Function] ->
  t m (BNFunc.Function, BNFunc.Function)
getCallGraphEdges bv funcs = do
  func <- S.fromList funcs
  ref <- liftListIO $ getCodeReferences bv (func ^. Func.start)
  caller <-
    liftListIO . fmap Set.toList $
      getFunctionsContaining bv (ref ^. Ref.addr)
  mcall <- liftIO $ getCallInstruction caller ref
  case mcall of
    Nothing -> S.nil
    Just _ -> S.yield (caller, func)

getCallGraph :: Graph () BNFunc.Function g => BNBinaryView -> [BNFunc.Function] -> IO g
getCallGraph = getCallGraphStreaming

getCallGraphStreaming :: Graph () BNFunc.Function g => BNBinaryView -> [BNFunc.Function] -> IO g
getCallGraphStreaming bv funcs = do
  edges <- S.toList . asyncly $ getCallGraphEdges bv funcs
  return . G.fromEdges . fmap ((),) $ edges
