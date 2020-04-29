module Blaze.CallGraph where

import Blaze.Prelude

import Binja.Core (BNBinaryView, getFunctionsContaining, getLLILInstructionIndexAtAddress)
import Binja.Function (Function, getFunctions)
import qualified Binja.Function as Func
import qualified Binja.MLIL as MLIL
import qualified Binja.Reference as Ref
import Binja.Reference (ReferenceSource, getCodeReferences)
import Blaze.Function (CallInstruction, toCallInstruction)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (Graph)
import qualified Data.Set as Set
import qualified ListT
import ListT (ListT)
import qualified Streamly.Prelude as S

liftListM' :: Monad m => m [a] -> ListT m a
liftListM' = ListT.fromFoldable <=< lift

getCallInstruction :: Function -> ReferenceSource -> IO (Maybe CallInstruction)
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
  [Function] ->
  t m (Function, Function)
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

getCallGraph :: Graph () Function g => BNBinaryView -> [Function] -> IO g
getCallGraph = getCallGraphStreaming

getCallGraphStreaming :: Graph () Function g => BNBinaryView -> [Function] -> IO g
getCallGraphStreaming bv funcs = do
  edges <- S.toList . asyncly $ getCallGraphEdges bv funcs
  return . G.fromEdges . fmap ((),) $ edges