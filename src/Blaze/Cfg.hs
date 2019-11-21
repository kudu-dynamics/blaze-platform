module Blaze.Cfg where

import Blaze.Prelude
import Binja.Core (BNBinaryView, getLowLevelILForInstruction, getFunctionsContaining, getLLILInstructionIndexAtAddress)
import Binja.Reference (getCodeReferences, ReferenceSource)
import qualified Binja.Reference as Ref
import Binja.Function (getFunctions, Function)
import qualified Binja.Function as Func
import Blaze.Types.Graph (Graph)
import qualified Blaze.Types.Graph as G
import qualified ListT 
import ListT ( ListT )
import qualified Data.Set as Set
import Blaze.Function (createCallSite, toCallInstruction) 
import qualified Binja.MLIL as MLIL

data SpanItem a b = SpanSpan (a, a)
                  | SpanBreak b
                  deriving (Show)

-- assumes [a] is sorted without duplicates and forall a in [a], a < hi
getSpanList :: Integral a => (b -> a) -> a -> a -> [b] -> [SpanItem a b]
getSpanList _ lo hi [] = if lo == hi then [] else [SpanSpan (lo, hi)]
getSpanList f lo hi (x:xs)
  | lo == n = (SpanBreak x):getSpanList f (lo + 1) hi xs
  | otherwise = SpanSpan (lo, n) : SpanBreak x : getSpanList f (n + 1) hi xs
  where
    n = f x

liftListM :: Monad m => m [a] -> ListT m a
liftListM = ListT.fromFoldable <=< lift

tbo :: IO [(Int, Text)]
tbo = ListT.toReverseList $ do
  n <- ListT.fromFoldable [0, 1, 2, 3]
  x <- liftListM . pure . take n $ repeat "hey"
  return (n, x)
  

getCallGraph :: Graph () Function g => BNBinaryView -> IO g
getCallGraph bv = do
  edges <- ListT.toReverseList $ do
    func <- liftListM $ getFunctions bv
    ref <- liftListM $ getCodeReferences bv (func ^. Func.start)
    caller <- liftListM . fmap Set.toList
              $ getFunctionsContaining bv (ref ^. Ref.addr)
    mcall <- liftIO $ do
      llilIndex <- getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
      llilFunc <- Func.getLLILFunction caller
      mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
      mlilFunc <- Func.getMLILFunction caller
      mlilSSAFunc <- Func.getMLILSSAFunction caller
      mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
      toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex
    case mcall of
      Nothing -> mzero
      Just _ -> ListT.fromFoldable [(caller, func)]
  return . G.fromEdges . fmap ((),) $ edges

