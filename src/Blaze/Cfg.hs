{-# LANGUAGE BangPatterns #-}

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
import Streamly (IsStream, asyncly)
import qualified Streamly.Prelude as S
import           Blaze.Graph.Alga                  ( AlgaGraph )

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

-- liftListM :: Monad m => m [a] -> ListT m a
-- liftListM = ListT.fromFoldable <=< lift
  
-- getFunctionsThatCallIntoFunction :: BNBinaryView -> Function -> IO [Function]
-- getFunctionsThatCallIntoFunction bv func = ListT.toReverseList $ do
--   --putText $ func ^. Func.name <> " " <> (show $ func ^. Func.start)
--   ref <- liftListM $ getCodeReferences bv (func ^. Func.start)
--   caller <- liftListM . fmap Set.toList
--             $ getFunctionsContaining bv (ref ^. Ref.addr)
--   mcall <- liftIO $ do
--     llilIndex <- getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
--     llilFunc <- Func.getLLILFunction caller
--     mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
--     mlilFunc <- Func.getMLILFunction caller
--     mlilSSAFunc <- Func.getMLILSSAFunction caller
--     mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
--     toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex
--   case mcall of
--     Nothing -> mzero
--     Just _ -> ListT.fromFoldable [caller]

-- getCallGraph :: Graph () Function g => BNBinaryView -> IO g
-- getCallGraph bv = do
--   funcs <- getFunctions bv
--   callers <- mapConcurrently (getFunctionsThatCallIntoFunction bv) funcs
--   let edges = do
--         (func, callers') <- zip funcs callers
--         caller <- callers'
--         return (func, caller)
--   return . G.fromEdges . fmap ((),) $ edges

---- Streamly versions
liftListM :: (Monad m, Monad (t m), MonadTrans t, IsStream t) => m [a] -> t m a
liftListM = S.fromList <=< lift

liftListIO :: (Monad m, Monad (t m), MonadTrans t, IsStream t, MonadIO m) => IO [a] -> t m a
liftListIO = liftListM . liftIO

getCallGraphEdges :: (MonadTrans t, MonadIO m, MonadIO (t m), Monad (t m), IsStream t)
                  => BNBinaryView -> t m (Function, Function)
getCallGraphEdges bv = do
  func <- liftListIO $ getFunctions bv
  ref <- liftListIO $ getCodeReferences bv (func ^. Func.start)
  caller <- liftListIO . fmap Set.toList
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
    Nothing -> S.nil
    Just _ -> S.fromFoldable [(caller, func)]

type CallGraph = AlgaGraph () Function 

getCallGraph :: BNBinaryView -> IO CallGraph
getCallGraph bv = do
  edges <- S.toList . asyncly $ getCallGraphEdges bv
  return . G.fromEdges . fmap ((),) $ edges

---- Non-streamly versions

-- liftListM :: Monad m => m [a] -> ListT m a
-- liftListM = ListT.fromFoldable <=< lift

-- liftListIO :: (MonadIO m) => IO [a] -> ListT m a
-- liftListIO = liftListM . liftIO

-- getCallers :: BNBinaryView -> Function -> (Set Function)
-- getCallers bv func =
--   do 
--     refs <- getCodeReferences bv (func ^. Func.start)
--     fmap (getFunctionsContaining bv . (^. Ref.addr)) refs

-- getCallGraphEdges :: (MonadIO m, MonadIO (ListT m))
--                   => BNBinaryView -> ListT m (Function, Function)
-- getCallGraphEdges bv = do
--   func <- liftListIO $ getFunctions bv
--   ref <- liftListIO $ getCodeReferences bv (func ^. Func.start)
--   caller <- liftListIO . fmap Set.toList
--             $ getFunctionsContaining bv (ref ^. Ref.addr)
--   mcall <- liftIO $ do
--     llilFunc <- Func.getLLILFunction caller
--     llilIndex <- getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
--     mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
--     mlilFunc <- Func.getMLILFunction caller
--     mlilSSAFunc <- Func.getMLILSSAFunction caller
--     mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
--     toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex
--   case mcall of
--     Nothing -> mempty
--     Just _ -> ListT.fromFoldable [(caller, func)]

-- type CallGraph = AlgaGraph () Function 

-- getCallGraph :: BNBinaryView -> IO CallGraph
-- getCallGraph bv = do
--   edges <- ListT.toReverseList $ getCallGraphEdges bv
--   return . G.fromEdges . fmap ((),) $ edges

-- getCallGraph' :: Graph () Function g => BNBinaryView -> IO g
-- getCallGraph' bv = do
--   edges <- ListT.toReverseList $ do
--     func <- liftListM $ getFunctions bv
--     ref <- liftListM $ getCodeReferences bv (func ^. Func.start)
--     caller <- liftListM . fmap Set.toList
--               $ getFunctionsContaining bv (ref ^. Ref.addr)
--     mcall <- liftIO $ do
--       llilIndex <- getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
--       llilFunc <- Func.getLLILFunction caller
--       mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
--       mlilFunc <- Func.getMLILFunction caller
--       mlilSSAFunc <- Func.getMLILSSAFunction caller
--       mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
--       toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex
--     case mcall of
--       Nothing -> mzero
--       Just _ -> ListT.fromFoldable [(caller, func)]
--   return . G.fromEdges . fmap ((),) $ edges

