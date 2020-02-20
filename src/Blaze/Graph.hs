module Blaze.Graph where

import qualified Prelude as P
import           Blaze.Prelude

import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Map.Lazy as LMap
import           Binja.BasicBlock                  ( BasicBlock
                                                   , BasicBlockFunction
                                                   , BlockEdge
                                                   )
import qualified Binja.BasicBlock     as BB
import           Binja.C.Enums                     ( BNBranchType( FalseBranch
                                                                 , TrueBranch
                                                                 )
                                                   )
import           Binja.Core                        ( BNBinaryView
                                                   , InstructionIndex(InstructionIndex)
                                                   
                                                   )
import           Binja.Function                    ( Function
                                                   , MLILSSAFunction
                                                   )
import qualified Binja.Function       as HFunction
import qualified Binja.MLIL           as MLIL
import           Blaze.Function                    ( createCallSite )
import qualified Blaze.Function       as Function
import           Blaze.Graph.Alga                  ( AlgaGraph )
import           Blaze.Types.Function              ( CallInstruction
                                                   , CallSite
                                                   , toCallInstruction
                                                   )
import           Blaze.Types.Graph                 ( Graph )
import qualified Blaze.Types.Graph    as G
import qualified Data.Set as Set
import qualified Streamly.Prelude as S
import Blaze.Types.Path (ConditionNode(ConditionNode))


type F = MLILSSAFunction


naiveLCS :: String -> String -> Int
naiveLCS [] _ = 0
naiveLCS _ [] = 0
naiveLCS (x:xs) (y:ys)
  | x == y    = 1 + naiveLCS xs ys
  | otherwise = max (naiveLCS (x:xs) ys) (naiveLCS xs (y:ys))


constructBasicBlockGraph :: (Graph (BlockEdge t) (BasicBlock t) g, BasicBlockFunction t)
                         => t -> IO g
constructBasicBlockGraph fn = do
  bbs <- BB.getBasicBlocks fn
  succs' <- traverse cleanSuccs bbs
  return . G.fromEdges . succsToEdges $ succs'
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb = (bb,) . catMaybes . fmap (\e -> (e,) <$>  (e ^. BB.target))
                    <$> BB.getOutgoingEdges bb
    succsToEdges :: [(a, [(e, a)])] -> [(e, (a, a))]
    succsToEdges xs = do
      (x, ys) <- xs
      (e, y) <- ys
      return (e, (x, y))

isBackEdge :: (Eq t, BasicBlockFunction t) => BlockEdge t -> IO Bool
isBackEdge be = case be ^. BB.target of
  Nothing -> return False
  Just dst -> if src == dst then return False else do
    b <- BB.getDominators src >>= return . (dst `elem`)
    if b then 
      putText $ "BackEdge: " <> show (src ^. BB.start) <> " -> " <> show (dst ^. BB.start)
      else return ()
    return b
  where
    src = be ^. BB.src

constructBasicBlockGraphWithoutBackEdges ::
  (Graph (BlockEdge t) (BasicBlock t) g, BasicBlockFunction t, Eq t)
  => t -> IO g
constructBasicBlockGraphWithoutBackEdges fn = do
  bbs <- BB.getBasicBlocks fn
  succs' <- traverse cleanSuccs bbs
  putText "Filtering out back edges"
  edges <- filterM (fmap not . isBackEdge . fst) . succsToEdges $ succs'
  putText $ "Filtered! " <> show (length edges) <> " edges."
  return $ G.fromEdges edges
--   return
--     . G.fromEdges
-- --    . filter (not . view BB.isBackEdge . fst)
--     . filter (not . isBackEdge . fst)
--     . succsToEdges $ succs'
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb = (bb,) . catMaybes . fmap (\e -> (e,) <$>  (e ^. BB.target))
                    <$> BB.getOutgoingEdges bb
    succsToEdges :: [(a, [(e, a)])] -> [(e, (a, a))]
    succsToEdges xs = do
      (x, ys) <- xs
      (e, y) <- ys
      return (e, (x, y))


getConditionNode :: BlockEdge F -> IO (Maybe ConditionNode)
getConditionNode edge = case edge ^. BB.branchType of
  TrueBranch -> f True
  FalseBranch -> f False
  _ -> return Nothing
  where
    bb = edge ^. BB.src
    fn = bb ^. BB.func . HFunction.func
    f isTrueBranch = do
      endInstr <- MLIL.instruction (bb ^. BB.func) (bb ^. BB.end - 1)
      case endInstr ^. MLIL.op of
        MLIL.IF op -> Just . ConditionNode fn isTrueBranch (op ^. MLIL.condition) <$> randomIO
        _ -> return Nothing

