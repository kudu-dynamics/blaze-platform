module Blaze.Graph 
  (module Exports,
   module Blaze.Graph)
where

import Binja.BasicBlock
  ( BasicBlock,
    BasicBlockFunction,
    BlockEdge,
  )
import qualified Binja.BasicBlock as BB
import Binja.C.Enums
  ( BNBranchType
      ( FalseBranch,
        TrueBranch
      ),
  )
import Binja.Core (InstructionIndex)
import Binja.Function (MLILSSAFunction)
import qualified Binja.Function as HFunction
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import Blaze.Types.Graph (Graph)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph as Exports
import Blaze.Types.Path (ConditionNode (ConditionNode))
import qualified Data.Set as Set


type F = MLILSSAFunction

naiveLCS :: String -> String -> Int
naiveLCS [] _ = 0
naiveLCS _ [] = 0
naiveLCS (x:xs) (y:ys)
  | x == y    = 1 + naiveLCS xs ys
  | otherwise = max (naiveLCS (x:xs) ys) (naiveLCS xs (y:ys))

isGotoInstr :: F -> InstructionIndex F -> IO Bool
isGotoInstr fn ix = do
  instr <- MLIL.instruction fn ix
  return $ case instr ^. MLIL.op of
    (MLIL.GOTO _) -> True
    _ -> False

isGotoNode :: BasicBlock F -> IO Bool
isGotoNode bb = do
  b <- isGotoInstr (bb ^. BB.func) (bb ^. BB.start)
  return $ bb ^. BB.end - bb ^. BB.start == 1 && b

collapseGotoBlocks :: (Graph (BlockEdge F) (BasicBlock F) g)
                   => g -> IO g
collapseGotoBlocks g =
  case Set.toList $ G.nodes g of
    [_] -> return g
    ns -> do  
      gotos <- fmap Set.fromList . filterM isGotoNode $ ns
      let edges = G.edges g
      return . G.fromEdges $ foldr (f gotos) [] edges
  where
    f gotos edge@(be, (bbSrc, bbDst)) xs
      | Set.member bbSrc gotos = xs
      | Set.member bbDst gotos = case Set.toList $ G.succs bbDst g of
            [bbTgt] -> ( be & BB.target ?~ bbTgt
                       , (bbSrc, bbTgt) )
                       : xs
            _ -> edge : xs
      | otherwise = edge : xs

succsToEdges :: [(a, [(e, a)])] -> [(e, (a, a))]
succsToEdges xs = do
  (x, ys) <- xs
  (e, y) <- ys
  return (e, (x, y))

constructBasicBlockGraph :: (Graph (BlockEdge t) (BasicBlock t) g, BasicBlockFunction t)
                         => t -> IO g
constructBasicBlockGraph fn = do
  bbs <- BB.getBasicBlocks fn
  case bbs of
    [bb] -> return $ G.fromNode bb
    _ -> do
      succs' <- traverse cleanSuccs bbs
      return . G.fromEdges . succsToEdges $ succs'
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb = (bb,) . mapMaybe (\e -> (e,) <$>  (e ^. BB.target))
                    <$> BB.getOutgoingEdges bb

isBackEdge :: (Eq t, BasicBlockFunction t) => BlockEdge t -> IO Bool
isBackEdge be = case be ^. BB.target of
  Nothing -> return False
  Just dst -> 
    if src == dst then 
      return False 
    else
      (dst `elem`) <$> BB.getDominators src
  where
    src = be ^. BB.src

constructBasicBlockGraphWithoutBackEdges ::
  (Graph (BlockEdge t) (BasicBlock t) g, BasicBlockFunction t, Eq t) =>
  t ->
  IO g
constructBasicBlockGraphWithoutBackEdges fn = do
  bbs <- BB.getBasicBlocks fn
  case bbs of
    [bb] ->
      return $ G.fromNode bb
    _ -> do
      succs' <- traverse cleanSuccs bbs
      edges <- filterM (fmap not . isBackEdge . fst) . succsToEdges $ succs'
      return $ G.fromEdges edges
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb =
      (bb,) . mapMaybe (\e -> (e,) <$> (e ^. BB.target))
        <$> BB.getOutgoingEdges bb

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

