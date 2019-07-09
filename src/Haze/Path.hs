{-# LANGUAGE TemplateHaskell #-}

module Haze.Path where

import           Haze.Prelude

import qualified Data.Set                   as Set
import           Haze.Function                      ( createCallSite )
import qualified Haze.Function as Function
import           Haze.Graph.Alga                    ( AlgaGraph )
import           Haze.Types.Function                ( CallInstruction
                                                    , CallSite
                                                    , toCallInstruction
                                                    )
import           Haze.Types.Graph                   ( Graph
                                                    )
import qualified Haze.Types.Graph           as G
import           Haze.Types.Path
import qualified Haze.Types.Pil             as Pil
import           Hinja.BasicBlock                   ( BasicBlock
                                                    , BasicBlockFunction
                                                    , BlockEdge
                                                    )
import qualified Hinja.BasicBlock           as BB
import           Hinja.Core                         ( BNBinaryView
                                                    , InstructionIndex
                                                    )
import qualified Hinja.Core                 as H
import           Hinja.Function                     ( Function
                                                    , MLILFunction
                                                    )
import qualified Hinja.Function as HFunction
import qualified Hinja.MLIL                 as MLIL

type BasicBlockGraph t = AlgaGraph () (BasicBlock t)

newtype AlgaPath = AlgaPath (PathGraph (AlgaGraph () Node))
  deriving (Graph () Node, Path)


-- constructBasicBlockGraph :: (Ord t, BasicBlockFunction t)
--                          => t -> IO (BasicBlockGraph t)
-- constructBasicBlockGraph fn = do

--   bbs <- BB.getBasicBlocks fn
--   succs <- traverse cleanSuccs bbs
--   return . G.edges $ succsToEdges succs
--   where
--     cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [BasicBlock t])
--     cleanSuccs bb = (bb,) . catMaybes . fmap (view BB.target)
--                     <$> BB.getOutgoingEdges bb
--     succsToEdges :: [(a, [a])] -> [(a, a)]
--     succsToEdges xs = do
--       (x, ys) <- xs
--       y <- ys
--       return (x, y)


-- constructBasicBlockGraph :: (Ord t, BasicBlockFunction t)
--                          => t -> IO (BasicBlockGraph t)
-- constructBasicBlockGraph fn = do
--   bbs <- BB.getBasicBlocks fn
--   succs' <- traverse cleanSuccs bbs
--   return . G.fromEdges . fmap ((),) $ succsToEdges succs'
--   where
--     cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [BasicBlock t])
--     cleanSuccs bb = (bb,) . catMaybes . fmap (view BB.target)
--                     <$> BB.getOutgoingEdges bb
--     succsToEdges :: [(a, [a])] -> [(a, a)]
--     succsToEdges xs = do
--       (x, ys) <- xs
--       y <- ys
--       return (x, y)


constructBasicBlockGraph :: (Graph () (BasicBlock t) g, Ord t, BasicBlockFunction t)
                         => t -> IO g
constructBasicBlockGraph fn = do
  bbs <- BB.getBasicBlocks fn
  succs' <- traverse cleanSuccs bbs
  return . G.fromEdges . fmap ((),) $ succsToEdges succs'
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [BasicBlock t])
    cleanSuccs bb = (bb,) . catMaybes . fmap (view BB.target)
                    <$> BB.getOutgoingEdges bb
    succsToEdges :: [(a, [a])] -> [(a, a)]
    succsToEdges xs = do
      (x, ys) <- xs
      y <- ys
      return (x, y)


type Condition = Pil.Expression

data CallCombo = CallCombo { callNode :: CallNode
                           , abstractPathNode :: AbstractPathNode
                           , retNode :: RetNode }
               deriving (Eq, Ord, Show)

callCombo :: Function -> CallSite -> IO CallCombo
callCombo fn cs = do
  call <- CallNode fn cs <$> randomIO
  ret <- RetNode fn cs <$> randomIO
  apn <- AbstractPathNode fn (Call call) (Ret ret) <$> randomIO
  return $ CallCombo call apn ret

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

convertBasicBlockToNodeList :: BNBinaryView -> BasicBlock F -> IO [Node]
convertBasicBlockToNodeList bv bb = do
  calls <- mapMaybe toCallInstruction <$> MLIL.fromBasicBlock bb
  let spans = getSpanList (view Function.index) (bb ^. BB.start) (bb ^. BB.end) calls
  concat <$> traverse f spans
  where
    fn = bb ^. BB.func . HFunction.func

    f :: SpanItem (InstructionIndex F) (CallInstruction) -> IO [Node]
    f (SpanSpan (lo, hi)) = (:[]) . SubBlock . SubBlockNode fn (bb ^. BB.start) lo hi <$> randomIO
    f (SpanBreak ci) = do
      cc <- createCallSite bv fn ci >>= callCombo fn
      return [ Call $ callNode cc
             , AbstractPath $ abstractPathNode cc
             , Ret $ retNode cc
             ]
      

createFuncGraph :: (Graph (Maybe Condition) Node g)
                => BNBinaryView -> Function -> IO g
createFuncGraph bv func = undefined
-- create map of bb -> [Node]
-- create map of bb -> first Node
-- create map of bb -> last Node
-- for each bb, get outgoing edges, convert them to "first Node"
-- create edges for nodes in each bb
-- create edges of last Node -> first Node for each bb (don't forget about conditionals!)

--- OOOOr, make an `expandNode` function
-- insertSubgraph :: Graph e n -> n -> Graph e n -> Graph e n
-- actually, you can only insert a path into a graph,
-- because a path has one input and one output


-- should represent paths as list-like structures with these features:
-- succ node, pred node, expand node, fromList, toList
-- need fast lookup
-- could use Graph underneath, but restrict access functions
-- should specify order in type somehow, ie. make sure Call->Ret->APN doesn't happen



--- call graph ------
--- get all calls out of function

--- create call graph from those

--- get function starts
--- get all call instructions that call those functions
