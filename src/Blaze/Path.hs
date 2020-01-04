module Blaze.Path
  ( module Exports
  , module Blaze.Path
  ) where

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
import           Blaze.Types.Path     as Exports
import qualified Blaze.Types.Path as P
import qualified Blaze.Types.Pil      as Pil
import qualified Data.Set as Set
import qualified Streamly.Prelude as S

type BasicBlockGraph t = AlgaGraph () (BasicBlock t)

-- type NodeGraph g = AlgaGraph (BlockEdge F) (BasicBlock F) g => g

newtype AlgaPath = AlgaPath (PathGraph (AlgaGraph () Node))
  deriving (Graph () Node, Path)

getNodeFunc :: Node -> Function
getNodeFunc (SubBlock x) = x ^. P.func
getNodeFunc (Call x) = x ^. P.func
getNodeFunc (Ret x) = x ^. P.func
getNodeFunc (AbstractCall x) = x ^. P.func
getNodeFunc (AbstractPath x) = x ^. P.func
getNodeFunc (Condition x) = x ^. P.func

instance Pretty AlgaPath where
  pretty p = case uncons (P.toList p) of
    Nothing -> ""
    Just (x, xs) -> "___ Starting in: " <> pretty (getNodeFunc x) <> " ___\n"
      <> f (x:xs)
    where
      f [] = ""
      f (x:xs) = pretty x <> "\n" <> f xs

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

data NodeList = NodeList
  { allNodes :: [Node]
  , firstNode' :: Node
  , lastNode' :: Node
  } deriving (Eq, Ord, Show)

getBBNodeMap :: BNBinaryView -> [BasicBlock F] -> IO (Map (BasicBlock F) NodeList)
getBBNodeMap bv bbs = fmap Map.fromList . S.toList . asyncly $ do
  bb <- S.fromList bbs
  bbNodes <- liftIO $ convertBasicBlockToNodeList bv bb
  let lnode = fromJust $ lastMay bbNodes -- bbNodes should never be empty
      fnode = fromJust $ headMay bbNodes
  S.yield (bb, NodeList { allNodes = bbNodes, firstNode' = fnode, lastNode' = lnode })


-- edge conditions are just nodes in this graph.
constructNodeGraph :: ( Graph () Node g )
                   => BNBinaryView -> MLILSSAFunction -> IO g
constructNodeGraph bv fn = do
  bbs <- BB.getBasicBlocks fn
  bbNodeMap <- getBBNodeMap bv bbs
  let bbNodeEdges = concatMap getNodeListEdges . Map.elems $ bbNodeMap
  succEdges <- concatMapM (getSuccEdges bbNodeMap) bbs
  return . G.fromEdges . fmap ((),) $ bbNodeEdges <> succEdges
  where
    getNodeListEdges :: NodeList -> [(Node, Node)]
    getNodeListEdges = pairs . allNodes

    getSuccEdges :: Map (BasicBlock F) NodeList -> BasicBlock F -> IO [(Node, Node)]
    getSuccEdges m bb = S.toList . asyncly $ do
      let p = lastNode' $ m ! bb
      e <- liftListIO $ BB.getOutgoingEdges bb
      mc <- liftIO $ getConditionNode e
      case e ^. BB.target of
        Nothing -> S.nil
        Just t -> case mc of
          Nothing -> S.yield (p, firstNode' $ m ! t)
          Just c -> S.fromFoldable [ (p, Condition c)
                                   , (Condition c, firstNode' $ m ! t)
                                   ]

constructNodeGraph' :: ( Graph (Maybe ConditionNode) Node g )
                   => BNBinaryView -> MLILSSAFunction -> IO g
constructNodeGraph' bv fn = do
  bbs <- BB.getBasicBlocks fn
  bbNodeMap <- getBBNodeMap bv bbs
  let bbNodeEdges = concatMap getNodeListEdges . Map.elems $ bbNodeMap
  succEdges <- concatMapM (getSuccEdges bbNodeMap) bbs
  return . G.fromEdges $ bbNodeEdges <> succEdges
  where
    getNodeListEdges :: NodeList -> [(Maybe ConditionNode, (Node, Node))]
    getNodeListEdges x = (Nothing,) <$> pairs (allNodes x)

    getSuccEdges :: Map (BasicBlock F) NodeList -> BasicBlock F -> IO [(Maybe ConditionNode, (Node, Node))]
    getSuccEdges m bb = S.toList . asyncly $ do
      let p = lastNode' $ m ! bb
      e <- liftListIO $ BB.getOutgoingEdges bb
      c <- liftIO $ getConditionNode e
      case e ^. BB.target of
        Nothing -> S.nil
        Just t -> S.yield (c, (p, firstNode' $ m ! t))            


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


type Condition = Pil.Expression

data CallCombo = CallCombo { callNode :: CallNode
                           , abstractPathNode :: AbstractPathNode
                           , retNode :: RetNode }
               deriving (Eq, Ord, Show)

callCombo :: Function -> CallSite -> IO CallCombo
callCombo fn cs' = do
  call <- CallNode fn cs' <$> randomIO
  ret <- RetNode fn cs' <$> randomIO
  apn <- AbstractPathNode fn (Call call) (Ret ret) <$> randomIO
  return $ CallCombo call apn ret

data SpanItem a b = SpanSpan (a, a)
                  | SpanBreak b
                  deriving (Show)

-- assumes [a] is sorted without duplicates and forall a in [a], a < hi
getSpanList :: Integral a => (b -> a) -> a -> a -> [b] -> [SpanItem a b]
getSpanList _ lo hi [] = if lo == hi then [] else [SpanSpan (lo, hi)]
getSpanList f lo hi (x:xs)
  | lo == n = SpanBreak x : getSpanList f (lo + 1) hi xs
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

    f :: SpanItem (InstructionIndex F) CallInstruction -> IO [Node]
    f (SpanSpan (lo, hi)) = (:[]) . SubBlock . SubBlockNode fn (bb ^. BB.start) lo hi <$> randomIO
    f (SpanBreak ci) = do
      n <- AbstractCallNode fn <$> createCallSite bv fn ci <*> randomIO
      return [AbstractCall n]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 xs

mpairs :: [a] -> [(a, Maybe a)]
mpairs [] = []
mpairs [x] = [(x, Nothing)]
mpairs (x:y:xs) = (x, Just y) : mpairs (y:xs)


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

pathFromBasicBlockList :: (Graph (BlockEdge F) (BasicBlock F) g, Path p)
                       => BNBinaryView -> g -> [BasicBlock F] -> IO p
pathFromBasicBlockList bv g = fmap (fromList . concat) . traverse f . mpairs
  where
    f :: (BasicBlock F, Maybe (BasicBlock F)) -> IO [Node]
    f (bb, Nothing) = convertBasicBlockToNodeList bv bb
    f (bb, (Just bbnext)) = do
      nodes <- convertBasicBlockToNodeList bv bb
      mcond <- case G.getEdgeLabel (bb, bbnext) g of
        Nothing -> return Nothing
        Just edge -> getConditionNode edge
      return . maybe nodes ((nodes <>) . (:[]) . Condition) $ mcond


simplePathsFromNodeGraph :: (Graph () Node g, Path p)
                         => g -> [p]
simplePathsFromNodeGraph = fmap fromList . G.findAllSimplePaths


simplePathsFromBasicBlockGraph :: (Graph (BlockEdge F) (BasicBlock F) g, Path p)
                               => BNBinaryView -> g -> IO [p]
simplePathsFromBasicBlockGraph bv g =
  traverse (pathFromBasicBlockList bv g) . G.findAllSimplePaths $ g

allSimpleFunctionPaths :: Path p => BNBinaryView -> Function -> IO [p]
allSimpleFunctionPaths bv fn = do
  mlilFn <- HFunction.getMLILSSAFunction fn
  ng <- constructNodeGraph bv mlilFn :: IO (AlgaGraph () Node)
  return $ simplePathsFromNodeGraph ng


-- allSimpleFunctionPaths :: Path p => BNBinaryView -> Function -> IO [p]
-- allSimpleFunctionPaths bv fn = do
--   mlilFn <- HFunction.getMLILSSAFunction fn
--   bbg <- constructBasicBlockGraph mlilFn :: IO (AlgaGraph (BlockEdge F) (BasicBlock F))
--   simplePathsFromBasicBlockGraph bv bbg


allSimpleFunctionPaths' :: Path p => BNBinaryView -> Function -> IO [p]
allSimpleFunctionPaths' bv fn = do
  mlilFn <- HFunction.getMLILSSAFunction fn
  putText "Constructing Basic Block Graph"
  bbg <- constructBasicBlockGraphWithoutBackEdges mlilFn :: IO (AlgaGraph (BlockEdge F) (BasicBlock F))
  --bbg <- constructBasicBlockGraph mlilFn :: IO (AlgaGraph (BlockEdge F) (BasicBlock F))
  putText $ "Nodes: " <> (show . Set.size $ G.nodes bbg)
  case Set.toList $ G.sources bbg of
    [s] -> do
      let c = LMap.lookup s $ G.countAllSimplePaths bbg
      putText $ "Simple paths count: " <> show c
      putText "Finding all simple paths2"
      let ps = G.findAllSimplePaths2 bbg s
      putText $ "Simple paths: " <> (show . length $ ps)
      traverse (pathFromBasicBlockList bv bbg) ps      
    xs -> P.error $ "Bad " <> show (fmap (\bb -> let (InstructionIndex n) = (bb ^. BB.start) in n) xs)
  -- putText "CONSTRUCTED"
  -- simplePathsFromBasicBlockGraph bv bbg


  
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
