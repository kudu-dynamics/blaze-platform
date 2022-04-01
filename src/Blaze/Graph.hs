module Blaze.Graph
  ( module Exports,
    module Blaze.Graph,
  )
where

import Binja.BasicBlock
  ( BasicBlock,
    BasicBlockFunction,
    BlockEdge,
  )
import qualified Binja.BasicBlock as BB
import Binja.Core (InstructionIndex)
import Binja.Function (MLILSSAFunction)
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph as Exports hiding (edge, label, src, dst)
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as Im
import qualified Data.IntSet as IntSet
import qualified Data.Graph.Dom as Dlt
import qualified Data.HashMap.Strict as HashMap

type F = MLILSSAFunction

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

collapseGotoBlocks ::
  (Graph (BlockEdge F) () (BasicBlock F) g) =>
  g ->
  IO g
collapseGotoBlocks g =
  case HashSet.toList $ G.nodes g of
    [_] -> return g
    ns -> do
      gotos <- fmap HashSet.fromList . filterM isGotoNode $ ns
      let es = G.edges g
      return . G.fromEdges $ foldl' (flip $ f gotos) [] es
  where
    f gotos edge@(G.LEdge be (G.Edge bbSrc bbDst)) xs
      | HashSet.member bbSrc gotos = xs
      | HashSet.member bbDst gotos = case HashSet.toList $ G.succs bbDst g of
        [bbTgt] ->
          G.LEdge
            (be & BB.target ?~ bbTgt)
            (G.Edge bbSrc bbTgt)            
            : xs
        _ -> edge : xs
      | otherwise = edge : xs

succsToEdges :: [(a, [(e, a)])] -> [(e, (a, a))]
succsToEdges xs = do
  (x, ys) <- xs
  (e, y) <- ys
  return (e, (x, y))

constructBasicBlockGraph ::
  (Graph (BlockEdge t) () (BasicBlock t) g, BasicBlockFunction t) =>
  t ->
  IO g
constructBasicBlockGraph fn = do
  bbs <- BB.getBasicBlocks fn
  case bbs of
    [bb] -> return $ G.fromNode bb
    _ -> do
      succs' <- traverse cleanSuccs bbs
      return . G.fromEdges . fmap fromTupleLEdge .succsToEdges $ succs'
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb =
      (bb,) . mapMaybe (\e -> (e,) <$> (e ^. BB.target))
        <$> BB.getOutgoingEdges bb

isBackEdge :: (Eq t, BasicBlockFunction t) => BlockEdge t -> IO Bool
isBackEdge be = case be ^. BB.target of
  Nothing -> return False
  Just dst ->
    if src == dst
      then return False
      else (dst `elem`) <$> BB.getDominators src
  where
    src = be ^. BB.src

constructBasicBlockGraphWithoutBackEdges ::
  (Graph (BlockEdge t) () (BasicBlock t) g, BasicBlockFunction t, Eq t) =>
  t ->
  IO g
constructBasicBlockGraphWithoutBackEdges fn = do
  bbs <- BB.getBasicBlocks fn
  case bbs of
    [bb] ->
      return $ G.fromNode bb
    _ -> do
      succs' <- traverse cleanSuccs bbs
      es <- filterM (fmap not . isBackEdge . fst) . succsToEdges $ succs'
      return . G.fromEdges . fmap fromTupleLEdge $ es
  where
    cleanSuccs :: BasicBlockFunction t => BasicBlock t -> IO (BasicBlock t, [(BlockEdge t, BasicBlock t)])
    cleanSuccs bb =
      (bb,) . mapMaybe (\e -> (e,) <$> (e ^. BB.target))
        <$> BB.getOutgoingEdges bb


------- Dominators

buildNodeMap :: (Graph e attr a g) => g -> DltMap a
buildNodeMap =
  Im.fromList . zip [0 ..] . HashSet.toList . nodes

-- buildNodeMap' :: (Graph e attr a g) => a -> g -> DltMap a
-- buildNodeMap' rootNode =
--   Im.fromList . zip [0 ..] . reachable rootNode

buildAdjMap :: [Dlt.Node] -> [Dlt.Edge] -> IntMap [Dlt.Node]
buildAdjMap ns =
  foldl' mergeEdges initialAdjMap
 where
  initialAdjMap :: IntMap [Dlt.Node]
  initialAdjMap = Im.fromList $ (,[]) <$> ns
  mergeEdges :: IntMap [Dlt.Node] -> Dlt.Edge -> IntMap [Dlt.Node]
  mergeEdges acc e =
    Im.adjust (snd e :) (fst e) acc

{- | Build a graph for use with Data.Graph.Dom for finding dominators
  and post-dominators.
  Note that we use unchecked HashMap lookups (!) as we know the
  entries must be present. That is, we know there is a corresponding
  Int for every CfNode.
-}
buildDltGraph
  :: forall e attr a g.
  (Hashable a, Eq a, Graph e attr a g)
  => a
  -> g
  -> DltMap a
  -> Dlt.Rooted
buildDltGraph rootNode g dltMap =
  -- NB: Must use 'fromAdj' since 'fromEdges' will not include nodes
  -- that don't have outgoing edges.
  (cfMap HashMap.! rootNode, Dlt.fromAdj dltAdj)
  where
    cfMap :: CfMap a
    cfMap = HashMap.fromList $ swap <$> Im.assocs dltMap
    dltNodes :: [Dlt.Node]
    dltNodes = (cfMap HashMap.!) <$> (HashSet.toList . nodes $ g)
    dltEdges :: [Dlt.Edge]
    dltEdges = do
      (LEdge _ (Edge src' dst')) <- edges g
      return (cfMap HashMap.! src', cfMap HashMap.! dst')
    dltAdj :: [(Dlt.Node, [Dlt.Node])]
    dltAdj = Im.toList $ buildAdjMap dltNodes dltEdges

-- | Convert a Blaze CFG to a dom-lt flow graph
dltGraphFromGraph
  :: forall e attr a g.
  (Hashable a, Eq a, Graph e attr a g)
  => a
  -> g
  -> (Dlt.Rooted, DltMap a)
dltGraphFromGraph rootNode g =
  (buildDltGraph rootNode g dltMap, dltMap)
 where
  dltMap :: DltMap a
  dltMap = buildNodeMap g

domHelper
  :: forall e attr a g.
  (Hashable a, Eq a, Graph e attr a g)
  => (Dlt.Rooted -> [(Dlt.Node, Dlt.Path)])
  -> a
  -> g
  -> HashMap a (HashSet a)
domHelper f rootNode g =
  HashMap.fromList . ((HashSet.fromList <$>) <$>) $ domList
 where
  dltRooted :: Dlt.Rooted
  dltMap :: DltMap a
  (dltRooted, dltMap) = dltGraphFromGraph rootNode g
  domList :: [(a, [a])]
  domList = bimap (dltMap Im.!) ((dltMap Im.!) <$>) <$> f dltRooted

{- | Finds all dominators for a CFG. Converts the CFG to a Data.Graph.Dom#Graph and then uses dom-lt
 to find dominators. The result is converted back to CfNodes before being returned.
 Per dom-lt, the complexity is:
 O(|E|*alpha(|E|,|V|)), where alpha(m,n) is "a functional inverse of Ackermann's function".
-}
-- getDominators :: (Hashable a, Eq a, Graph e attr a g) => a -> g -> Dominators a
-- getDominators rootNode = Dominators . domHelper Dlt.dom rootNode

getDominatorMapping
  :: forall a attr g e. (Hashable a, Eq a, Graph e attr a g)
  => a
  -> g
  -> HashMap a (HashSet a)
getDominatorMapping rootNode g = foldl' (flip buildDominatedMapping) HashMap.empty allNodes
  where
    allNodes = reachable rootNode g
    allNodesSet = HashSet.fromList allNodes
    getDominatedBy :: a -> [a]
    getDominatedBy n = HashSet.toList
      . HashSet.difference allNodesSet
      . HashSet.fromList
      . bfsReachable rootNode
      . removeNode n
      $ g
    buildDominatedMapping :: a -> HashMap a (HashSet a) -> HashMap a (HashSet a)
    buildDominatedMapping n m = foldr alterIfNotEqual m $ getDominatedBy n
      where
        alterIfNotEqual :: a -> HashMap a (HashSet a) -> HashMap a (HashSet a)
        alterIfNotEqual n' m'
          | n' == n = m'
          | otherwise = HashMap.alter addOrCreate n' m'
        addOrCreate :: Maybe (HashSet a) -> Maybe (HashSet a)
        addOrCreate Nothing = Just $ HashSet.singleton n
        addOrCreate (Just s) = Just $ HashSet.insert n s

-- | Nodes reachable from n in bfs order, excludes self unless can be reached later
bfsReachable :: Graph e attr a g => a -> g -> [a]
--bfsReachable n g = concat $ bfs (HashSet.toList $ succs n g) g
bfsReachable n g = concat $ bfs [n] g

  
getDominators :: (Hashable a, Eq a, Graph e attr a g) => a -> g -> Dominators a
getDominators rootNode = Dominators . getDominatorMapping rootNode

getPostDominators_ :: (Hashable a, Eq a, Graph e attr a g) => a -> g -> PostDominators a
getPostDominators_ termNode = PostDominators . getDominatorMapping termNode . G.transpose

-- -- | Gets all post dominators. termNode should be the only terminal node in the graph.
-- getPostDominators_ :: (Hashable a, Eq a, Graph e attr a g) => a -> g -> PostDominators a
-- getPostDominators_ termNode = PostDominators . domHelper Dlt.pdom termNode

-- | Gets all post dominators. If there are multiple terminal nodes,
--   each will point to `dummyTermNode`.
getPostDominators
  :: (Hashable a, Eq a, Graph e attr a g)
  => a
  -> e
  -> g
  -> PostDominators a
getPostDominators dummyTermNode dummyTermEdgeLabel g =
  case HashSet.toList $ G.getTermNodes g of
    [] -> domEmpty
    [x] -> getPostDominators_ x g
    xs -> domRemoveNode dummyTermNode
      . getPostDominators_ dummyTermNode
      $ foldl' (flip f) g xs
      where
        f x = G.addEdge (G.LEdge dummyTermEdgeLabel $ G.Edge x dummyTermNode)


test :: Dlt.Rooted
test = ( 15,
         Im.fromList
         [ (0,IntSet.fromList [12])
         , (1,IntSet.fromList [3])
         , (2,IntSet.fromList [1,11])
         , (3,IntSet.fromList [20])
         , (4,IntSet.fromList [3])
         , (5,IntSet.fromList [8])
         , (6,IntSet.fromList [21])
         , (7,IntSet.fromList [2,13])
         , (8,IntSet.fromList [7,18])
         , (9,IntSet.fromList [20])
         , (10,IntSet.fromList [5])
         , (11,IntSet.fromList [4,14])
         , (12,IntSet.fromList [17])
         , (13,IntSet.fromList [3])
         , (14,IntSet.fromList [9,16])
         , (15,IntSet.fromList [6])  
         , (16,IntSet.fromList [3])
         , (17,IntSet.fromList [10])
         , (18,IntSet.fromList [19])
         , (19,IntSet.fromList [3])
         , (20,IntSet.fromList [])
         , (21,IntSet.fromList [0])
         ])

test2 :: Dlt.Rooted
test2 = ( 20,
         Im.fromList
         [ (0,IntSet.fromList [12])
         , (1,IntSet.fromList [3])
         , (2,IntSet.fromList [1,11])
         , (3,IntSet.fromList [20])
         , (4,IntSet.fromList [3])
         , (5,IntSet.fromList [8])
         , (6,IntSet.fromList [21])
         , (7,IntSet.fromList [2,13])
         , (8,IntSet.fromList [7,18])
         , (9,IntSet.fromList [20])
         , (10,IntSet.fromList [5])
         , (11,IntSet.fromList [4,14])
         , (12,IntSet.fromList [17])
         , (13,IntSet.fromList [3])
         , (14,IntSet.fromList [9,16])
         , (15,IntSet.fromList [6])
         , (16,IntSet.fromList [3])
         , (17,IntSet.fromList [10])
         , (18,IntSet.fromList [19])
         , (19,IntSet.fromList [3])
         , (20,IntSet.fromList [])
         , (21,IntSet.fromList [0])
         ])
