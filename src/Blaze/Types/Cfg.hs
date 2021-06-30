module Blaze.Types.Cfg
  ( module Blaze.Types.Cfg
  , G.succs
  , G.preds
  , G.nodes
  , G.removeNode
  , G.addNodes
  , G.hasNode
  , G.transpose
  , G.bfs
  , G.sinks
  ) where

import qualified Prelude as P
import Blaze.Graph (Graph)
import qualified Blaze.Graph as Graph
import qualified Blaze.Types.Graph as G
import Blaze.Prelude hiding (pred, succ)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Pil (Stmt, RetOp, Expression, TailCallOp, BranchCondOp, CallDest)
import Blaze.Types.Pil.Common (Ctx)
import qualified Data.Set as Set

type PilNode = CfNode [Stmt]
type PilEdge = CfEdge [Stmt]
type PilCallNode = CallNode [Stmt]
type PilBbNode = BasicBlockNode [Stmt]
type PilNodeMapEntry = (PilNode, [Stmt])
type PilCfg = Cfg [Stmt]

-- TODO: Consider adding more depending on what is being represented.
data BranchType
  = TrueBranch
  | FalseBranch
  | UnconditionalBranch
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Hashable BranchType

data BasicBlockNode a = BasicBlockNode
  { ctx :: Ctx
  , start :: Address
  , end :: Address
  , uuid :: UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

-- TODO: maybe one day add `expr` parameter to this type
-- to be used instead of `()` in `CallDest ()`
data CallNode a = CallNode
  { ctx :: Ctx
  , start :: Address
  , callDest :: CallDest ()
  , uuid :: UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data EnterFuncNode a = EnterFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , uuid :: UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data LeaveFuncNode a = LeaveFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , uuid :: UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

{- |Terminal nodes are nodes in CFG that have no successor.
In a function, these nodes correspond to either: a return statement that
resumes control flow to the caller; an exit statement (see NORET in MLIL SSA)
that terminates normal control flow of the program (i.e., there may be signal handlers);
and a tail call where control flow moves to the call target. 
-}
data TerminalNode a 
  = TermRet (ReturnNode a)
  | TermExit (ExitNode a)
  | TermTailCall (TailCallNode a)
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
  deriving anyclass (Hashable)

data ReturnNode a = ReturnNode
  { basicBlock :: BasicBlockNode a
  , retOp :: RetOp Expression
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
  deriving anyclass (Hashable)

newtype ExitNode a = ExitNode
  { basicBlock :: BasicBlockNode a
  }
  deriving (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data TailCallNode a = TailCallNode
  { basicBlock :: BasicBlockNode a
  , tailCallOp :: TailCallOp Expression
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
  deriving anyclass (Hashable)

data BranchNode a = BranchNode
  { basicBlock :: BasicBlockNode a
  , branchCondOp :: BranchCondOp Expression
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
  deriving anyclass (Hashable)

-- TODO: Consider moving the interprocedural nodes into a separate type
--       specific to InterCfg. Especially now with EnterFunc/LeaveFunc
-- TODO: Once the Cfg only needs to support PIL statements, remove type parameter
--       a and use more-specific definitions of the various node types. E.g.,
--       a CallNode should have a single CallStatement entry, not a nodeData of type a.
-- TODO: Consider the different needs of CFG representations. E.g., a CFG corresponding
--       to a function vs. a CFG corresponding to an arbitrary function CFG subgraph
data CfNode a
  = BasicBlock (BasicBlockNode a)
  | Call (CallNode a)
  | EnterFunc (EnterFuncNode a)
  | LeaveFunc (LeaveFuncNode a)
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

-- | Attempts to set uuid field for cfnode constructor
mkUniqueNode :: CfNode a -> IO (CfNode a)
mkUniqueNode n = do
  uuid' <- randomIO
  return $ case n of
    BasicBlock x -> BasicBlock $ x & #uuid .~ uuid'
    Call x -> Call $ x & #uuid .~ uuid'
    EnterFunc x -> EnterFunc $ x & #uuid .~ uuid'
    LeaveFunc x -> LeaveFunc $ x & #uuid .~ uuid'


data CfEdge a = CfEdge
  { src :: CfNode a
  , dst :: CfNode a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

fromTupleEdge :: (BranchType, (CfNode a, CfNode a)) -> CfEdge a
fromTupleEdge (bt, (a, b)) = CfEdge a b bt

fromLEdge :: G.LEdge BranchType (CfNode a) -> CfEdge a
fromLEdge (G.LEdge bt (G.Edge s d)) = CfEdge s d bt

toLEdge :: CfEdge a -> G.LEdge BranchType (CfNode a)
toLEdge (CfEdge a b bt) = G.LEdge bt (G.Edge a b)

data CodeReference a = CodeReference
  { function :: Function
  , startIndex :: a
  , endIndex :: a
  }
  deriving (Eq, Ord, Functor, Show, Generic)

type NodeRefMap a b = HashMap a b

type NodeRefMapEntry a b = (a, b)

newtype Dominators a = Dominators (HashMap (CfNode a) (HashSet (CfNode a)))
  deriving (Eq, Ord, Show, Generic)

newtype PostDominators a = PostDominators (HashMap (CfNode a) (HashSet (CfNode a)))
  deriving (Eq, Ord, Show, Generic)

{- | A non-empty graph that consists of a strongly-connected component
 with a single root node (a node with no incoming edges).
 This is intended to be the graph representation of a CFG.
 A user of this API probably wants to work with the 'Cfg' type that
 includes additional information about the CFG.
-}
-- uses `CfNode ()` as identifier, which gets full `CfNode a` in attr map
type ControlFlowGraph a = AlgaGraph BranchType (CfNode a) (CfNode ())

asIdNode :: CfNode a -> CfNode ()
asIdNode = void

getFullNode' :: ControlFlowGraph a -> CfNode () -> Maybe (CfNode a)
getFullNode' g n = G.getNodeAttr n g

getFullNode :: Cfg a -> CfNode () -> CfNode a
getFullNode g = fromJust . getFullNode' (g ^. #graph)

getFullNodeMay :: Cfg a -> CfNode () -> Maybe (CfNode a)
getFullNodeMay g = getFullNode' $ g ^. #graph

getFullEdge :: Ord a => Cfg a -> G.Edge (CfNode a) -> Maybe (CfEdge a)
getFullEdge cfg e = CfEdge (e ^. #src) (e ^. #dst) <$> G.getEdgeLabel e cfg

toFullNodeSet :: Ord a => Cfg a -> Set (CfNode ()) -> Set (CfNode a)
toFullNodeSet g = Set.map $ getFullNode g

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: forall a .Ord a
                   => CfNode a
                   -> [CfNode a]
                   -> [CfEdge a]
                   -> ControlFlowGraph a
mkControlFlowGraph root' ns es = Graph.addNodesWithAttrs attrList
  . Graph.fromEdges
  $ fmap asIdNode . toLEdge <$> es
  where
    nodesInEdges = concatMap (\e -> [e ^. #src, e ^. #dst]) es
    allNodes :: [CfNode a]
    allNodes = Set.toList . Set.fromList $ (root' : ns) <> nodesInEdges

    attrList :: [(CfNode (), CfNode a)]
    attrList = (\n -> (asIdNode n, n)) <$> allNodes

asAttrTuple :: CfNode a -> (CfNode (), CfNode a)
asAttrTuple x = (asIdNode x, x)

-- TODO: Consider removing type parameter once a PIL CFG can be constructed
--       w/o an intermediate MLIL SSA CFG.
data Cfg a = Cfg
  { graph :: ControlFlowGraph a
  , root :: CfNode a
  }
  deriving (Eq, Show, Generic)

mkCfg :: forall a. Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
mkCfg root' rest es =
  Cfg
    { graph = mkControlFlowGraph root' rest es
    , root = root'
    }

edges :: Ord a => Cfg a -> [CfEdge a]
edges = fmap fromLEdge . G.edges

removeEdge :: Ord a => CfEdge a -> Cfg a -> Cfg a
removeEdge e = G.removeEdge $ toLEdge e ^. #edge

addEdge :: Ord a => CfEdge a -> Cfg a -> Cfg a
addEdge e = G.addEdge $ toLEdge e

addEdges :: Ord a => [CfEdge a] -> Cfg a -> Cfg a
addEdges xs cfg = foldl' (flip addEdge) cfg xs

removeEdges :: Ord a => [CfEdge a] -> Cfg a -> Cfg a
removeEdges xs cfg = foldl' (flip removeEdge) cfg xs
  
-- TODO: move this to Graph class
predEdges :: Ord a => CfNode a -> Cfg a -> Set (CfEdge a)
predEdges n cfg = Set.map (\pred -> fromJust . getFullEdge cfg $ G.Edge pred n)
  . G.preds n
  $ cfg

-- TODO: move this to Graph class
succEdges :: Ord a => CfNode a -> Cfg a -> Set (CfEdge a)
succEdges n cfg = Set.map (fromJust . getFullEdge cfg . G.Edge n)
  . G.succs n
  $ cfg

getNodeData :: CfNode a -> Maybe a
getNodeData = \case
  BasicBlock x -> Just $ x ^. #nodeData
  Call x -> Just $ x ^. #nodeData
  EnterFunc x -> Just $ x ^. #nodeData
  LeaveFunc x -> Just $ x ^. #nodeData

setNodeData :: Ord a => a -> CfNode a -> Cfg a -> Cfg a
setNodeData a n = G.setNodeAttr (fmap (const a) n) n

updateNodeData :: Ord a => (a -> a) -> CfNode a -> Cfg a -> Cfg a
updateNodeData f n cfg =
  maybe cfg (\x -> setNodeData (f x) n cfg) $ getNodeData n

mergeBranchTypeDefault :: BranchType -> BranchType -> BranchType
mergeBranchTypeDefault UnconditionalBranch UnconditionalBranch = UnconditionalBranch
mergeBranchTypeDefault TrueBranch FalseBranch = UnconditionalBranch
mergeBranchTypeDefault FalseBranch TrueBranch = UnconditionalBranch
mergeBranchTypeDefault bt1 bt2 = P.error
  $ "removeAndRebindEdges: cannot merge branch types: "
  <> show bt1 <> " "
  <> show bt2

-- | Removes a node and makes edges from preds to succs
--   uses pred->node branch type for pred->succ
removeAndRebindEdges :: Ord a => CfNode a -> Cfg a -> Cfg a
removeAndRebindEdges = removeAndRebindEdges_ mergeBranchTypeDefault

-- | Removes a node and makes edges from preds to succs
--   uses pred->node branch type for pred->succ
--   mergeBranchType specifies how to merge branch type of
--   newly created edge with possible existing edge
removeAndRebindEdges_ :: Ord a
  => (BranchType -> BranchType -> BranchType)
  -> CfNode a -> Cfg a -> Cfg a
removeAndRebindEdges_ mergeBranchType n cfg' = G.removeNode n
  . addEdges newEdges
  $ cfg'
  where
    preds = Set.toList $ G.preds n cfg'
    succs = Set.toList $ G.succs n cfg'
    newEdges = do
      pred <- preds
      let bt = fromJust $ G.getEdgeLabel (G.Edge pred n) cfg'
      succ <- succs
      case G.getEdgeLabel (G.Edge pred succ) cfg' of
        Nothing -> return $ CfEdge pred succ bt
        Just preexistingEdgeType -> return
          . CfEdge pred succ
          $ mergeBranchType bt preexistingEdgeType

-- | Removes any nodes where predicate is True, except root node.
removeNodesBy
  :: Ord a
  => (BranchType -> BranchType -> BranchType)
  -> (CfNode a -> Bool)
  -> Cfg a
  -> Cfg a
removeNodesBy branchMerge p cfg = foldl'
  (flip $ removeAndRebindEdges_ branchMerge)
  cfg
  targetNodes
  where
    targetNodes = filter g . Set.toList $ G.nodes cfg
    g x = x /= (cfg ^. #root) && p x

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance Ord a => Graph BranchType (CfNode a) (CfNode a) (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use mkCfg to construct a CFG."
  fromEdges _ = error "Use mkCfg to construct a CFG."
  succs node g = toFullNodeSet g 
    . G.succs (asIdNode node)
    . view #graph
    $ g
  preds node g = toFullNodeSet g
    . G.preds (asIdNode node)
    . view #graph
    $ g
  nodes g = toFullNodeSet g . G.nodes $ g ^. #graph
  edges g = fmap (fmap $ getFullNode g) . G.edges $ g ^. #graph

  getEdgeLabel edge = G.getEdgeLabel (asIdNode <$> edge) . view #graph
  setEdgeLabel label edge cfg = cfg & #graph %~ G.setEdgeLabel label (asIdNode <$> edge)

  -- Sort of pointless. Could just return `node`
  getNodeAttr node = G.getNodeAttr (asIdNode node) . view #graph

  setNodeAttr attr node cfg = cfg & #graph %~ G.setNodeAttr attr (asIdNode node)
  
  removeEdge edge = over #graph $ G.removeEdge (asIdNode <$> edge)
  removeNode node = over #graph $ G.removeNode (asIdNode node)
  addNodes nodes = over #graph $
    G.addNodesWithAttrs (asAttrTuple <$> nodes)
  addNodesWithAttrs nodes = over #graph $
    G.addNodesWithAttrs (over _1 asIdNode <$> nodes)
  addEdge lblEdge = over #graph $
    G.addEdge (asIdNode <$> lblEdge)
    . G.addNodesWithAttrs (asAttrTuple <$> toList lblEdge)

  hasNode node = Graph.hasNode (asIdNode node) . view #graph
  transpose = over #graph Graph.transpose
  bfs startNodes cfg = fmap (fmap $ getFullNode cfg)
    . Graph.bfs (asIdNode <$> startNodes)
    . view #graph
    $ cfg

  -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
  subgraph pred cfg =
    cfg & #graph %~ Graph.subgraph (pred . getFullNode cfg)

data FuncContext = FuncContext
  { func :: Function
  , uuid :: UUID
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data LoopContext = LoopContext
  { funcCtx :: FuncContext
  , uuid :: UUID
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- TODO: Merge with Ctx in Blaze.Types.Common
--       This version provides more information but use
--       existing Ctx to avoid too many changes at once.
data Context 
  = FuncCtx FuncContext
  | LoopCtx LoopContext 
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- |Track an incrementing counter for use in generating variable names.
newtype Counter a = Counter (State Int a)
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState Int)

runCounter :: Counter a -> Int -> (a, Int)
runCounter (Counter s) = runState s

nextVal :: Counter Int
nextVal = do
  x <- get
  put (x + 1)
  return x 

