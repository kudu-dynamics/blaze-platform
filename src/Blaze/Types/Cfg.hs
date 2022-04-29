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
import Blaze.Types.Pil (Stmt, RetOp, Expression, BranchCondOp, CallDest, CtxId, Ctx)
import qualified Blaze.Types.Pil as Pil
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Lens (ix)

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

-- | A node type that represents a "grouped" sub-CFG within a larger CFG
data GroupingNode a = GroupingNode
  { termNode :: CfNode a
  , uuid :: UUID
  , grouping :: Cfg a
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
  | TermNoRet (NoRetNode a)
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

newtype NoRetNode a = NoRetNode
  { basicBlock :: BasicBlockNode a
  }
  deriving (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data BranchNode a = BranchNode
  { basicBlock :: BasicBlockNode a
  , branchCondOp :: BranchCondOp Expression
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
  deriving anyclass (Hashable)


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
  | Grouping (GroupingNode a)
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
    Grouping x -> Grouping $ x & #uuid .~ uuid'

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

asIdEdge :: CfEdge a -> CfEdge ()
asIdEdge = void

getFullNode' :: ControlFlowGraph a -> CfNode () -> Maybe (CfNode a)
getFullNode' g n = G.getNodeAttr n g

getFullNode :: Cfg a -> CfNode () -> CfNode a
getFullNode g = fromJust . getFullNode' (g ^. #graph)

getFullNodeMay :: Cfg a -> CfNode () -> Maybe (CfNode a)
getFullNodeMay g = getFullNode' $ g ^. #graph

getFullEdge :: (Hashable a, Eq a) => Cfg a -> G.Edge (CfNode a) -> Maybe (CfEdge a)
getFullEdge cfg e = CfEdge (e ^. #src) (e ^. #dst) <$> G.getEdgeLabel e cfg

toFullNodeSet :: (Hashable a, Eq a) => Cfg a -> HashSet (CfNode ()) -> HashSet (CfNode a)
toFullNodeSet g = HashSet.map $ getFullNode g

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: forall a .(Hashable a, Eq a)
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
    allNodes = HashSet.toList . HashSet.fromList $ (root' : ns) <> nodesInEdges

    attrList :: [(CfNode (), CfNode a)]
    attrList = (\n -> (asIdNode n, n)) <$> allNodes

asAttrTuple :: CfNode a -> (CfNode (), CfNode a)
asAttrTuple x = (asIdNode x, x)

-- TODO: Consider removing type parameter once a PIL CFG can be constructed
--       w/o an intermediate MLIL SSA CFG.
data Cfg a = Cfg
  { graph :: ControlFlowGraph a
  , root :: CfNode a
  -- CtxIds are incremental, and this is the Id that should be used the next time
  -- a new Ctx is needed (ie for expanding a function call in Interprocedural module).
  -- After it is used, this should be incremented.
  , nextCtxIndex :: CtxId
  }
  deriving (Eq, Ord, Show, Generic)

incNextCtxIndex :: Cfg a -> Cfg a
incNextCtxIndex = over #nextCtxIndex (+1)

instance Functor Cfg where
  fmap f cfg = Cfg
    { graph = G.mapAttrs (fmap f) $ cfg ^. #graph
    , root = f <$> (cfg ^. #root)
    , nextCtxIndex = cfg ^. #nextCtxIndex
    }

instance Foldable Cfg where
  foldMap f = G.foldMapAttrs g . view #graph
    where
      g = \case
        Grouping x -> f (x ^. #nodeData) `mappend` foldMap f (x ^. #grouping)
        BasicBlock x -> f $ x ^. #nodeData
        Call x -> f $ x ^. #nodeData
        EnterFunc x -> f $ x ^. #nodeData
        LeaveFunc x -> f $ x ^. #nodeData

instance Traversable Cfg where
  traverse f cfg = Cfg
    <$> G.traverseAttrs (traverse f) (cfg ^. #graph)
    <*> traverse f (cfg ^. #root)
    <*> pure (cfg ^. #nextCtxIndex)

instance Hashable a => Hashable (Cfg a) where
  hashWithSalt n = hashWithSalt n . toTransport
  hash = hash . toTransport

instance ToJSON a => ToJSON (Cfg a) where
 toJSON = toJSON . toTransport

instance (Eq a, Hashable a, FromJSON a) => FromJSON (Cfg a) where
 parseJSON = fmap fromTransport . parseJSON


mkCfg :: forall a. (Hashable a, Eq a) => CtxId -> CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
mkCfg nextCtxIndex_ root_ rest es =
  Cfg
    { graph = mkControlFlowGraph root_ rest es
    , root = root_
    , nextCtxIndex = nextCtxIndex_
    }

edges :: (Hashable a, Eq a) => Cfg a -> [CfEdge a]
edges = fmap fromLEdge . G.edges

removeIdEdge :: CfEdge () -> Cfg a -> Cfg a
removeIdEdge e cfg = cfg & #graph %~ G.removeEdge (toLEdge e ^. #edge)

removeEdge :: (Hashable a, Eq a) => CfEdge a -> Cfg a -> Cfg a
removeEdge e = G.removeEdge $ toLEdge e ^. #edge

addEdge :: (Hashable a, Eq a) => CfEdge a -> Cfg a -> Cfg a
addEdge e = G.addEdge $ toLEdge e

addEdges :: (Hashable a, Eq a) => [CfEdge a] -> Cfg a -> Cfg a
addEdges xs cfg = foldl' (flip addEdge) cfg xs

removeEdges :: (Hashable a, Eq a) => [CfEdge a] -> Cfg a -> Cfg a
removeEdges xs cfg = foldl' (flip removeEdge) cfg xs

-- TODO: move this to Graph class
predEdges :: (Hashable a, Eq a) => CfNode a -> Cfg a -> HashSet (CfEdge a)
predEdges n cfg = HashSet.map (\pred -> fromJust . getFullEdge cfg $ G.Edge pred n)
  . G.preds n
  $ cfg

-- TODO: move this to Graph class
succEdges :: (Hashable a, Eq a) => CfNode a -> Cfg a -> HashSet (CfEdge a)
succEdges n cfg = HashSet.map (fromJust . getFullEdge cfg . G.Edge n)
  . G.succs n
  $ cfg

getNodeData_ :: Cfg a -> CfNode () -> a
getNodeData_ g n = getNodeData $ getFullNode g n

getNodeData :: CfNode a -> a
getNodeData = \case
  BasicBlock x -> x ^. #nodeData
  Call x -> x ^. #nodeData
  EnterFunc x -> x ^. #nodeData
  LeaveFunc x -> x ^. #nodeData
  Grouping x -> x ^. #nodeData

getNodeUUID :: CfNode a -> UUID
getNodeUUID = \case
  BasicBlock x -> x ^. #uuid
  Call x -> x ^. #uuid
  EnterFunc x -> x ^. #uuid
  LeaveFunc x -> x ^. #uuid
  Grouping x -> x ^. #uuid

setNodeData :: (Hashable a, Eq a) => a -> CfNode a -> Cfg a -> Cfg a
setNodeData a n = G.setNodeAttr (fmap (const a) n) n

updateNodeData :: (Hashable a, Eq a) => (a -> a) -> CfNode a -> Cfg a -> Cfg a
updateNodeData f n = setNodeData (f $ getNodeData n) n

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
removeAndRebindEdges :: (Hashable a, Eq a) => CfNode a -> Cfg a -> Cfg a
removeAndRebindEdges = removeAndRebindEdges_ mergeBranchTypeDefault

-- | Removes a node and makes edges from preds to succs
--   uses pred->node branch type for pred->succ
--   mergeBranchType specifies how to merge branch type of
--   newly created edge with possible existing edge
removeAndRebindEdges_ :: (Hashable a, Eq a)
  => (BranchType -> BranchType -> BranchType)
  -> CfNode a -> Cfg a -> Cfg a
removeAndRebindEdges_ mergeBranchType n cfg' = G.removeNode n
  . addEdges newEdges
  $ cfg'
  where
    preds = HashSet.toList $ G.preds n cfg'
    succs = HashSet.toList $ G.succs n cfg'
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
  :: (Hashable a, Eq a)
  => (BranchType -> BranchType -> BranchType)
  -> (CfNode a -> Bool)
  -> Cfg a
  -> Cfg a
removeNodesBy branchMerge p cfg = foldl'
  (flip $ removeAndRebindEdges_ branchMerge)
  cfg
  targetNodes
  where
    targetNodes = filter g . HashSet.toList $ G.nodes cfg
    g x = asIdNode x /= asIdNode (cfg ^. #root) && p x

-- | Insert node between two other nodes.
insertNodeBetween :: (Hashable a, Eq a)
  => CfNode a
  -> CfNode a
  -> CfNode a
  -> Cfg a
  -> Cfg a
insertNodeBetween nodeA nodeB nodeMiddle cfg' =
  case G.getEdgeLabel (G.Edge nodeA nodeB) cfg' of
    Nothing -> cfg'
    Just lbl -> G.addEdge (G.LEdge lbl $ G.Edge nodeA nodeMiddle)
      . G.addEdge (G.LEdge lbl $ G.Edge nodeMiddle nodeB)
      . G.addNodes [nodeMiddle]
      . G.removeEdge (G.Edge nodeA nodeB)
      $ cfg'

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance (Hashable a, Eq a) => Graph BranchType (CfNode a) (CfNode a) (Cfg a) where
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

  setNodeAttr attr node cfg = cfg
    & #graph %~ G.setNodeAttr attr (asIdNode node)
    & #root %~ updateRoot
    where
      updateRoot oldRoot
        | asIdNode oldRoot == asIdNode node = attr
        | otherwise = oldRoot

  -- useless since `nodes` already returns nodes with their attrs
  getNodeAttrMap cfg = HashMap.fromList
    . fmap f
    . HashMap.toList
    . G.getNodeAttrMap
    $ cfg ^. #graph
    where
      f (_, b) = (b, b)

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

  reachable n cfg = fmap (getFullNode cfg)
    . Graph.reachable (asIdNode n)
    $ cfg ^. #graph

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

-- | Turns CallNode with TailCall op into two nodes:
-- 1. Regular callnode with Def/Call
-- 2. BasicBlockNode with return
splitTailCallNodes :: forall m. MonadIO m => Cfg [Stmt] -> m (Cfg [Stmt])
splitTailCallNodes ogCfg = foldM splitTailCall ogCfg tcNodes
  where
    splitTailCall :: Cfg [Stmt]
                  -> ( CfNode [Stmt]
                     , CallNode [Stmt]
                     , Pil.TailCallOp Pil.Expression
                     )
                  -> m (Cfg [Stmt])
    splitTailCall cfg (n, call, tc) = case tc ^. #ret of
      Nothing ->
        return $ setNodeData ([Pil.Call callOp] <> outStores) n cfg
      Just (pv, sz) -> do
        uuid' <- liftIO randomIO
        let retNode = BasicBlock $ BasicBlockNode
              { ctx = call ^. #ctx
              , start = call ^. #start
              , end = call ^. #start
              , uuid = uuid'
              , nodeData = outStores
                           <> [ Pil.Ret . Pil.RetOp
                                . Pil.Expression sz
                                . Pil.VAR . Pil.VarOp
                                $ pv
                              ]
              }
        case HashSet.toList $ G.succs n cfg of
          [] -> return
            . setNodeData [callStmt] n
            . G.addEdge (G.LEdge UnconditionalBranch $ G.Edge n retNode)
            $ cfg
          [succ] -> return
            . setNodeData [callStmt] n
            . insertNodeBetween n succ retNode
            $ cfg
          _ -> P.error "splitTailCallNodes: TailCall node has multiple successors"
        where
          callStmt = Pil.Def . Pil.DefOp pv
            . Pil.Expression sz . Pil.CALL
            $ callOp
      where
        outStores = drop 1 $ call ^. #nodeData
        callOp :: Pil.CallOp Pil.Expression
        callOp = Pil.CallOp (tc ^. #dest) (tc ^. #name) (tc ^. #args)

    getTcNodeOp :: CfNode [Stmt]
                -> Maybe ( CfNode [Stmt]
                         , CallNode [Stmt]
                         , Pil.TailCallOp Pil.Expression
                         )
    getTcNodeOp n = (n,,) <$> n ^? #_Call <*> n ^? #_Call . #nodeData . ix 0 . #_TailCall

    tcNodes = mapMaybe getTcNodeOp . HashSet.toList $ G.nodes ogCfg

mapAttrs :: (a -> b) -> Cfg a -> Cfg b
mapAttrs f cfg = Cfg
  { root = fmap f $ cfg ^. #root
  , graph = G.mapAttrs (fmap f) $ cfg ^. #graph
  , nextCtxIndex = cfg ^. #nextCtxIndex
  }

-- | Traverses monadic action over Cfg attrs.
-- The first action is on the root node, the result of which is remembered
-- and used again later when root is encountered in the nodeAttrList
-- so that the monadic action isn't run twice for the same node.
traverseAttrs :: Monad m => (a -> m b) -> Cfg a -> m (Cfg b)
traverseAttrs f cfg = do
  root' <- traverse f $ cfg ^. #root
  attrList <- fmap HashMap.toList
              . traverse (g root')
              . G.getNodeAttrMap
              $ cfg ^. #graph
  return $ Cfg { root = root'
               , graph = G.addNodesWithAttrs attrList
                         . G.fromEdges
                         . G.edges
                         $ cfg ^. #graph
               , nextCtxIndex = cfg ^. #nextCtxIndex
               }
  where
    rootId = asIdNode $ cfg ^. #root
    g root' n
      | rootId == asIdNode n = return root'
      | otherwise = traverse f n

gatherCfgData :: (Hashable a, Eq a) => Cfg [a] -> [a]
gatherCfgData = concatMap concat . HashMap.elems . G.getNodeAttrMap

data UndecidedIfBranches = UndecidedIfBranches
  { falseEdge :: CfEdge ()
  , trueEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

-- BranchingType and BranchCond will need to be refactored at some point
-- to accomodate for switch statements, jump tables
data BranchingType = OnlyTrue (CfEdge ())
                   | OnlyFalse (CfEdge ())
                   | Undecided UndecidedIfBranches
                   deriving (Eq, Ord, Show, Generic)

data BranchCond a = BranchCond
  { conditionStatementIndex :: Maybe Int
  , condition :: a
  , branchingType :: BranchingType
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


-- | CfgTransport is an intermediary type for Hashable, FromJSON, and ToJSON instances
data CfgTransport a = CfgTransport
  { transportEdges :: [CfEdge ()]
  , transportRoot :: CfNode ()
  , transportNodes :: [(CfNode (), CfNode a)]
  , transportNextCtxIndex :: CtxId
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

toTransport :: forall a. Cfg a -> CfgTransport a
toTransport pcfg = CfgTransport
  { transportEdges = edges'
  , transportRoot = root'
  , transportNodes = nodes'
  , transportNextCtxIndex = pcfg ^. #nextCtxIndex
  }
  where
    root' = void $ pcfg ^. #root

    nodes' :: [(CfNode (), CfNode a)]
    nodes' = HashMap.toList $ pcfg ^. #graph . #nodeAttrMap

    edges' :: [CfEdge ()]
    edges' = fmap fromLEdge . G.edges $ pcfg ^. #graph

fromTransport :: (Eq a, Hashable a) => CfgTransport a -> Cfg a
fromTransport t = mkCfg (t ^. #transportNextCtxIndex) root' nodes' edges'
  where
    nodeMap = HashMap.fromList $ t ^. #transportNodes

    fullNode = fromJust . flip HashMap.lookup nodeMap

    fullEdge e = CfEdge
      { src = fullNode $ e ^. #src
      , dst = fullNode $ e ^. #dst
      , branchType = e ^. #branchType
      }

    root' = fullNode $ t ^. #transportRoot

    nodes' = snd <$> t ^. #transportNodes

    edges' = fullEdge <$> t ^. #transportEdges
