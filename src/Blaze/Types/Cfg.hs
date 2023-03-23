{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Blaze.Prelude hiding (pred, succ)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (Graph, Identifiable (getNodeId), NodeId (NodeId), Edge (Edge), LEdge (LEdge))
import Blaze.Types.Graph qualified as G
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Graph.Alga as Ag
import Blaze.Types.Pil (BranchCondOp, CallDest, Ctx, CtxId, Expression, RetOp, Stmt)
import Blaze.Types.Pil qualified as Pil
import Control.Arrow ((&&&))
import Control.Lens (ix)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.HashSet qualified as HashSet
import Prelude qualified as P

type PilNode = CfNode [Stmt]
type PilEdge = CfEdge PilNode
type PilCallNode = CallNode [Stmt]
type PilBbNode = BasicBlockNode [Stmt]
type PilNodeMapEntry = (PilNode, [Stmt])
type PilCfg = Cfg PilNode

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

data CallNode a = CallNode
  { ctx :: Ctx
  , start :: Address
  , callDest :: CallDest Expression
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
  { termNodeId :: NodeId UUID
  , uuid :: UUID
  , grouping :: Cfg (CfNode a)
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

instance Identifiable (CfNode a) UUID where
  getNodeId = NodeId . getNodeUUID

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
  { src :: a
  , dst :: a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

fromTupleEdge :: (BranchType, (a, a)) -> CfEdge a
fromTupleEdge (bt, (x, y)) = CfEdge x y bt

fromLEdge :: LEdge BranchType a -> CfEdge a
fromLEdge (LEdge bt (Edge s d)) = CfEdge s d bt

toLEdge :: CfEdge a -> LEdge BranchType a
toLEdge (CfEdge a b bt) = LEdge bt (Edge a b)

data CodeReference a = CodeReference
  { function :: Function
  , startIndex :: a
  , endIndex :: a
  }
  deriving (Eq, Ord, Functor, Show, Generic, Hashable)

type NodeRefMap a b = HashMap a b

type NodeRefMapEntry a b = (a, b)

{- | A non-empty graph that consists of a strongly-connected component
 with a single root node (a node with no incoming edges).
 This is intended to be the graph representation of a CFG.
 A user of this API probably wants to work with the 'Cfg' type that
 includes additional information about the CFG.
-}
type ControlFlowGraph n = AlgaGraph BranchType UUID n

getEdge :: Hashable a => Cfg (CfNode a) -> Edge (CfNode a) -> Maybe (CfEdge (CfNode a))
getEdge cfg e = CfEdge (e ^. #src) (e ^. #dst) <$> G.getEdgeLabel e cfg

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph ::
  (Identifiable a UUID, Hashable a) =>
  a ->
  [a] ->
  [CfEdge a] ->
  ControlFlowGraph a
mkControlFlowGraph root' ns es =
  G.addNodes (root' : ns) $ G.fromEdges (fmap toLEdge es)


data MkCfgRootError
  = ZeroRootNodes
  | MultipleRootNodes
  deriving (Eq, Ord, Read, Show, Generic, Hashable)

mkCfgFindRoot ::
  (Identifiable a UUID, Hashable a) =>
  CtxId ->
  [a] ->
  [CfEdge a] ->
  Either MkCfgRootError (Cfg a)
mkCfgFindRoot nextCtxIndex_ nodes_ edges_ = case HashSet.toList (G.sources g) of
  [] -> Left ZeroRootNodes
  [rootNode] -> Right $ Cfg
    { graph =  g
    , rootId = getNodeId rootNode
    , nextCtxIndex = nextCtxIndex_
    }
  _ -> Left MultipleRootNodes
  where
    g = G.addNodes nodes_
        . G.fromEdges
        . fmap toLEdge
        $ edges_

-- TODO: Consider removing type parameter once a PIL CFG can be constructed
--       w/o an intermediate MLIL SSA CFG.
data Cfg n = Cfg
  { graph :: ControlFlowGraph n
  , rootId :: NodeId UUID
  -- CtxIds are incremental, and this is the Id that should be used the next time
  -- a new Ctx is needed (ie for expanding a function call in Interprocedural module).
  -- After it is used, this should be incremented.
  , nextCtxIndex :: CtxId
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

incNextCtxIndex :: Cfg n -> Cfg n
incNextCtxIndex = over #nextCtxIndex (+1)

instance (Identifiable n UUID, Hashable n) => Hashable (Cfg n) where
  hashWithSalt n = hashWithSalt n . toTransport
  hash = hash . toTransport

instance (Identifiable n UUID, Hashable n, ToJSON n) => ToJSON (Cfg n) where
 toJSON = toJSON . toTransport

instance (Identifiable n UUID, Hashable n, FromJSON n) => FromJSON (Cfg n) where
 parseJSON = fmap fromTransport . parseJSON


mkCfg ::
  (Identifiable n UUID, Hashable n) =>
  CtxId ->
  n ->
  [n] ->
  [CfEdge n] ->
  Cfg n
mkCfg nextCtxIndex_ root_ rest es =
  Cfg
    { graph = mkControlFlowGraph root_ rest es
    , rootId = getNodeId root_
    , nextCtxIndex = nextCtxIndex_
    }

edges :: Hashable a => Cfg (CfNode a) -> [CfEdge (CfNode a)]
edges = fmap fromLEdge . G.edges

removeEdge :: Hashable a => CfEdge (CfNode a) -> Cfg (CfNode a) -> Cfg (CfNode a)
removeEdge e = G.removeEdge $ toLEdge e ^. #edge

addEdge :: Hashable a => CfEdge (CfNode a) -> Cfg (CfNode a) -> Cfg (CfNode a)
addEdge e = G.addEdge $ toLEdge e

addEdges :: Hashable a => [CfEdge (CfNode a)] -> Cfg (CfNode a) -> Cfg (CfNode a)
addEdges xs cfg = foldl' (flip addEdge) cfg xs

removeEdges :: Hashable a => [CfEdge (CfNode a)] -> Cfg (CfNode a) -> Cfg (CfNode a)
removeEdges xs cfg = foldl' (flip removeEdge) cfg xs

-- TODO: move this to Graph class
predEdges :: Hashable a => CfNode a -> Cfg (CfNode a) -> HashSet (CfEdge (CfNode a))
predEdges n cfg = HashSet.map fromLEdge $ G.predEdges n cfg

-- TODO: move this to Graph class
succEdges :: Hashable a => CfNode a -> Cfg (CfNode a) -> HashSet (CfEdge (CfNode a))
succEdges n cfg = HashSet.map fromLEdge $ G.succEdges n cfg

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

setNodeData :: a -> CfNode a -> CfNode a
setNodeData d = \case
  BasicBlock n -> BasicBlock $ n & #nodeData .~ d
  Call n -> Call $ n & #nodeData .~ d
  EnterFunc n -> EnterFunc $ n & #nodeData .~ d
  LeaveFunc n -> LeaveFunc $ n & #nodeData .~ d
  -- TODO: Need to support updating the grouped CFG inside as well?
  Grouping n -> Grouping $ n & #nodeData .~ d

updateNodeData ::
  (a -> a) ->
  CfNode a ->
  CfNode a
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
removeAndRebindEdges :: Hashable a => CfNode a -> Cfg (CfNode a) -> Cfg (CfNode a)
removeAndRebindEdges = removeAndRebindEdges_ mergeBranchTypeDefault

-- | Removes a node and makes edges from preds to succs
--   uses pred->node branch type for pred->succ
--   mergeBranchType specifies how to merge branch type of
--   newly created edge with possible existing edge
removeAndRebindEdges_ ::
  Hashable a =>
  (BranchType -> BranchType -> BranchType)
  -> CfNode a -> Cfg (CfNode a) -> Cfg (CfNode a)
removeAndRebindEdges_ mergeBranchType n cfg' = G.removeNode n
  . addEdges newEdges
  $ cfg'
  where
    preds = HashSet.toList $ G.preds n cfg'
    succs = HashSet.toList $ G.succs n cfg'
    newEdges = do
      pred <- preds
      let bt = fromJust $ G.getEdgeLabel (Edge pred n) cfg'
      succ <- succs
      case G.getEdgeLabel (Edge pred succ) cfg' of
        Nothing -> return $ CfEdge pred succ bt
        Just preexistingEdgeType -> return
          . CfEdge pred succ
          $ mergeBranchType bt preexistingEdgeType

-- | Removes any nodes where predicate is True, except root node.
removeNodesBy ::
  Hashable a =>
  (BranchType -> BranchType -> BranchType) ->
  (CfNode a -> Bool) ->
  Cfg (CfNode a) ->
  Cfg (CfNode a)
removeNodesBy branchMerge p cfg =
  foldl'
    (flip $ removeAndRebindEdges_ branchMerge)
    cfg
    targetNodes
 where
  targetNodes = filter g . HashSet.toList $ G.nodes cfg
  g x = getNodeId x /= (cfg ^. #rootId) && p x

-- | Insert node between two other nodes.
insertNodeBetween ::
  Hashable a =>
  CfNode a ->
  CfNode a ->
  CfNode a ->
  Cfg (CfNode a) ->
  Cfg (CfNode a)
insertNodeBetween nodeA nodeB nodeMiddle cfg' =
  case G.getEdgeLabel (Edge nodeA nodeB) cfg' of
    Nothing -> cfg'
    Just lbl ->
      G.addEdge (LEdge lbl $ Edge nodeA nodeMiddle)
        . G.addEdge (LEdge lbl $ Edge nodeMiddle nodeB)
        . G.addNodes [nodeMiddle]
        . G.removeEdge (Edge nodeA nodeB)
        $ cfg'

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance (Hashable a, Identifiable a UUID) => Graph BranchType a Cfg where
  succs node = G.succs node . view #graph
  preds node = G.preds node . view #graph
  nodes g = G.nodes $ g ^. #graph
  edges g = G.edges $ g ^. #graph

  getEdgeLabel edge = G.getEdgeLabel edge . view #graph
  setEdgeLabel label edge cfg = cfg & #graph %~ G.setEdgeLabel label edge

  removeEdge edge = over #graph $ G.removeEdge edge
  removeNode node = over #graph $ G.removeNode node
  addNodes nodes = over #graph $ G.addNodes nodes
  addEdge lblEdge = over #graph $ G.addEdge lblEdge

  hasNode node = G.hasNode node . view #graph
  updateNode f node = over #graph (G.updateNode f node)

  transpose = over #graph G.transpose
  bfs startNodes = G.bfs startNodes . view #graph

  -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
  subgraph pred cfg = cfg & #graph %~ G.subgraph pred

  reachable n cfg = G.reachable n $ cfg ^. #graph

-- | A convenience function that uses 'fromJust' to extract the root
-- node from a 'Maybe n'.
getRootNode :: Cfg n -> n
getRootNode cfg = fromJust $ Ag.getNode (cfg ^. #graph) (cfg ^. #rootId)

class HasCtx a where
  getCtx :: a -> Ctx

instance HasCtx a => HasCtx (Cfg a) where
  getCtx = getCtx . getRootNode

instance HasCtx (CfNode a) where
  getCtx = \case
    BasicBlock bb -> bb ^. #ctx
    Call n -> n ^. #ctx
    EnterFunc n -> n ^. #prevCtx
    LeaveFunc n -> n ^. #nextCtx
    Grouping n -> getCtx $ n ^. #grouping

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

{- | Turns CallNode with TailCall op into two nodes:
 1. Regular callnode with Def/Call
 2. BasicBlockNode with return
-}
splitTailCallNodes :: forall m. (MonadIO m, (Identifiable PilNode UUID)) => Cfg PilNode -> m (Cfg PilNode)
splitTailCallNodes ogCfg = foldM splitTailCall ogCfg tcNodes
 where
  splitTailCall ::
    Cfg PilNode ->
    ( PilNode
    , CallNode [Stmt]
    , Pil.TailCallOp Pil.Expression
    ) ->
    m (Cfg PilNode)
  splitTailCall cfg (n, call, tc) = case tc ^. #ret of
    Nothing ->
      return $ G.updateNode (setNodeData ([Pil.Call callOp] <> outStores)) n cfg
    Just (pv, sz) -> do
      uuid' <- liftIO randomIO
      let retNode =
            BasicBlock $
              BasicBlockNode
                { ctx = call ^. #ctx
                , start = call ^. #start
                , end = call ^. #start
                , uuid = uuid'
                , nodeData =
                    outStores
                      <> [ Pil.Ret . Pil.RetOp
                            . Pil.Expression sz
                            . Pil.VAR
                            . Pil.VarOp
                            $ pv
                         ]
                }
      case HashSet.toList $ G.succs n cfg of
        [] ->
          return
            . G.updateNode (setNodeData [callStmt]) n
            . G.addEdge (LEdge UnconditionalBranch $ Edge n retNode)
            $ cfg
        [succ] ->
          return
            . G.updateNode (setNodeData [callStmt]) n
            . insertNodeBetween n succ retNode
            $ cfg
        _ -> P.error "splitTailCallNodes: TailCall node has multiple successors"
     where
      callStmt =
        Pil.Def . Pil.DefOp pv
          . Pil.Expression sz
          . Pil.CALL
          $ callOp
   where
    outStores :: [Stmt]
    outStores = drop 1 $ call ^. #nodeData
    callOp :: Pil.CallOp Pil.Expression
    callOp = Pil.CallOp (tc ^. #dest) (tc ^. #name) (tc ^. #args)

  getTcNodeOp ::
    PilNode ->
    Maybe
      ( PilNode
      , CallNode [Stmt]
      , Pil.TailCallOp Pil.Expression
      )
  getTcNodeOp n = (n,,) <$> n ^? #_Call <*> n ^? #_Call . #nodeData . ix 0 . #_TailCall

  tcNodes = mapMaybe getTcNodeOp . HashSet.toList $ G.nodes ogCfg

data UndecidedIfBranches n = UndecidedIfBranches
  { falseEdge :: CfEdge n
  , trueEdge :: CfEdge n
  } deriving (Eq, Ord, Show, Generic)

-- BranchingType and BranchCond will need to be refactored at some point
-- to accomodate for switch statements, jump tables
data BranchingType n = OnlyTrue (CfEdge n)
                     | OnlyFalse (CfEdge n)
                     | Undecided (UndecidedIfBranches n)
                     deriving (Eq, Ord, Show, Generic)

data BranchCond n a = BranchCond
  { conditionStatementIndex :: Maybe Int
  , condition :: a
  , branchingType :: BranchingType n
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


-- | CfgTransport is an intermediary type for Hashable, FromJSON, and ToJSON instances
data CfgTransport n = CfgTransport
  { transportEdges :: [CfEdge n]
  , transportRootId :: NodeId UUID
  , transportNodes :: [(NodeId UUID, n)]
  , transportNextCtxIndex :: CtxId
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

toTransport ::
  forall n.
  (Hashable n, Identifiable n UUID) =>
  Cfg n ->
  CfgTransport n
toTransport pcfg =
  CfgTransport
    { transportEdges = edges'
    , transportRootId = pcfg ^. #rootId
    , transportNodes = nodes'
    , transportNextCtxIndex = pcfg ^. #nextCtxIndex
    }
 where
  nodes' :: [(NodeId UUID, n)]
  nodes' = (getNodeId &&& identity) <$> (HashSet.toList . G.nodes $ pcfg ^. #graph)

  edges' :: [CfEdge n]
  edges' = fmap fromLEdge . G.edges $ pcfg ^. #graph

fromTransport :: (Identifiable n UUID, Hashable n) => CfgTransport n -> Cfg n
fromTransport t = mkCfg (t ^. #transportNextCtxIndex) root' nodes' edges'
  where
    root' = snd . fromJust $ find ((== t ^. #transportRootId) . fst) (t ^. #transportNodes)
    nodes' = snd <$> t ^. #transportNodes
    edges' = t ^. #transportEdges
