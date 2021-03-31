module Blaze.Types.Cfg where

import Blaze.Graph (Graph)
import qualified Blaze.Graph as Graph
import qualified Blaze.Types.Graph as G
import Blaze.Prelude hiding (pred)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Control.Arrow ((&&&))
import Blaze.Types.Pil (Stmt, RetOp, Expression, TailCallOp, BranchCondOp)
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
  { function :: Function
  , start :: Address
  , end :: Address
  , uuid :: Maybe UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data CallNode a = CallNode
  { function :: Function
  , start :: Address
  , uuid :: Maybe UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data EnterFuncNode a = EnterFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , uuid :: Maybe UUID
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data LeaveFuncNode a = LeaveFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , uuid :: Maybe UUID
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
  uuid' <- Just <$> randomIO
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
asIdNode = fmap $ const ()

getFullNode' :: ControlFlowGraph a -> CfNode () -> CfNode a
getFullNode' g n = fromJust $ G.getNodeAttr n g

getFullNode :: Cfg a -> CfNode () -> CfNode a
getFullNode g =  getFullNode' $ g ^. #graph


-- sadly inefficient
toFullNodeSet :: Ord a => Cfg a -> Set (CfNode ()) -> Set (CfNode a)
toFullNodeSet g s = Set.fromList
  . fmap (getFullNode g)
  . Set.toList
  $ s


-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: forall a. Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> ControlFlowGraph a
mkControlFlowGraph root' ns es = Graph.addNodesWithAttrs attrList
  . Graph.fromEdges
  $ fmap asIdNode . toLEdge <$> es
  where
    attrList :: [(CfNode (), CfNode a)]
    attrList = (\n -> (asIdNode n, n)) <$> (root' : ns)

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

-- TODO: Is there a deriving trick to have the compiler generate this?
-- TODO: Separate graph construction from graph use and/or graph algorithms
instance Ord a => Graph BranchType (CfNode a) (CfNode a) (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use mkCfg to construct a CFG."
  fromEdges _ = error "Use mkCfg to construct a CFG."
  succs node g = toFullNodeSet g 
    . G.succs (asIdNode node) . view #graph
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




-- module Blaze.Types.Cfg where

-- import Blaze.Graph (Graph)
-- import qualified Blaze.Graph as Graph
-- import Blaze.Prelude hiding (pred)
-- import Blaze.Types.Function (Function)
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import Control.Arrow ((&&&))
-- import Blaze.Types.Pil (Stmt, RetOp, Expression, TailCallOp, BranchCondOp)
-- import Blaze.Types.Pil.Common (Ctx)

-- type PilNode = CfNode [Stmt]
-- type PilEdge = CfEdge [Stmt]
-- type PilCallNode = CallNode [Stmt]
-- type PilBbNode = BasicBlockNode [Stmt]
-- type PilNodeMapEntry = (PilNode, [Stmt])
-- type PilCfg = Cfg [Stmt]

-- -- TODO: Consider adding more depending on what is being represented.
-- data BranchType
--   = TrueBranch
--   | FalseBranch
--   | UnconditionalBranch
--   deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- instance Hashable BranchType

-- data BasicBlockNode a = BasicBlockNode
--   { function :: Function
--   , start :: Address
--   , end :: Address
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data CallNode a = CallNode
--   { function :: Function
--   , start :: Address
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data EnterFuncNode a = EnterFuncNode
--   { prevCtx :: Ctx
--   , nextCtx :: Ctx
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data LeaveFuncNode a = LeaveFuncNode
--   { prevCtx :: Ctx
--   , nextCtx :: Ctx
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- {- |Terminal nodes are nodes in CFG that have no successor.
-- In a function, these nodes correspond to either: a return statement that
-- resumes control flow to the caller; an exit statement (see NORET in MLIL SSA)
-- that terminates normal control flow of the program (i.e., there may be signal handlers);
-- and a tail call where control flow moves to the call target. 
-- -}
-- data TerminalNode a 
--   = TermRet (ReturnNode a)
--   | TermExit (ExitNode a)
--   | TermTailCall (TailCallNode a)
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data ReturnNode a = ReturnNode
--   { basicBlock :: BasicBlockNode a
--   , retOp :: RetOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- newtype ExitNode a = ExitNode
--   { basicBlock :: BasicBlockNode a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor)
--   deriving anyclass (Hashable, FromJSON, ToJSON)

-- data TailCallNode a = TailCallNode
--   { basicBlock :: BasicBlockNode a
--   , tailCallOp :: TailCallOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data BranchNode a = BranchNode
--   { basicBlock :: BasicBlockNode a
--   , branchCondOp :: BranchCondOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- -- TODO: Consider moving the interprocedural nodes into a separate type
-- --       specific to InterCfg. Especially now with EnterFunc/LeaveFunc
-- -- TODO: Once the Cfg only needs to support PIL statements, remove type parameter
-- --       a and use more-specific definitions of the various node types. E.g.,
-- --       a CallNode should have a single CallStatement entry, not a nodeData of type a.
-- -- TODO: Consider the different needs of CFG representations. E.g., a CFG corresponding
-- --       to a function vs. a CFG corresponding to an arbitrary function CFG subgraph
-- data CfNode a
--   = BasicBlock (BasicBlockNode a)
--   | Call (CallNode a)
--   | EnterFunc (EnterFuncNode a)
--   | LeaveFunc (LeaveFuncNode a)
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data CfEdge a = CfEdge
--   { src :: CfNode a
--   , dst :: CfNode a
--   , branchType :: BranchType
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- mkEdge :: (BranchType, (CfNode a, CfNode a)) -> CfEdge a
-- mkEdge (bt, (s, d)) =
--   CfEdge s d bt

-- data CodeReference a = CodeReference
--   { function :: Function
--   , startIndex :: a
--   , endIndex :: a
--   }
--   deriving (Eq, Ord, Functor, Show, Generic)

-- type NodeRefMap a b = HashMap a b

-- type NodeRefMapEntry a b = (a, b)

-- newtype Dominators a = Dominators (HashMap (CfNode a) (HashSet (CfNode a)))
--   deriving (Eq, Ord, Show, Generic)

-- newtype PostDominators a = PostDominators (HashMap (CfNode a) (HashSet (CfNode a)))
--   deriving (Eq, Ord, Show, Generic)

-- {- | A non-empty graph that consists of a strongly-connected component
--  with a single root node (a node with no incoming edges).
--  This is intended to be the graph representation of a CFG.
--  A user of this API probably wants to work with the 'Cfg' type that
--  includes additional information about the CFG.
-- -}
-- type ControlFlowGraph a = AlgaGraph BranchType a (CfNode a)

-- -- TODO: How to best "prove" this generates a proper ControlFlowGraph?
-- mkControlFlowGraph :: Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> ControlFlowGraph a
-- mkControlFlowGraph root' ns es =
--   Graph.addNodes (root' : ns) . Graph.fromEdges $
--     (view #branchType &&& (view #src &&& view #dst)) <$> es

-- -- TODO: Consider removing type parameter once a PIL CFG can be constructed
-- --       w/o an intermediate MLIL SSA CFG.
-- data Cfg a = Cfg
--   { graph :: ControlFlowGraph a
--   , root :: CfNode a
--   }
--   deriving (Eq, Show, Generic)

-- mkCfg :: forall a. Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
-- mkCfg root' rest es =
--   Cfg
--     { graph = mkControlFlowGraph root' rest es
--     , root = root'
--     }

-- -- TODO: Is there a deriving trick to have the compiler generate this?
-- -- TODO: Separate graph construction from graph use and/or graph algorithms
-- instance Ord a => Graph BranchType a (CfNode a) (Cfg a) where
--   empty = error "The empty function is unsupported for CFGs."
--   fromNode _ = error "Use mkCfg to construct a CFG."
--   fromEdges _ = error "Use mkCfg to construct a CFG."
--   succs node = Graph.succs node . view #graph
--   preds node = Graph.preds node . view #graph
--   nodes = Graph.nodes . view #graph
--   edges = Graph.edges . view #graph
--   getEdgeLabel edge = Graph.getEdgeLabel edge . view #graph
--   setEdgeLabel label edge cfg = cfg & #graph %~ Graph.setEdgeLabel label edge
--   getNodeAttr node = Graph.getNodeAttr node . view #graph
--   setNodeAttr attr node cfg = cfg & #graph %~ Graph.setNodeAttr attr node
  
--   removeEdge edge = over #graph $ Graph.removeEdge edge
--   removeNode node = over #graph $ Graph.removeNode node
--   addNodes nodes = over #graph $ Graph.addNodes nodes
--   addEdge lblEdge = over #graph $ Graph.addEdge lblEdge
--   hasNode node = Graph.hasNode node . view #graph
--   transpose = over #graph Graph.transpose
--   bfs startNodes = Graph.bfs startNodes . view #graph

--   -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
--   subgraph pred = over #graph $ Graph.subgraph pred

-- data FuncContext = FuncContext
--   { func :: Function
--   , uuid :: UUID
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- data LoopContext = LoopContext
--   { funcCtx :: FuncContext
--   , uuid :: UUID
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- -- TODO: Merge with Ctx in Blaze.Types.Common
-- --       This version provides more information but use
-- --       existing Ctx to avoid too many changes at once.
-- data Context 
--   = FuncCtx FuncContext
--   | LoopCtx LoopContext 
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- -- |Track an incrementing counter for use in generating variable names.
-- newtype Counter a = Counter (State Int a)
--   deriving (Functor)
--   deriving newtype (Applicative, Monad, MonadState Int)

-- runCounter :: Counter a -> Int -> (a, Int)
-- runCounter (Counter s) = runState s

-- nextVal :: Counter Int
-- nextVal = do
--   x <- get
--   put (x + 1)
--   return x 













-- module Blaze.Types.Cfg where

-- import qualified Blaze.Graph as Graph
-- import Blaze.Types.Graph (Graph)
-- import qualified Blaze.Types.Graph as G
-- import Blaze.Prelude hiding (pred)
-- import Blaze.Types.Function (Function)
-- import Blaze.Types.Pil (Stmt, RetOp, Expression, TailCallOp, BranchCondOp)
-- import Blaze.Types.Pil.Common (Ctx)
-- import Blaze.Types.Graph.Alga (AlgaGraph)

-- type PilNode = CfNode [Stmt]
-- type PilEdge = CfEdge [Stmt]
-- type PilCallNode = CallNode [Stmt]
-- type PilBbNode = BasicBlockNode [Stmt]
-- type PilNodeMapEntry = (PilNode, [Stmt])
-- type PilCfg = Cfg [Stmt]

-- -- TODO: Consider adding more depending on what is being represented.
-- data BranchType
--   = TrueBranch
--   | FalseBranch
--   | UnconditionalBranch
--   deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- instance Hashable BranchType

-- data BasicBlockNode a = BasicBlockNode
--   { function :: Function
--   , start :: Address
--   , end :: Address
--   , uuid :: Maybe UUID
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data CallNode a = CallNode
--   { function :: Function
--   , start :: Address
--   , uuid :: Maybe UUID
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data EnterFuncNode a = EnterFuncNode
--   { prevCtx :: Ctx
--   , nextCtx :: Ctx
--   , uuid :: Maybe UUID
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data LeaveFuncNode a = LeaveFuncNode
--   { prevCtx :: Ctx
--   , nextCtx :: Ctx
--   , uuid :: Maybe UUID
--   , nodeData :: a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- {- |Terminal nodes are nodes in CFG that have no successor.
-- In a function, these nodes correspond to either: a return statement that
-- resumes control flow to the caller; an exit statement (see NORET in MLIL SSA)
-- that terminates normal control flow of the program (i.e., there may be signal handlers);
-- and a tail call where control flow moves to the call target. 
-- -}
-- data TerminalNode a 
--   = TermRet (ReturnNode a)
--   | TermExit (ExitNode a)
--   | TermTailCall (TailCallNode a)
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data ReturnNode a = ReturnNode
--   { basicBlock :: BasicBlockNode a
--   , retOp :: RetOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- newtype ExitNode a = ExitNode
--   { basicBlock :: BasicBlockNode a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
--   deriving anyclass (Hashable, FromJSON, ToJSON)

-- data TailCallNode a = TailCallNode
--   { basicBlock :: BasicBlockNode a
--   , tailCallOp :: TailCallOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- data BranchNode a = BranchNode
--   { basicBlock :: BasicBlockNode a
--   , branchCondOp :: BranchCondOp Expression
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)
--   deriving anyclass (Hashable)

-- -- TODO: Consider moving the interprocedural nodes into a separate type
-- --       specific to InterCfg. Especially now with EnterFunc/LeaveFunc
-- -- TODO: Once the Cfg only needs to support PIL statements, remove type parameter
-- --       a and use more-specific definitions of the various node types. E.g.,
-- --       a CallNode should have a single CallStatement entry, not a nodeData of type a.
-- -- TODO: Consider the different needs of CFG representations. E.g., a CFG corresponding
-- --       to a function vs. a CFG corresponding to an arbitrary function CFG subgraph
-- data CfNode a
--   = BasicBlock (BasicBlockNode a)
--   | Call (CallNode a)
--   | EnterFunc (EnterFuncNode a)
--   | LeaveFunc (LeaveFuncNode a)
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
--   deriving anyclass (Hashable)

-- data CfEdge a = CfEdge
--   { branchType :: BranchType
--   , src :: CfNode a
--   , dst :: CfNode a
--   }
--   deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
--   deriving anyclass (Hashable)

-- fromTupleEdge :: (BranchType, (CfNode a, CfNode a)) -> CfEdge a
-- fromTupleEdge (bt, (a, b)) = CfEdge bt a b

-- fromLEdge :: G.LEdge BranchType (CfNode a) -> CfEdge a
-- fromLEdge (G.LEdge bt (G.Edge s d)) = mkEdge' bt s d


-- data CodeReference a = CodeReference
--   { function :: Function
--   , startIndex :: a
--   , endIndex :: a
--   }
--   deriving (Eq, Ord, Functor, Show, Generic)

-- type NodeRefMap a b = HashMap a b

-- type NodeRefMapEntry a b = (a, b)

-- newtype Dominators a = Dominators (HashMap (CfNode a) (HashSet (CfNode a)))
--   deriving (Eq, Ord, Show, Generic)

-- newtype PostDominators a = PostDominators (HashMap (CfNode a) (HashSet (CfNode a)))
--   deriving (Eq, Ord, Show, Generic)

-- {- | A non-empty graph that consists of a strongly-connected component
--  with a single root node (a node with no incoming edges).
--  This is intended to be the graph representation of a CFG.
--  A user of this API probably wants to work with the 'Cfg' type that
--  includes additional information about the CFG.
-- -}
-- type ControlFlowGraph a = AlgaGraph BranchType a (CfNode ())

-- mkControlFlowGraph' :: forall a. Ord a
--   => CfNode a
--   -> [CfNode a]
--   -> [CfEdge a]
--   -> ControlFlowGraph a
-- mkControlFlowGraph' root' ns es = do
--   Graph.addNodes (root' : ns) . G.fromEdges $ es

-- -- TODO: How to best "prove" this generates a proper ControlFlowGraph?
-- -- | Automatically makes nodes and edges unique. Returns (root node, graph)
-- mkControlFlowGraph :: forall a. (Hashable a, Ord a) => CfNode a -> [CfNode a] -> [CfEdge a] -> IO (CfNode a, ControlFlowGraph a)
-- mkControlFlowGraph root' ns es = U.build $ do
--   rootNode <- U.add root'
--   nodes <- traverse U.add ns
--   edges <- traverse (traverse U.add) es 
--   return (rootNode, mkControlFlowGraph' rootNode nodes edges)


-- -- TODO: Consider removing type parameter once a PIL CFG can be constructed
-- --       w/o an intermediate MLIL SSA CFG.
-- data Cfg a = Cfg
--   { graph :: ControlFlowGraph a
--   , root :: Unique (CfNode a)
--   }
--   deriving (Eq, Show, Generic)

-- buildEdge :: (MonadIO m, Eq a, Hashable a)
--           => CfEdge a
--           -> U.Builder (CfNode a) m (CfEdgeUnique a)
-- buildEdge = traverse U.add

-- buildNode :: (MonadIO m, Eq a, Hashable a)
--           => CfNode a
--           -> U.Builder (CfNode a) m (Unique (CfNode a))
-- buildNode = U.add


-- buildCfg :: forall m a. (Hashable a, Ord a, MonadIO m)
--          => CfNode a
--          -> [CfNode a]
--          -> [G.LEdge BranchType (CfNode a)]
--          -> U.Builder (CfNode a) m (Cfg a)
-- buildCfg root' rest es = do
--   rootNode <- U.add root'
--   nodes <- traverse U.add rest
--   edges <- traverse (traverse U.add) es 
--   return $ mkCfg' rootNode nodes edges

-- mkCfg :: forall a. (Hashable a, Ord a) => CfNode a -> [CfNode a] -> [G.LEdge BranchType (CfNode a)] -> IO (Cfg a)
-- mkCfg root' rest es = U.build $ buildCfg root' rest es

-- mkCfg' :: forall a. Ord a
--        => Unique (CfNode a)
--        -> [Unique (CfNode a)]
--        -> [G.LEdge BranchType (Unique (CfNode a))] -> Cfg a
-- mkCfg' rootNode nodes edges = Cfg
--   { graph = mkControlFlowGraph' rootNode nodes edges
--   , root = rootNode
--   }


-- -- TODO: Is there a deriving trick to have the compiler generate this?
-- -- TODO: Separate graph construction from graph use and/or graph algorithms
-- instance Ord n => Graph BranchType () (Unique (CfNode n)) (Cfg n) where
--   empty = error "The empty function is unsupported for CFGs."
--   fromNode _ = error "Use mkCfg to construct a CFG."
--   fromEdges _ = error "Use mkCfg to construct a CFG."
--   succs node = Graph.succs node . view #graph
--   preds node = Graph.preds node . view #graph
--   nodes = Graph.nodes . view #graph
--   edges = Graph.edges . view #graph
--   getEdgeLabel edge = Graph.getEdgeLabel edge . view #graph
--   setEdgeLabel label edge cfg = cfg & #graph %~ Graph.setEdgeLabel label edge
--   getNodeAttr node = Graph.getNodeAttr node . view #graph
--   setNodeAttr attr node cfg = cfg & #graph %~ Graph.setNodeAttr attr node
  
--   removeEdge edge = over #graph $ Graph.removeEdge edge
--   removeNode node = over #graph $ Graph.removeNode node
--   addNodes nodes' = over #graph $ Graph.addNodes nodes'
--   addNodesWithAttrs nodes' = over #graph $ Graph.addNodesWithAttrs nodes'
--   addEdge lblEdge = over #graph $ Graph.addEdge lblEdge
--   hasNode node = Graph.hasNode node . view #graph
--   transpose = over #graph Graph.transpose
--   bfs startNodes = Graph.bfs startNodes . view #graph

--   -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
--   subgraph pred = over #graph $ Graph.subgraph pred

-- updateNode :: (CfNode n -> CfNode n) -> Unique (CfNode n) -> Cfg n -> Cfg n
-- updateNode f n g = g & #graph %~ U.updateNode f n

-- setNode :: CfNode n -> Unique (CfNode n) -> Cfg n -> Cfg n
-- setNode n' n g = g & #graph %~ U.setNode n' n


-- data FuncContext = FuncContext
--   { func :: Function
--   , uuid :: UUID
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- data LoopContext = LoopContext
--   { funcCtx :: FuncContext
--   , uuid :: UUID
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- -- TODO: Merge with Ctx in Blaze.Types.Common
-- --       This version provides more information but use
-- --       existing Ctx to avoid too many changes at once.
-- data Context 
--   = FuncCtx FuncContext
--   | LoopCtx LoopContext 
--   deriving (Eq, Ord, Show, Generic)
--   deriving anyclass (Hashable)

-- -- |Track an incrementing counter for use in generating variable names.
-- newtype Counter a = Counter (State Int a)
--   deriving (Functor)
--   deriving newtype (Applicative, Monad, MonadState Int)

-- runCounter :: Counter a -> Int -> (a, Int)
-- runCounter (Counter s) = runState s

-- nextVal :: Counter Int
-- nextVal = do
--   x <- get
--   put (x + 1)
--   return x 
