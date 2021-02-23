module Blaze.Types.Cfg where

import Blaze.Graph (Graph)
import qualified Blaze.Graph as Graph
import Blaze.Prelude hiding (pred)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Control.Arrow ((&&&))
import Blaze.Types.Pil (Stmt, RetOp, Expression, TailCallOp)
import Blaze.Types.Pil.Common (Ctx)

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
  deriving (Eq, Ord, Show, Generic)

instance Hashable BranchType

data BasicBlockNode a = BasicBlockNode
  { function :: Function
  , start :: Address
  , end :: Address
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data CallNode a = CallNode
  { function :: Function
  , start :: Address
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data EnterFuncNode a = EnterFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data LeaveFuncNode a = LeaveFuncNode
  { prevCtx :: Ctx
  , nextCtx :: Ctx
  , nodeData :: a
  }
  deriving (Eq, Ord, Show, Generic)
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
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data ReturnNode a = ReturnNode
  { basicBlock :: BasicBlockNode a
  , retOp :: RetOp Expression
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype ExitNode a = ExitNode
  { basicBlock :: BasicBlockNode a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data TailCallNode a = TailCallNode
  { basicBlock :: BasicBlockNode a
  , tailCallOp :: TailCallOp Expression
  }
  deriving (Eq, Ord, Show, Generic)
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
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data CfEdge a = CfEdge
  { src :: CfNode a
  , dst :: CfNode a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

mkEdge :: (BranchType, (CfNode a, CfNode a)) -> CfEdge a
mkEdge (bt, (s, d)) =
  CfEdge s d bt

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
type ControlFlowGraph a = AlgaGraph BranchType (CfNode a)

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: Ord a => CfNode a -> [CfNode a] -> [CfEdge a] -> ControlFlowGraph a
mkControlFlowGraph root' ns es =
  Graph.addNodes (root' : ns) . Graph.fromEdges $
    (view #branchType &&& (view #src &&& view #dst)) <$> es

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
instance Ord a => Graph BranchType (CfNode a) (Cfg a) where
  empty = error "The empty function is unsupported for CFGs."
  fromNode _ = error "Use mkCfg to construct a CFG."
  fromEdges _ = error "Use mkCfg to construct a CFG."
  succs node = Graph.succs node . view #graph
  preds node = Graph.preds node . view #graph
  nodes = Graph.nodes . view #graph
  edges = Graph.edges . view #graph
  getEdgeLabel edge = Graph.getEdgeLabel edge . view #graph
  setEdgeLabel label edge cfg = cfg & #graph %~ Graph.setEdgeLabel label edge
  removeEdge edge = over #graph $ Graph.removeEdge edge
  removeNode node = over #graph $ Graph.removeNode node
  addNodes nodes = over #graph $ Graph.addNodes nodes
  addEdge lblEdge = over #graph $ Graph.addEdge lblEdge
  hasNode node = Graph.hasNode node . view #graph
  transpose = over #graph Graph.transpose
  bfs startNodes = Graph.bfs startNodes . view #graph

  -- TODO: Standard subgraph doesn't make sense for a rooted graph. How to remedy?
  subgraph pred = over #graph $ Graph.subgraph pred

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