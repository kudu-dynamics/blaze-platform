module Blaze.Cfg.EdgeGraph where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)

import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Cfg (Cfg, CfNode, BranchType, CfEdge)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Graph as G
import Blaze.Types.Graph (EdgeGraphNode, DominatorMapping)
import Data.SBV.Dynamic (SVal, svNot)
import Blaze.Types.Graph.Alga (AlgaGraph)

data UndecidedBranchingEdges = UndecidedBranchingEdges
  { falseEdge :: CfEdge ()
  , trueEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

data BranchingType = OnlyTrue (CfEdge ())
                   | OnlyFalse (CfEdge ())
                   | Undecided UndecidedBranchingEdges
                   deriving (Eq, Ord, Show, Generic)

data BranchCond a = BranchCond
  { conditionStatementIndex :: Int
  , condition :: a
  , branchingType :: BranchingType
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


type CfgEdgeGraph = AlgaGraph () () (EdgeGraphNode BranchType (CfNode ()))

cfgToEdgeGraph :: Cfg a -> CfgEdgeGraph
cfgToEdgeGraph = G.toEdgeGraph . view #graph

filterEdges
  :: DominatorMapping m
  => m (EdgeGraphNode BranchType (CfNode ()))
  -> m (CfEdge ())
filterEdges = G.domMapMaybe f
  where
    f :: EdgeGraphNode BranchType (CfNode ()) -> Maybe (CfEdge ())
    f (G.NodeNode _) = Nothing
    f (G.EdgeNode e) = Just $ Cfg.fromLEdge e

mkEdgeConstraintMap
  :: [BranchCond SVal]
  -> HashMap (CfEdge ()) SVal
mkEdgeConstraintMap = HashMap.fromList . concatMap f
  where
    f bc = case bc ^. #branchingType of
      OnlyTrue e -> [(e, bc ^. #condition)]
      OnlyFalse e -> [(e, svNot $ bc ^. #condition)]
      Undecided UndecidedBranchingEdges {falseEdge = fe, trueEdge = te} ->
        [ (fe, svNot $ bc ^. #condition)
        , (te, bc ^. #condition)
        ]
