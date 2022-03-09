{- | GroupingCfg is a Cfg that can contain groups of nodes as a single block
This module provides functionality for grouping, ungrouping, and regrouping.
-}

module Blaze.Types.Cfg.Grouping2 where

import Blaze.Prelude

import Blaze.Types.Cfg (BranchType, BasicBlockNode(BasicBlockNode), CallNode(CallNode), EnterFuncNode(EnterFuncNode), LeaveFuncNode(LeaveFuncNode))
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph)

import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap


data GroupingNode a = GroupingNode
  { termNode :: CfNode a
  , grouping :: Cfg a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data CfNode a
  = Normal (Cfg.CfNode a)
  | Grouping (GroupingNode a)
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data CfEdge a = CfEdge
  { src :: CfNode a
  , dst :: CfNode a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

fromLEdge :: G.LEdge BranchType (CfNode a) -> CfEdge a
fromLEdge (G.LEdge bt (G.Edge s d)) = CfEdge s d bt

toLEdge :: CfEdge a -> G.LEdge BranchType (CfNode a)
toLEdge (CfEdge a b bt) = G.LEdge bt (G.Edge a b)

type ControlFlowGraph a = AlgaGraph BranchType (CfNode a) (CfNode ())

asIdNode :: CfNode a -> CfNode ()
asIdNode = void

asIdEdge :: CfEdge a -> CfEdge ()
asIdEdge = void

data Cfg a = Cfg
  { graph :: ControlFlowGraph a
  , root :: CfNode a
  }
  deriving (Eq, Ord, Show, Generic)

instance Functor Cfg where
  fmap f cfg = Cfg
    { graph = G.mapAttrs (fmap f) $ cfg ^. #graph
    , root = f <$> (cfg ^. #root)
    }

instance Foldable Cfg where
  foldMap f = G.foldMapAttrs g . view #graph
    where
      g = \case
        Grouping x -> foldMap f $ x ^. #grouping
        Normal x -> f $ Cfg.getNodeData x

instance Traversable Cfg where
  traverse f cfg = Cfg
    <$> (G.traverseAttrs (traverse f) $ cfg ^. #graph)
    <*> (traverse f $ cfg ^. #root)

instance Hashable a => Hashable (Cfg a) where
  hashWithSalt n = hashWithSalt n . toTransport
  hash = hash . toTransport

instance ToJSON a => ToJSON (Cfg a) where
 toJSON = toJSON . toTransport

instance (Eq a, Hashable a, FromJSON a) => FromJSON (Cfg a) where
 parseJSON = fmap fromTransport . parseJSON

mkCfg :: forall a. (Hashable a, Eq a) => CfNode a -> [CfNode a] -> [CfEdge a] -> Cfg a
mkCfg root' rest es =
  Cfg
    { graph = mkControlFlowGraph root' rest es
    , root = root'
    }

-- TODO: How to best "prove" this generates a proper ControlFlowGraph?
mkControlFlowGraph :: forall a .(Hashable a, Eq a)
                   => CfNode a
                   -> [CfNode a]
                   -> [CfEdge a]
                   -> ControlFlowGraph a
mkControlFlowGraph root' ns es = G.addNodesWithAttrs attrList
  . G.fromEdges
  $ fmap asIdNode . toLEdge <$> es
  where
    nodesInEdges = concatMap (\e -> [e ^. #src, e ^. #dst]) es
    allNodes :: [CfNode a]
    allNodes = HashSet.toList . HashSet.fromList $ (root' : ns) <> nodesInEdges

    attrList :: [(CfNode (), CfNode a)]
    attrList = (\n -> (asIdNode n, n)) <$> allNodes

-- | CfgTransport is an intermediary type for Hashable, FromJSON, and ToJSON instances
data CfgTransport a = CfgTransport
  { transportEdges :: [CfEdge ()]
  , transportRoot :: CfNode ()
  , transportNodes :: [(CfNode (), CfNode a)]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

toTransport :: forall a. Cfg a -> CfgTransport a
toTransport pcfg = CfgTransport
  { transportEdges = edges'
  , transportRoot = root'
  , transportNodes = nodes'
  }
  where
    root' = void $ pcfg ^. #root

    nodes' :: [(CfNode (), CfNode a)]
    nodes' = HashMap.toList $ pcfg ^. #graph . #nodeAttrMap

    edges' :: [CfEdge ()]
    edges' = fmap fromLEdge . G.edges $ pcfg ^. #graph

fromTransport :: (Eq a, Hashable a) => CfgTransport a -> Cfg a
fromTransport t = mkCfg root' nodes' edges'
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


-- | Record of the group layouts, including inner groups of groups.
type GroupingTree = [GroupSpec]

-- | Root dominator and term post-dominator nodes for group, including inner groups.
-- root and term are stored as non-group Cfg CfNode so that we don't point to groups
data GroupSpec = GroupSpec
  { groupRoot :: Cfg.CfNode ()
  , groupTerm :: Cfg.CfNode ()
  , innerGroups :: GroupingTree
  }

