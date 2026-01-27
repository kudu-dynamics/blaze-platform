-- | Similar to CfNode but contains TODO

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Blaze.Types.HighCfNode where

import Blaze.Prelude hiding (HasField, pred, succ)
import Blaze.Types.Graph (Identifiable (getNodeId), NodeId (NodeId))
import Blaze.Types.Graph qualified as G
import Data.HashSet qualified as HashSet
import Blaze.Types.Cfg (Cfg, CfNode, CfEdge (..))
import qualified Blaze.Cfg as Cfg
import GHC.Records (HasField(..))

-- TODO: Once the Cfg only needs to support PIL statements, remove type parameter
--       a and use more-specific definitions of the various node types. E.g.,
--       a CallNode should have a single CallStatement entry, not a nodeData of type a.
-- TODO: Consider the different needs of CFG representations. E.g., a CFG corresponding
--       to a function vs. a CFG corresponding to an arbitrary function CFG subgraph
data HighCfNode a
  = BasicBlock (Cfg.BasicBlockNode a)
  | Call (Cfg.CallNode a)
  | EnterFunc (Cfg.EnterFuncNode a)
  | LeaveFunc (Cfg.LeaveFuncNode a)
  | Loop (LoopNode a)
  | WhileDo (LoopNode a) (CfNode a) -- Clause node
  | Sequence (SequenceNode a)
  | ForLoop (ForLoopNode a)
  | Grouping (Cfg.GroupingNode a) -- Will eventually be deprecated
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

instance Identifiable (HighCfNode a) UUID where
  getNodeId = NodeId . getNodeUUID

instance HasField "uuid" (HighCfNode a) UUID where
  getField = getNodeUUID


-- | A natural loop similar to grouping represents a sub-CFG, specially a natural loop, within a larger CFG
-- We only assume reducible loops where there is 1 entry (the header) and
-- There may be nested loops in the body
-- There may be exits in the body
data LoopNode a = LoopNode
  { termNodeId :: NodeId UUID
  , uuid :: UUID
  , nodeData :: a
  , header :: CfNode a
  , body :: Cfg (HighCfNode a)
  , tail :: CfNode a
  , backEdge :: CfEdge (HighCfNode a)
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data ForLoopNode a = ForLoopNode
  { termNodeId :: NodeId UUID
  , uuid :: UUID
  , nodeData :: a
  , header:: HighCfNode a
  , initNode :: HighCfNode a
  , initStmt :: a
  , condNode :: HighCfNode a
  , condStmt:: a
  , incNode :: HighCfNode a
  , incStmt :: a
  , body :: Cfg (HighCfNode a)
  , exitNodes:: [HighCfNode a]
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

data SequenceNode a = SequenceNode
  { termNodeId :: NodeId UUID
  , uuid :: UUID
  , body :: Cfg (HighCfNode a)
  , nodeData :: a
  , last :: HighCfNode a
  }
  deriving (Eq, Ord, Show, Generic, Functor, FromJSON, ToJSON, Foldable, Traversable)
  deriving anyclass (Hashable)

getNodeData :: HighCfNode a -> a
getNodeData = \case
  BasicBlock x -> x ^. #nodeData
  Call x -> x ^. #nodeData
  EnterFunc x -> x ^. #nodeData
  LeaveFunc x -> x ^. #nodeData
  Loop x -> x ^. #nodeData
  Sequence x -> x ^. #nodeData
  WhileDo _ x -> Cfg.getNodeData x
  Grouping x -> x ^. #nodeData
  -- TODO
  ForLoop x -> x ^. #nodeData

getNodeUUID :: HighCfNode a -> UUID
getNodeUUID = \case
  BasicBlock x -> x ^. #uuid
  Call x -> x ^. #uuid
  EnterFunc x -> x ^. #uuid
  LeaveFunc x -> x ^. #uuid
  Loop x -> x ^. #uuid
  ForLoop x -> x ^. #uuid
  Sequence x -> x ^. #uuid
  WhileDo x _ -> x ^. #uuid
  Grouping x -> x ^. #uuid

-- | Sets a node's UUID.
-- WARNING: If you change the UUID of a node in a Cfg, you must also update
-- all the places in the underlying AlgaGraph that use the old UUID.
-- This can be used safely with `safeMap` and `safeTraverse`.
setNodeUUID :: UUID -> HighCfNode a -> HighCfNode a
setNodeUUID newUuid = \case
  BasicBlock x -> BasicBlock $ x & #uuid .~ newUuid
  Call x -> Call $ x & #uuid .~ newUuid
  EnterFunc x -> EnterFunc $ x & #uuid .~ newUuid
  LeaveFunc x -> LeaveFunc $ x & #uuid .~ newUuid
  Loop x -> Loop $ x & #uuid .~ newUuid
  Sequence x -> Sequence $ x & #uuid .~ newUuid
  WhileDo x y -> WhileDo (x & #uuid .~ newUuid) y
  ForLoop x -> ForLoop (x & #uuid .~ newUuid)
  Grouping x -> Grouping (x & #uuid .~ newUuid)

setNodeData :: a -> HighCfNode a -> HighCfNode a
setNodeData d = \case
  BasicBlock n -> BasicBlock $ n & #nodeData .~ d
  Call n -> Call $ n & #nodeData .~ d
  EnterFunc n -> EnterFunc $ n & #nodeData .~ d
  LeaveFunc n -> LeaveFunc $ n & #nodeData .~ d
  -- TODO: Need to support updating the grouped CFG inside as well?
  Loop n -> Loop $ n & #nodeData .~ d
  Sequence n -> Sequence $ n & #nodeData .~ d
  WhileDo n y -> WhileDo (n & #nodeData .~ d) y
  ForLoop x -> ForLoop (x & #nodeData .~ d)
  Grouping x -> Grouping (x & #nodeData .~ d)

updateNodeData ::
  (a -> a) ->
  HighCfNode a ->
  HighCfNode a
updateNodeData f n = setNodeData (f $ getNodeData n) n

getSubCfg :: HighCfNode a -> Maybe (Cfg (HighCfNode a))
getSubCfg = \case
  Loop n -> Just $ n ^. #body
  ForLoop n -> Just $ n ^. #body
  Sequence n -> Just $ n ^. #body
  WhileDo n _ -> Just $ n ^. #body
  _ -> Nothing

toHighCfNode :: CfNode a -> HighCfNode a
toHighCfNode (Cfg.BasicBlock n) = BasicBlock n
toHighCfNode (Cfg.Call n) = Call n
toHighCfNode (Cfg.EnterFunc n) = EnterFunc n
toHighCfNode (Cfg.LeaveFunc n) = LeaveFunc n
toHighCfNode (Cfg.Grouping n) = Grouping n


-- A list of sub CFGs and their parents
getSubCfgs :: (Hashable a) => Cfg (HighCfNode a) -> [(Cfg (HighCfNode a), HighCfNode a)]
getSubCfgs cfg' = mapMaybe (\n -> (,n) <$> getSubCfg n) (HashSet.toList (G.nodes cfg'))

-- Recursively get all sub-CFGs
getSubCfgsDeep :: (Hashable a) => Cfg (HighCfNode a) -> [(Cfg (HighCfNode a), HighCfNode a)]
getSubCfgsDeep cfg' = concatMap go (getSubCfgs cfg')
  where
    go (subCfg, parent) =
      (subCfg, parent) : concatMap go (getSubCfgs subCfg)

