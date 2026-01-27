{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Blaze.Types.HighCfg where

import Blaze.Prelude hiding (pred, succ)
import Blaze.Types.Graph qualified as G
import Blaze.Types.Cfg (BranchType, CfEdge (CfEdge), Cfg)
import Blaze.Types.HighCfNode
import Data.HashSet qualified as HS

data HighCfg a = HighCfg
  { -- Externalized edges nodes in different CFGs
    highEdges :: HashSet (HighEdge a)
  , focus :: Cfg (HighCfNode a)
  }
  deriving (Show, Eq, Ord)

data HighEdge a = HighEdge
  { src :: UUID
  , dst :: UUID
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

toHighEdge ::
  CfEdge (HighCfNode a) ->
  Cfg (HighCfNode a) ->
  Cfg (HighCfNode a) ->
  HighEdge a
toHighEdge (CfEdge srcNode dstNode branchType) _srcCfg _dstCfg =
  HighEdge
    { src = getNodeUUID srcNode
    , dst = getNodeUUID dstNode
    , branchType = branchType
    }

nodes :: Hashable a => HighCfg a -> [(Cfg (HighCfNode a), HighCfNode a)]
nodes (HighCfg _ focus) = getSubCfgsDeep focus

{- | FOCUS OPERATIONS
Focus operations allow moving up and down the CFG hierarchy for mutations
Moving up and down means changing the focus and updating the high edges depending on the direction
-}

-- Moving the focus up to a parent High CFG
-- Since we're moving up a level there maybe have been mutations in the focus
-- update the node too 
refocusUp :: Cfg (HighCfNode a) -> HighCfg a -> HighCfg a
refocusUp parentFocus highCfg = highCfg{focus = parentFocus}

-- | Moving the focus down to a sub-CFG. Inherit the parent's high edges.
refocusDown :: Cfg (HighCfNode a) -> HighCfg a -> HighCfg a
refocusDown newFocus highCfg = highCfg{focus = newFocus}

-- Refocus down to get a pure result without modifying the HighCfg and you don't need to keep the new HighCfg
withRefocusDown :: Cfg (HighCfNode a) -> HighCfg a -> (HighCfg a -> b) -> b
withRefocusDown newFocus highCfg f = f $ refocusDown newFocus highCfg

-- Lookup nodes by UUID in only the focused CFG
lookupInFocus :: (Hashable a) => HashSet UUID -> HighCfg a -> HashSet (HighCfNode a)
lookupInFocus uuids highCfg =
  HS.filter (\n -> getNodeUUID n `HS.member` uuids) $ G.nodes highCfg.focus

mkHighCfg :: Cfg (HighCfNode a) -> HighCfg a
mkHighCfg focus = HighCfg {highEdges = HS.empty, focus = focus}

