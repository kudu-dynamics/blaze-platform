{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude
import Blaze.Types.Cfg (CfNode (BasicBlock, Call, EnterFunc, LeaveFunc), PilCfg, PilNode)
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt)
import qualified Data.Set as Set

-- concat $ G.bfs [(unInterCfg icfg) ^. #root] ((unInterCfg icfg) ^. #graph)

getStmts :: PilNode -> [Stmt]
getStmts = \case
  BasicBlock n -> n ^. #nodeData
  Call n -> n ^. #nodeData
  EnterFunc n -> n ^. #nodeData
  LeaveFunc n -> n ^. #nodeData

-- TODO: May turn this into some sort of fold that updates the
--       graph's node attributes as we go.
-- copyProp :: [[Stmt]] -> [[Stmt]]
-- copyProp nodesStmts =
--   PA._copyProp copyPropState <$> nodesStmts
--  where
--   allStmts :: [Stmt]
--   allStmts = concat nodesStmts
--   copyPropState :: CopyPropState
--   copyPropState = PA.buildCopyPropState allStmts
copyProp :: InterCfg -> InterCfg
copyProp icfg =
  -- PA._copyProp copyPropState <$> nodesStmts
  InterCfg $
    foldl'
      ( \cfg_ node ->
          case G.getNodeAttr node cfg_ of
            Just stmts ->
              G.setNodeAttr (PA._copyProp copyPropState stmts) node cfg_
            Nothing ->
              cfg_
      )
      cfg
      (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  nodesStmts :: [[Stmt]]
  nodesStmts = mapMaybe (`G.getNodeAttr` cfg) $ Set.toList (G.nodes cfg)
  allStmts :: [Stmt]
  allStmts = concat nodesStmts
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

-- constantProp :: [[Stmt]] -> [[Stmt]]
-- constantProp nodesStmts =
--   PA._constantProp constPropState <$> nodesStmts
--  where
--   allStmts :: [Stmt]
--   allStmts = concat nodesStmts
--   constPropState :: ConstPropState
--   constPropState = PA.buildConstPropState allStmts
constantProp :: InterCfg -> InterCfg
constantProp icfg =
  -- PA._constantProp constPropState <$> nodesStmts
  InterCfg $
    foldl'
      ( \cfg_ node ->
          case G.getNodeAttr node cfg_ of
            Just stmts ->
              G.setNodeAttr (PA._constantProp constPropState stmts) node cfg_
            Nothing ->
              cfg_
      )
      cfg
      (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  nodesStmts :: [[Stmt]]
  nodesStmts = mapMaybe (`G.getNodeAttr` cfg) $ Set.toList (G.nodes cfg)
  allStmts :: [Stmt]
  allStmts = concat nodesStmts
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts
