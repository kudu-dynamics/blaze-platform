module Blaze.Cfg.Analysis where

import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (PilNode, CfNode (BasicBlock, Call, EnterFunc, LeaveFunc))

-- concat $ G.bfs [(unInterCfg icfg) ^. #root] ((unInterCfg icfg) ^. #graph)

getStmts :: PilNode -> [Stmt]
getStmts = \case
  BasicBlock n -> n ^. #nodeData
  Call n -> n ^. #nodeData
  EnterFunc n -> n ^. #nodeData
  LeaveFunc n -> n ^. #nodeData

-- TODO: May turn this into some sort of fold that updates the
--       graph's node attributes as we go.
copyProp :: [[Stmt]] -> [[Stmt]]
copyProp nodesStmts =
  PA._copyProp copyPropState <$> nodesStmts
 where
  allStmts :: [Stmt]
  allStmts = concat nodesStmts
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: [[Stmt]] -> [[Stmt]]
constantProp nodesStmts =
  PA._constantProp constPropState <$> nodesStmts
 where
  allStmts :: [Stmt]
  allStmts = concat nodesStmts
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts