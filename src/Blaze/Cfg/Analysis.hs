{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude
import Blaze.Types.Cfg (PilCfg)
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt)
import qualified Data.Set as Set

copyProp :: InterCfg -> InterCfg
copyProp icfg =
  InterCfg $
    foldr
    (G.updateNodeAttr (PA._copyProp copyPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (maybe [] concat . (`G.getNodeAttr` cfg))
             $ Set.toList (G.nodes cfg)
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: InterCfg -> InterCfg
constantProp icfg =
  InterCfg $
    foldr
    (G.updateNodeAttr (PA._constantProp constPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (maybe [] concat . (`G.getNodeAttr` cfg))
             $ Set.toList (G.nodes cfg)
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts
