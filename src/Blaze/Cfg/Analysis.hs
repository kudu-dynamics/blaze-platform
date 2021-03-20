{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Analysis where

import qualified Blaze.Graph as G
import Blaze.Pil.Analysis (ConstPropState, CopyPropState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Prelude
import Blaze.Types.Cfg (PilCfg)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg), unInterCfg)
import Blaze.Types.Pil (Stmt)
import qualified Data.Set as Set

copyProp :: InterCfg -> InterCfg
copyProp icfg =
  InterCfg $
    foldr
    (Cfg.updateNode (PA._copyProp copyPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (concatMap concat) . Set.toList . G.nodes $ cfg
  copyPropState :: CopyPropState
  copyPropState = PA.buildCopyPropState allStmts

constantProp :: InterCfg -> InterCfg
constantProp icfg =
  InterCfg $
    foldr
    (Cfg.updateNode (PA._constantProp constPropState <$>))
    cfg
    (Set.toList $ G.nodes cfg)
 where
  cfg :: PilCfg
  cfg = unInterCfg icfg
  allStmts :: [Stmt]
  allStmts = concatMap (concatMap concat) . Set.toList . G.nodes $ cfg
  constPropState :: ConstPropState
  constPropState = PA.buildConstPropState allStmts
