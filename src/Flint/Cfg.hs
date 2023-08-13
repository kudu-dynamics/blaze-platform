module Flint.Cfg ( module Flint.Cfg ) where

import Flint.Prelude hiding (sym)

import qualified Blaze.Cfg.Interprocedural as InterCfg
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Types.Cfg (CfNode, CallNode, PilCallNode, PilCfg, HasCtx(getCtx))
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Pil (CallDest)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst ( RecurSubst(recurSubst) )
import qualified Data.HashSet as HashSet

-- data BuilderState = BuilderState
--   {  
-- type Builder = State 

getCfgForCallDest :: CfgStore -> CallDest expr -> IO (Maybe PilCfg)
getCfgForCallDest store = \case
  -- Pil.CallAddr ptr -> do
  --   sym <- ptr ^. #symbol
  --   CfgStore.getFromFuncName_ sym store
  Pil.CallFunc func -> CfgStore.getFuncCfg store func
  _ -> return Nothing

getCfgForCallNode :: CfgStore -> PilCallNode -> IO (Maybe PilCfg)
getCfgForCallNode store = getCfgForCallDest store . view #callDest

-- | Expands all the call nodes in a Cfg, `depth` times.
-- A `depth` of 0 means it will not expand any calls.
expandCfgToDepth_
  :: IO UUID
  -> CfgStore
  -> Word64
  -> PilCfg
  -> IO PilCfg
expandCfgToDepth_ _ _ 0 cfg = return cfg
expandCfgToDepth_ genUuid store depth cfg = case callNodes of
  [] -> return cfg
  _ -> do
    expandedCfg <- foldM expandCall cfg callNodes
    expandCfgToDepth_ genUuid store (depth - 1) expandedCfg
  where
    callNodes :: [PilCallNode]
    callNodes = mapMaybe (^? #_Call) . HashSet.toList $ Cfg.nodes cfg

    expandCall :: PilCfg -> PilCallNode -> IO PilCfg
    expandCall cfg' cnode = getCfgForCallNode store cnode >>= \case
      Nothing -> return cfg
      Just innerCfg -> do
        let innerCfg' = recurSubst (\_ -> cfg' ^. #nextCtxIndex) innerCfg
        case InterCfg.getCallStmt cnode of
          Nothing -> return cfg'
          Just cstmt -> do
            uuid <- genUuid
            return . Cfg.incNextCtxIndex $ InterCfg.expandCall_
              cfg'
              cnode
              cstmt
              innerCfg'
              (getCtx innerCfg')
              uuid

-- | Expands all the call nodes in a Cfg, `depth` times.
-- A `depth` of 0 means it will not expand any calls.
expandCfgToDepth
  :: CfgStore
  -> Word64
  -> PilCfg
  -> IO PilCfg
expandCfgToDepth = expandCfgToDepth_ randomIO
