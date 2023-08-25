module Flint.Cfg.Store ( module Flint.Cfg.Store ) where

import Flint.Prelude

import Flint.Types.Cfg.Store
import qualified Flint.Types.CachedCalc as CC

import Blaze.Cfg.Path (makeCfgAcyclic)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (calcDescendantsMap)
import qualified Blaze.Types.Graph as G

import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType) 

import Blaze.Types.Cfg (PilCfg, PilNode)

import qualified Data.HashSet as HashSet


-- This is a cache of Cfgs for functions.
-- This version only supports functions from a single binary.

-- If you have a bunch of object files, you can collect them like so:
-- gcc -no-pie -Wl,--unresolved-symbols=ignore-all -o full_collection_binary *.o

init :: IO CfgStore
init = CfgStore <$> atomically CC.create

-- | Gets the stored Cfg for a function, if it exists in the store.
getFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFuncCfgInfo store func = join <$> CC.get func (store ^. #cache)

getFuncCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFuncCfg store func = fmap (view #cfg) <$> getFuncCfgInfo store func

-- | Adds a func/cfg to the store.
-- Overwrites existing function Cfg.
-- Any Cfgs in the store should have a CtxId of 0
addFunc
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a -> CfgStore -> Function -> IO ()
addFunc imp store func = CC.setCalc func (store ^. #cache) $ do
  (fmap (view #result) <$> ImpCfg.getCfg imp func 0) >>= \case
    Nothing -> return Nothing
    Just cfg -> do
      let cfgWithoutBackedges = makeCfgAcyclic cfg
          dmap = calcDescendantsMap cfgWithoutBackedges
          calls = mapMaybe (^? #_Call) . HashSet.toList . G.nodes $ cfgWithoutBackedges
      return . Just $ CfgInfo
        { cfg = cfg
        , acyclicCfg = cfgWithoutBackedges
        , descendantsMap = dmap
        , calls = calls
        }

-- -- | TODO: use sqlite or something beside hashmap so we can use Function as key
-- cfgFromFunc :: CfgStore -> Function -> IO (Maybe PilCfg)
-- cfgFromFunc store func = return $ HashMap.lookup func store

-- getFromFuncName :: Text -> CfgStore -> IO [(Function, PilCfg)]
-- getFromFuncName fname store = return . filter nameMatches $ HashMap.toList store
--   where
--     nameMatches (func, _cfg) = func ^. #name == fname

-- -- | Convenience function to get the first Cfg in store with that name
-- getFromFuncName_ :: Text -> CfgStore -> IO (Maybe PilCfg)
-- getFromFuncName_ fname store = fmap snd . headMay $ getFromFuncName fname store
