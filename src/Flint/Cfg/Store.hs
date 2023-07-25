module Flint.Cfg.Store ( module Flint.Cfg.Store ) where

import Flint.Prelude

import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Types.Function (Function)

import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType) 

import Blaze.Types.Cfg (PilCfg, PilNode)

import qualified Data.HashMap.Strict as HashMap


init :: CfgStore
init = HashMap.empty

-- | Adds a func/cfg to the store.
-- Any Cfgs in the store should have a CtxId of 0
addFuncCfg_ :: Function -> PilCfg -> CfgStore -> CfgStore
addFuncCfg_ func cfg store = HashMap.alter (Just . maybe [v] (v:)) k store
  where
    k = func ^. #name
    v = (func, cfg)

addFunc
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a -> Function -> CfgStore -> IO CfgStore
addFunc imp func store = ImpCfg.getCfg imp func 0 >>= \case
  Nothing -> return store
  Just r -> return $ addFuncCfg_ func cfg store
    where cfg = r ^. #result

-- | TODO: use sqlite or something beside hashmap so we can use Function as key
cfgFromFunc :: Function -> CfgStore -> Maybe PilCfg
cfgFromFunc func store = do
  vs <- HashMap.lookup (func ^. #name) store
  r <- headMay . filter ((== func) . fst) $ vs
  return $ snd r

getFromFuncName :: Text -> CfgStore -> [(Function, PilCfg)]
getFromFuncName fname = fromMaybe [] . HashMap.lookup fname

-- | Convenience function to get the first Cfg in store with that name
getFromFuncName_ :: Text -> CfgStore -> Maybe PilCfg
getFromFuncName_ fname store = fmap snd $ HashMap.lookup fname store >>= headMay
