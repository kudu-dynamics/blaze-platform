{- HLINT ignore "Redundant <$>" -}

module Flint.Cfg.Store ( module Flint.Cfg.Store ) where

import Flint.Prelude

import Flint.Types.Cfg.Store
import qualified Flint.Types.CachedCalc as CC

import qualified Blaze.CallGraph as CG
import Blaze.Cfg.Path (makeCfgAcyclic)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (calcDescendantsMap, DescendantsMap(DescendantsMap))
import qualified Blaze.Types.Graph as G

import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType) 

import Blaze.Types.CallGraph (CallGraph)
import Blaze.Types.Cfg (PilCfg, PilNode)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Util (getMemoized)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap


-- This is a cache of Cfgs for functions.
-- This version only supports functions from a single binary.

-- If you have a bunch of object files, you can collect them like so:
-- gcc -no-pie -Wl,--unresolved-symbols=ignore-all -o full_collection_binary *.o
-- OR you can maybe use this one:
-- ld -r -o combined.o *.o


init
  :: ( CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     , CfgImporter imp
     )
  => imp -> IO CfgStore
init imp = do
  store <- CfgStore
    <$> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
    <*> CG.getFunctions imp
    <*> atomically CC.create
    <*> atomically CC.create
    <*> atomically CC.create
  CC.setCalc () (store ^. #callGraphCache) $ do
    CG.getCallGraph imp $ store ^. #funcs
  CC.setCalc () (store ^. #transposedCallGraphCache) $ do
    cg <- getCallGraph store
    return $ G.transpose cg
  -- Set up calcs for ancestors
  forM_ (store ^. #funcs) $ \func -> do
    CC.setCalc func (store ^. #ancestorsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #transposedCallGraphCache)
      return $ G.getDescendants func cg
    CC.setCalc func (store ^. #descendantsCache) $ do
      cg <- fromJust <$> CC.get () (store ^. #callGraphCache)
      return $ G.getDescendants func cg
    CC.setCalc func (store ^. #callSitesCache) $ do
      CG.getCallSites imp func
    addFunc imp store func
  return store

getNewUuid :: UUID -> StateT (HashMap UUID UUID) IO UUID
getNewUuid old = do
  m <- get
  case HashMap.lookup old m of
    Just new -> return new
    Nothing -> do
      new <- lift randomIO
      modify $ HashMap.insert old new
      return new

traverseDescendantsMap
  :: forall m a b.
     ( Monad m
     , Hashable a
     , Hashable b
     )
  => (a -> m b)
  -> DescendantsMap a
  -> StateT (HashMap a b) m (DescendantsMap b)
traverseDescendantsMap f (DescendantsMap dmap) =
  DescendantsMap . HashMap.fromList <$> traverse g (HashMap.toList dmap)
  where
    g :: (a, HashSet a) -> StateT (HashMap a b) m (b, HashSet b)
    g (x, s) = do
      x' <- getMemoized f x
      s' <- fmap HashSet.fromList . traverse (getMemoized f) . HashSet.toList $ s
      return (x', s')

-- | Gets the CfgInfo, but all the UUIDs are fresh and new
getFreshFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFreshFuncCfgInfo store func = getFuncCfgInfo store func >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> flip evalStateT HashMap.empty $ do
    cfg' <- Cfg.safeTraverse_ updateNodeId $ cfgInfo ^. #cfg
    acyclicCfg' <- Cfg.safeTraverse_ updateNodeId $ cfgInfo ^. #acyclicCfg
    dmap' <- traverseDescendantsMap updateNodeId $ cfgInfo ^. #descendantsMap
    calls' <- fmap (mapMaybe (^? #_Call))
              . traverse (getMemoized updateNodeId . Cfg.Call)
              $ cfgInfo ^. #calls
    return . Just $ CfgInfo
      { cfg = cfg'
      , acyclicCfg = acyclicCfg'
      , descendantsMap = dmap'
      , calls = calls'
      }
      where
        updateNodeId :: PilNode -> IO PilNode
        updateNodeId n = do
          new <- randomIO
          return $ Cfg.setNodeUUID new n

-- | Gets the stored Cfg for a function, if it exists in the store.
getFuncCfgInfo :: CfgStore -> Function -> IO (Maybe CfgInfo)
getFuncCfgInfo store func = join <$> CC.get func (store ^. #cfgCache)

getAncestors :: CfgStore -> Function -> IO (Maybe (HashSet Function))
getAncestors store func = CC.get func $ store ^. #ancestorsCache

getFuncCfg :: CfgStore -> Function -> IO (Maybe PilCfg)
getFuncCfg store func = fmap (view #cfg) <$> getFuncCfgInfo store func

getCallGraph :: CfgStore -> IO CallGraph
getCallGraph store = fromJust <$> CC.get () (store ^. #callGraphCache)

getTransposedCallGraph :: CfgStore -> IO CallGraph
getTransposedCallGraph store = fromJust <$> CC.get () (store ^. #transposedCallGraphCache)


-- | Adds a func/cfg to the store.
-- Overwrites existing function Cfg.
-- Any Cfgs in the store should have a CtxId of 0
addFunc
  :: ( CfgImporter a
     , NodeDataType a ~ PilNode)
  => a -> CfgStore -> Function -> IO ()
addFunc imp store func = CC.setCalc func (store ^. #cfgCache) $ do
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
