module Flint.Cfg.Path where

import Flint.Prelude

import Flint.Types.Query (Query, GetFunction)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as CfgPath
import qualified Blaze.Path as Path
import Blaze.Types.Graph (DescendantsMap)
import Blaze.Types.Function (Function)
import Blaze.Types.Cfg (CallNode)
import Blaze.Types.Pil (Stmt)

import qualified Data.HashSet as HashSet

type CallDepth = Word64
type CallExpansionChooser = CallDepth -> [CallNode [Stmt]] -> IO [CallNode [Stmt]]

getCallNodeFunc :: CallNode a -> Maybe Function
getCallNodeFunc n = n ^? #callDest . #_CallFunc

expandAllToDepth :: CallDepth -> CallExpansionChooser
expandAllToDepth expandDepthLimit currentDepth nodes
  | currentDepth < expandDepthLimit = return nodes
  | otherwise = return []

-- | This function explores from the start of a function, expanding calls
--   on a path until it reaches the depth limit.
--   It returns Nothing if the starting func Cfg is not in the store.
exploreFromStartingFunc_
  :: ((Int, Int) -> IO Int) -- Pick random branch
  -> CallExpansionChooser
  -> CallDepth
  -> CfgStore
  -> Function
  -> IO (Maybe PilPath)
exploreFromStartingFunc_ pickFromRange expansionChooser currentDepth store startingFunc = CfgStore.getFuncCfgInfo store startingFunc >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> do
    er <- CfgPath.sampleRandomPath_
      pickFromRange
      (cfgInfo ^. #descendantsMap)
      (cfgInfo ^. #acyclicCfg)
    case er of
      Left err -> error $ "Unexpected sampleRandomPath_ failure: " <> show err
      Right mainPath -> do
        let callsOnPath = mapMaybe (^? #_Call) . HashSet.toList . Path.nodes $ mainPath
        callsToExpand <- expansionChooser currentDepth callsOnPath
        innerPaths <- flip mapMaybeM callsToExpand $ \callNode -> runMaybeT $ do
          destFunc <- hoistMaybe $ getCallNodeFunc callNode
          innerPath <- liftMaybeTIO $ exploreFromStartingFunc_
            pickFromRange
            expansionChooser
            (currentDepth + 1)
            store
            destFunc
          leaveFuncUuid <- liftIO randomIO -- TODO: pass in UUID-making func?
          return (callNode, innerPath, leaveFuncUuid)
        return
          . Just
          $ foldl (\p (callNode, innerPath, leaveFuncUuid) -> fromJust $
                    CfgPath.expandCall leaveFuncUuid p callNode innerPath)
            mainPath
            innerPaths

-- | Gets a random path from the starting function to the target basic block.
sampleToTarget_
  :: (Int -> IO Int) -- If multiple calls reach target, pick from them.
  -> ((Int, Int) -> IO Int) -- Pick random branch amonst those that reach target
  -> DescendantsMap Function
  -> CfgStore
  -> Function
  -> Address
  -> IO (Maybe PilPath)
sampleToTarget_ pickCallToTarget pickBranch dmap store startingFunc targetAddr = return Nothing

samplesFromQuery_
  :: GetFunction a
  => (Int -> IO Int) -- Pick random target node
  -> ((Int, Int) -> IO Int) -- Pick random branch
  -> CfgStore
  -> Query a
  -> IO [PilPath]
samplesFromQuery_ pickRandomTargetNode pickRandomBranch store q = do
  return []
