{- HLINT ignore "Use <$>" -}
module Blaze.Cfg.Path
 ( module Blaze.Cfg.Path
 , module Exports
 ) where

import Blaze.Prelude

import Blaze.Types.Cfg (Cfg, CfNode, CallNode, HasCtx(getCtx), BranchType(UnconditionalBranch), setNodeUUID)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg.Grouping (unfoldGroups)
import Blaze.Types.Cfg.Path as Exports
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (DescendantsMap)
import Blaze.Cfg.Interprocedural ()
import qualified Blaze.Cfg.Interprocedural as CfgI
import qualified Blaze.Path as P
import Blaze.Path (SampleRandomPathError, SampleRandomPathError', ChildChooser)
import Blaze.Types.Pil (Stmt, CallStatement, Expression)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst))
import qualified Blaze.Pil.Construct as C
import qualified Blaze.Cfg.Loop as Loop

import qualified Data.HashSet as HashSet
import Data.List (nub)


-- | Generates all paths from a Cfg that do not visit the same node twice.
-- This may include paths that end in the body of a loop and thus
-- do not have a return node.
-- Calls unfoldGroups on Cfg to ensure there are no groups.
-- Note: Loop summary nodes might one day enable us to return only returning paths.
getAllSimplePaths :: (Ord a, Hashable a) => Cfg (CfNode a) -> [Path (CfNode a)]
getAllSimplePaths cfg = mkCfPath <$> P.getAllPaths (\_ _ -> error "should not revisit") 0
  (Cfg.getRootNode cfg')
  (cfg' ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg'
    mkCfPath = Path nextCtxIndex_ outerCtx_
    (cfg', _) = unfoldGroups cfg


-- | Generates all paths from a Cfg that do not visit the same node twice.
-- This may include paths that end in the body of a loop and thus
-- do not have a return node.
-- Returns only paths containing all the nodes specified in `reqNodes`
-- Calls unfoldGroups on Cfg to ensure there are no groups.
getSimplePathsContaining
  :: (Ord a, Hashable a)
  => HashSet (CfNode a)
  -> Cfg (CfNode a)
  -> [Path (CfNode a)]
getSimplePathsContaining reqNodes cfg = mkCfPath <$> P.getPathsContaining (\_ _ -> error "should not revisit") 0
  (Cfg.getRootNode cfg')
  (cfg' ^. #graph)
  reqNodes
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg'
    mkCfPath = Path nextCtxIndex_ outerCtx_

    (cfg', _) = unfoldGroups cfg

-- | Gets all non-looping paths that reach a return node.
getSimpleReturnPaths
  :: (Ord a, Hashable a)
  => Cfg (CfNode a)
  -> [Path (CfNode a)]
getSimpleReturnPaths cfg = getSimplePathsContaining (G.getTermNodes cfg) cfg


-- | Expands a call node with another path. Updates ctxIds of inner path,
-- as well as node UUIDs.
-- If inner path does not return, snips off path after call expansion.
-- Returns Nothing if call node not found.
-- WARNING: innerPath must contain nodes with unique node ids!
--
expandCall
  :: UUID
  -> PilPath
  -> CallNode [Stmt]
  -> PilPath
  -> Maybe PilPath
expandCall leaveFuncUuid outerPath callNode innerPath
  | not (P.hasNode (Cfg.Call callNode) outerPath) = Nothing
  | otherwise = do
      callStmt <- CfgI.getCallStmt callNode
      return $ Path
        { nextCtxIndex = outerPathNextCtxIndex'
        , outerCtx = outerPath ^. #outerCtx
        , path = P.expandNode (Cfg.Call callNode) outerPathPerhapsSnipped (wrapInnerPath callStmt ^. #path)
        }
  where
    outerPathPerhapsSnipped = case retExpr of
      Nothing -> P.removeAfterNode (Cfg.Call callNode) $ outerPath ^. #path
      Just _ -> outerPath ^. #path

    outerPathNextCtxIndex' = outerPath ^. #nextCtxIndex + innerPath ^. #nextCtxIndex
    innerPath' = recurSubst (+ (outerPath ^. #nextCtxIndex)) innerPath
    innerPathCtx' = getCtx innerPath'

    wrapInnerPath :: CallStatement -> PilPath
    wrapInnerPath cstmt = P.append (mkEnterNodePath cstmt) UnconditionalBranch
      . maybe innerPath' (P.append innerPath' UnconditionalBranch)
      $ mkLeaveNodePath cstmt

    mkEnterNodePath :: CallStatement -> PilPath
    mkEnterNodePath = build outerPathNextCtxIndex' . P.start . mkEnterNode
    mkEnterNode :: CallStatement -> CfNode [Stmt]
    mkEnterNode = Cfg.EnterFunc
      . CfgI.mkEnterFuncNode (callNode ^. #uuid) (outerPath ^. #outerCtx) innerPathCtx'

    -- | Return expression of inner path.
    --   Returns Nothing if last stmt of last node is not ret
    retExpr :: Maybe Expression
    retExpr = (lastMay . Cfg.getNodeData $ P.end innerPath') >>= \case
      Pil.Ret x -> return $ x ^. #value
      _ -> Nothing

    mkLeaveNodePath :: CallStatement -> Maybe PilPath
    mkLeaveNodePath = fmap (build outerPathNextCtxIndex' . P.start) . mkLeaveNode
    mkLeaveNode :: CallStatement -> Maybe (CfNode [Stmt])
    mkLeaveNode cstmt = case retExpr of
      Nothing -> Nothing
      Just _ -> return . Cfg.LeaveFunc $ Cfg.LeaveFuncNode
        { prevCtx = innerPathCtx'
        , nextCtx = outerPath ^. #outerCtx
        , uuid = leaveFuncUuid
        , nodeData = maybeToList $ C.def' <$> cstmt ^. #resultVar <*> retExpr
        }

expandCallWithNewInnerPathIds :: UUID -> PilPath -> CallNode [Stmt] -> PilPath -> IO (Maybe PilPath)
expandCallWithNewInnerPathIds leaveFuncUuid outerPath callNode innerPath = do
  innerPath' <- safeTraverse giveNewUUID innerPath
  return $ expandCall leaveFuncUuid outerPath callNode innerPath'
  where
    giveNewUUID :: CfNode [Stmt] -> IO (CfNode [Stmt])
    giveNewUUID n = do
      uuid <- randomIO
      return $ setNodeUUID uuid n

makeCfgAcyclic ::
  forall a.
  Hashable a =>
  Cfg (CfNode a) ->
  Cfg (CfNode a)
makeCfgAcyclic cfg = Cfg.removeEdges bedges cfg
  where
    bedges = view #edge <$> Loop.getBackEdges cfg

-- | Samples numSamples random paths from a Cfg.
-- This sampler expects an acyclic Cfg. Otherwise, it might pursue branches
-- that only reach a required node through a loop, which it, at the moment,
-- cannot follow, so the path will lack the required node.
-- It also expects that the Cfg has no groups.
sampleRandomPathsContaining_
  :: (Ord a, Hashable a)
  => DescendantsMap (CfNode a)
  -> HashSet (CfNode a)
  -> Int
  -> Cfg (CfNode a)
  -> IO [Path (CfNode a)]
sampleRandomPathsContaining_ dmap reqSomeNodes numSamples cfg = do
  paths <- replicateM numSamples
    (P.sampleRandomPath
      dmap
      (\ _ _ -> error "should not revisit")
      0
      (Cfg.getRootNode cfg)
      (cfg ^. #graph)
      reqSomeNodes
    )
  return . fmap mkCfPath . nub . mapMaybe hush $ paths
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_

-- | Gets a single random path from a CFG.
sampleRandomPath_
  :: Hashable a
  => ((Int, Int) -> IO Int)
  -> DescendantsMap (CfNode a)
  -> Cfg (CfNode a)
  -> IO (Either (SampleRandomPathError' (CfNode a)) (Path (CfNode a)))
sampleRandomPath_ pickBranch dmap cfg
  = fmap (fmap mkCfPath)
  $ P.sampleRandomPath_
    pickBranch
    dmap
    (\ _ _ -> error "should not revisit")
    0
    (Cfg.getRootNode cfg)
    (cfg ^. #graph)
    HashSet.empty
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_

-- | Gets numSamples random paths from Cfg.
-- This version preps the Cfg by unfolding groups and making it acyclic.
sampleRandomPathsContaining
  :: (Ord a, Hashable a)
  => HashSet (CfNode a)
  -> Int
  -> Cfg (CfNode a)
  -> IO [Path (CfNode a)]
sampleRandomPathsContaining reqSomeNodes numSamples cfg = do
  let dmap = G.calcDescendantsMap cfg''
  sampleRandomPathsContaining_ dmap reqSomeNodes numSamples cfg''
  where
    (cfg', _) = unfoldGroups cfg
    cfg'' = makeCfgAcyclic cfg'

-- | Gets a single random path from a CFG.
sampleRandomPath_'
  :: forall a s e m. (Monad m, Hashable a)
  => ChildChooser s e m BranchType (CfNode a) -- Choose between children
  -> s -- initial chooser state
  -> Cfg (CfNode a)
  -> ExceptT (SampleRandomPathError e) m (Path (CfNode a))
sampleRandomPath_' chooser initState cfg
  = fmap mkCfPath
  $ P.sampleRandomPath_'
    chooser
    initState
    (\ _ _ -> error "should not revisit")
    0
    (Cfg.getRootNode cfg)
    (cfg ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_
