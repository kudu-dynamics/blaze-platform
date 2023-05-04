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
import Blaze.Cfg.Interprocedural ()
import qualified Blaze.Cfg.Interprocedural as CfgI
import qualified Blaze.Path as P
import Blaze.Types.Pil (Stmt, CallStatement, Expression)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst))
import qualified Blaze.Pil.Construct as C


-- | Generates all non-looping paths from a Cfg.
-- Calls unfoldGroups on Cfg to ensure there are no groups.
getAllSimplePaths :: (Ord a, Hashable a) => Cfg (CfNode a) -> [Path (CfNode a)]
getAllSimplePaths cfg = mkCfPath <$> P.getAllPaths (\_ _ -> error "should not revisit") 0
  (Cfg.getRootNode cfg')
  (cfg' ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg'
    mkCfPath = Path nextCtxIndex_ outerCtx_
    (cfg', _) = unfoldGroups cfg

-- | Generates all non-looping paths from a Cfg.
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
-- Returns Nothing if call node not found.
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
        , path = P.expandNode (Cfg.Call callNode) (outerPath ^. #path) (wrapInnerPath callStmt ^. #path)
        }
  where
    outerPathNextCtxIndex' = outerPath ^. #nextCtxIndex + innerPath ^. #nextCtxIndex
    innerPath' = recurSubst (+ (outerPath ^. #nextCtxIndex)) innerPath
    innerPathCtx' = getCtx innerPath'
    
    wrapInnerPath :: CallStatement -> PilPath
    wrapInnerPath cstmt = P.append (mkEnterNodePath cstmt) UnconditionalBranch
      $ P.append innerPath' UnconditionalBranch (mkLeaveNodePath cstmt)

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

    mkLeaveNodePath :: CallStatement -> PilPath
    mkLeaveNodePath = build outerPathNextCtxIndex' . P.start . mkLeaveNode
    mkLeaveNode :: CallStatement -> CfNode [Stmt]
    mkLeaveNode cstmt = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
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
