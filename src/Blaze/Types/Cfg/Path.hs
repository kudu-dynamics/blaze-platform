module Blaze.Types.Cfg.Path where

import Blaze.Prelude hiding (succ, pred)
import Blaze.Types.Cfg (Cfg(Cfg), CfNode, BranchType, HasCtx(getCtx))
import qualified Blaze.Cfg as Cfg
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Graph (Identifiable, LEdge(LEdge), Edge(Edge))
import qualified Blaze.Types.Graph as G
import Blaze.Types.Path (IsPath, PathBuilder)
import qualified Blaze.Types.Path as P
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Types.Path.Alga as AlgaPath
import Blaze.Types.Pil (Stmt, CtxId, Ctx)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst), FlatSubst(flatSubst))

data Path a = Path
  { nextCtxIndex :: CtxId
  , outerCtx :: Ctx
  , path :: AlgaPath BranchType UUID a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance HasCtx (Path a) where
  getCtx = view #outerCtx

instance FlatSubst CtxId (Path a) where
  flatSubst f p = p & #nextCtxIndex %~ flatSubst f
                       & #outerCtx %~ flatSubst f

instance RecurSubst CtxId a => RecurSubst CtxId (Path a)

type CfPath a = Path (CfNode a)
type PilPath = Path (CfNode [Stmt])

safeMap
  :: ( Hashable a
     , Hashable b
     , Identifiable a UUID
     , Identifiable b UUID
     , Show a
     )
  => (a -> b)
  -> Path a
  -> Path b
safeMap f = over #path $ AlgaPath.safeMap f

safeTraverse
  :: ( Hashable a
     , Hashable b
     , Identifiable a UUID
     , Identifiable b UUID
     , Applicative f
     , Show a
     )
  => (a -> f b)
  -> Path a
  -> f (Path b)
safeTraverse f = traverseOf #path $ AlgaPath.safeTraverse f

instance (Show a, RecurSubst CtxId a, Identifiable a UUID, Hashable a) => IsPath BranchType a Path where
  root = P.root . view #path
  end = P.end . view #path
  succ n = P.succ n . view #path
  pred n = P.pred n . view #path
  succEdge n = P.succEdge n . view #path
  predEdge n = P.predEdge n . view #path
  hasNode n = P.hasNode n . view #path
  nodes = P.nodes . view #path
  toPathGraph = P.toPathGraph . view #path
  toNodeList = P.toNodeList . view #path
  toEdgeList = P.toEdgeList . view #path

  -- Use `expandCall` instead. This won't link callsite args with params.
  expandNode n p1 p2 = Path
    { nextCtxIndex = p1 ^. #nextCtxIndex + p2 ^. #nextCtxIndex
    , outerCtx = p1 ^. #outerCtx
    , path = P.expandNode n (p1 ^. #path)
             $ recurSubst (+ (p1 ^. #nextCtxIndex)) <$> (p2 ^. #path)
    }

  -- Use responsibly. The can make invalid paths with overlappings ctxIds.  
  append p1 l p2 = p1 & #path %~ (\p1' -> P.append p1' l (p2 ^. #path))

  removeAfterNode n = over #path $ P.removeAfterNode n
  -- The outerCtx might not make much sense after this
  removeBeforeNode n = over #path $ P.removeBeforeNode n

-- | Constructs a Path from sequential edges.
-- This doesn't check to make sure nodes aren't grouping nodes.
fromEdges
  :: ( Identifiable a UUID
     , Hashable a
     , HasCtx a
     )
  => CtxId
  -> a
  -> [LEdge BranchType a]
  -> Maybe (Path a)
fromEdges nextCtxIndex_ rootNode_ es
  = Path nextCtxIndex_ (getCtx rootNode_) <$> P.fromEdges rootNode_ es


-- | Converts a Cfg containing a single path into a Path. Fails if Cfg is multi-pathed.
fromCfg :: (Identifiable a UUID, HasCtx a, Hashable a) => Cfg a -> Maybe (Path a)
fromCfg cfg = Path (cfg ^. #nextCtxIndex) (getCtx cfg)
  <$> P.fromEdges (Cfg.getRootNode cfg) (G.edges $ cfg ^. #graph)

toCfg :: (Show a, Identifiable a UUID, Hashable a) => Path a -> Cfg a
toCfg p = Cfg
  { graph = g
  , rootId = G.getNodeId r
  , nextCtxIndex = p ^. #nextCtxIndex
  }
  where
    (r, g) = P.toPathGraph $ p ^. #path

build :: (Show a, Hashable a) => CtxId -> PathBuilder BranchType (CfNode a) -> Path (CfNode a)
build nextCtxIndex_ pb = Path
  { nextCtxIndex = nextCtxIndex_
  , outerCtx = getCtx $ P.root p 
  , path = p
  }
  where
    p = P.build pb

toStmts :: PilPath -> [Stmt]
toStmts p = concatMap getStmtsFromEdge (snd $ P.toEdgeList p) <> Cfg.getNodeData (P.end p)
  where
    getStmtsFromEdge :: LEdge Cfg.BranchType (CfNode [Stmt]) -> [Stmt]
    getStmtsFromEdge (LEdge lbl (Edge a _)) = case lbl of
      Cfg.UnconditionalBranch -> Cfg.getNodeData a
      Cfg.TrueBranch -> changeLastStmtToConstraint True
      Cfg.FalseBranch -> changeLastStmtToConstraint False
      where
        ndata :: [Stmt]
        ndata = Cfg.getNodeData a
        changeLastStmtToConstraint isTrueBranch = case lastMay ndata of
          Nothing -> ndata
          Just stmt -> case stmt of
            Pil.BranchCond (Pil.BranchCondOp cond) ->
              take (length ndata - 1) ndata
              <> [ Pil.Constraint
                   . Pil.ConstraintOp
                   $ bool (C.not cond $ cond ^. #size) cond isTrueBranch
                 ]
            _ -> ndata
              
