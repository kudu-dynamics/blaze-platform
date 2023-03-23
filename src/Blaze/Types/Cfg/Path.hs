module Blaze.Types.Cfg.Path where

import Blaze.Prelude hiding (succ, pred)
import Blaze.Types.Cfg (Cfg(Cfg), CfNode, BranchType, HasCtx(getCtx))
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Graph (Identifiable, LEdge)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Path (IsPath, PathBuilder)
import qualified Blaze.Types.Path as P
import Blaze.Types.Path.Alga (AlgaPath)
import Blaze.Types.Pil (Stmt, CtxId, Ctx)
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

instance (RecurSubst CtxId a, Identifiable a UUID, Hashable a) => IsPath BranchType a Path where
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

toCfg :: (Identifiable a UUID, Hashable a) => Path a -> Cfg a
toCfg p = Cfg
  { graph = g
  , rootId = G.getNodeId r
  , nextCtxIndex = p ^. #nextCtxIndex
  }
  where
    (r, g) = P.toPathGraph $ p ^. #path

build :: Hashable a => CtxId -> PathBuilder BranchType (CfNode a) -> Path (CfNode a)
build nextCtxIndex_ pb = Path
  { nextCtxIndex = nextCtxIndex_
  , outerCtx = getCtx $ P.root p 
  , path = p
  }
  where
    p = P.build pb
