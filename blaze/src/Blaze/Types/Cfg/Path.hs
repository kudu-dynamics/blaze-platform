module Blaze.Types.Cfg.Path
  ( module Blaze.Types.Cfg.Path
  , module Exports
  ) where

import Blaze.Prelude hiding (succ, pred)
import Blaze.Types.Cfg (Cfg(Cfg), CfNode, BranchType, HasCtx(getCtx))
import qualified Blaze.Cfg as Cfg
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Graph (Identifiable, LEdge(LEdge), Edge(Edge))
import qualified Blaze.Types.Graph as G
import Blaze.Types.Path (IsPath, PathBuilder)
import Blaze.Types.Path as Exports (start)
import qualified Blaze.Types.Path as P
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Types.Path.Alga as AlgaPath
import Blaze.Types.Pil (Stmt, CtxId, Ctx)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst), FlatSubst(flatSubst))

import qualified Data.HashMap.Strict as HashMap

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

-- | Traverses all the nodes, memoizing results to ensure consistency.
-- This doesn't touch the outerCtx or nextCtxIndex fields, so if the traverse
-- function changes the Ctx of all the nodes, the outerCtx might no longer match.
safeTraverse_
  :: ( Hashable a
     , Hashable b
     , Identifiable a UUID
     , Identifiable b UUID
     , Monad m
     , Show a
     )
  => (a -> m b)
  -> Path a
  -> StateT (HashMap a b) m (Path b)
safeTraverse_ f = traverseOf #path $ AlgaPath.safeTraverse_ f

safeTraverse
  :: ( Hashable a
     , Hashable b
     , Identifiable a UUID
     , Identifiable b UUID
     , Monad m
     , Show a
     )
  => (a -> m b)
  -> Path a
  -> m (Path b)
safeTraverse f p = evalStateT (safeTraverse_ f p) HashMap.empty 

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
      Cfg.UnconditionalBranch -> ndata
      Cfg.TrueBranch -> changeLastStmtToConstraint True
      Cfg.FalseBranch -> changeLastStmtToConstraint False
      where
        -- | Gets the arg exprs out of the node data from EnterFuncNode
        -- This is sort of dirty because it relies on the behavior of
        -- Interprecedural.mkEnterFuncNode to define the args in the proper order.
        -- To make this more reliable, we could add an `args :: [Pil.Expression]`
        -- field to the `Cfg.EnterFuncNode a` type. 
        getArgs' :: Cfg.EnterFuncNode [Stmt] -> [Pil.Expression]
        getArgs' (Cfg.EnterFuncNode _ _ _ xs) = getArg <$> xs
          where
            getArg (Pil.Def x) = x ^. #value
            getArg _ = error "Unexpected arg format"
        ndata :: [Stmt]
        ndata = (<> Cfg.getNodeData a) $ case a of
          Cfg.EnterFunc x -> [ Pil.EnterContext
                               . Pil.EnterContextOp (x ^. #nextCtx)
                               $ getArgs' x
                             ]
          Cfg.LeaveFunc x -> [ Pil.ExitContext
                               $ Pil.ExitContextOp (x ^. #prevCtx) (x ^. #nextCtx)
                             ]
          _ -> []
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
