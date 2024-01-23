{-# LANGUAGE DefaultSignatures #-}
module Blaze.Types.Pil.Analysis.Subst where

import Blaze.Prelude

import Blaze.Types.Cfg (CfNode, Cfg)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Pil (CtxId, Ctx, PilVar, ExprOp, Expression, Statement)
import qualified Blaze.Types.Pil as Pil


-- | Substitutes anything except inner polymorphic types
class FlatSubst a b where
  flatSubst :: (a -> a) -> b -> b

-- | Substitutes type itself and polymorphic inner types as well.
class FlatSubst a b => RecurSubst a b where
  recurSubst :: (a -> a) -> b -> b
  -- | Default implementation is for case where b is a functor
  default recurSubst :: (Functor f, b ~ f c, RecurSubst a c) => (a -> a) -> b -> b
  recurSubst f b = recurSubst f <$> flatSubst f b

-- PilVar Substs

instance FlatSubst PilVar PilVar where
  flatSubst f = f

instance FlatSubst PilVar (ExprOp a) where
  flatSubst f = \case
    (Pil.VAR x) -> Pil.VAR $ x & #src %~ f
    (Pil.VAR_FIELD x) -> Pil.VAR_FIELD $ x & #src %~ f
    (Pil.VAR_JOIN x) -> Pil.VAR_JOIN $ x & #high %~ f
                                         & #low %~ f
    -- TODO: instead of this catchall, write out each case so that
    -- any new ExprOps we add later get a warning. 
    x -> x

instance RecurSubst PilVar a => RecurSubst PilVar (ExprOp a)

instance FlatSubst PilVar Expression where
  flatSubst _ = identity

instance RecurSubst PilVar Expression where
  recurSubst f = over #op $ recurSubst f

-- | Substs all PilVars in statement,
-- Unlike `Analysis.substVars_`, which ignores dest of Defs.
instance FlatSubst PilVar (Statement a) where
  flatSubst f = \case
    Pil.Def x -> Pil.Def $ x & #var %~ f
    Pil.DefPhi x -> Pil.DefPhi $ x & #dest %~ f
                                   & #src %~ fmap f
    -- TODO: explicitly handle each case so we are warned of new statement types
    x -> x

instance RecurSubst PilVar a => RecurSubst PilVar (Statement a)

---- CtxId Substs

instance FlatSubst CtxId CtxId where
  flatSubst f = f

instance FlatSubst CtxId Ctx where
  flatSubst = over #ctxId

instance FlatSubst CtxId PilVar where
  flatSubst f pv = pv & #ctx %~ fmap (flatSubst f)

instance FlatSubst CtxId (Pil.ExprOp expr) where
  -- This shallow substs all the CtxIds in every PilVar in an ExprOp
  flatSubst f = flatSubst (flatSubst f :: PilVar -> PilVar)

instance RecurSubst CtxId expr => RecurSubst CtxId (Pil.ExprOp expr)

instance FlatSubst CtxId (Pil.Statement expr) where
  -- | This substs the bare Ctxs then substs in any PilVars
  flatSubst f stmt = flatSubst (flatSubst f :: PilVar -> PilVar)
    $ case stmt of
        Pil.EnterContext x -> Pil.EnterContext $ x & #ctx %~ flatSubst f
        Pil.ExitContext x -> Pil.ExitContext $ x & #leavingCtx %~ flatSubst f
                                                 & #returningToCtx %~ flatSubst f
        -- TODO: match each case explicitly
        x -> x
        
instance RecurSubst CtxId expr => RecurSubst CtxId (Pil.Statement expr)

instance FlatSubst CtxId Expression where
  flatSubst _ = identity

instance RecurSubst CtxId Expression where
  recurSubst f = over #op $ recurSubst f

instance FlatSubst CtxId (CfNode a) where
  flatSubst f = \case
    Cfg.BasicBlock x -> Cfg.BasicBlock $ x & #ctx %~ flatSubst f
    Cfg.Call x -> Cfg.Call $ x & #ctx %~ flatSubst f
    Cfg.EnterFunc x -> Cfg.EnterFunc $ x & #prevCtx %~ flatSubst f
                                         & #nextCtx %~ flatSubst f
    Cfg.LeaveFunc x -> Cfg.LeaveFunc $ x & #prevCtx %~ flatSubst f
                                         & #nextCtx %~ flatSubst f
    Cfg.Grouping x -> Cfg.Grouping $ x & #grouping %~ flatSubst f

instance RecurSubst CtxId a => RecurSubst CtxId (CfNode a)

instance FlatSubst CtxId (Cfg a) where
  flatSubst f = over #nextCtxIndex $ flatSubst f

instance RecurSubst CtxId a => RecurSubst CtxId (Cfg a)

instance FlatSubst CtxId [a] where
  flatSubst _ = identity

instance RecurSubst CtxId a => RecurSubst CtxId [a]

--------- Expression

instance FlatSubst Expression (ExprOp a) where
  flatSubst _ op = op

instance RecurSubst Expression a => RecurSubst Expression (ExprOp a)

instance FlatSubst Expression Expression where
  flatSubst f = f

instance RecurSubst Expression Expression where
  recurSubst f = over #op $ recurSubst f

instance FlatSubst Expression (Statement a) where
  flatSubst _ stmt = stmt

instance RecurSubst Expression a => RecurSubst Expression (Statement a)
