module Blaze.Pil.Analysis where

import           Blaze.Prelude

import           Blaze.Types.Pil        ( Expression( Expression )
                                        , PilVar
                                        , Statement( Def )
                                        , Stmt
                                        )
import qualified Blaze.Types.Pil as Pil
import qualified Data.Map        as Map
import qualified Data.Set        as Set

getDefinedVars_ :: Stmt -> [PilVar]
getDefinedVars_ (Def d) = [d ^. Pil.var]
getDefinedVars_ _ = []

getDefinedVars :: [Stmt] -> Set PilVar
getDefinedVars = Set.fromList . concatMap getDefinedVars_

getVarsFromExpr_ :: Expression -> [PilVar]
getVarsFromExpr_ e = case e ^. Pil.op of
  (Pil.VAR vop) -> [vop ^. Pil.src]
  (Pil.VAR_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_PHI x) -> x ^. Pil.dest : x ^. Pil.src
  (Pil.VAR_SPLIT x) -> [x ^. Pil.high, x ^. Pil.low]
  x -> concatMap getVarsFromExpr_ x

getVarsFromExpr :: Expression -> Set PilVar
getVarsFromExpr = Set.fromList . getVarsFromExpr_

getRefVars_ :: Stmt -> Set PilVar
getRefVars_ = Set.fromList . concatMap getVarsFromExpr_

-- |Get all vars references in any of the provided statements.
--  NB: Vars that are defined but not used are not considered
--  referenced.
getRefVars :: [Stmt] -> Set PilVar
getRefVars = Set.unions . map getRefVars_

getFreeVars :: [Stmt] -> Set PilVar
getFreeVars xs = Set.difference allVars defined
  where
    defined = getDefinedVars xs
    allVars = getRefVars xs

substVarsInExpr :: (PilVar -> PilVar) -> Expression -> Expression
substVarsInExpr f e = case e ^. Pil.op of
  (Pil.VAR x) -> e & Pil.op .~ Pil.VAR (x & Pil.src %~ f)
  (Pil.VAR_FIELD x) -> e & Pil.op .~ Pil.VAR_FIELD (x & Pil.src %~ f)
  (Pil.VAR_ALIASED x) -> e & Pil.op .~ Pil.VAR_ALIASED (x & Pil.src %~ f)
  (Pil.VAR_ALIASED_FIELD x) -> e & Pil.op .~ Pil.VAR_ALIASED_FIELD (x & Pil.src %~ f)
  (Pil.VAR_PHI x) -> e & Pil.op .~ Pil.VAR_PHI (x & Pil.src %~ fmap f
                                                    & Pil.dest %~ f)
  (Pil.VAR_SPLIT x) -> e & Pil.op .~ Pil.VAR_SPLIT (x & Pil.high %~ f
                                                        & Pil.low %~ f)
  _ -> e

substVars_ :: (PilVar -> PilVar) -> Stmt -> Stmt
substVars_ f = fmap $ substVarsInExpr f

substVars :: (PilVar -> PilVar) -> [Stmt] -> [Stmt]
substVars f = fmap $ substVars_ f

substVarExprInExpr :: (PilVar -> Maybe Expression) -> Expression -> Expression
substVarExprInExpr f x = case x ^. Pil.op of
  (Pil.VAR (Pil.VarOp v)) -> maybe x identity $ f v
  _ -> x

substVarExpr_ :: (PilVar -> Maybe Expression) -> Stmt -> Stmt
substVarExpr_ f = fmap $ substVarExprInExpr f

substVarExpr :: (PilVar -> Maybe Expression) -> [Stmt] -> [Stmt]
substVarExpr f = fmap $ substVarExpr_ f

-----------------

type EqMap a = Map a a


addToEqMap :: Ord a => (a, a) -> EqMap a -> EqMap a
addToEqMap (v1, v2) m = case Map.lookup v2 m of
  Nothing -> Map.insert v1 v2 m
  Just origin -> Map.insert v1 origin m

updateVarEqMap :: Stmt -> EqMap PilVar -> EqMap PilVar
updateVarEqMap (Def (Pil.DefOp v1 (Expression _ (Pil.VAR (Pil.VarOp v2))))) m
  = addToEqMap (v1, v2) m
updateVarEqMap _ m = m

-- |Each var equivalent to another var is resolved to the 
--  earliest defined var. E.g., a = 1, b = a, c = b will
--  result in c mapping to a.
getVarEqMap :: [Stmt] -> EqMap PilVar
getVarEqMap = updateMapsToInOriginVars . foldr updateVarEqMap Map.empty
  where
    updateMapsToInOriginVars :: EqMap PilVar -> EqMap PilVar
    updateMapsToInOriginVars m = fmap f m
      where
        omap = originsMap m
        omap' = Map.mapWithKey mergePilVars omap
        f v = case Map.lookup v omap' of
          Nothing -> v
          (Just v') -> v'

originsMap :: EqMap PilVar -> Map PilVar (Set PilVar)
originsMap = foldr f Map.empty . Map.toList
  where
    f (v1, v2) m = Map.alter g v1 m where
      g Nothing = Just $ Set.singleton v2
      g (Just s) = Just $ Set.insert v2 s

-- |Merges the mapsTo of every var in set into the origin var.
mergePilVars :: PilVar -> Set PilVar -> PilVar
mergePilVars originVar s = originVar & Pil.mapsTo .~ x where
  x = foldr Set.union Set.empty y
  y = fmap (view Pil.mapsTo) . Set.toList $ Set.insert originVar s

