module Blaze.Pil.Analysis where

import           Blaze.Prelude

import           Blaze.Types.Pil        ( Expression( Expression )
                                        , PilVar
                                        , Statement( Def )
                                        , Stmt
                                        )
import qualified Blaze.Types.Pil as Pil

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet        as HSet
import Data.HashSet (HashSet)

getDefinedVars_ :: Stmt -> [PilVar]
getDefinedVars_ (Def d) = [d ^. Pil.var]
getDefinedVars_ _ = []

getDefinedVars :: [Stmt] -> HashSet PilVar
getDefinedVars = HSet.fromList . concatMap getDefinedVars_

getVarsFromExpr_ :: Expression -> [PilVar]
getVarsFromExpr_ e = case e ^. Pil.op of
  (Pil.VAR vop) -> [vop ^. Pil.src]
  (Pil.VAR_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_PHI x) -> x ^. Pil.dest : x ^. Pil.src
  (Pil.VAR_SPLIT x) -> [x ^. Pil.high, x ^. Pil.low]
  x -> concatMap getVarsFromExpr_ x

getVarsFromExpr :: Expression -> HashSet PilVar
getVarsFromExpr = HSet.fromList . getVarsFromExpr_

getDefinedVar :: Stmt -> Maybe PilVar
getDefinedVar (Def d) = Just $ d ^. Pil.var
getDefinedVar _ = Nothing

getVarsFromStmt :: Stmt -> HashSet PilVar
getVarsFromStmt s = foldr f init s
  where
    init = maybe HSet.empty HSet.singleton $ getDefinedVar s
    f x vs = HSet.union (getVarsFromExpr x) vs

getRefVars_ :: Stmt -> HashSet PilVar
getRefVars_ = HSet.fromList . concatMap getVarsFromExpr_

-- |Get all vars references in any of the provided statements.
--  NB: Vars that are defined but not used are not considered
--  referenced.
getRefVars :: [Stmt] -> HashSet PilVar
getRefVars = HSet.unions . map getRefVars_

getFreeVars :: [Stmt] -> HashSet PilVar
getFreeVars xs = HSet.difference allVars defined
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
  _ -> x & Pil.op %~ fmap (substVarExprInExpr f)

substVarExpr_ :: (PilVar -> Maybe Expression) -> Stmt -> Stmt
substVarExpr_ f = fmap $ substVarExprInExpr f

substVarExpr :: (PilVar -> Maybe Expression) -> [Stmt] -> [Stmt]
substVarExpr f = fmap $ substVarExpr_ f

-----------------

type EqMap a = HashMap a a


addToEqMap :: (Hashable a, Ord a) => (a, a) -> EqMap a -> EqMap a
addToEqMap (v1, v2) m = case HMap.lookup v2 m of
  Nothing -> HMap.insert v1 v2 m
  Just origin -> HMap.insert v1 origin m

updateVarEqMap :: Stmt -> EqMap PilVar -> EqMap PilVar
updateVarEqMap (Def (Pil.DefOp v1 (Expression _ (Pil.VAR (Pil.VarOp v2))))) m
  = addToEqMap (v1, v2) m
updateVarEqMap _ m = m

-- |Each var equivalent to another var is resolved to the 
--  earliest defined var. E.g., a = 1, b = a, c = b will
--  result in c mapping to a.
getVarEqMap :: [Stmt] -> EqMap PilVar
getVarEqMap = updateMapsToInOriginVars . foldr updateVarEqMap HMap.empty
  where
    updateMapsToInOriginVars :: EqMap PilVar -> EqMap PilVar
    updateMapsToInOriginVars m = fmap f m
      where
        omap = originsMap m
        omap' = HMap.mapWithKey mergePilVars omap
        f v = case HMap.lookup v omap' of
          Nothing -> v
          (Just v') -> v'

originsMap :: EqMap PilVar -> HashMap PilVar (HashSet PilVar)
originsMap = foldr f HMap.empty . HMap.toList
  where
    f (v1, v2) = HMap.alter g v1 where
      g Nothing = Just $ HSet.singleton v2
      g (Just s) = Just $ HSet.insert v2 s

-- |Merges the mapsTo of every var in set into the origin var.
mergePilVars :: PilVar -> HashSet PilVar -> PilVar
mergePilVars originVar s = originVar & Pil.mapsTo .~ x where
  x = foldr HSet.union HSet.empty y
  y = fmap (view Pil.mapsTo) . HSet.toList $ HSet.insert originVar s

---- Constant Propagation
type VarExprMap = HashMap PilVar Expression

data ConstantFold = ConstantFold
  { exprMap :: VarExprMap
  , stmts :: Set Stmt}

constantProp :: [Stmt] -> [Stmt]
constantProp xs = substVarExpr (\v -> HMap.lookup v (exprMap constFold)) 
                               [x | x <- xs, not . Set.member x $ stmts constFold]
  where addConst cf stmt var cnst = ConstantFold { exprMap = HMap.insert var cnst (exprMap cf)
                                                 , stmts = Set.insert stmt (stmts cf)}
        constFold = foldl' f (ConstantFold HMap.empty Set.empty) xs
          where f cfAcc stmt = 
                  case stmt of 
                    (Pil.Def (Pil.DefOp var (Pil.Expression sz (Pil.CONST constOp)))) 
                      -> addConst cfAcc stmt var (Pil.Expression sz (Pil.CONST constOp))
                    _
                      -> cfAcc
