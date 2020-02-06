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

---- Var -> Var substitution
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

---- Var -> Expression substitution
substVarExprInExpr :: (PilVar -> Maybe Expression) -> Expression -> Expression
substVarExprInExpr f x = case x ^. Pil.op of
  (Pil.VAR (Pil.VarOp v)) -> maybe x identity $ f v
  _ -> x & Pil.op %~ fmap (substVarExprInExpr f)

substVarExpr_ :: (PilVar -> Maybe Expression) -> Stmt -> Stmt
substVarExpr_ f = fmap $ substVarExprInExpr f

substVarExpr :: (PilVar -> Maybe Expression) -> [Stmt] -> [Stmt]
substVarExpr f = fmap $ substVarExpr_ f

---- Expression -> Expression substitution
substExprInExpr :: (Expression -> Maybe Expression) -> Expression -> Expression
-- substExprInExpr f x = maybe x recurse $ f x
substExprInExpr f x = recurse $ maybe x identity (f x)
  where
    recurse e = e & Pil.op %~ fmap (substExprInExpr f)

substExprs :: (Expression -> Maybe Expression) -> [Stmt] -> [Stmt]
substExprs f = fmap (fmap $ substExprInExpr f)

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

data ConstPropState
  = ConstPropState
      { exprMap :: VarExprMap,
        stmts :: Set Stmt
      }

constantProp :: [Stmt] -> [Stmt]
constantProp xs =
  substVarExpr
    (\v -> HMap.lookup v (exprMap constPropResult))
    [x | x <- xs, not . Set.member x $ stmts constPropResult]
  where
    addConst s stmt var cnst =
      ConstPropState
        { exprMap = HMap.insert var cnst (exprMap s),
          stmts = Set.insert stmt (stmts s)
        }
    constPropResult = foldl' f (ConstPropState HMap.empty Set.empty) xs
      where
        f constPropState stmt =
          case stmt of
            (Pil.Def (Pil.DefOp var (Pil.Expression sz (Pil.CONST constOp)))) ->
              addConst constPropState stmt var (Pil.Expression sz (Pil.CONST constOp))
            _ ->
              constPropState


---- Copy Propagation
type ExprMap = HashMap Expression Expression

reduceMap :: (Eq a, Hashable a) => HashMap a a -> HashMap a a
reduceMap m = fmap reduceKey m
  where
    reduceKey v = maybe v reduceKey (HMap.lookup v m)

data CopyPropState
  = CopyPropState
      { mapping :: ExprMap,
        copyStmts :: Set Stmt
      }

copyProp :: [Stmt] -> [Stmt]
copyProp xs =
  substExprs
    (\(e :: Expression) -> HMap.lookup e (mapping copyPropResult'))
    [x | x <- xs, not . Set.member x $ copyStmts copyPropResult']
  where
    addCopy s stmt copy orig =
      CopyPropState
        { mapping = HMap.insert copy orig (mapping s),
          copyStmts = Set.insert stmt (copyStmts s)
        }
    copyPropResult = foldl' f (CopyPropState HMap.empty Set.empty) xs
      where
        f copyPropState stmt =
          case stmt of
            (Pil.Def (Pil.DefOp lh_var rh_expr@(Pil.Expression sz (Pil.VAR (Pil.VarOp _))))) ->
              addCopy copyPropState stmt (Pil.Expression sz (Pil.VAR (Pil.VarOp lh_var))) rh_expr
            _ ->
              copyPropState
    copyPropResult' = copyPropResult {mapping = reduceMap (mapping copyPropResult)}

data MemEquivGroup = MemEquivGroup
  { store :: Stmt
  , storeAddr :: Expression
  , references :: [(Stmt, Expression)]}

data MemEquivGroupState = MemEquivGroupState
  { allGroups :: [MemEquivGroup]
  , liveGroups :: HashMap Expression MemEquivGroup}

data Load expr = Load expr
               | LoadSSA expr
               | LoadStruct expr
               | LoadStructSSA expr

-- findLoads :: Stmt -> [Load]
-- findLoads stmt = []

findMemEquivGroups :: [Stmt] -> [MemEquivGroup]
findMemEquivGroups = allGroups . memEquivGroupState
  where
    memEquivGroupState = foldl' f (MemEquivGroupState [] HMap.empty)
    f s stmt = case stmt of
      (Pil.Store storeOp) ->
        MemEquivGroupState
          { allGroups = case HMap.lookup addr (liveGroups s) of
              -- Previous equiv group for addr is being replaced
              -- TODO: Check if this store stmt uses the previous definition and update
              --       the old equiv group if necessary
              Just g -> g : allGroups s
              Nothing -> allGroups s,
            liveGroups = HMap.insert addr newGroup (liveGroups s)
          }
        where
          addr = storeOp ^. Pil.addr
          newGroup =
            MemEquivGroup {store = stmt, storeAddr = addr, references = []}
      _ -> s


-- |Copy propagation via memory. Finds and simplifies variables that are copied
-- through symbolic memory addresses that are identified to be equivalent.
-- This is done by constructing store-load chains similar to def-use chains.
copyPropMem :: [Stmt] -> [Stmt]
copyPropMem xs = substExprs (\v -> HMap.lookup v (mapping propResult)) xs
  where
    propResult = foldl' f (CopyPropState HMap.empty Set.empty) xs
      where
        f propState stmt = undefined
