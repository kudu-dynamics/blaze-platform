{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.InferenceDependent2 where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Prelude as P
import Blaze.Types.Pil ( Expression(Expression)
                       , ExprOp
                       , OperationSize
                       , Statement
                       , PilVar
                       )
import qualified Blaze.Types.Pil as Pil
import qualified Data.Map as Map
-- import Data.HashMap.Strict (HashMap)
import qualified Binja.Variable as V
import qualified Binja.C.Enums as E
import qualified Binja.MLIL as MLIL
-- import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
-- import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Blaze.Pil.Analysis as Analysis
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified Data.STRef as ST
import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NG
import qualified Data.List.NonEmpty as NE
import Blaze.Types.Pil.Inference2


--- unifyTypes :: 

class VarSubst a where
  varSubst :: HashMap Sym Sym -> a -> a

instance VarSubst Sym where
  varSubst m v = maybe v identity $ HashMap.lookup v m

instance VarSubst a => VarSubst (PilType a) where
  varSubst m = fmap (varSubst m)

instance VarSubst SymType where
  varSubst m (SVar v) = SVar $ varSubst m v
  varSubst m (SType t) = SType $ varSubst m t

instance VarSubst Constraint where
  varSubst m (Constraint (v, t)) = Constraint (varSubst m v, varSubst m t)

-- | Swaps first key with second in map. 
updateSolKey :: (Hashable k, Eq k) => k -> k -> v -> HashMap k v -> HashMap k v
updateSolKey kOld kNew v m = HashMap.insert kNew v (HashMap.delete kOld m)

-- | Unifies constraint with all other constraints and solutions in state.
unifyConstraint :: Constraint -> Unify ()
unifyConstraint cx@(Constraint (preSubstSym, _preSubstType)) = do
  omap <- use originMap
  case varSubst omap cx of
    (Constraint (a, (SVar b))) 
      | a == b -> return ()  -- redundant constraint
      | otherwise -> do
          c <- addVarEq a b
          -- Do we actually need to subst with updated omap until
          -- all constraints are unified?
          let subMap = HashMap.fromList [(a, c), (b, c)]
          solutions %= fmap (varSubst subMap)

    -- look up 'a' in solutions map and unify it with existing solution for 'a'
    (Constraint (a, (SType t))) -> do
      -- TODO: occurs check here? (error if 'a' is used in 't')
      -- maybe we don't need occurs check since it's flat, hence "lazy"
      -- if unification fails with infinite loop, look here...
      sols <- use solutions
      case HashMap.lookup a sols of
        Nothing -> do
          originMap %= HashMap.insert preSubstSym a
          solutions %= HashMap.insert a t
        Just t' -> do
          t'' <- unifyPilTypes t t'
          solutions %= HashMap.insert a t''


-- hopefully this will never get into an infinite loop
unify :: Unify ()
unify = popConstraint >>= \case
  Nothing -> return ()
  Just cx -> unifyConstraint cx >> unify 

------------------------------------------

-- | True if second is at the same level or below in the type lattice
--   doesn't check recursive types or type sizes/signs.
--   Used to order args for unification
isTypeDescendent :: PilType a -> PilType a -> Bool
isTypeDescendent (TArray _ _) t = case t of
  TArray _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TInt _ _) t = case t of
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TFloat _) t = case t of
  TFloat _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TBitVector _) t = case t of
  TBitVector _ -> True
  -- I think these should be descendents
  TFloat _ -> True
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TPointer _ _) t = case t of
  TPointer _ _ -> True
  TArray _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TChar t = case t of
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TFunction _ _) t = case t of
  (TFunction _ _) -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TRecord _) t = case t of
  TRecord _ -> True
  TPointer _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TBottom t = case t of
  TBottom -> True
  _ -> False
isTypeDescendent (TVBitWidth _) t = case t of
  TVBitWidth _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TVLength _) t = case t of
  TVLength _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TVSign _) t = case t of
  TVSign _ -> True
  TBottom -> True
  _ -> False


unifyPilTypes :: PilType Sym -> PilType Sym -> Unify (PilType Sym)
unifyPilTypes TBottom _ = return TBottom
unifyPilTypes _ TBottom = return TBottom
unifyPilTypes pt1 pt2 =
  case (isTypeDescendent pt1 pt2, isTypeDescendent pt2 pt1) of
    (False, False) -> err
    (False, True) -> unifyPilTypes pt2 pt1
    _ -> case pt1 of
      TArray len1 et1 -> case pt2 of
        (TArray len2 et2) ->
          TArray <$> addVarEq len1 len2 <*> addVarEq et1 et2
        (TPointer _w et2) -> TArray len1 <$> addVarEq et1 et2
        _ -> err
      TInt w1 sign1 -> case pt2 of
        TInt w2 sign2 -> TInt <$> addVarEq w1 w2
                              <*> addVarEq sign1 sign2
        TPointer w2 pointeeType1 -> do
          addConstraint $ Constraint (sign1, SType $ TVSign False)
          flip TPointer pointeeType1 <$> addVarEq w1 w2
        TChar -> do
          addConstraint $ Constraint (w1, SType $ TVBitWidth charSize)
          addConstraint $ Constraint (sign1, SType $ TVSign False)
          return TChar
        _ -> err

      TChar -> case pt2 of
        TChar -> return TChar
        _ -> err
      TFloat w1 -> case pt2 of
        TFloat w2 -> TFloat <$> addVarEq w1 w2
        _ -> err
      TBitVector w1 -> case pt2 of
        TBitVector w2 -> TBitVector <$> addVarEq w1 w2
        TFloat w2 -> TFloat <$> addVarEq w1 w2
        TInt w2 s -> TInt <$> addVarEq w1 w2 <*> pure s
        TChar -> do
          addConstraint $ Constraint (w1, SType $ TVBitWidth 8)
          return TChar
        _ -> err
      TPointer w1 pointeeType1 -> case pt2 of
        TPointer w2 pointeeType2 ->
          TPointer <$> addVarEq w1 w2
                   <*> addVarEq pointeeType1 pointeeType2
        _ -> err
      TFunction ret1 params1 -> err -- don't know how to unify at the moment...
      TRecord m1 -> case pt2 of
        TRecord m2 -> TRecord <$> mergeRecords m1 m2
        TPointer _ t -> fmap TRecord . mergeRecords m1 . HashMap.fromList $ [(0, t)]
        _ -> err

      TVBitWidth bw1 -> case pt2 of
        TVBitWidth bw2
          | bw1 == bw2 -> return $ TVBitWidth bw1
          | otherwise -> err
        _ -> err

      TVLength len1 -> case pt2 of
        TVLength len2
          | len1 == len2 -> return $ TVLength len1
          | otherwise -> err
        _ -> err

      TVSign s1 -> case pt2 of
        TVSign s2
          | s1 == s2 -> return $ TVSign s1
          | otherwise -> err
        _ -> err

      _ -> err

  where
    err = throwError $ IncompatibleTypes pt1 pt2
    


mergeRecords :: HashMap BitWidth Sym
             -> HashMap BitWidth Sym
             -> Unify (HashMap BitWidth Sym)
mergeRecords = undefined

