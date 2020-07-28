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
unifyConstraint cx = do
  omap <- use originMap
  case varSubst omap cx of
    (Constraint (a, (SVar b))) 
      | a == b -> return ()  -- redundant constraint
      | otherwise -> do
      c <- addVarEq a b
      let subMap = HashMap.fromList [(a, c), (b, c)]
      constraints %= fmap (varSubst subMap)
      solutions %= fmap (varSubst subMap) -- update syms in PilTypes
      -- now update solutions and constraints with subMap
      sols <- use solutions
      case (HashMap.lookup a sols, HashMap.lookup b sols) of
        (Nothing, Nothing) -> return ()
        (Just t, Nothing)
          | a == c -> return ()
          | otherwise -> solutions %= updateSolKey a c t
        (Nothing, Just t)
          | b == c -> return ()
          | otherwise -> solutions %= updateSolKey b c t

        -- in this case, both 'a' and 'b' have solutions.
        -- they must be unified, deleted and replaced with new solution at 'c'
        -- though really one of them is 'c' if the sols were constructed correctly
        (Just t1, Just t2) -> do
          t <- unifyPilTypes t1 t2
          solutions %= HashMap.insert c t . HashMap.delete a . HashMap.delete b

-- look up 'a' in solutions map and unify it with existing solution for 'a'
unifyConstraint (Constraint (a, (SType t))) = do
  sols <- use solutions
  case HashMap.lookup a sols of
    Nothing -> solutions %= HashMap.insert a t
    Just t' -> do
      t'' <- unifyPilTypes t t'
      solutions %= HashMap.insert a t''

        



-- unifyConstraint (Constraint (a, (SType b))) = do
--   sols <- use solutions
--   case HashMap.lookup a sols of
--     Nothing -> 
--   undefined

unifyPilTypes :: PilType Sym -> PilType Sym -> Unify (PilType Sym)
unifyPilTypes pt1 pt2 = return pt1 -- TODO 
