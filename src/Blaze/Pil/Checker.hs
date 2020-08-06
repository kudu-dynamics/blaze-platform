{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.InferenceDependent where

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
import Blaze.Types.Pil.Checker
import Blaze.Pil.Checker.Constraints ( createVarSymMap
                                     , stmtTypeConstraints
                                     )
import Blaze.Pil.Checker.Unification ( unify )
import Blaze.Pil.Checker.OriginMap ( originMapToGroupMap )

flatToDeepSyms :: HashMap Sym (PilType Sym) -> HashMap Sym DeepSymType
flatToDeepSyms m = fmap f m
  where
    f :: PilType Sym -> DeepSymType
    f = DSType . fmap g

    g :: Sym -> DeepSymType
    g s = maybe (DSVar s) f $ HashMap.lookup s m


unifyConstraints :: [Constraint] -> UnifyState
unifyConstraints cxs = snd $ runUnify unify initialState
  where
    initialState = UnifyState { _constraints = cxs
                              , _solutions = HashMap.empty
                              , _errors = []
                              , _originMap = HashMap.empty
                              }


-- for debugging...
stmtsConstraints :: [Statement Expression]
                 -> Either ConstraintGenError ( [Statement SymExpression]
                                              , [Constraint]
                                              , ConstraintGenState )
stmtsConstraints stmts = case er of
  Left err -> Left err
  Right (symStmts, cxs) -> Right (symStmts, cxs, s)
  where
    (er, s) = runConstraintGen_ $ do
      createVarSymMap stmts
      (symStmts, cxs) <- foldM getStmtConstraints ([], []) stmts
      return (symStmts, Constraint <$> cxs)
      
    getStmtConstraints :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> ConstraintGen ([Statement SymExpression], [(Sym, SymType)])
    getStmtConstraints (symStmts, cxs) stmt = do
      (sstmt, cxs') <- stmtTypeConstraints stmt
      return ( symStmts <> [sstmt] -- maybe should use a Vector
             , cxs' <> cxs)


stmtSolutions :: [Statement Expression]
              -> Either ConstraintGenError ( [Statement SymExpression]
                                           , ConstraintGenState
                                           , UnifyState
                                           )
stmtSolutions stmts = case er of
  Left err -> Left err
  Right (symStmts, cxs) -> Right (symStmts, gst, unifyConstraints cxs)
  where
    (er, gst) = runConstraintGen_ $ do
      createVarSymMap stmts
      (symStmts, cxs) <- foldM getStmtConstraints ([], []) stmts
      return (symStmts, Constraint <$> cxs)
      
    getStmtConstraints :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> ConstraintGen ([Statement SymExpression], [(Sym, SymType)])
    getStmtConstraints (symStmts, cxs) stmt = do
      (sstmt, cxs') <- stmtTypeConstraints stmt
      return ( symStmts <> [sstmt] -- maybe should use a Vector
             , cxs' <> cxs)

-- | main function to type check / infer statements
--   currently only returning types of pilvars in stmts for testing.
checkStmts :: [Statement Expression] -> Either ConstraintGenState TypeReport
checkStmts = fmap toReport . stmtSolutions
  where
    toReport :: ( [Statement SymExpression]
                , ConstraintGenState
                , UnifyState
                )
             -> TypeReport
    toReport (stmts, s, unSt) = TypeReport
      { _symTypeStmts = fmap (fmap fillTypesInStmt) stmts
      , _symStmts = stmts
      , _varSymMap = s ^. varSymMap
      , _varSymTypeMap = pilVarMap
      , _varEqMap = originMapToGroupMap eqMap
      , _errors = errs
      }
      where
        sols :: HashMap Sym (PilType Sym)
        sols = unSt ^. solutions
        errs = unSt ^. errors
        eqMap = unSt ^. originMap
        deepSols = flatToDeepSyms sols
        fillTypesInStmt :: InfoExpression SymInfo
                        -> InfoExpression (SymInfo, Maybe DeepSymType)
        fillTypesInStmt x = InfoExpression
          ( x ^. info
          , do
              originSym <- HashMap.lookup (x ^. info . sym) eqMap
              HashMap.lookup originSym deepSols
          )
          (fmap fillTypesInStmt $ x ^. op)

        pilVarMap :: HashMap PilVar DeepSymType
        pilVarMap = fmap f $ s ^. varSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols


