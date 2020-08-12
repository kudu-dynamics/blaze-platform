{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Types.Pil ( Expression
                       , Statement
                       , PilVar
                       )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Checker
import Blaze.Pil.Checker.Constraints ( createVarSymMap
                                     , addStmtTypeConstraints
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
                                              , ConstraintGenState )
stmtsConstraints stmts' = case er of
  Left err -> Left err
  Right symStmts' -> Right (symStmts', s)
  where
    (er, s) = runConstraintGen_ $ do
      createVarSymMap stmts'
      mapM addStmtTypeConstraints stmts'


stmtSolutions :: [Statement Expression]
              -> Either ConstraintGenError ( [Statement SymExpression]
                                           , ConstraintGenState
                                           , UnifyState
                                           )
stmtSolutions stmts' = case er of
  Left err -> Left err
  Right symStmts' -> Right ( fmap (varSubst $ ust ^. originMap) symStmts'
                           , gst
                           , ust)
  where
    ust = unifyConstraints cxs
    cxs = gst ^. constraints
    (er, gst) = runConstraintGen_ $ do
      createVarSymMap stmts'
      mapM addStmtTypeConstraints stmts'

-- | main function to type check / infer statements
--   currently only returning types of pilvars in stmts for testing.
checkStmts :: [Statement Expression] -> Either ConstraintGenError TypeReport
checkStmts = fmap toReport . stmtSolutions
  where
    toReport :: ( [Statement SymExpression]
                , ConstraintGenState
                , UnifyState
                )
             -> TypeReport
    toReport (stmts', s, unSt) = TypeReport
      --TODO: make sure EVERY sym var in all of these is an ORIGIN sym var
      -- todo: symTypeStmts, symStmts, errs
      -- done: varSymTypeMap, varSymMap
      -- NA: varEqMap (map of origins to equals); probably can remove
      --     if everything else is an origin var.
      { _symTypeStmts = fmap (fmap fillTypesInStmt) stmts'
      , _symStmts = stmts'
      , _varSymMap = originsVarSymMap
      , _varSymTypeMap = pilVarMap
      , _varEqMap = originMapToGroupMap eqMap
      , _errors = errs
      }
      where
        originsVarSymMap = varSubst eqMap <$> s ^. varSymMap
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
        pilVarMap = fmap f originsVarSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols


