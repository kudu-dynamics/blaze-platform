{-|
The PIL type checker performs simultaneous type checking and type inference from
on PIL statements and their corresponding expressions.

The general flow of type checking is:
* Create symbols for each PIL expression
* Add type constraints on the symbols
  * Assign types (or supertypes) to expressions
  * Assign equivalence between type symbols
* Unify type constraints
* Unified types can then be used to:
  * Solve constraints in ICFGs
  * Show typed statements/expressions
  * Show type errors

***The following is aspirational and describes using a dependency graph approach
to typechecking.***

The major steps of type checking a list of PIL statements is to:
1. Add intial type information to symbols, equivalence constraints on symbols, and
   generate a flat type representation where any nested types (e.g., the 'pointeeType' of a 'TPointer')
   are represented as a 'Sym' and the current type definition can be looked up using a mapping of
   'Sym' to 'FlatPilType'.
   Construct a mapping of PIL statementes and expressions to symbols.
   Construct a mapping of equivalent symbols.
   Construct a mapping of symbols to type information.
1a. Parse every statement and expression and create symbols for every PIL expression and variable.
1b. Add symbol equivalence mappings while processing statements.
1c. Associate type information surmised by the current expression/statement with the corresponding symbol.
2. Canonicalize the symbols.
2a. Use the 'Sym' to 'Sym' mappings to identify sets of equivalent symbols.
2b. Select a canonical symbol for each set of equivalent symbols.
2c. Update the 'Sym' keys in the 'Sym' to 'FlatPilType' mapping to use canonical symbols.
2d. Update all 'Sym' values for 'FlatPilType' fields to use canonical symbols.
2e. Update all 'Sym' values in the 'SymInfo' of 'Statement SymExpression' values to use
    canonical symbls.
3. Perform unification for each symbol by considering all partial type information
3a. Unify pairs of partial type information by considering the subtype relationship
    and applying typing rules. Potentially record a type checking error.

Additionally, we may consider adding support for type-level value constraints to represent
relationships between lengths, bitwidths, signedness, and other type-level attributes.
-}

{- HLINT ignore "Use if" -}
module Blaze.Pil.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Types.Pil (Expression, Statement, PilVar, FuncVar)
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Blaze.Types.Pil.Checker hiding (stmtIndex)
import Blaze.Pil.Checker.Constraints (addStmtTypeConstraints)
import Blaze.Pil.Checker.Unification ( unify )
import Blaze.Pil.Analysis ( originMapToGroupMap )
import qualified Blaze.Pil.Analysis as Analysis


flatToDeepSyms :: HashMap Sym (PilType Sym) -> HashMap Sym DeepSymType
flatToDeepSyms flatSymHm = HashMap.mapWithKey (parseF HashSet.empty) flatSymHm
  where
    -- Lookup a sym. If there is no entry return a DSVar,
    -- if there is a PilType value than parse it and
    -- return DSType or DSRecursive
    substSym :: HashSet Sym -> Sym -> DeepSymType
    substSym hs s = case HashMap.lookup s flatSymHm of
      Nothing -> DSVar s
      Just v -> parseF hs s v
    substSymRecurse :: HashSet Sym -> Sym -> Sym -> DeepSymType
    substSymRecurse hs og s = case s == og of
      -- If two syms are the same, return a DSVar
      True -> DSVar s
      -- Syms are different
      False -> case HashMap.lookup s flatSymHm of
        -- No entry, return a DSVar
        Nothing -> DSVar s
        -- There is PilType value, go parse it
        Just v -> parseFRecurse hs s v
    -- A parseF that recurses into types?
    parseFRecurse :: HashSet Sym -> Sym -> PilType Sym -> DeepSymType
    parseFRecurse sSyms sym' ptS = case HashSet.member sym' sSyms of
      True -> DSVar sym'
      -- Symbol hasn't been seen, fmap over the PilType
      False -> DSType $ fmap (substSymRecurse (HashSet.insert sym' sSyms) sym') ptS
    parseF :: HashSet Sym -> Sym -> PilType Sym -> DeepSymType
    parseF sSyms sym' ptS = case HashSet.member sym' sSyms of
      -- Have already seen symbol, return DSVar
      True -> DSVar sym'
      -- TODO: bubble up seen syms of children with DeepSymType rather than
      --       calling subSyms at every level
      False -> case HashSet.member sym' $ subSyms HashSet.empty flatSymHm sym' of
        -- Have already seen symbol, use DSRecursive
        True -> DSRecursive sym' $ fmap (substSymRecurse (HashSet.insert sym' sSyms) sym') ptS
        -- Haven't seen symbol, so...???
        False -> DSType $ fmap (substSym $ HashSet.insert sym' sSyms) ptS

-- |
subSyms :: HashSet Sym -> HashMap Sym (PilType Sym) -> Sym -> HashSet Sym
subSyms handledSyms types symbol = case HashSet.member symbol handledSyms of
  True -> HashSet.singleton symbol
  False -> case HashMap.lookup symbol types of
    Nothing -> HashSet.empty
    Just v -> HashSet.unions . map (subSyms (HashSet.insert symbol handledSyms) types) $ foldr (:) [] v

unifyConstraints :: [Constraint] -> UnifyState
unifyConstraints cxs = snd $ runUnify unify initialState
  where
    initialState = UnifyState { constraints = cxs
                              , solutions = HashMap.empty
                              , errors = []
                              , originMap = HashMap.empty
                              , currentStmt = 0
                              }

-- | Adds constraints for all statements.
-- This is a good place to add constraints that require
-- access to all statements.
addAllConstraints ::
  [(Int, Statement Expression)] ->
  Either ConstraintGenError
    ( [Statement SymExpression],
      ConstraintGenState
    )
addAllConstraints indexedStmts =
  fmap (, gst) er
  where
    er :: Either ConstraintGenError [Statement SymExpression]
    gst :: ConstraintGenState
    (er, gst) = runConstraintGen_ $ do
      mapM
        ( \(i, s) -> do
            #currentStmt .= i
            addStmtTypeConstraints s
        )
        indexedStmts

-- | Attempt to find a unification solution to the provided statements.
stmtSolutions ::
  [(Int, Statement Expression)] ->
  Either ConstraintGenError
    ( [Statement SymExpression],
      ConstraintGenState,
      UnifyState
    )
stmtSolutions indexedStmts = do
  (symStmts', genState) <- addAllConstraints indexedStmts
  -- TODO: Why are the constraints reversed?
  let unifyState = unifyConstraints . reverse $ genState ^. #constraints
  -- Ensure all vars are resolved to the original/canonical/origin var
  return (fmap (varSubst $ unifyState ^. #originMap) symStmts',
          genState,
          unifyState)

-- | This is the main function to check / infer types for a collection of statements.
checkIndexedStmts :: [(Int, Statement Expression)] -> Either ConstraintGenError TypeReport
checkIndexedStmts indexedStmts = fmap toReport . stmtSolutions $ indexedStmts
  where
    toReport :: ( [Statement SymExpression]
                , ConstraintGenState
                , UnifyState
                )
             -> TypeReport
    toReport (stmts', s, unSt) = TypeReport
      { symTypedStmts = zip (fmap fst indexedStmts) $ fmap (fmap fillTypesInStmt) stmts'
      , symStmts = zip (fmap fst indexedStmts) stmts'
      , varSymMap = originsVarSymMap
      , varSymTypeMap = pilVarMap
      , varEqMap = originMapToGroupMap eqMap
      , funcSymTypeMap = funcVarMap
      , funcSymMap = originsFuncVarSymMap
      , errors = errs
      , flatSolutions = sols
      , solutions = deepSols
      , originMap = unSt ^. #originMap
      , errorConstraints = errorConstraints'
      , ogConstraints = s ^. #constraints
      }
      where
        errorConstraints' :: HashMap Sym [Constraint]
        errorConstraints' = HashMap.fromList
          . fmap ( toSnd
                   (getConstraintsForSym (unSt ^. #originMap) (s ^. #constraints))
                   . view #sym
                 )
          $ errs
        originsVarSymMap :: HashMap PilVar Sym
        originsVarSymMap = varSubst eqMap <$> s ^. #varSymMap
        sols :: HashMap Sym (PilType Sym)
        sols = unSt ^. #solutions
        errs :: [UnifyConstraintsError DeepSymType]
        errs = fmap f <$> unSt ^. #errors
          where
            f s' = maybe (DSVar s') identity $ HashMap.lookup s' deepSols
        eqMap :: HashMap Sym Sym
        eqMap = unSt ^. #originMap
        deepSols :: HashMap Sym DeepSymType
        deepSols = flatToDeepSyms sols
        fillTypesInStmt :: InfoExpression SymInfo
                        -> InfoExpression (SymInfo, Maybe DeepSymType)
        fillTypesInStmt x = InfoExpression
          ( x ^. #info
          , do
              originSym <- HashMap.lookup (x ^. #info . #sym) eqMap
              HashMap.lookup originSym deepSols
          )
          (fmap fillTypesInStmt $ x ^. #op)
        pilVarMap :: HashMap PilVar DeepSymType
        pilVarMap = fmap f originsVarSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols
        originsFuncVarSymMap :: HashMap (FuncVar SymExpression) Sym
        originsFuncVarSymMap = varSubst eqMap <$> s ^. #funcSymMap
        funcVarMap :: HashMap (FuncVar SymExpression) DeepSymType
        funcVarMap = fmap f originsFuncVarSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols

checkStmts :: [Statement Expression] -> Either ConstraintGenError TypeReport
checkStmts = checkIndexedStmts . zip [0..]

removeUnusedPhi :: [(Int, Pil.Stmt)] -> [(Int, Pil.Stmt)]
removeUnusedPhi stmts' = filter (not . Analysis.isUnusedPhi refs . view _2) stmts'
  where
    refs = Analysis.getRefVars . fmap snd $ stmts'

pilTypeWithSym :: HashMap Sym Sym -> Sym -> PilType Sym -> Maybe (PilType Sym)
pilTypeWithSym originMap' s pt = do
  sorigin <- HashMap.lookup s originMap' <|> return s
  subst sorigin
  where
    f :: Sym -> Sym -> (Bool, Sym)
    f sorigin x = maybe (False, x) (True,) $ do
      xorigin <- HashMap.lookup x originMap'
      if xorigin == sorigin
        then Just xorigin
        else Nothing

    ptWithBool :: Sym -> PilType (Bool, Sym)
    ptWithBool sorigin = fmap (f sorigin) pt

    subst :: Sym -> Maybe (PilType Sym)
    subst sorigin = if or . fmap fst $ ptWithBool sorigin
      then return . fmap snd $ ptWithBool sorigin
      else Nothing


symTypeWithSym :: HashMap Sym Sym -> Sym -> SymType -> Maybe SymType
symTypeWithSym originMap' s stype = case stype of
  SVar s' -> case originMatch s s' of
    Nothing -> Nothing
    Just _ -> return stype
  SType pt -> SType <$> pilTypeWithSym originMap' s pt
  where
    originMatch a b = do
      aOrigin <- HashMap.lookup a originMap'
      bOrigin <- HashMap.lookup b originMap'
      if aOrigin == bOrigin then Just aOrigin else Nothing

-- | Returns a constraint that contains the given symbol,
-- or that contains a symbol determined to be equivalent during unification
constraintWithSym :: HashMap Sym Sym -> Sym -> Constraint -> Maybe Constraint
constraintWithSym originMap' s c@(Constraint stmtIndex s' stype)
  | s == s' = return c
  | HashMap.lookup s originMap' == HashMap.lookup s' originMap' =
      return $ Constraint stmtIndex s stype
  | otherwise = Constraint stmtIndex s' <$> symTypeWithSym originMap' s stype

getConstraintsForSym
  :: HashMap Sym Sym
  -> [Constraint]
  -> Sym
  -> [Constraint]
getConstraintsForSym originMap' xs s = mapMaybe (constraintWithSym originMap' s) xs

-- TODO: Consider introducing an IndexedStmt type to avoid the awkwardness
--       below where we split a [(Int, Stmt)] to process all [Stmt] and then
--       reassemble.

-- | Get the size of a type in bits.
bitSize :: PilType Sym -> Maybe BitWidth
bitSize = \case
  TBool -> Nothing
  TChar _s -> Just charSize
  TInt bw _s -> bw
  TFloat bw -> bw
  TBitVector bw -> bw
  TPointer bw _pt -> bw
  TUnit -> Nothing
  TCString l -> (charSize *) . fromIntegral <$> l
  TArray _ _ -> Nothing
  TRecord _ -> Nothing
  TBottom _ -> Nothing
  TFunction _ _ -> Nothing
