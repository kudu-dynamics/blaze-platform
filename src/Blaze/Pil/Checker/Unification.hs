{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.Unification where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Checker
    ( Constraint(Constraint),
      SymType(SType, SVar),
      PilType(TBottom, TBitVector, TFloat, TInt, TChar, TBool, TUnit,
              TPointer, TArray, TFunction, TRecord,
              TCString
             ),
      Sym,
      UnifyConstraintsError(UnifyConstraintsError),
      UnifyError(IncompatibleTypes, IncompatibleTypeLevelValues),
      UnifyState,
      VarSubst(varSubst),
      Unify,
      popConstraint,
      assignType,
      equals )
import Blaze.Pil.Analysis (addToOriginMap)

-- | Adds new var equality, returning the origin sym.
-- If the equality merges two groups, it picks the origin associated
-- with the second symbol and changes the origins of the first group to
-- the second origin. It also removes the solution associated with the
-- first origin and adds it as constraint to be later merged as solution.
addVarEq :: MonadState UnifyState m => Sym -> Sym -> m Sym
addVarEq a b = do
  m <- use #originMap
  let (v, mr, m') = addToOriginMap a b m
  case mr of
    Nothing -> return ()
    Just retiredSym -> do
      sols <- use #solutions
      case HashMap.lookup retiredSym sols of
        Nothing -> return ()
        Just rt -> do
          assignType retiredSym rt
          #solutions %= HashMap.delete retiredSym
  #originMap .= m'
  return v


--------------------------------------------------------------------
---------- unification and constraint solving ----------------------

-- | Swaps first key with second in map.
updateSolKey :: Hashable k => k -> k -> v -> HashMap k v -> HashMap k v
updateSolKey kOld kNew v m = HashMap.insert kNew v (HashMap.delete kOld m)

-- | Applies originMap substitutions to solution types.
substSolutions :: Unify ()
substSolutions = do
  omap <- use #originMap
  #solutions %= fmap (varSubst omap)


-- | Unifies constraint with all other constraints and solutions in state.
unifyConstraint :: Constraint -> Unify ()
unifyConstraint cx@(Constraint _ preSubstSym _preSubstType) = do
  omap <- use #originMap
  case varSubst omap cx of
    (Constraint _ a (SVar b))
      | a == b -> return ()  -- redundant constraint
      | otherwise -> void $ addVarEq a b

          -- Do we actually need to subst with updated omap until
          -- all constraints are unified?
          -- Note: tests pass with this commented out and substSolutions called after
          -- so it should probably get removed
          -- let subMap = HashMap.fromList [(a, c), (b, c)]
          -- solutions %= fmap (varSubst subMap)

    -- look up 'a' in solutions map and unify it with existing solution for 'a'
    (Constraint i a ((SType t))) -> do
      -- TODO: occurs check here? (error if 'a' is used in 't')
      -- maybe we don't need occurs check since it's flat, hence "lazy"
      -- if unification fails with infinite loop, look here...
      sols <- use #solutions
      case HashMap.lookup a sols of
        Nothing -> do

          -- why? is this only needed when preSubstSym == a?
          #originMap %= HashMap.insert preSubstSym a

          -- BUG: some var eq gets added to vareqmap and solutions aren't updated
          -- when (a /= preSubstSym)
          --   $ solutions %= fmap (varSubst . HashMap.fromList $ [(preSubstSym, a)])

          #solutions %= HashMap.insert a t
        Just t' -> do
          #currentStmt .= i
          t'' <- catchError (unifyPilTypes t t') $ \err -> do
            #errors %= (UnifyConstraintsError i a err :)
            return (TBottom a)
          #solutions %= HashMap.insert a t''


-----------------------------------------

-- | True if second is the same type or a subtype of the first.
--   Indirect subtypes (through transitivity) are explicitly
--   listed as type descendants.
--   Doesn't check recursive types or type sizes/signs.
--   Used to order args for unification
isTypeDescendant :: PilType a -> PilType a -> Bool
isTypeDescendant (TArray _ _) t = case t of
  TArray _ _ -> True
  TCString _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant TBool t = case t of
  TBool -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TInt _ _) t = case t of
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TFloat _) t = case t of
  TFloat _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TBitVector _) t = case t of
  TBitVector _ -> True
  TArray _ _ -> True
  TBool -> True
  TCString _ -> True
  TChar _ -> True
  TFloat _ -> True
  TInt _ _ -> True
  TPointer _ _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TPointer _ _) t = case t of
  TPointer _ _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TChar _) t = case t of
  TChar _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TFunction _ _) t = case t of
  (TFunction _ _) -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TRecord _) t = case t of
  TRecord _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant (TCString _) t = case t of
  TCString _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendant TUnit t = case t of
  TUnit -> True
  _ -> False
isTypeDescendant (TBottom _) t = case t of
  TBottom _ -> True
  _ -> False

-- | Unify type-level values according to the `checkVal` and `selectVal` functions
-- provided by the caller.
-- The values are assumed to be wrapped in a (`Maybe a`) and the `checkVal` predicate is
-- only run when the two values being unified are both `Just a`s.
unifyVal ::
  (a -> a -> Bool) ->
  (Maybe a -> Maybe a -> Maybe a) ->
  Maybe a ->
  Maybe a ->
  Either (UnifyError Sym) (Maybe a)
unifyVal checkVal selectVal v1 v2 =
  case (v1, v2) of
    (Just v1Val, Just v2Val) | not $ checkVal v1Val v2Val -> Left IncompatibleTypeLevelValues
    _ -> Right $ selectVal v1 v2

-- | This function is used to unify two types, provided as arguments. The first type `pt1`
-- is checked to be a super type for `pt2`. Note that the subtype relationship does permit
-- a type to be its own subtype (e.g., TChar _ <: TChar _), but the type-level values will
-- be considered before determing whether the types can be unified.
unifyPilTypes :: PilType Sym -> PilType Sym -> Unify (PilType Sym)
-- if there are two TBottoms with different syms, they will be eq'd later in originsMap
unifyPilTypes (TBottom s) _ = return $ TBottom s
unifyPilTypes _ (TBottom s) = return $ TBottom s
unifyPilTypes pt1 pt2 =
  case (isTypeDescendant pt1 pt2, isTypeDescendant pt2 pt1) of
    -- TODO: Add a case where if types aren't the same but both are descendants of
    --       one another then we report an error? Can we assert this with Haskell types
    --       instead and catch such a relationship at compile time?
    (False, False) -> err
    (False, True) -> unifyPilTypes pt2 pt1
    _ -> case pt1 of
      TUnit -> case pt2 of
        TUnit -> return TUnit
        _ -> err
      TArray len1 et1 -> case pt2 of
        TArray len2 et2 -> case unifyVal (==) (<|>) len1 len2 of
          Left _ -> err
          Right unifiedLen ->
            TArray unifiedLen <$> addVarEq et1 et2
        TCString len2 -> case unifyVal (==) (<|>) len1 len2 of
          Left _ -> err
          Right unifiedLen -> do
            -- Ensure this is an array of characters
            assignType et1 (TChar $ Just 8)
            -- TODO: Is it okay to assert through a symbol constraint and provide the
            --       unified type assuming the constraint holds?
            return $ TCString unifiedLen
        _ -> err
      TBool -> case pt2 of
        TBool -> return TBool
        _ -> err
      TInt w1 sign1 -> case pt2 of
        -- TODO: Is this an example where instead of assigning a concrete value
        --       or nothing we should use a symbolic constraint?
        TInt w2 sign2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> case unifyVal (==) (<|>) sign1 sign2 of
            Left _ -> err
            Right unifiedSign -> return $ TInt unifiedWidth unifiedSign
        TPointer w2 pointeeType1 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> case unifyVal (==) (<|>) sign1 (Just False) of
            Left _ -> err
            Right _unifiedSign -> return $ TPointer unifiedWidth pointeeType1
        TChar w2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          -- TODO: Here we assume a character is always unsigned. Be on the look
          --       out for ways this may break type checking that aren't actual
          --       errors.
          Right _unifiedWidth -> case unifyVal (==) (<|>) sign1 (Just False) of
            Left _ -> err
            Right _unifiedSign -> return $ TChar w2
        _ -> err

      TChar w1 -> case pt2 of
        TChar w2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TChar unifiedWidth
        _ -> err

      TFloat w1 -> case pt2 of
        TFloat w2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TFloat unifiedWidth
        _ -> err
      -- TODO: Should TBitVector unify with any other type as long as the width
      --       of the TBitVector is the same or larger than the width of the other type?
      -- TODO: Do we want to constraint the size of the TBitVector after it is unified
      --       with a more specific type? Is there anything missed by not doing this?
      TBitVector w1 -> case pt2 of
        TBitVector w2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TBitVector unifiedWidth
        TFloat w2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TFloat unifiedWidth
        TInt w2 s -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TInt unifiedWidth s
        TPointer w2 pt -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> return $ TPointer unifiedWidth pt
        TChar sign2 -> case unifyVal (==) (<|>) w1 (Just 8) of
          Left _ -> err
          Right _unifiedWidth -> return $ TChar sign2
        -- TODO: Is this right? Sometimes values of various widths are used as a bool and we're losing
        --       bitwidth information here. Maybe this is a use case for casts?
        TBool -> return TBool
        -- TODO: Use element size and length to match up with BitVector?
        TArray len2 elem2 -> do
          -- Set length and element type to be same
          -- TODO: Consider support unification of element type as well
          return $ TArray len2 elem2
        _ -> err

      TPointer w1 pointeeType1 -> case pt2 of
        TPointer w2 pointeeType2 -> case unifyVal (==) (<|>) w1 w2 of
          Left _ -> err
          Right unifiedWidth -> TPointer unifiedWidth <$> addVarEq pointeeType1 pointeeType2
        _ -> err

      TFunction _ret1 _params1 -> err -- don't know how to unify at the moment...
        -- need map of FuncArg(name,address,arg#/ret) -> most general type
        -- in state

      TRecord m1 -> case pt2 of
        TRecord m2 -> TRecord <$> unifyRecords m1 m2
        _ -> err

      TCString len1 -> case pt2 of
        TCString len2 -> case unifyVal (==) (<|>) len1 len2 of
          Left _ -> err
          Right unifiedLen -> return $ TCString unifiedLen
        _ -> err

      _ -> err

  where
    err = throwError $ IncompatibleTypes pt1 pt2


-- hopefully this will never get into an infinite loop
unify :: Unify ()
unify = unifyLoop >> substSolutions
  where
    unifyLoop = popConstraint >>= \case
      Nothing -> return ()
      Just cx -> unifyConstraint cx >> unifyLoop



-------------------------------------------------------------------
---------------- record unification -------------------------------

-- | Merges field offset maps.
-- TODO: can't just constrain two syms at same offset to be equal and unify
-- because it might be something like [(0, Word64), (0, Word32)] which is maybe ok.
-- maybe there should be different types of constraints, like Contains,
-- or MostGeneral (for function args)
-- or just different list in state to keep record Field constraints.

-- | Merges field offset maps. Currently just unifies fields at identical offsets
-- doesn't look for overlapping widths.
unifyRecords :: HashMap BitOffset Sym
             -> HashMap BitOffset Sym
             -> Unify (HashMap BitOffset Sym)
unifyRecords a = foldM f a . HashMap.toList
  where
    f :: HashMap BitOffset Sym -> (BitOffset, Sym) -> Unify (HashMap BitOffset Sym)
    f m (boff, s) = case HashMap.lookup boff m of
      Nothing -> return $ HashMap.insert boff s m
      Just s2 -> do
        equals s2 s
        return m

-- -- | given the fields in the hashmap, find the greatest (offset + known width)
-- --   This doesn't consider padding or error on overlapping fields.
-- getMinimumRecordWidth :: IsType a => HashMap BitWidth a -> BitWidth
-- getMinimumRecordWidth m = max maxOffset maxOffsetPlusKnownWidth
--   where
--     -- for if a field has a big offset, but an unkown width
--     maxOffset = foldr max 0 . HashMap.keys $ m
--     maxOffsetPlusKnownWidth = foldr max 0
--                               . mapMaybe fieldReach
--                               . HashMap.toList $ m
--     fieldReach (off, pt) = (+ off) <$> getTypeWidth pt


-- -- | if field has offset 32 and width 16, its range is (32, 48)
-- --   if field has offset 32, but unknown width, it's range is (32, 33) | --
-- getFieldRange :: IsType a => BitWidth -> a-> (BitWidth, BitWidth)
-- getFieldRange off = (off,) . (+off) . maybe 1 identity . getTypeWidth

-- getFieldRanges :: IsType a => HashMap BitWidth a -> [(BitWidth, BitWidth)]
-- getFieldRanges m = uncurry getFieldRange <$> HashMap.toList m

-- doFieldRangesOverlap :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
-- doFieldRangesOverlap (start, end) (start', end') =
--   start >= start' && start < end'
--   || end > start' && end <= end'
--   || start' >= start && start' < end
--   || end' > start && end' <= end

-- secondRangeContainsFirst :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
-- secondRangeContainsFirst (start, end) (start', end') =
--   start >= start' && start < end'
--   && end >= start' && end <= end'

-- -- | returns Nothing if there is a known conflict.
-- --   TODO: allow some overlapping fields, like an Int16 in an Int32 | --
-- addFieldToRecord :: ( MonadError UnifyError m
--                     , MonadState UnifyWithSubsState m
--                     )
--                  => HashMap BitWidth SymType
--                  -> BitWidth
--                  -> SymType
--                  -> m (HashMap BitWidth SymType)
-- addFieldToRecord m off pt = case HashMap.lookup off m of
--   Just pt' -> do
--     x <- unifyWithSubsM pt pt'
--     checkOverlap x $ HashMap.delete off m
--     return $ HashMap.insert off x m
--   Nothing -> do
--     checkOverlap pt m
--     return $ HashMap.insert off pt m
--   where
--     checkOverlap pt' m'
--       | (not . any (doFieldRangesOverlap $ getFieldRange off pt') . getFieldRanges $ m') = return ()
--       | otherwise = throwError $ OverlappingRecordField (HashMap.insert off pt' m') off

-- mergeRecords :: ( MonadError UnifyError m
--                 , MonadState UnifyWithSubsState m
--                 )
--              => HashMap BitWidth SymType
--              -> HashMap BitWidth SymType
--              -> m (HashMap BitWidth SymType)
-- mergeRecords m1 = foldM (uncurry . addFieldToRecord) m1 . HashMap.toList
