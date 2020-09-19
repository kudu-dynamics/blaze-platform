{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.Unification where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Pil.Checker.OriginMap (addVarEq)
import Blaze.Types.Pil.Checker

--------------------------------------------------------------------
---------- unification and constraint solving ----------------------

-- | Swaps first key with second in map. 
updateSolKey :: (Hashable k, Eq k) => k -> k -> v -> HashMap k v -> HashMap k v
updateSolKey kOld kNew v m = HashMap.insert kNew v (HashMap.delete kOld m)

-- | Applies originMap substitutions to solution types.
substSolutions :: Unify ()
substSolutions = do
  omap <- use originMap
  solutions %= fmap (varSubst omap)


-- | Unifies constraint with all other constraints and solutions in state.
unifyConstraint :: Constraint -> Unify ()
unifyConstraint cx@(Constraint _ preSubstSym _preSubstType) = do
  omap <- use originMap
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
      sols <- use solutions
      case HashMap.lookup a sols of
        Nothing -> do

          -- why? is this only needed when preSubstSym == a?
          originMap %= HashMap.insert preSubstSym a

          -- BUG: some var eq gets added to vareqmap and solutions aren't updated
          -- when (a /= preSubstSym)
          --   $ solutions %= fmap (varSubst . HashMap.fromList $ [(preSubstSym, a)])

          solutions %= HashMap.insert a t
        Just t' -> do
          currentStmt .= i
          t'' <- catchError (unifyPilTypes t t') $ \err -> do
            errors %= (UnifyConstraintsError i a err :)
            return (TBottom a)
          solutions %= HashMap.insert a t''


-----------------------------------------

-- | True if second is at the same level or below in the type lattice
--   doesn't check recursive types or type sizes/signs.
--   Used to order args for unification
isTypeDescendent :: PilType a -> PilType a -> Bool
isTypeDescendent (TArray _ _) t = case t of
  TArray _ _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent TBool t = case t of
  TBool -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TInt _ _) t = case t of
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  -- TQueryChar -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TFloat _) t = case t of
  TFloat _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TBitVector _) t = case t of
  TBitVector _ -> True
  -- I think these should be descendents
  -- TFloat _ -> True
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  -- TQueryChar -> True
  TBool -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TPointer _ _) t = case t of
  TPointer _ _ -> True
  TArray _ _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent TChar t = case t of
  TChar -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TFunction _ _) t = case t of
  (TFunction _ _) -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TRecord _) t = case t of
  TRecord _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TContainsFirst _) t = case t of
  TContainsFirst _ -> True
  TRecord _ -> True
  TArray _ _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TBottom _) t = case t of
  TBottom _ -> True
  _ -> False
isTypeDescendent (TVBitWidth _) t = case t of
  TVBitWidth _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TVLength _) t = case t of
  TVLength _ -> True
  TBottom _ -> True
  _ -> False
isTypeDescendent (TVSign _) t = case t of
  TVSign _ -> True
  TBottom _ -> True
  _ -> False


unifyPilTypes :: PilType Sym -> PilType Sym -> Unify (PilType Sym)
-- if there are two TBottoms with different syms, they will be eq'd later in originsMap
unifyPilTypes (TBottom s) _ = return $ TBottom s 
unifyPilTypes _ (TBottom s) = return $ TBottom s
unifyPilTypes pt1 pt2 =
  case (isTypeDescendent pt1 pt2, isTypeDescendent pt2 pt1) of
    (False, False) -> err
    (False, True) -> unifyPilTypes pt2 pt1
    _ -> case pt1 of        
      TArray len1 et1 -> case pt2 of
        TArray len2 et2 ->
          TArray <$> addVarEq len1 len2 <*> addVarEq et1 et2
        _ -> err
      TBool -> case pt2 of
        TBool -> return TBool
        _ -> err
      TInt w1 sign1 -> case pt2 of
        TInt w2 sign2 -> TInt <$> addVarEq w1 w2
                              <*> addVarEq sign1 sign2
        TPointer w2 pointeeType1 -> do
          addConstraint_ sign1 . SType $ TVSign False
          flip TPointer pointeeType1 <$> addVarEq w1 w2
        TChar -> do
          addConstraint_ w1 . SType $ TVBitWidth charSize
          addConstraint_ sign1 . SType $ TVSign False
          return TChar
        -- TQueryChar -> do
        --   addConstraint_ w1 . SType $ TVBitWidth charSize
        --   addConstraint_ sign1 . SType $ TVSign False
        --   return TQueryChar

        _ -> err

      TChar -> case pt2 of
        TChar -> return TChar
        _ -> err

      -- TQueryChar -> case pt2 of
      --   TQueryChar -> return TQueryChar
      --   _ -> err
      
      TFloat w1 -> case pt2 of
        TFloat w2 -> TFloat <$> addVarEq w1 w2
        _ -> err
      TBitVector w1 -> case pt2 of
        TBitVector w2 -> TBitVector <$> addVarEq w1 w2
        TFloat w2 -> TFloat <$> addVarEq w1 w2
        TInt w2 s -> TInt <$> addVarEq w1 w2 <*> pure s
        TPointer w2 pt -> TPointer <$> addVarEq w1 w2 <*> pure pt

        TChar -> do
          addConstraint_ w1 . SType $ TVBitWidth 8
          return TChar

        -- TQueryChar -> do
        --   addConstraint_ w1 . SType $ TVBitWidth 8
        --   return TQueryChar


        TBool -> return TBool
        _ -> err
      TPointer w1 pointeeType1 -> case pt2 of
        TPointer w2 pointeeType2 ->
          TPointer <$> addVarEq w1 w2
                   <*> addVarEq pointeeType1 pointeeType2
        TArray len2 et2 -> TArray len2 <$> addVarEq pointeeType1 et2

        _ -> err
      TFunction _ret1 _params1 -> err -- don't know how to unify at the moment...
        -- need map of FuncArg(name,address,arg#/ret) -> most general type
        -- in state


      TRecord m1 -> case pt2 of
        TRecord m2 -> TRecord <$> unifyRecords m1 m2
        _ -> err

      TContainsFirst t1 -> case pt2 of
        TContainsFirst t2 -> TContainsFirst <$> addVarEq t1 t2
        TRecord m2 -> do
          let m1 = HashMap.fromList [(0, t1)]
          TRecord <$> unifyRecords m1 m2
        TArray len2 t2 -> TArray len2 <$> addVarEq t1 t2
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
        addConstraint_ s2 (SVar s)
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
