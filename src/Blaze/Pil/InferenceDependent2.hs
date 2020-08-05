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


--------------------------------------------------------------
------ Constraint generation phase ---------------------------

addVarSym :: PilVar -> Sym -> ConstraintGen ()
addVarSym pv sym' = varSymMap %= HashMap.insert pv sym'

-- | Create mapping of each PilVar to a symbol
createVarSymMap :: [Statement Expression] -> ConstraintGen ()
createVarSymMap stmts = do
  let vars = Analysis.getAllVars stmts
  mapM_ f $ HashSet.toList vars
  where
    f var = newSym >>= addVarSym var


incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1

-- | Creates new, unused Sym
newSym :: ConstraintGen Sym
newSym = do
  x <- use currentSym
  currentSym %= incrementSym
  return x

-- | get pilvar's cooresponding Sym from state
lookupVarSym :: PilVar -> ConstraintGen Sym
lookupVarSym pv = do
  vsm <- use varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> throwError $ CannotFindPilVarInVarSymMap pv
    Just s -> return s

lookupSymExpr :: Sym -> ConstraintGen SymExpression
lookupSymExpr sym' = do
  m <- use symMap
  case HashMap.lookup sym' m of
    Nothing -> throwError $ CannotFindSymInSymMap
    Just x -> return x

addSymExpression :: Sym -> SymExpression -> ConstraintGen ()
addSymExpression sym' x = symMap %= HashMap.insert sym' x

byteOffsetToBitWidth :: ByteOffset -> BitWidth
byteOffsetToBitWidth n
  -- For now, assume all offsets are positive.
  -- negative offsets will require thinking about how to deal with a type's size
  -- and how to check if nested records with negative fields conflict with
  -- the parent record's other fields.
  | n < 0 = P.error "Unexpected negative offset"
  | otherwise = fromIntegral $ n * 8


-- | Generates constraints for all syms in SymExpression.
exprTypeConstraints :: SymExpression -> ConstraintGen [(Sym, SymType)]
exprTypeConstraints = undefined
-- exprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
--   Pil.ADC x -> integralBinOpFirstArgIsReturn Nothing True x
--   Pil.ADD x -> integralBinOpFirstArgIsReturn Nothing True x

--   -- should this be unsigned ret because overflow is always positive?
--   Pil.ADD_OVERFLOW x -> integralBinOpFirstArgIsReturn Nothing True x

--   Pil.AND x -> bitVectorBinOp x

--   --   shift right...?
--   Pil.ASR x -> integralFirstArgIsReturn x

-- --   BOOL_TO_INT _ -> 

--   -- TODO get most general type for this and args:
--   Pil.CALL _ -> return [ (r, SType $ THasWidth sz') ]
  
--   Pil.CEIL x -> floatUnOp x
--   Pil.CMP_E x -> integralBinOpReturnsBool x
--   Pil.CMP_NE x -> integralBinOpReturnsBool x

--   Pil.CMP_SGE x -> signedBinOpReturnsBool True x
--   Pil.CMP_SGT x -> signedBinOpReturnsBool True x
--   Pil.CMP_SLE x -> signedBinOpReturnsBool True x
--   Pil.CMP_SLT x -> signedBinOpReturnsBool True x
--   Pil.CMP_UGE x -> signedBinOpReturnsBool False x
--   Pil.CMP_UGT x -> signedBinOpReturnsBool False x
--   Pil.CMP_ULE x -> signedBinOpReturnsBool False x
--   Pil.CMP_ULT x -> signedBinOpReturnsBool False x
--   Pil.CONST _ -> do
--     unknownSignednessSym <- newSym
--     return [(r, SType $ TInt sz' (SVar unknownSignednessSym))]
--   Pil.CONST_PTR _ -> retPointer
--   Pil.ConstStr x -> return [(r, SType $ TArray
--                                 ( SType . TVLength . fromIntegral . Text.length
--                                   $ x ^. Pil.value )
--                                 ( SType TChar ))]
--   Pil.DIVS x -> integralBinOpFirstArgIsReturn (Just True) False x
--   Pil.DIVS_DP x -> integralBinOpDP (Just True) x
--   Pil.DIVU x -> integralBinOpFirstArgIsReturn (Just False) False x
--   Pil.DIVU_DP x -> integralBinOpDP (Just False) x
--   Pil.FABS x -> floatUnOp x
--   Pil.FADD x -> floatBinOp x
--   Pil.FCMP_E x -> floatBinOpReturnsBool x
--   Pil.FCMP_GE x -> floatBinOpReturnsBool x
--   Pil.FCMP_GT x -> floatBinOpReturnsBool x
--   Pil.FCMP_LE x -> floatBinOpReturnsBool x
--   Pil.FCMP_LT x -> floatBinOpReturnsBool x
--   Pil.FCMP_O x -> floatBinOpReturnsBool x
--   Pil.FCMP_NE x -> floatBinOpReturnsBool x
--   Pil.FCMP_UO x -> floatBinOpReturnsBool x
--   Pil.FDIV x -> floatBinOp x

--   Pil.FIELD_ADDR x -> do
--     fieldType <- SVar <$> newSym
--     let recType = SType . TRecord . HashMap.fromList $
--           -- for now, assuming all offsets are positive...
--           [ (byteOffsetToBitWidth $ x ^. Pil.offset, fieldType) ]
--     return [ ( r, SType $ TPointer sz' fieldType )
--            , ( x ^. Pil.baseAddr . info . sym, SType $ TPointer sz' recType )
--            ]

--   Pil.FLOAT_CONST _ -> retFloat

-- -- TODO: should there be a link between sz of bitvec and sz of float?
--   Pil.FLOAT_CONV x -> do
--     bvWidth <- SVar <$> newSym
--     return [ ( x ^. Pil.src . info . sym, SType $ TBitVector bvWidth )
--            , ( r, SType $ TFloat sz' )
--            ]
--   Pil.FLOAT_TO_INT x -> floatToInt x
--   Pil.FLOOR x -> floatUnOp x
--   Pil.FMUL x -> floatBinOp x
--   Pil.FNEG x -> floatUnOp x
--   Pil.FSQRT x -> floatUnOp x
--   Pil.FTRUNC x -> floatUnOp x
--   Pil.FSUB x -> floatBinOp x

-- --   what does IMPORT do?
-- --   assuming it just casts an Int to a pointer
--   Pil.IMPORT _ -> retPointer

--   Pil.INT_TO_FLOAT x -> intToFloat x

--   Pil.LOAD x -> do
--     ptrWidth <- SVar <$> newSym
--     ptrType <- SVar <$> newSym
--     return [ ( x ^. Pil.src . info . sym, SType $ TPointer ptrWidth ptrType )
--            , ( r, ptrType )
--            , ( r, SType $ THasWidth sz' )
--            ]

--   -- should _x have any influence on the type of r?
--   Pil.LOW_PART _x -> return [(r, SType $ THasWidth sz')]

--   Pil.LSL x -> integralFirstArgIsReturn x
--   Pil.LSR x -> integralFirstArgIsReturn x
--   Pil.MODS x -> integralBinOpFirstArgIsReturn (Just True) False x
--   Pil.MODS_DP x -> integralBinOpDP (Just True) x
--   Pil.MODU x -> integralBinOpFirstArgIsReturn (Just False) False x
--   Pil.MODU_DP x -> integralBinOpDP (Just False) x
--   Pil.MUL x -> integralBinOpFirstArgIsReturn Nothing True x
--   Pil.MULS_DP x -> integralBinOpDP (Just True) x
--   Pil.MULU_DP x -> integralBinOpDP (Just False) x
--   Pil.NEG x -> integralUnOp (Just True) x
--   Pil.NOT x -> bitVectorUnOp x
--   Pil.OR x -> bitVectorBinOp x
--   Pil.RLC x -> integralFirstArgIsReturn x
--   Pil.ROL x -> integralFirstArgIsReturn x
--   Pil.ROR x -> integralFirstArgIsReturn x
--   Pil.ROUND_TO_INT x -> floatToInt x
--   Pil.RRC x -> integralFirstArgIsReturn x
--   Pil.SBB x -> signedBinOpReturnsBool True x
-- --   -- STORAGE _ -> unknown
-- --   StrCmp _ -> intRet
-- --   StrNCmp _ -> intRet
-- --   MemCmp _ -> intRet

--   -- should this somehow be linked to the type of the stack var?
--   -- its type could change every Store.
--   Pil.STACK_LOCAL_ADDR _ -> retPointer

--   Pil.SUB x -> integralBinOpFirstArgIsReturn (Just True) True x
--   Pil.SX x -> integralExtendOp x

-- --   TEST_BIT _ -> boolRet -- ? tests if bit in int is on or off
--   Pil.UNIMPL _ -> return [ (r, SType $ TBitVector sz' ) ]
--   Pil.UPDATE_VAR x -> do
--     v <- lookupVarSym $ x ^. Pil.dest
--     -- How should src and dest be related?
--     -- Can't express that `offset + width(src) == width(dest)`
--     --  without `+` and `==` as type level operators.
--     return [ (r, SVar v) ]

--   Pil.VAR x -> do
--     v <- lookupVarSym $ x ^. Pil.src
--     return [ (r, SVar v)
--            , (v, SType $ THasWidth sz')
--            ]
--   Pil.VAR_FIELD _ ->
--     -- TODO: can we know anything about src PilVar by looking at offset + result size?
--     return [ (r, SType $ TBitVector sz') ]

--   -- binja is wrong about the size of VarSplit.
--   Pil.VAR_SPLIT x -> do
--     low <- lookupVarSym $ x ^. Pil.low
--     high <- lookupVarSym $ x ^. Pil.high
--     return [ (r, SType $ TBitVector sz2x')
--            , (low, SType $ TBitVector sz')
--            , (high, SType $ TBitVector sz')
--            ]

--   Pil.XOR x -> bitVectorBinOp x
--   Pil.ZX x -> integralExtendOp x
-- --   -- _ -> unknown

-- --   Extract _ -> bitvecRet
--   _ -> P.error . show $ op'
--     --throwError UnhandledExpr
--   where
--     sz' = SType $ TVBitWidth sz
--     sz2x' = SType . TVBitWidth $ sz * 2
--     getBoolRet = SType . TInt sz' . SVar <$> newSym
  
--     retBool = do
--       b <- getBoolRet
--       return [(r, b)]

--     retFloat :: ConstraintGen [(Sym, SymType)]
--     retFloat = return [ (r, SType $ TFloat sz') ]

--     retPointer = do
--       pt <- newSym
--       return [ (r, SType (TPointer sz' (SVar pt))) ]

--     bitVectorUnOp :: (Pil.HasSrc x SymExpression) => x -> ConstraintGen [(Sym, SymType)]
--     bitVectorUnOp x =
--       return [ (r, SType $ TBitVector sz')
--              , (r, SVar $ x ^. Pil.src . info . sym)
--              ]

--     bitVectorBinOp :: ( Pil.HasLeft x SymExpression
--                       , Pil.HasRight x SymExpression)
--                    => x -> ConstraintGen [(Sym, SymType)]
--     bitVectorBinOp x =
--       return [ (r, SType $ TBitVector sz')
--              , (r, SVar $ x ^. Pil.left . info . sym)
--              , (r, SVar $ x ^. Pil.right . info . sym)
--              ]

--     integralExtendOp :: (Pil.HasSrc x SymExpression) => x -> ConstraintGen [(Sym, SymType)]
--     integralExtendOp x = do
--       argSizeType <- SVar <$> newSym
--       signednessType <- SVar <$> newSym
--       return [ (r, SType $ TInt sz' signednessType)
--              , (x ^. Pil.src . info . sym, SType $ TInt argSizeType signednessType)
--              ]


--     integralUnOp :: (Pil.HasSrc x SymExpression)
--                  => Maybe Bool
--                  -> x
--                  -> ConstraintGen [(Sym, SymType)]
--     integralUnOp mSignedness x = do
--       signednessType <- case mSignedness of
--         Nothing -> SVar <$> newSym
--         Just b -> return . SType . TVSign $ b
--       return [ (r, SType (TInt sz' signednessType))
--              , (r, SVar $ x ^. Pil.src . info . sym)
--              ]

--     integralBinOpFirstArgIsReturn :: ( Pil.HasLeft x SymExpression
--                                      , Pil.HasRight x SymExpression)
--                                   => Maybe Bool -> Bool -> x
--                                   -> ConstraintGen [(Sym, SymType)]
--     integralBinOpFirstArgIsReturn mSignedness secondArgSameWidth x = do
--       signednessType <- case mSignedness of
--         Nothing -> SVar <$> newSym
--         Just b -> return . SType . TVSign $ b
--       arg2Sign <- SVar <$> newSym
--       secondArgWidth <- bool (SVar <$> newSym) (return sz') secondArgSameWidth
--       return [ (r, SType (TInt sz' signednessType))
--              , (r, SVar $ x ^. Pil.left . info . sym)
--              , (x ^. Pil.right . info . sym, SType $ TInt secondArgWidth arg2Sign)
--              ]


--     integralBinOpUnrelatedArgs :: ( Pil.HasLeft x SymExpression
--                                   , Pil.HasRight x SymExpression)
--                                => Maybe Bool
--                                -> x
--                                -> ConstraintGen [(Sym, SymType)]
--     integralBinOpUnrelatedArgs mSignedness x = do
--       signednessType <- case mSignedness of
--         Nothing -> SVar <$> newSym
--         Just b -> return . SType . TVSign $ b
--       arg1Sign <- SVar <$> newSym
--       arg2Sign <- SVar <$> newSym
--       arg1Width <- SVar <$> newSym
--       arg2Width <- SVar <$> newSym
--       return [ (r, SType (TInt sz' signednessType))
--              , (x ^. Pil.left . info . sym, SType $ TInt arg1Width arg1Sign)
--              , (x ^. Pil.right . info . sym, SType $ TInt arg2Width arg2Sign)
--              ]

--     integralBinOp :: ( Pil.HasLeft x SymExpression
--                      , Pil.HasRight x SymExpression)
--                   => Maybe Bool
--                   -> x
--                   -> ConstraintGen [(Sym, SymType)]
--     integralBinOp mSignedness x = do
--       signednessType <- case mSignedness of
--         Nothing -> SVar <$> newSym
--         Just b -> return . SType . TVSign $ b
--       return [ (r, SType (TInt sz' signednessType))
--              , (r, SVar $ x ^. Pil.left . info . sym)
--              , (r, SVar $ x ^. Pil.right . info . sym)
--              ]

--     -- first arg is double-precision of second and return
--     -- signedness of args can apparently be anything
--     integralBinOpDP :: ( Pil.HasLeft x SymExpression
--                        , Pil.HasRight x SymExpression)
--                     => Maybe Bool
--                     -> x
--                     -> ConstraintGen [(Sym, SymType)]
--     integralBinOpDP mSignedness x = do
--       retSignednessType <- case mSignedness of
--         Nothing -> SVar <$> newSym
--         Just b -> return . SType . TVSign $ b
--       return [ (r, SType (TInt sz' retSignednessType))
--              , (x ^. Pil.left . info . sym, SType $ TInt sz2x' retSignednessType)
--              , (r, SVar $ x ^. Pil.right . info . sym)
--              ]


--     integralBinOpReturnsBool :: ( Pil.HasLeft x SymExpression
--                                 , Pil.HasRight x SymExpression ) 
--                              => x
--                              -> ConstraintGen [(Sym, SymType)]
--     integralBinOpReturnsBool x = do
--       b <- getBoolRet
--       argWidthSym <- newSym
--       argSignSym <- newSym
--       let argType = SType (TInt (SVar argWidthSym) (SVar argSignSym))
--       return [ ( r, b )
--              , ( x ^. Pil.left . info . sym, argType )
--              , ( x ^. Pil.right . info . sym, argType )
--              ]

--     signedBinOpReturnsBool :: ( Pil.HasLeft x SymExpression
--                               , Pil.HasRight x SymExpression )
--                            => Bool
--                            -> x
--                            -> ConstraintGen [(Sym, SymType)]
--     signedBinOpReturnsBool isSignedBool x = do
--       b <- getBoolRet
--       argWidthSym <- newSym
--       return [ (r, b)
--              , (x ^. Pil.left . info . sym, SType (TInt (SVar argWidthSym) (SType $ TVSign isSignedBool)))
--              , (x ^. Pil.right . info . sym, SType (TInt (SVar argWidthSym) (SType $ TVSign isSignedBool)))
--              ]

--     intToFloat :: (Pil.HasSrc x SymExpression)
--                => x
--                -> ConstraintGen [(Sym, SymType)]
--     intToFloat x = do
--       intWidth <- SVar <$> newSym
--       intSign <- SVar <$> newSym
--       return [ ( x ^. Pil.src . info . sym, SType $ TInt intWidth intSign )
--              , ( r, SType $ TFloat sz' )
--              ]


--     floatToInt :: (Pil.HasSrc x SymExpression)
--                => x
--                -> ConstraintGen [(Sym, SymType)]
--     floatToInt x = do
--       floatWidth <- SVar <$> newSym
--       intSign <- SVar <$> newSym
--       return [ ( x ^. Pil.src . info . sym, SType $ TFloat floatWidth )
--              , ( r, SType $ TInt sz' intSign )
--              ]

--     floatBinOp :: ( Pil.HasLeft x SymExpression
--                   , Pil.HasRight x SymExpression )
--                => x
--                -> ConstraintGen [(Sym, SymType)]
--     floatBinOp x = do
--       return [ (r, SType (TFloat sz'))
--              , (r, SVar $ x ^. Pil.left . info . sym)
--              , (r, SVar $ x ^. Pil.right . info . sym)
--              ]

--     floatUnOp :: (Pil.HasSrc x SymExpression)
--               => x
--               -> m [(Sym, SymType)]
--     floatUnOp x = do
--       return [ (r, SType (TFloat sz'))
--              , (r, SVar $ x ^. Pil.src . info . sym)
--              ]

--     floatBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
--                           => x
--                           -> ConstraintGen [(Sym, SymType)]
--     floatBinOpReturnsBool x = do
--       b <- getBoolRet
--       argWidthSym <- newSym
--       let argType = SType (TFloat $ SVar argWidthSym)
--       return [ ( r, b )
--              , ( x ^. Pil.left . info . sym, argType )
--              , ( x ^. Pil.right . info . sym, argType )
--              ]

--     integralFirstArgIsReturn :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
--                              => x
--                              -> ConstraintGen [(Sym, SymType)]
--     integralFirstArgIsReturn x = do
--       intWidth <- SVar <$> newSym
--       shifterWidth <- SVar <$> newSym
--       intSign <- SVar <$> newSym
--       shifterSign <- SVar <$> newSym
--       let n = SType $ TInt intWidth intSign
--       return [ ( x ^. Pil.left . info . sym, n )
--              , ( x ^. Pil.right . info . sym
--                , SType $ TInt shifterWidth shifterSign )
--              , ( r, n )
--              ]


--------------------------------------------------------------------
---------- unification and constraint solving ----------------------

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
        TArray len1 et2 -> TArray len1 <$> addVarEq pointeeType1 et2

        _ -> err
      TFunction ret1 params1 -> err -- don't know how to unify at the moment...
        -- need map of FuncArg(name,address,arg#/ret) -> most general type
        -- in state


      TRecord m1 -> case pt2 of
        TRecord m2 -> TRecord <$> unifyRecords m1 m2
        TPointer _ t -> fmap TRecord . unifyRecords m1 . HashMap.fromList $ [(0, t)]
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
    
flatToDeepSyms :: HashMap Sym (PilType Sym) -> HashMap Sym DeepSymType
flatToDeepSyms = undefined


-- | Merges field offset maps.
-- TODO: can't just constrain two syms at same offset to be equal and unify
-- because it might be something like [(0, Word64), (0, Word32)] which is maybe ok.
-- maybe there should be different types of constraints, like Contains,
-- or MostGeneral (for function args)
-- or just different list in state to keep record Field constraints.
unifyRecords :: HashMap BitWidth Sym
             -> HashMap BitWidth Sym
             -> Unify (HashMap BitWidth Sym)
unifyRecords a b = undefined

