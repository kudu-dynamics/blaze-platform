{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.Inference2 where

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

type BitWidth = Bits
type ByteWidth = Bytes

data PilType t = TAny
               | TStream { elemType :: t }
               | TArray { len :: Word64, elemType :: t }
               | TChar
               | TNumber
               | TInteger
               | TInt { intWidth :: BitWidth }
               | TSigned { intWidth :: BitWidth }
               | TUnsigned { intWidth :: BitWidth } 
               | TReal
               | TFloat { bitWidth :: BitWidth }
               | TBitVector { bitWidth :: BitWidth }
               | TPointer { bitWidth :: BitWidth, pointeeType :: t }
               | TRecord (HashMap BitWidth t)
               | TBottom
               | TFunction { ret :: t, params :: [t] }
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data T = T (PilType T)
  deriving (Eq, Ord, Read, Show)

unT :: T -> PilType T
unT (T pt) = pt

-- data RecordField = RecordField { offset :: Word64, fieldType :: PilType }
--   deriving (Eq, Ord, Read, Show)

-- data TFunctionType = TFunctionType
--   { _name :: Text
--   , _ret :: PilType
--   , _params :: [PilType]
--   } deriving (Eq, Ord, Show, Read)

-- $(makeFieldsNoPrefix ''TFunctionType)


newtype Sym = Sym Int
            deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Sym

data SymType = SVar Sym
             | SType (PilType SymType)
             deriving (Eq, Ord, Read, Show, Generic)

data CheckerError = CannotFindPilVarInVarSymMap PilVar
                  | CannotFindSymInSymMap
                  | UnhandledExpr
                  | UnhandledStmt
  deriving (Eq, Ord, Show)

incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1


data InfoExpression a = InfoExpression
  { _info :: a
  , _op :: ExprOp (InfoExpression a)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

$(makeFieldsNoPrefix ''InfoExpression)

data SymInfo = SymInfo
  { _size :: BitWidth
  , _sym :: Sym
  } deriving (Eq, Ord, Show, Generic)

$(makeFieldsNoPrefix ''SymInfo)


type SymExpression = InfoExpression SymInfo

type SymTypeExpression = InfoExpression SymType

type TypedExpression = InfoExpression (PilType T)

data TypeReport = TypeReport
  { _symTypeStmts :: [Statement SymTypeExpression]
--  , _typedStmts :: [Statement TypedExpression]
  , _varSymTypeMap :: HashMap PilVar SymType
--  , _varTypeMap :: HashMap PilVar (PilType T)
--  , _unresolvedStmts :: [Statement SymExpression]
  -- , _unresolvedSyms :: [(Sym, Sym)]
  -- , _unresolvedTypes :: [(Sym, PilType SymType, PilType SymType)]
  } deriving (Eq, Ord, Show, Generic)

$(makeFieldsNoPrefix ''TypeReport)


data CheckerState = CheckerState
  { _currentSym :: Sym
  , _symMap :: HashMap Sym SymExpression
  , _varSymMap :: HashMap PilVar Sym
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''CheckerState)

emptyCheckerState :: CheckerState
emptyCheckerState = CheckerState (Sym 0) HashMap.empty HashMap.empty

newtype Checker a = Checker
  { _runChecker :: ExceptT CheckerError (StateT CheckerState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError CheckerError
           , MonadState CheckerState
           )

runChecker :: Checker a -> CheckerState -> (Either CheckerError a, CheckerState)
runChecker m ss = runIdentity . flip runStateT ss . runExceptT . _runChecker $ m

runChecker_ :: Checker a -> (Either CheckerError a, CheckerState)
runChecker_ m = runChecker m emptyCheckerState


lookupVarSym :: (MonadState CheckerState m, MonadError CheckerError m)
             => PilVar -> m Sym
lookupVarSym pv = do
  vsm <- use varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> throwError $ CannotFindPilVarInVarSymMap pv
    Just s -> return s

lookupSymExpr :: (MonadState CheckerState m, MonadError CheckerError m)
             => Sym -> m SymExpression
lookupSymExpr sym = do
  m <- use symMap
  case HashMap.lookup sym m of
    Nothing -> throwError $ CannotFindSymInSymMap
    Just x -> return x


addSymExpression :: MonadState CheckerState m => Sym -> SymExpression -> m ()
addSymExpression sym x = symMap %= HashMap.insert sym x

addVarSym :: MonadState CheckerState m => PilVar -> Sym -> m ()
addVarSym pv sym = varSymMap %= HashMap.insert pv sym

-- |Create mapping of each PilVar to a symbol| --
createVarSymMap :: MonadState CheckerState m => [Statement Expression] -> m ()
createVarSymMap stmts = do
  let vars = Analysis.getAllVars stmts
  mapM_ f $ HashSet.toList vars
  where
    f var = newSym >>= addVarSym var

newSym :: MonadState CheckerState m => m Sym
newSym = do
  x <- use currentSym
  currentSym %= incrementSym
  return x

toSymExpression :: MonadState CheckerState m => Expression -> m SymExpression
toSymExpression (Expression sz op) = do
  symOp <- traverse toSymExpression op
  s <- newSym
  let bitSize = fromIntegral sz * 8
      sexpr = InfoExpression (SymInfo bitSize s) symOp
  addSymExpression s sexpr
  return sexpr


-- toSymExpression :: MonadState CheckerState m => Expression -> m Sym
-- toSymExpression (Expression sz op) = do
--   symOp <- traverse toSymExpression op
--   let bitSize = fromIntegral sz * 8
--       sexpr = SymExpression bitSize symOp
--   s <- newSym
--   addSymExpression s sexpr
--   return s

exprTypeRules :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
              => SymExpression -> m [(Sym, SymType)]
exprTypeRules (InfoExpression (SymInfo sz r) op) = case op of
--   ADC n -> inheritIntRet n
  Pil.ADD x -> integralBinOp x
--   ADD_OVERFLOW n -> inheritIntRet n
--   ADDRESS_OF _ -> pointerRet
--   ADDRESS_OF_FIELD _ -> pointerRet
--   AND _ -> bitvecRet
--   ASR _ -> bitvecRet
--   BOOL_TO_INT _ -> uintRet
--   CALL _ -> unknown
--   CEIL _ -> bitvecRet
  Pil.CMP_E x -> integralBinOpReturnsBool x
  Pil.CMP_NE x -> integralBinOpReturnsBool x

  -- CMP_SGE _ -> signedBinOpReturnsBool x
--   CMP_SGT _ -> boolRet
--   CMP_SLE _ -> boolRet
--   CMP_SLT _ -> boolRet
--   CMP_UGE _ -> boolRet
--   CMP_UGT _ -> boolRet
--   CMP_ULE _ -> boolRet
--   CMP_ULT _ -> boolRet
  Pil.CONST _ -> return [(r, SType $ TInt sz)]
--   CONST_PTR _ -> pointerRet
--   ConstStr _ -> stringRet
--   DIVS _ -> intRet
--   DIVS_DP _ -> intRet
--   DIVU _ -> uintRet
--   DIVU_DP _ -> uintRet
--   FABS _ -> floatRet
--   FADD _ -> floatRet
--   FCMP_E _ -> boolRet
--   FCMP_GE _ -> boolRet
--   FCMP_GT _ -> boolRet
--   FCMP_LE _ -> boolRet
--   FCMP_LT _ -> boolRet
--   FCMP_O _ -> boolRet
--   FCMP_NE _ -> boolRet
--   FCMP_UO _ -> boolRet
--   FDIV _ -> floatRet
--   FLOAT_CONST _ -> floatRet
--   FLOAT_CONV _ -> floatRet
--   FLOAT_TO_INT _ -> intRet
--   FLOOR _ -> bitvecRet
--   FMUL _ -> floatRet
--   FNEG _ -> floatRet
--   FSQRT _ -> floatRet
--   FTRUNC _ -> floatRet
--   FSUB _ -> floatRet
--   IMPORT _ -> bitvecRet
--   INT_TO_FLOAT _ -> floatRet
--   LOAD _ -> bitvecRet
--   -- LOAD_STRUCT _ -> bitvecRet
--   LOW_PART _ -> bitvecRet
--   LSL _ -> bitvecRet
--   LSR _ -> bitvecRet
--   MODS _ -> intRet
--   MODS_DP _ -> intRet
--   MODU _ -> uintRet
--   MODU_DP _ -> uintRet
--   MUL n -> inheritIntRet n
--   MULS_DP _ -> intRet
--   MULU_DP _ -> uintRet
--   NEG _ -> bitvecRet
--   NOT _ -> boolRet
--   OR _ -> bitvecRet
--   RLC _ -> bitvecRet
--   ROL _ -> bitvecRet
--   ROR _ -> bitvecRet
--   ROUND_TO_INT _ -> intRet
--   RRC _ -> bitvecRet
--   SBB n -> inheritIntRet n
--   -- STORAGE _ -> unknown
--   StrCmp _ -> intRet
--   StrNCmp _ -> intRet
--   MemCmp _ -> intRet
--   SUB n -> inheritIntRet n
--   SX n -> inheritIntUnary $ n ^. src
--   TEST_BIT _ -> boolRet -- ? tests if bit in int is on or off
--   UNIMPL _ -> bitvecRet -- should this be unknown?
  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. Pil.src
    return [(r, SVar v)]
--   VAR_ALIASED _ -> bitvecRet
--   VAR_ALIASED_FIELD _ -> bitvecRet
--   VAR_FIELD _ -> bitvecRet
--   VAR_SPLIT _ -> bitvecRet
--   XOR _ -> bitvecRet
--   ZX n -> inheritIntUnary $ n ^. src
--   -- _ -> unknown

--   -- the following were missing from the Clojure implementation
--   -- i think because the _SSA version got renamed and replaced the nonssa
--   -- LOAD_SSA _ -> bitvecRet
--   -- LOAD_STRUCT_SSA _ -> bitvecRet
--   VAR_PHI _ -> unknown -- should be removed by analysis

--   Extract _ -> bitvecRet
  _ -> throwError UnhandledExpr
  where

    integralBinOp :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => x -> m [(Sym, SymType)]
    integralBinOp x = return
      [ (r, SType (TInt sz))
      , (r, SVar $ x ^. Pil.left . info . sym)
      , (r, SVar $ x ^. Pil.right . info . sym)
      ]

    integralBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                             => x -> m [(Sym, SymType)]
    integralBinOpReturnsBool x = return
      [ (r, SType (TInt sz))
      , (x ^. Pil.left . info . sym, SVar $ x ^. Pil.right . info . sym)
      ]

  -- TODO: need to be able to return attributes,
  -- specificially that the type is signed, with width unknown.
    -- signedBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
    --                          => x -> m [(Sym, SymType)]
    -- signedBinOpReturnsBool x = return
    --   [ (r, SType (TInt sz))
    --   , (x ^. Pil.left . info . sym, SType 
    --   , (x ^. Pil.left . info . sym, SVar $ x ^. Pil.right . info . sym)
    --   ]

getAllExprTypeRules :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
                    => SymExpression -> m [(Sym, SymType)]
getAllExprTypeRules x@(InfoExpression (SymInfo _ thisExprSym) op') = do
  rulesForThisExpr <- exprTypeRules x
  rulesForChildren <- foldM f  [] op'
  return $ rulesForThisExpr <> rulesForChildren
  where
    f :: [(Sym, SymType)] -> SymExpression -> m [(Sym, SymType)]
    f rules sexpr = (<> rules) <$> getAllExprTypeRules sexpr

-- get all rules for stmts
-- preserve [Statement SymExpression]
-- needs to get all rules and all Stmts
stmtTypeRules :: (MonadState CheckerState m, MonadError CheckerError m)
              => Statement Expression -> m (Statement SymExpression, [(Sym, SymType)])
stmtTypeRules (Pil.Def (Pil.DefOp pv expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. info . sym
  pvSym <- lookupVarSym pv
  exprRules <- getAllExprTypeRules symExpr
  return ( Pil.Def (Pil.DefOp pv symExpr)
         , [ (pvSym, SVar exprSym) ]
           <> exprRules )
stmtTypeRules (Pil.Constraint (Pil.ConstraintOp expr)) = do
  symExpr <- toSymExpression expr
  exprRules <- getAllExprTypeRules symExpr
  return ( Pil.Constraint (Pil.ConstraintOp symExpr)
         , exprRules )
stmtTypeRules _ = throwError UnhandledStmt

type Rule = (Sym, SymType)

type StmtRule = (Statement Sym, Rule)

--toTypedExpression :: SymExpression -> TypedExpression

-- symStatementToTypedStatement :: Statement SymExpression -> Statement TypedExpression
-- symStatementToTypedStatement 


-----------------------------
-- need to preserve type for every Sym

-- wandUnification :: [(Sym, SymType)] -> [(Sym, SymType)]
-- wandUnification constraints' = wand constraints' []

-- | if second SymType is SVar equal to first Sym, replaces with first SymType |--
subst :: (Sym, SymType) -> (Sym, SymType) -> (Sym, SymType)
subst (sym, st) x@(sym', SVar v)
  | v == sym = (sym', st)
  | otherwise = x
subst _ x = x


-- unification' :: [(Sym, SymType)] -> [(Sym, SymType)] -> [(Sym, SymType)]
-- unification' [] solutions = solutions
-- unification' constraints solutions =

unifyRules :: [(Sym, SymType)] -> [(Sym, SymType)]
unifyRules xs = undefined

unifyRules' :: [(Sym, SymType)] -> [(Sym, SymType)]
unifyRules' xs' = f xs' []
  where
    f :: [(Sym, SymType)] -> [(Sym, SymType)] -> [(Sym, SymType)]
    f [] xs = xs
    f (x:xs) ys = f xs ((subst x <$> ys) <> [x]) -- should use a Vector

-- | if a symbol type cannot be inferred, it will be in the [(Sym, Sym)] | --
splitSVarsAndSTypes :: [(Sym, SymType)] -> ([(Sym, Sym)], [(Sym, PilType SymType)])
splitSVarsAndSTypes xs = (mapMaybe getSVar xs, mapMaybe getSType xs)
  where
    getSVar :: (Sym, SymType) -> Maybe (Sym, Sym)
    getSVar (sym, SVar x) = Just (sym, x)
    getSVar _ = Nothing

    getSType :: (Sym, SymType) -> Maybe (Sym, PilType SymType)
    getSType (sym, SType x) = Just (sym, x)
    getSType _ = Nothing

-- | fst result is list of syms that can't unify, second is inferred type map | --
-- unifyPilTypes :: [(Sym, PilType)] -> ([(Sym, PilType, PilType)], HashMap Sym PilType)
-- unifyPilTypes = foldr f ([], HashMap.empty)
--   where
--     f :: (Sym, PilType) -> ([(Sym, PilType, PilType)], HashMap Sym PilType) -> ([(Sym, PilType, PilType)], HashMap Sym PilType)
--     f (sym, pt) (conflicts, m) = case HashMap.lookup sym m of
--       Just pt' -> case mostSpecificUnifyType pt pt' of
--         Nothing -> ((sym, pt, pt'):conflicts, m)
--         Just msuPt -> (conflicts, HashMap.insert sym msuPt m)
--       Nothing -> (conflicts, HashMap.insert sym pt m)

-- | replaces Sym's with expressions, fails if Sym missing in map |--
symInfoToPilType :: HashMap Sym (PilType a) -> SymInfo -> Maybe (PilType a)
symInfoToPilType m si = HashMap.lookup (si ^. sym) m

-- | replaces Sym's with expressions, fails if Sym missing in map |--
toTypedExpression :: HashMap Sym (PilType T) -> SymExpression -> Maybe TypedExpression
toTypedExpression m = traverse (symInfoToPilType m)

-- unifyStmts :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
--               => [Statement Expression] -> m [StmtRule]
-- unifyStmts = concatMapM f
--   where
--     f stmt = do




------------------- unification --------------

charSize :: BitWidth
charSize = 8

class IsType a where
  isSubTypeOf :: a -> a -> Bool
  getTypeWidth :: a -> Maybe BitWidth

instance IsType T where
  isSubTypeOf (T pt1) (T pt2) = isSubTypeOf pt1 pt2
  getTypeWidth (T pt) = getTypeWidth pt

instance IsType SymType where
  isSubTypeOf (SVar _) _ = True -- ???
  isSubTypeOf _ (SVar _) = False -- ??? maybe an SVar is like a TAny?
  isSubTypeOf (SType pt1) (SType pt2) = isSubTypeOf pt1 pt2

  getTypeWidth (SVar _) = Nothing
  getTypeWidth (SType pt) = getTypeWidth pt


-- | True if second is at the same level or below in the type lattice
--   doesn't check recursive types | --
isTypeDescendent :: PilType a -> PilType a -> Bool
isTypeDescendent TAny _ = True
isTypeDescendent (TStream _) t = case t of
  TStream _ -> True
  TArray _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TArray _ _) t = case t of
  TArray _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TNumber t = case t of
  TNumber -> True
  TInteger -> True
  TInt _ -> True
  TSigned _ -> True
  TUnsigned _ -> True
  TPointer _ _ -> True
  TReal -> True
  TFloat _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent TInteger t = case t of
  TInteger -> True
  TInt _ -> True
  TSigned _ -> True
  TUnsigned _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TInt _) t = case t of
  TInt _ -> True
  TSigned _ -> True
  TUnsigned _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TUnsigned _) t = case t of
  TUnsigned _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TSigned _) t = case t of
  TSigned _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TReal t = case t of
  TReal -> True
  TFloat _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TFloat _) t = case t of
  TFloat _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TBitVector _) t = case t of
  TBitVector _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TPointer _ _) t = case t of
  TPointer _ _ -> True
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
  TBottom -> True
  _ -> False
isTypeDescendent TBottom t = case t of
  TBottom -> True
  _ -> False

instance IsType a => IsType (PilType a) where
  getTypeWidth (TArray len' et) = (* (fromIntegral len')) <$> getTypeWidth et
  getTypeWidth TChar = Just 8
  getTypeWidth (TInt w) = Just w
  getTypeWidth (TSigned w) = Just w
  getTypeWidth (TUnsigned w) = Just w
  getTypeWidth (TFloat w) = Just w
  getTypeWidth (TBitVector w) = Just w
  getTypeWidth (TPointer w _) = Just w
  getTypeWidth (TRecord m) = Just $ getMinimumRecordWidth m
  getTypeWidth _ = Nothing

  isSubTypeOf TAny _ = True
  isSubTypeOf (TStream et1) t = case t of
    (TStream et2) -> isSubTypeOf et1 et2
    (TArray _ et2) -> isSubTypeOf et1 et2
    TBottom -> True
    _ -> False
  isSubTypeOf (TArray len1 et1) t = case t of
    -- should an array with a greater length be a subtype of one with lesser?
    (TArray len2 et2) -> len1 == len2 && isSubTypeOf et1 et2
    TBottom -> True
    _ -> False
  isSubTypeOf TNumber t = case t of
    TNumber -> True
    TInteger -> True
    TInt _ -> True
    TSigned _ -> True
    TUnsigned _ -> True
    TPointer _ _ -> True
    TReal -> True
    TFloat _ -> True
    TChar -> True
    TBottom -> True
    _ -> False
  isSubTypeOf TInteger t = case t of
    TInteger -> True
    TInt _ -> True
    TSigned _ -> True
    TUnsigned _ -> True
    TPointer _ _ -> True
    TChar -> True
    TBottom -> True
    _ -> False
  isSubTypeOf (TInt sz1) t = case t of
    TInt sz2 -> sz1 == sz2
    TSigned sz2 -> sz1 == sz2
    TUnsigned sz2 -> sz1 == sz2
    TPointer sz2 _ -> sz1 == sz2
    TChar -> sz1 == charSize
    TBottom -> True
    _ -> False
  isSubTypeOf (TUnsigned sz1) t = case t of
    TUnsigned sz2 -> sz1 == sz2
    TPointer sz2 _ -> sz1 == sz2
    TChar -> sz1 == charSize
    TBottom -> True
    _ -> False
  isSubTypeOf (TSigned sz1) t = case t of
    TSigned sz2 -> sz1 == sz2
    TBottom -> True
    _ -> False
  isSubTypeOf TReal t = case t of
    TReal -> True
    TFloat _ -> True
    TBottom -> True
    _ -> False
  isSubTypeOf (TFloat sz1) t = case t of
    TFloat sz2 -> sz1 == sz2
    TBottom -> True
    _ -> False
  isSubTypeOf (TBitVector sz1) t = case t of
    TBitVector sz2 -> sz1 == sz2
    TBottom -> True
    _ -> False
  isSubTypeOf (TPointer sz1 pt1) t = case t of
    TPointer sz2 pt2 -> sz1 == sz2 && isSubTypeOf pt1 pt2
    TBottom -> True
    _ -> False
  isSubTypeOf TChar t = case t of
    TChar -> True
    TBottom -> True
    _ -> False
  isSubTypeOf (TFunction ret1 params1) t = case t of
    (TFunction ret2 params2) ->
      length params1 == length params2
      && isSubTypeOf ret1 ret2
      && all (uncurry isSubTypeOf) (zip params1 params2)
    TBottom -> True
    _ -> False
  -- isSubTypeOf TRecord t = case t of
  --   TRecord -> TRecord
  --   TBottom -> True
  --   _ -> False
  isSubTypeOf TBottom t = case t of
    TBottom -> True
    _ -> False


-- | given the fields in the hashmap, find the greatest (offset + known width)
--   This doesn't consider padding or error on overlapping fields. | --
getMinimumRecordWidth :: IsType a => HashMap BitWidth a -> BitWidth
getMinimumRecordWidth m = max maxOffset maxOffsetPlusKnownWidth
  where
    -- for if a field has a big offset, but an unkown width
    maxOffset = foldr max 0 . HashMap.keys $ m
    maxOffsetPlusKnownWidth = foldr max 0
                              . mapMaybe fieldReach
                              . HashMap.toList $ m
    fieldReach (off, pt) = (+ off) <$> getTypeWidth pt


-- | if field has offset 32 and width 16, its range is (32, 48)
--   if field has offset 32, but unknown width, it's range is (32, 33) | --
getFieldRange :: IsType a => BitWidth -> a-> (BitWidth, BitWidth)
getFieldRange off = (off,) . (+off) . maybe 1 identity . getTypeWidth

getFieldRanges :: IsType a => HashMap BitWidth a -> [(BitWidth, BitWidth)]
getFieldRanges m = uncurry getFieldRange <$> HashMap.toList m

doFieldRangesOverlap :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
doFieldRangesOverlap (start, end) (start', end') = 
  start >= start' && start < end'
  || end > start' && end <= end'
  || start' >= start && start' < end
  || end' > start && end' <= end

secondRangeContainsFirst :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
secondRangeContainsFirst (start, end) (start', end') =
  start >= start' && start < end'
  && end >= start' && end <= end'


data UnificationDirection = MostSpecific
                          | MostGeneral
                          deriving (Eq, Ord, Read, Show, Generic)

-- -- | returns Nothing if there is a known conflict.
-- --   TODO: allow some overlapping fields, like an Int16 in an Int32 | --
-- addFieldToRecord :: IsType a
--                  => UnificationDirection
--                  -> HashMap BitWidth (PilType a)
--                  -> BitWidth
--                  -> PilType a
--                  -> Maybe (HashMap BitWidth (PilType a))
-- addFieldToRecord uniDir m off pt = case HashMap.lookup off m of
--   Just pt' -> do
--     uniPt <- case uniDir of
--                MostSpecific -> mostSpecificUnifyType pt' pt
--                MostGeneral -> mostGeneralUnifyType pt' pt
--     bool Nothing (Just $ HashMap.insert off uniPt m)
--       . not . any (doFieldRangesOverlap $ getFieldRange off pt)
--       . getFieldRanges $ HashMap.delete off m  
--   Nothing -> bool Nothing (Just $ HashMap.insert off pt m)
--     . not . any (doFieldRangesOverlap $ getFieldRange off pt)
--     $ getFieldRanges m
  


-- mostSpecificUnifyRecordFields :: HashMap BitWidth PilType -> HashMap BitWidth PilType -> Maybe (HashMap BitWidth PilType)
-- mostSpecificUnifyRecordFields m1 m2 = 

-- Will need special cases for records, to get most/least specific for each field

-- | unifies to the most specific type, i.e. TInt and TInteger => TInt | --
mostSpecificUnifyType :: IsType a => PilType a -> PilType a -> Maybe (PilType a)
--mostSpecificUnifyType (TRecord m1) (TRecord m2) = do
  
mostSpecificUnifyType a b = case (isSubTypeOf a b, isSubTypeOf b a) of
  (True, _) -> Just b
  (_, True) -> Just a
  _ -> Nothing

-- | unifies to the most general type, i.e. TInt and TInteger => TInteger | --
mostGeneralUnifyType :: IsType a => PilType a -> PilType a -> Maybe (PilType a)
mostGeneralUnifyType a b = case (isSubTypeOf a b, isSubTypeOf b a) of
  (True, _) -> Just a
  (_, True) -> Just b
  _ -> Nothing


---------- unification ----------------------

data Constraint = Constraint (Sym, SymType)
  deriving (Eq, Ord, Read, Show, Generic)

-- | solutions should be the "final unification" for any sym.
-- | complex types might still contain SVars subject to substitution
-- | but the type structure shouldn't change.
newtype Solution = Solution (Sym, SymType)
  deriving (Eq, Ord, Read, Show, Generic)


data UnifyError = IncompatibleTypes (PilType SymType) (PilType SymType)
                | OverlappingRecordField { recordFields :: (HashMap BitWidth SymType)
                                         , offendingOffset :: BitWidth
                                         }
                deriving (Eq, Ord, Read, Show, Generic)

data UnifyWithSubsState = UnifyWithSubsState
                          { _accSubs :: [Constraint]
                            -- , _solutions :: [(Sym, SymType)]
                             -- , _errors :: [UnifyError]
                          } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyWithSubsState)

data UnifyResult = UnifyResult { _solutions :: [(Sym, SymType)]
                               , _errors :: [UnifyError]
                               } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyResult)


newtype UnifyWithSubs a = UnifyWithSubs { _runUnifyWithSubs :: ExceptT UnifyError (StateT UnifyWithSubsState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError UnifyError
           , MonadState UnifyWithSubsState
           )

runUnifyWithSubs :: UnifyWithSubs a -> UnifyWithSubsState -> (Either UnifyError a, UnifyWithSubsState)
runUnifyWithSubs m s = runIdentity . flip runStateT s . runExceptT . _runUnifyWithSubs $ m


addSubs :: MonadState UnifyWithSubsState m => [Constraint] -> m ()
addSubs subs = accSubs %= (<> subs)


unifyWithSubs :: SymType -> SymType -> (Either UnifyError SymType, [Constraint])
unifyWithSubs t1 t2 =
  let (er, ustate) = runUnifyWithSubs (unifyWithSubsM t1 t2) (UnifyWithSubsState []) in
    (er, ustate ^. accSubs)

-- | unifies to most specific and spits out substitutions | --
unifyWithSubsM :: ( MonadError UnifyError m
                  , MonadState UnifyWithSubsState m
                  )
               => SymType -> SymType -> m SymType
unifyWithSubsM (SVar a) (SVar b) = addSubs [Constraint (b, SVar a)] >> pure (SVar a)
unifyWithSubsM (SVar a) (SType pt) = addSubs [Constraint (a, SType pt)] >> pure (SType pt)
unifyWithSubsM a@(SType _) b@(SVar _) = unifyWithSubsM b a
unifyWithSubsM (SType pt1) (SType pt2) =
  case (isTypeDescendent pt1 pt2, isTypeDescendent pt2 pt1) of
    (False, False) -> err
    (False, True) -> unifyWithSubsM (SType pt2) (SType pt1)
    _ -> case pt1 of
      TAny -> solo pt2
      (TStream et1) -> case pt2 of
        (TStream et2) -> SType . TStream <$> unifyWithSubsM et1 et2
        (TArray len2 et2) -> SType . TArray len2 <$> unifyWithSubsM et1 et2
        _ -> err
      (TArray len1 et1) -> case pt2 of
        (TArray len2 et2)
          | len1 == len2 -> SType . TArray len1 <$> unifyWithSubsM et1 et2
          | otherwise -> err -- array length mismatch
        _ -> err
      TNumber -> case pt2 of
        TNumber -> solo TNumber
        TInteger -> solo TInteger
        TInt w -> solo $ TInt w
        TSigned w -> solo $ TSigned w
        TUnsigned w -> solo $ TUnsigned w
        TPointer w ptype -> solo $ TPointer w ptype
        TReal -> solo TReal
        TFloat w -> solo $ TFloat w
        TChar -> solo TChar
        _ -> err
      TInteger -> case pt2 of
        TInteger -> solo TInteger
        TInt w -> solo $ TInt w
        TSigned w -> solo $ TSigned w
        TUnsigned w -> solo $ TUnsigned w
        TPointer w ptype -> solo $ TPointer w ptype
        TChar -> solo TChar
        _ -> err
      TUnsigned w1 -> case pt2 of
        TUnsigned w2 -> soloW w1 w2 $ TUnsigned w1
        TPointer w2 ptype -> soloW w1 w2 $ TPointer w2 ptype
        TChar -> soloW w1 charSize TChar
        _ -> err
      TChar -> case pt2 of
        TChar -> solo TChar
        _ -> err
      TSigned w1 -> case pt2 of
        TSigned w2 -> soloW w1 w2 $ TSigned w1
        _ -> err
      TReal -> case pt2 of
        TReal -> solo TReal
        TFloat w -> solo $ TFloat w
        _ -> err
      TFloat w1 -> case pt2 of
        TFloat w2 -> soloW w1 w2 $ TFloat w2
        _ -> err
      TBitVector w1 -> case pt2 of
        TBitVector w2 -> soloW w1 w2 $ TBitVector w2
        _ -> err
      TPointer w1 pointeeType1 -> case pt2 of
        TPointer w2 pointeeType2
          | w1 == w2 -> SType . TPointer w2 <$> unifyWithSubsM pointeeType1 pointeeType2
          | otherwise -> err
        _ -> err
      TFunction ret1 params1 -> err -- don't know how to unify at the moment...
      TRecord m1 -> case pt2 of
        TRecord m2 -> SType . TRecord <$> mergeRecords m1 m2
        _ -> err
      _ -> err
  where
    err = throwError $ IncompatibleTypes pt1 pt2
    solo = pure . SType
    soloW w1 w2 x
      | w1 == w2 = solo x
      | otherwise = err
    --duo a b = unifyWithSubsM


-- | returns Nothing if there is a known conflict.
--   TODO: allow some overlapping fields, like an Int16 in an Int32 | --
addFieldToRecord :: ( MonadError UnifyError m
                    , MonadState UnifyWithSubsState m
                    )
                 => HashMap BitWidth SymType
                 -> BitWidth
                 -> SymType
                 -> m (HashMap BitWidth SymType)
addFieldToRecord m off pt = case HashMap.lookup off m of
  Just pt' -> do
    x <- unifyWithSubsM pt pt'
    checkOverlap x $ HashMap.delete off m
    return $ HashMap.insert off x m
  Nothing -> do
    checkOverlap pt m
    return $ HashMap.insert off pt m
  where
    checkOverlap pt' m'
      | (not . any (doFieldRangesOverlap $ getFieldRange off pt') . getFieldRanges $ m') = return ()
      | otherwise = throwError $ OverlappingRecordField (HashMap.insert off pt' m') off

mergeRecords :: ( MonadError UnifyError m
                , MonadState UnifyWithSubsState m
                )
             => HashMap BitWidth SymType
             -> HashMap BitWidth SymType
             -> m (HashMap BitWidth SymType)
mergeRecords m1 = foldM (uncurry . addFieldToRecord) m1 . HashMap.toList


-- unifyConstraints :: ( MonadError UnifyError m
--                     , MonadState UnifyState m
--                     )
--                  => m ()
-- unifyConstraints = undefined      


substitute' :: (Sym, SymType) -> SymType -> SymType
substitute' (v, t) (SVar v')
  | v == v' = t
  | otherwise = SVar v'
substitute' x (SType pt) = SType $ substitute' x <$> pt

class Substitute a where
  substitute :: Solution -> a -> a

instance Substitute Constraint where
  substitute (Solution (v, t)) (Constraint (v', t')) =
    Constraint . (v',) $ substitute' (v, t) t'

instance Substitute Solution where
  substitute (Solution (v, t)) (Solution (v', t')) =
    Solution . (v',) $ substitute' (v, t) t'

-- unifyConstraintWith :: ( MonadError UnifyError m
--                        , MonadState UnifyState m
--                        )
--                     => (Sym, SymType) -> [(Sym, SymType)] -> m (Sym, SymType)
-- unifyConstraintWith (v, t) (v', t')
--   | v == v' = unifyWithSubs t t'

-- unifyConstraints :: [(Sym, SymType)]
--                  -> [(Sym, SymType)]
--                  -> [UnifyError]
--                  -> ([(Sym, SymType)], [UnifyError])
-- unifyConstraints [] sols errs = (sols, errs)
-- unifyConstraints (c:cxs) sols errs =
--   let (cxs', sols', errs') = unifyConstraintWithAll c cxs sols errs in
--     unifyConstraints cxs' sols' errs'

data UnifyConstraintsResult = UnifyConstraintsResult
  { _constraints :: [(Sym, SymType)]
  , _solutions :: [(Sym, SymType)]
  , _errors :: [UnifyError]
  } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyConstraintsResult)





-- | unifies (v, t) with any (v', t') where v == v'
-- | returns most unified type for v, plus any extra subs
-- | (which should be appended to end of constraints list)
getMostUnifiedConstraintAndSubs :: Constraint
                                -> [Constraint]
                                -> (Solution, [Constraint], [UnifyError])
getMostUnifiedConstraintAndSubs (Constraint cx) cxs = foldr f (Solution cx, [], []) cxs
  where
    f (Constraint (cv, ct)) ((Solution (sv, st)), newConstraints, errs)
      | cv /= sv = (Solution (sv, st), (Constraint (cv,ct)):newConstraints, errs)
      | otherwise = let (er, subs) = unifyWithSubs st ct in
          case er of
            Left uerr -> (Solution (sv, st), subs <> newConstraints, uerr:errs)
            Right ut -> (Solution (sv, ut), subs <> newConstraints, errs)


-- unifyConstraintWithConstraints :: (Sym, SymType)
--                                -> [(Sym, SymType)]
--                                -> [(Sym, SymType)]
--                                -> [UnifyError]
--                                -> UnifyConstraintsResult
-- unifyConstraintWithConstraints (v, t) [] newConstraints errs' =
--   UnifyConstraintsResult { _constraints = newConstraints
--                          , _solutions = [(v, t)]
--                          , _errs = err's
--                          }
-- unifyConstraintWithConstraints (v, t) ((v', t'):cxs) sols errs
--   | v == v' = 
  
  
  

-- | cx - constraint "at bat"
-- | cxs - non-solution constraint list, minus cx
-- | sols - solutions (the final unification for any var, subject to substitution)
-- | outputs (updated constraints, updated solutions, errors)
unifyConstraintWithAll :: Constraint
                       -> [Constraint]
                       -> [Solution]
                       -> ([Constraint], [Solution], [UnifyError])
unifyConstraintWithAll cx cxs sols  =
  ( substitute sol <$> constraintsWithSubs
  , sol : (substitute sol <$> sols)
  , errs'
  )
  where
    (sol, constraintsWithSubs, errs') = getMostUnifiedConstraintAndSubs cx cxs
    

unifyConstraints' :: [Constraint]
                  -> [Solution]
                  -> [UnifyError]
                  -> ([Solution], [UnifyError])
unifyConstraints' [] sols errs = (sols, errs)
unifyConstraints' (cx:cxs) sols errs =
  unifyConstraints' cxs' sols' (errs <> errs')
  where
      (cxs', sols', errs') = unifyConstraintWithAll cx cxs sols

unifyConstraints :: [Constraint] -> ([Solution], [UnifyError])
unifyConstraints cxs = unifyConstraints' cxs [] []


-------------------------

stmtSolutions :: [Statement Expression]
              -> Either CheckerError ([Statement SymExpression], [Solution], [UnifyError], CheckerState)
stmtSolutions stmts = case er of
  Left err -> Left err
  Right (symStmts, cxs) -> Right (symStmts, sols, errs, s)
    where
      (sols, errs) = unifyConstraints cxs
  where
    (er, s) = runChecker_ $ do
      createVarSymMap stmts
      (symStmts, rules) <- foldM getStmtRules ([], []) stmts
      return (symStmts, Constraint <$> rules)
      
    getStmtRules :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> Checker ([Statement SymExpression], [(Sym, SymType)])
    getStmtRules (symStmts, rules) stmt = do
      (s, rs) <- stmtTypeRules stmt
      return ( symStmts <> [s] -- maybe should use a Vector
             , rs <> rules)
      

checkStmts :: [Statement Expression] -> Either CheckerError TypeReport
checkStmts = fmap toReport . stmtSolutions
  where
    toReport :: ([Statement SymExpression], [Solution], [UnifyError], CheckerState)
             -> TypeReport
    toReport (stmts, sols, errs, s) = TypeReport
      { _symTypeStmts = []
      , _varSymTypeMap = pilVarMap
      }
      where
        solutionsMap :: HashMap Sym SymType
        solutionsMap = HashMap.fromList . fmap coerce $ sols

        pilVarMap :: HashMap PilVar SymType
        pilVarMap = fmap f $ s ^. varSymMap
          where
            f :: Sym -> SymType
            f sv = maybe (SVar sv) identity $ HashMap.lookup sv solutionsMap

-- checkStmts :: [Statement Expression] -> TypeReport
-- checkStmts stmts = do
--   (symStmts, rules) <- foldM getStmtRules ([], []) stmts
--   let ur = unifyRules rules
--       (symSymMap, symPilTypeMap) = splitSVarsAndSTypes ur
--   -- "ur" should have every Sym on left side, so
--   undefined
--   where
--     getStmtRules :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> Checker ([Statement SymExpression], [(Sym, SymType)])
--     getStmtRules (symStmts, rules) stmt = do
--       (s, rs) <- stmtTypeRules stmt
--       return ( symStmts <> [s] -- maybe should use a Vector
--              , rs <> rules)
