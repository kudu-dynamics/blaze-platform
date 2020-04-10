{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.Inference2 where

import Blaze.Prelude hiding (Type, sym, bitSize)
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

data IntWidth = I8
              | I16
              | I32
              | I64
              | I128
              deriving (Eq, Ord, Read, Show)

data FloatWidth = F32
                | F64
                | F80
                deriving (Eq, Ord, Read, Show)

data PilType = TAny
             | TArray { elemType :: PilType, len :: Word64 }
             | TNumber
             | TInteger
             | TInt { intWidth :: BitWidth }
             | TSigned { intWidth :: BitWidth }
             | TUnsigned { intWidth :: BitWidth } 
             | TReal
             | TFloat { bitWidth :: BitWidth }
             | TBitVector { bitWidth :: BitWidth }
             | TString { len :: Word64 }
             | TPointer { bitWidth :: BitWidth }
--             | TRecord -- eventully [PilType] to get fields
             | TBottom
             deriving (Eq, Ord, Read, Show)

newtype Sym = Sym Int
            deriving (Eq, Ord, Show, Generic)

instance Hashable Sym

data SymType = SVar Sym
             | SType PilType
             deriving (Eq, Ord, Show, Generic)

data CheckerError = CannotFindPilVarInVarSymMap PilVar
                  | CannotFindSymInSymMap
                  | UnhandledExpr
                  | UnhandledStmt
  deriving (Eq, Ord, Show)

incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1

data SymExpression = SymExpression
  { _size :: BitWidth
  , _op :: ExprOp Sym
  } deriving (Eq, Ord, Show, Generic)

data SymState = SymState
  { _currentSym :: Sym
  , _symMap :: HashMap Sym SymExpression
  , _varSymMap :: HashMap PilVar Sym
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''SymState)

type SymMonad a = State SymState a

lookupVarSym :: (MonadState SymState m, MonadError CheckerError m)
             => PilVar -> m Sym
lookupVarSym pv = do
  vsm <- use varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> throwError $ CannotFindPilVarInVarSymMap pv
    Just s -> return s

lookupSymExpr :: (MonadState SymState m, MonadError CheckerError m)
             => Sym -> m SymExpression
lookupSymExpr sym = do
  m <- use symMap
  case HashMap.lookup sym m of
    Nothing -> throwError $ CannotFindSymInSymMap
    Just x -> return x


addSymExpression :: MonadState SymState m => Sym -> SymExpression -> m ()
addSymExpression sym x = symMap %= HashMap.insert sym x

addVarSym :: MonadState SymState m => PilVar -> Sym -> m ()
addVarSym pv sym = varSymMap %= HashMap.insert pv sym

-- |Create mapping of each PilVar to a symbol| --
createVarSymMap :: MonadState SymState m => [Statement Expression] -> m ()
createVarSymMap stmts = do
  let vars = Analysis.getRefVars stmts
  mapM_ f $ HashSet.toList vars
  where
    f var = newSym >>= addVarSym var
  

newSym :: MonadState SymState m => m Sym
newSym = do
  x <- use currentSym
  currentSym %= incrementSym
  return x

toSymExpression :: MonadState SymState m => Expression -> m Sym
toSymExpression (Expression sz op) = do
  symOp <- traverse toSymExpression op
  let bitSize = fromIntegral sz * 8
      sexpr = SymExpression bitSize symOp
  s <- newSym
  addSymExpression s sexpr
  return s

exprTypeRules :: forall m. (MonadState SymState m, MonadError CheckerError m)
              => Sym -> SymExpression -> m [(Sym, SymType)]
exprTypeRules r (SymExpression sz op) = case op of
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

--   CMP_SGE _ -> boolRet
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

    integralBinOp :: (Pil.HasLeft x Sym, Pil.HasRight x Sym) => x -> m [(Sym, SymType)]
    integralBinOp x = return
      [ (r, SType (TInt sz))
      , (r, SVar $ x ^. Pil.left)
      , (r, SVar $ x ^. Pil.right)
      ]

    integralBinOpReturnsBool :: (Pil.HasLeft x Sym, Pil.HasRight x Sym)
                             => x -> m [(Sym, SymType)]
    integralBinOpReturnsBool x = return  
      [ (r, SType (TInt sz))
      , (x ^. Pil.left, SVar $ x ^. Pil.right)
      ]

getAllExprTypeRules :: forall m. (MonadState SymState m, MonadError CheckerError m)
                    => Sym -> SymExpression -> m [(Sym, SymType)]
getAllExprTypeRules thisExprSym x@(SymExpression _ op) = do
  rulesForThisExpr <- exprTypeRules thisExprSym x
  sm <- use symMap
  rulesForChildren <-foldM (f sm) [] op
  return $ rulesForThisExpr <> rulesForChildren
  where
    f :: HashMap Sym SymExpression -> [(Sym, SymType)] -> Sym -> m [(Sym, SymType)]
    f sm rules sym = case HashMap.lookup sym sm of
      Nothing -> throwError CannotFindSymInSymMap
      Just x' -> (<> rules) <$> getAllExprTypeRules sym x' 

-- get all rules for stmts
-- preserve [Statement SymExpression]
-- needs to get all rules and all Stmts
stmtTypeRules :: (MonadState SymState m, MonadError CheckerError m)
              => Statement Expression -> m (Statement Sym, [(Sym, SymType)])
stmtTypeRules (Pil.Def (Pil.DefOp pv expr)) = do
  exprSym <- toSymExpression expr
  pvSym <- lookupVarSym pv
  symExpr <- lookupSymExpr exprSym
  exprRules <- getAllExprTypeRules exprSym symExpr
  return ( Pil.Def (Pil.DefOp pv exprSym)
         , [ (pvSym, SVar exprSym) ]
           <> exprRules )
stmtTypeRules (Pil.Constraint (Pil.ConstraintOp expr)) = do
  exprSym <- toSymExpression expr
  symExpr <- lookupSymExpr exprSym
  exprRules <- getAllExprTypeRules exprSym symExpr
  return ( Pil.Constraint (Pil.ConstraintOp exprSym)
         , exprRules )
stmtTypeRules _ = throwError UnhandledStmt

type Rule = (Sym, SymType)

type StmtRule = (Statement Sym, Rule)

-- need to preserve type for every Sym

-- | if second SymType is SVar euqal to first Sym, replaces with first SymType |--
subst :: (Sym, SymType) -> (Sym, SymType) -> (Sym, SymType)
subst (sym, st) x@(sym', SVar v)
  | v == sym = (sym', st)
  | otherwise = x
subst _ x = x

unifyRules :: [(Sym, SymType)] -> [(Sym, SymType)]
unifyRules xs' = f xs' []
  where
    f :: [(Sym, SymType)] -> [(Sym, SymType)] -> [(Sym, SymType)]
    f [] xs = xs
    f (x:xs) ys = f xs (subst x <$> ys)

-- | if a symbol type cannot be inferred, it will be in the [(Sym, Sym)] | --
splitSVarsAndSTypes :: [(Sym, SymType)] -> ([(Sym, Sym)], [(Sym, PilType)])
splitSVarsAndSTypes xs = (mapMaybe getSVar xs, mapMaybe getSType xs)
  where
    getSVar :: (Sym, SymType) -> Maybe (Sym, Sym)
    getSVar (sym, SVar x) = Just (sym, x)
    getSVar _ = Nothing

    getSType :: (Sym, SymType) -> Maybe (Sym, PilType)
    getSType (sym, SType x) = Just (sym, x)
    getSType _ = Nothing

-- | fst result is list of syms that can't unify, second is inferred type map | --
unifyPilTypes :: [(Sym, PilType)] -> ([(Sym, PilType, PilType)], HashMap Sym PilType)
unifyPilTypes = foldr f ([], HashMap.empty)
  where
    f :: (Sym, PilType) -> ([(Sym, PilType, PilType)], HashMap Sym PilType) -> ([(Sym, PilType, PilType)], HashMap Sym PilType)
    f (sym, pt) (conflicts, m) = case HashMap.lookup sym m of
      Just pt' -> case mostSpecificUnifyType pt pt' of
        Nothing -> ((sym, pt, pt'):conflicts, m)
        Just msuPt -> (conflicts, HashMap.insert sym msuPt m)
      Nothing -> (conflicts, HashMap.insert sym pt m)

-- unifyStmts :: forall m. (MonadState SymState m, MonadError CheckerError m)
--               => [Statement Expression] -> m [StmtRule]
-- unifyStmts = concatMapM f
--   where
--     f stmt = do




------------------- unification --------------

-- | True if second is equal or subtype of first | --
isSubTypeOf :: PilType -> PilType -> Bool
isSubTypeOf TAny _ = True
isSubTypeOf (TArray et1 len1) t = case t of
  (TArray et2 len2) -> et1 == et2 && len1 == len2
  (TString len2) -> et1 == (TUnsigned 8) && len1 == len2
  TBottom -> True
  _ -> False
isSubTypeOf TNumber t = case t of
  TNumber -> True
  TInteger -> True
  TInt _ -> True
  TSigned _ -> True
  TUnsigned _ -> True
  TPointer _ -> True
  TReal -> True
  TFloat _ -> True
  TBottom -> True
  _ -> False
isSubTypeOf TInteger t = case t of
  TInteger -> True
  TInt _ -> True
  TSigned _ -> True
  TUnsigned _ -> True
  TPointer _ -> True
  TBottom -> True
  _ -> False
isSubTypeOf (TInt sz1) t = case t of
  TInt sz2 -> sz1 == sz2
  TSigned sz2 -> sz1 == sz2
  TUnsigned sz2 -> sz1 == sz2
  TPointer sz2 -> sz1 == sz2
  TBottom -> True
  _ -> False
isSubTypeOf (TUnsigned sz1) t = case t of
  TUnsigned sz2 -> sz1 == sz2
  TPointer sz2 -> sz1 == sz2
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
-- isSubTypeOf TRecord t = case t of
--   TRecord -> TRecord
--   TBottom -> True
--   _ -> False
isSubTypeOf TBottom t = case t of
  TBottom -> True
  _ -> False

  

-- | unifies to the most specific type, i.e. TInt and TInteger => TInt | --
mostSpecificUnifyType :: PilType -> PilType -> Maybe PilType
mostSpecificUnifyType a b = case (isSubTypeOf a b, isSubTypeOf b a) of
  (True, _) -> Just b
  (_, True) -> Just a
  _ -> Nothing

-- | unifies to the most general type, i.e. TInt and TInteger => TInteger | --
mostGeneralType :: PilType -> PilType -> Maybe PilType
mostGeneralType a b = case (isSubTypeOf a b, isSubTypeOf b a) of
  (True, _) -> Just a
  (_, True) -> Just b
  _ -> Nothing


-- -- | finds nearest common ancestor in type lattice
-- generalUnify :: PilType -> PilType -> PilType


