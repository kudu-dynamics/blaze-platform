module Blaze.Pil.Inference where

import Blaze.Prelude hiding (Type)
import qualified Prelude as P
import Blaze.Types.Pil
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

data UnificationError = UWidth
                      | USign
                      | UPtrSigned
                      deriving (Eq, Ord, Show)

-- most specific unification
-- goes from general to specific

unify :: Type -> Type -> Either UnificationError Type
unify TBool TBool = Right TBool

unify t@(TBitVec b1) (TBitVec b2)
  | b1 ^. width /= b2 ^. width = Left UWidth
  | otherwise = Right t

unify t@(TInt n1) (TInt n2)
  | n1 ^. width /= n2 ^. width = Left UWidth
  | n1 ^. signed /= n2 ^. signed = Left USign
  | otherwise = Right t
unify b@(TBitVec _) n@(TInt _) = unify n b
unify t@(TInt n) (TBitVec b)
  | b ^. width /= n ^. width = Left UWidth
  | otherwise = Right t

unify (TPtr p1) (TPtr p2)
  | p1 ^. width /= p2 ^. width = Left UWidth
  | otherwise = unify (p1 ^. pointeeType) (p2 ^. pointeeType)
unify b@(TBitVec _) p@(TPtr _) = unify p b
unify t@(TPtr p) (TBitVec b)
  | p ^. width /= b ^. width = Left UWidth
  | otherwise = Right t
unify b@(TInt _) p@(TPtr _) = unify p b
unify t@(TPtr p) (TInt n)
  | n ^. signed = Left UPtrSigned
  | p ^. width /= n ^. width = Left UWidth
  | otherwise = Right t

unify _ _ = P.error "type not implemented in unify"


getExprType :: TypeEnv -> Expression ->  Maybe Type
getExprType env x = case x ^. op of
  ADC n -> inheritIntRet n
  ADD n -> inheritIntRet n
  ADD_OVERFLOW n -> inheritIntRet n
  ADDRESS_OF _ -> pointerRet
  ADDRESS_OF_FIELD _ -> pointerRet
  AND _ -> bitvecRet
  ASR _ -> bitvecRet
  BOOL_TO_INT _ -> uintRet
  CALL _ -> unknown
  CEIL _ -> bitvecRet
  CMP_E _ -> boolRet
  CMP_NE _ -> boolRet

  CMP_SGE _ -> boolRet
  CMP_SGT _ -> boolRet
  CMP_SLE _ -> boolRet
  CMP_SLT _ -> boolRet
  CMP_UGE _ -> boolRet
  CMP_UGT _ -> boolRet
  CMP_ULE _ -> boolRet
  CMP_ULT _ -> boolRet
  CONST _ -> bitvecRet
  CONST_PTR _ -> pointerRet
  ConstStr _ -> stringRet
  DIVS _ -> intRet
  DIVS_DP _ -> intRet
  DIVU _ -> uintRet
  DIVU_DP _ -> uintRet
  FABS _ -> floatRet
  FADD _ -> floatRet
  FCMP_E _ -> boolRet
  FCMP_GE _ -> boolRet
  FCMP_GT _ -> boolRet
  FCMP_LE _ -> boolRet
  FCMP_LT _ -> boolRet
  FCMP_O _ -> boolRet
  FCMP_NE _ -> boolRet
  FCMP_UO _ -> boolRet
  FDIV _ -> floatRet
  FLOAT_CONST _ -> floatRet
  FLOAT_CONV _ -> floatRet
  FLOAT_TO_INT _ -> intRet
  FLOOR _ -> bitvecRet
  FMUL _ -> floatRet
  FNEG _ -> floatRet
  FSQRT _ -> floatRet
  FTRUNC _ -> floatRet
  FSUB _ -> floatRet
  IMPORT _ -> bitvecRet
  INT_TO_FLOAT _ -> floatRet
  LOAD _ -> bitvecRet
  LOAD_STRUCT _ -> bitvecRet
  LOW_PART _ -> bitvecRet
  LSL _ -> bitvecRet
  LSR _ -> bitvecRet
  MODS _ -> intRet
  MODS_DP _ -> intRet
  MODU _ -> uintRet
  MODU_DP _ -> uintRet
  MUL n -> inheritIntRet n
  MULS_DP _ -> intRet
  MULU_DP _ -> uintRet
  NEG _ -> bitvecRet
  NOT _ -> boolRet
  OR _ -> bitvecRet
  RLC _ -> bitvecRet
  ROL _ -> bitvecRet
  ROR _ -> bitvecRet
  ROUND_TO_INT _ -> intRet
  RRC _ -> bitvecRet
  SBB n -> inheritIntRet n
  -- STORAGE _ -> unknown
  StrCmp _ -> intRet
  StrNCmp _ -> intRet
  MemCmp _ -> intRet
  SUB n -> inheritIntRet n
  SX _ -> intRet
  TEST_BIT _ -> unknown
  UNIMPL -> bitvecRet
  VAR n -> Map.lookup (n ^. src) env
  VAR_ALIASED _ -> bitvecRet
  VAR_ALIASED_FIELD _ -> bitvecRet
  VAR_FIELD _ -> bitvecRet
  VAR_SPLIT _ -> bitvecRet
  XOR _ -> bitvecRet
  ZX _ -> intRet -- shouldn't this be uintRet?
  -- _ -> unknown

  -- the following were missing from the Clojure implementation
  -- i think because the _SSA version got renamed and replaced the nonssa
  LOAD_SSA _ -> bitvecRet
  LOAD_STRUCT_SSA _ -> bitvecRet
  VAR_PHI _ -> unknown -- should be removed by analysis
  
  where
    sz :: Int
    sz = fromIntegral $ x ^. size
    boolRet = Just TBool
    pointerRet = Just $ TPtr $ PtrType sz (TObs [])
    bitvecRet = Just $ TBitVec $ BitVecType sz
    floatRet = Just $ TFloat $ FloatType sz
    stringRet = Just $ TString
    intRet = Just . TInt $ IntType sz True
    uintRet = Just . TInt $ IntType sz False

    unknown = Nothing

    isSignedInt :: Type -> Bool
    isSignedInt (TInt n) = n ^. signed
    isSignedInt _ = False

    inheritIntRet :: forall x. (HasLeft x Expression, HasRight x Expression) => x -> Maybe Type
    inheritIntRet y = do
      t1 <- getExprType env (y ^. left) 
      t2 <- getExprType env (y ^. right)
      bool uintRet intRet $ isSignedInt t1 || isSignedInt t2


