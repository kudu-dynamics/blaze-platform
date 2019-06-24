module Haze.Pil where

import Haze.Prelude hiding (Symbol, Type)
import Haze.Types.Pil
import qualified Hinja.MLIL as MLIL

convertExpr :: MLIL.Expression t -> MExpression
convertExpr e = MExpression
  { _size = e ^. size
  , _op = convertExprOp $ e ^. op
  }

convertExprOp :: MLIL.Operation (MLIL.Expression t) -> Maybe (ExprOp MExpression)
convertExprOp mop =
  case mop of
    (MLIL.ADC x) -> Just . ADC $ f x
    (MLIL.ADD x) -> Just . ADD $ f x
    (MLIL.ADDRESS_OF x) -> Just . ADDRESS_OF $ f x
    (MLIL.ADDRESS_OF_FIELD x) -> Just . ADDRESS_OF_FIELD $ f x
    (MLIL.ADD_OVERFLOW x) -> Just . ADD_OVERFLOW $ f x
    (MLIL.AND x) -> Just . AND $ f x
    (MLIL.ASR x) -> Just . ASR $ f x
    (MLIL.BOOL_TO_INT x) -> Just . BOOL_TO_INT $ f x
    (MLIL.CEIL x) -> Just . CEIL $ f x
    (MLIL.CMP_E x) -> Just . CMP_E $ f x
    (MLIL.CMP_NE x) -> Just . CMP_NE $ f x
    (MLIL.CMP_SGE x) -> Just . CMP_SGE $ f x
    (MLIL.CMP_SGT x) -> Just . CMP_SGT $ f x
    (MLIL.CMP_SLE x) -> Just . CMP_SLE $ f x
    (MLIL.CMP_SLT x) -> Just . CMP_SLT $ f x
    (MLIL.CMP_UGE x) -> Just . CMP_UGE $ f x
    (MLIL.CMP_UGT x) -> Just . CMP_UGT $ f x
    (MLIL.CMP_ULE x) -> Just . CMP_ULE $ f x
    (MLIL.CMP_ULT x) -> Just . CMP_ULT $ f x
    (MLIL.CONST x) -> Just . CONST $ f x
    (MLIL.CONST_PTR x) -> Just . CONST_PTR $ f x
    (MLIL.DIVS x) -> Just . DIVS $ f x
    (MLIL.DIVS_DP x) -> Just . DIVS_DP $ f x
    (MLIL.DIVU x) -> Just . DIVU $ f x
    (MLIL.DIVU_DP x) -> Just . DIVU_DP $ f x
    (MLIL.FABS x) -> Just . FABS $ f x
    (MLIL.FADD x) -> Just . FADD $ f x
    (MLIL.FCMP_E x) -> Just . FCMP_E $ f x
    (MLIL.FCMP_GE x) -> Just . FCMP_GE $ f x
    (MLIL.FCMP_GT x) -> Just . FCMP_GT $ f x
    (MLIL.FCMP_LE x) -> Just . FCMP_LE $ f x
    (MLIL.FCMP_LT x) -> Just . FCMP_LT $ f x
    (MLIL.FCMP_NE x) -> Just . FCMP_NE $ f x
    (MLIL.FCMP_O x) -> Just . FCMP_O $ f x
    (MLIL.FCMP_UO x) -> Just . FCMP_UO $ f x
    (MLIL.FDIV x) -> Just . FDIV $ f x
    (MLIL.FLOAT_CONST x) -> Just . FLOAT_CONST $ f x
    (MLIL.FLOAT_CONV x) -> Just . FLOAT_CONV $ f x
    (MLIL.FLOAT_TO_INT x) -> Just . FLOAT_TO_INT $ f x
    (MLIL.FLOOR x) -> Just . FLOOR $ f x
    (MLIL.FMUL x) -> Just . FMUL $ f x
    (MLIL.FNEG x) -> Just . FNEG $ f x
    (MLIL.FSQRT x) -> Just . FSQRT $ f x
    (MLIL.FSUB x) -> Just . FSUB $ f x
    (MLIL.FTRUNC x) -> Just . FTRUNC $ f x
    (MLIL.IMPORT x) -> Just . IMPORT $ f x
    (MLIL.INT_TO_FLOAT x) -> Just . INT_TO_FLOAT $ f x
    (MLIL.LOAD x) -> Just . LOAD $ f x
    (MLIL.LOAD_SSA x) -> Just . LOAD_SSA $ f x
    (MLIL.LOAD_STRUCT x) -> Just . LOAD_STRUCT $ f x
    (MLIL.LOAD_STRUCT_SSA x) -> Just . LOAD_STRUCT_SSA $ f x
    (MLIL.LOW_PART x) -> Just . LOW_PART $ f x
    (MLIL.LSL x) -> Just . LSL $ f x
    (MLIL.LSR x) -> Just . LSR $ f x
    (MLIL.MODS x) -> Just . MODS $ f x
    (MLIL.MODS_DP x) -> Just . MODS_DP $ f x
    (MLIL.MODU x) -> Just . MODU $ f x
    (MLIL.MODU_DP x) -> Just . MODU_DP $ f x
    (MLIL.MUL x) -> Just . MUL $ f x
    (MLIL.MULS_DP x) -> Just . MULS_DP $ f x
    (MLIL.MULU_DP x) -> Just . MULU_DP $ f x
    (MLIL.NEG x) -> Just . NEG $ f x
    (MLIL.NOT x) -> Just . NOT $ f x
    (MLIL.OR x) -> Just . OR $ f x
    (MLIL.RLC x) -> Just . RLC $ f x
    (MLIL.ROL x) -> Just . ROL $ f x
    (MLIL.ROR x) -> Just . ROR $ f x
    (MLIL.ROUND_TO_INT x) -> Just . ROUND_TO_INT $ f x
    (MLIL.RRC x) -> Just . RRC $ f x
    (MLIL.SBB x) -> Just . SBB $ f x
    (MLIL.SUB x) -> Just . SUB $ f x
    (MLIL.SX x) -> Just . SX $ f x
    (MLIL.TEST_BIT x) -> Just . TEST_BIT $ f x
    (MLIL.UNIMPL) -> Just UNIMPL
--    (MLIL.VAR x) -> Just . VarOp expr)
    (MLIL.VAR_ALIASED x) -> Just . VAR_ALIASED $ f x
    (MLIL.VAR_ALIASED_FIELD x) -> Just . VAR_ALIASED_FIELD $ f x
--    (MLIL.VAR_FIELD x) -> Just . VarFieldOp expr)
    (MLIL.VAR_PHI x) -> Just . VAR_PHI $ f x
--    (MLIL.VAR_SPLIT x) -> Just . VarSplitOp expr)
    (MLIL.VAR_SPLIT_SSA x) -> Just . VAR_SPLIT_SSA $ f x
    (MLIL.VAR_SSA x) -> Just . VAR_SSA $ f x
    (MLIL.VAR_SSA_FIELD x) -> Just . VAR_SSA_FIELD $ f x
    (MLIL.XOR x) -> Just . XOR $ f x
    (MLIL.ZX x) -> Just . ZX $ f x
    _ -> Nothing
  where
    f :: Functor m => m (MLIL.Expression t) -> m MExpression
    f = fmap convertExpr

-- does an MEXpression and its children have all `Just` op fields?
isMExpressionComplete :: MExpression -> Bool
isMExpressionComplete mexpr = case mexpr ^. op of
  Nothing -> False
  Just exop -> foldr (\mexpr' b -> isMExpressionComplete mexpr' && b) True exop

convertMExpression :: MExpression -> Maybe Expression
convertMExpression mexpr = bool Nothing (Just $ convertCompleteMExpression mexpr)
  $ isMExpressionComplete mexpr
  where
    convertCompleteMExpression :: MExpression -> Expression
    convertCompleteMExpression mexpr' = Expression
      { _size = mexpr' ^. size
      , _op = fmap convertCompleteMExpression . fromJust $ mexpr' ^. op
      }

  
--     MLIL.ADDRESS_OF_FIELD x -> ADDRESS_OF_FIELD <$> f x
--     MLIL.ADD_OVERFLOW x -> ADD_OVERFLOW <$> f x
--     MLIL.AND x -> AND <$> f x
--     MLIL.ASR x -> ASR <$> f x
--     MLIL.BOOL_TO_INT x -> BOOL_TO_INT <$> f x
--     MLIL.CEIL x -> CIEL <$> f x
--     MLIL.CMP_E x -> CMP_E <$> f x
--     MLIL.CMP_NE x -> CMP_NE <$> f x
--     MLIL.CMP_SGE x -> CMP_SGE <$> f x
--     MLIL.CMP_SGT x -> CMP_SGT <$> f x
--     MLIL.CMP_SLE x -> CMP_SLE <$> f x
--     MLIL.CMP_SLT x -> CMP_SLT <$> f x
--     MLIL.CMP_UGE x -> CMP_UGE <$> f x
--     MLIL.CMP_UGT x -> CMP_UGT <$> f x
--     MLIL.CMP_ULE x -> CMP_ULE <$> f x
--     MLIL.CMP_ULT x -> CMP_ULT <$> f x
--     MLIL.CONST x -> CMP_CONST <$> f x
--     MLIL.CONST_PTR x -> CONST_PTR <$> f x
--     MLIL.DIVS x -> DIVS <$> f x
--     MLIL.DIVS_DP x -> DIVS_DP <$> f x
--     MLIL.DIVU x -> DIVU <$> f x
--     MLIL.DIVU_DP x -> DIVU_DP <$> f x
--     MLIL.FABS x -> FABS <$> f x
--     MLIL.FADD x -> FADD <$> f x
--     MLIL.FCMP_E x -> FCMP_E <$> f x
--     MLIL.FCMP_GE x -> FCMP_GE <$> f x
--     MLIL.FCMP_GT x -> FCMP_GT <$> f x
--     MLIL.FCMP_LE x -> FCMP_LE <$> f x
--     MLIL.FCMP_LT x -> FCMP_LT <$> f x
--     MLIL.FCMP_NE x -> FCMP_NE <$> f x
--     MLIL.FCMP_O x -> FCMP_O <$> f x
--     MLIL.FCMP_UO x -> FCMP_UO <$> f x
--     MLIL.FDIV x -> FDIV <$> f x
--     MLIL.FLOAT_CONST x -> FLOAT_CONST <$> f x
--     MLIL.FLOAT_CONV x -> FLOAT_CONV <$> f x
--     MLIL.FLOAT_TO_INT x -> FLOAT_TO_INT <$> f x
--     MLIL.FLOOR x -> FLOOR <$> f x
--     MLIL.FMUL x -> FMUL <$> f x
--     MLIL.FNEG x -> FNEG <$> f x
--     MLIL.FSQRT x -> FSQRT <$> f x
--     MLIL.FSUB x -> FSUB <$> f x
--     MLIL.FTRUNC x -> FTRUNC <$> f x
--     MLIL.IMPORT x -> IMPORT <$> f x
--     MLIL.INT_TO_FLOAT x -> INT_TO_FLOAT <$> f x
--     MLIL.LOAD x -> LOAD <$> f x
--     MLIL.LOAD_SSA x -> LOAD_SSA <$> f x
--     MLIL.LOAD_STRUCT x -> LOAD_STRUCT <$> f x
--     MLIL.LOAD_STRUCT_SSA x -> LOAD_STRUCT_SSA <$> f x
--     MLIL.LOW_PART x -> LOW_PART <$> f x
--     MLIL.LSL x -> LSL <$> f x
--     MLIL.LSR x -> LSR <$> f x
--     MLIL.MODS x -> MODS <$> f x
--     MLIL.MODS_DP x -> MODS_DP <$> f x
--     MLIL.MODU x -> MODU <$> f x
--     MLIL.MODU_DP x -> MODU_DP <$> f x
--     MLIL.MUL x -> MUL <$> f x
--     MLIL.MULS_DP x -> MULS_DP <$> f x
--     MLIL.MULU_DP x -> MULU_DP <$> f x
--     MLIL.NEG x -> NEG <$> f x
--     MLIL.NOT x -> NOT <$> f x
--     MLIL.OR x -> OR <$> f x
--     MLIL.RLC x -> RLC <$> f x
--     MLIL.ROL x -> ROL <$> f x
--     MLIL.ROR x -> ROR <$> f x
--     MLIL.ROUND_TO_INT x -> ROUND_TO_INT <$> f x
--     MLIL.RRC x -> RRC <$> f x
--     MLIL.SBB x -> SBB <$> f x
--     MLIL.SUB x -> SUB <$> f x
--     MLIL.SX x -> SX <$> f x
--     MLIL.TEST_BIT x -> TEST_BIT <$> f x
--     MLIL.UNIMPL -> UNIMPL
-- --    MLIL.VAR x -> VarOp expr)
--     MLIL.VAR_ALIASED x -> VAR_ALIASED <$> f x
--     MLIL.VAR_ALIASED_FIELD x -> VAR_ALIASED_FIELD <$> f x
-- --    MLIL.VAR_FIELD x -> VarFieldOp expr)
--     MLIL.VAR_PHI x -> VAR_PHI <$> f x
-- --    MLIL.VAR_SPLIT x -> VarSplitOp expr)
--     MLIL.VAR_SPLIT_SSA x -> VAR_SPLIT_SSA <$> f x
--     MLIL.VAR_SSA x -> VAR_SSA <$> f x
--     MLIL.VAR_SSA_FIELD x -> VAR_SSA_FIELD <$> f x
--     MLIL.XOR x -> XOR <$> f x
--     MLIL.ZX x -> ZX <$> f x
    -- changeExpr mlilExpr = Expression
    --   { _size = mlilExpr ^. size
    --   , _op  =
      

-- convertExpr :: MLIL.Expression t -> Maybe Expression
-- convertExpr mexpr = Expression
--   { _size = mexpr ^. size
--   , _op = 
  

-- convertInstr :: MLIL.Instruction t -> [Statement]
