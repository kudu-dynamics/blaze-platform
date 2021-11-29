module Blaze.Util.MLILDemo where

import Blaze.Prelude
import Blaze.Util.MLIL
import Binja.C.Enums (BNVariableSourceType(StackVariableSourceType))
import qualified Binja.Variable as Var
import qualified Binja.MLIL as MLIL
import System.Directory (listDirectory)


-- looks like ADDRESS_OF is always with a stack var
matchNonStackAddressOf :: MLIL.Operation (MLIL.Expression F) -> Bool
matchNonStackAddressOf (MLIL.ADDRESS_OF x) =
  (x ^. MLIL.src . Var.sourceType) /= StackVariableSourceType
matchNonStackAddressOf _ = False

-- args are stored at positive location
matchPositiveStackOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchPositiveStackOffset (MLIL.ADDRESS_OF x) =
  (x ^. MLIL.src . Var.sourceType) == StackVariableSourceType
  && (x ^. MLIL.src . Var.storage) >= 0
matchPositiveStackOffset _ = False

-- found 0
matchVarAliasedWithNonStackVar :: MLIL.Operation (MLIL.Expression F) -> Bool
matchVarAliasedWithNonStackVar (MLIL.VAR_ALIASED x) =
  x ^. MLIL.src . MLIL.var . Var.sourceType /= StackVariableSourceType
matchVarAliasedWithNonStackVar _ = False

-- found 0
matchSetVarAliasedWithNonStackVar :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarAliasedWithNonStackVar (MLIL.SET_VAR_ALIASED x) =
  x ^. MLIL.prev . MLIL.src . MLIL.var . Var.sourceType
  /= StackVariableSourceType
matchSetVarAliasedWithNonStackVar _ = False

-- found some in Bin0
matchVarFieldWithNonZeroOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchVarFieldWithNonZeroOffset (MLIL.VAR_SSA_FIELD x) =
  x ^. MLIL.offset /= 0
matchVarFieldWithNonZeroOffset _ = False


-- found 0 in bin0, bin1, ls, kill
matchSetVarFieldWhereVarHasNoWidth :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarFieldWhereVarHasNoWidth (MLIL.SET_VAR_SSA_FIELD x) =
  isNothing $ x ^. MLIL.prev . MLIL.src . MLIL.var . Var.varType
matchSetVarFieldWhereVarHasNoWidth _ = False

-- 0 in bin0, bin1, kill, ls
matchSetVarFieldWithNegativeOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarFieldWithNegativeOffset (MLIL.SET_VAR_SSA_FIELD x) =
  (x ^. MLIL.offset) < 0
matchSetVarFieldWithNegativeOffset _ = False

-- 16 in bin0, of 3688 adds
matchAddWhereArgSizesUnequal :: MLIL.Operation (MLIL.Expression F) -> Bool
matchAddWhereArgSizesUnequal (MLIL.ADD x) =
  x ^. MLIL.left . MLIL.size /= x ^. MLIL.right . MLIL.size
matchAddWhereArgSizesUnequal _ = False

-- the only time !(size ret == size arg1 == size arg2) is when an arg is ADDRESS_OF
-- which binja thinks is size 0. but we convert it to the arch address size in PIL
matchAddReturnAndFirstArgSizeUnequal :: OpWithSize F -> Bool
matchAddReturnAndFirstArgSizeUnequal (OpWithSize sz (MLIL.ADD x)) =
  (x ^. MLIL.right . MLIL.size /= sz) ||
  (x ^. MLIL.left . MLIL.size /= sz)
matchAddReturnAndFirstArgSizeUnequal _ = False

-- 0 in bin0, 0 in bin1
-- BUT! There are a bunch that ADD a negative const...
matchLoadWithSub :: MLIL.Operation (MLIL.Expression F) -> Bool
matchLoadWithSub (MLIL.LOAD_SSA (MLIL.LoadSSAOp (MLIL.Expression _ _ _ (MLIL.SUB _)) _)) = True
matchLoadWithSub _ = False

-- 226 in bin1
-- 44 in Bin0
matchLoadWithAddNegativeConst :: MLIL.Operation (MLIL.Expression F) -> Bool
matchLoadWithAddNegativeConst (MLIL.LOAD_SSA
                       (MLIL.LoadSSAOp
                        (MLIL.Expression _ _ _
                         (MLIL.ADD
                          (MLIL.AddOp
                           (MLIL.Expression _ _ _
                            (MLIL.VAR_SSA _))
                           (MLIL.Expression _ _ _
                            (MLIL.CONST (MLIL.ConstOp n))))))
                         _))
  | n < 0 = True
  | otherwise = False
matchLoadWithAddNegativeConst _ = False


-- 419 in bin1
-- 818 in Bin0
matchLoadWithAddConst :: MLIL.Operation (MLIL.Expression F) -> Bool
matchLoadWithAddConst (MLIL.LOAD_SSA
                       (MLIL.LoadSSAOp
                        (MLIL.Expression _ _ _
                         (MLIL.ADD
                          (MLIL.AddOp
                           (MLIL.Expression _ _ _
                            (MLIL.VAR_SSA _))
                           (MLIL.Expression _ _ _
                            (MLIL.CONST _)))))
                         _)) = True
matchLoadWithAddConst _ = False


matchUnevenVarSplit :: OpWithSize F -> Bool
matchUnevenVarSplit (OpWithSize sz (MLIL.VAR_SPLIT_SSA (MLIL.VarSplitSSAOp v1 v2))) = 
  maybe True not $ do
    w1 <- getWidth v1
    w2 <- getWidth v2
    return $ w1 == w2 && (2 * w1 == coerce sz)
  where
    getWidth v = v ^? MLIL.var . Var.varType . _Just . Var.width
matchUnevenVarSplit _ = False


matchAnd :: MLIL.Operation (MLIL.Expression F) -> Bool
matchAnd (MLIL.AND _) = True
matchAnd _ = False


-- 0 found
matchTestBit :: MLIL.Operation (MLIL.Expression F) -> Bool
matchTestBit (MLIL.TEST_BIT _) = True
matchTestBit _ = False


-- found 0
matchSBB :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSBB (MLIL.SBB _) = True
matchSBB _ = False

-- found 10 ADC's in bin0, 10 which had these attributes
matchADC :: MLIL.Operation (MLIL.Expression F) -> Bool
matchADC (MLIL.ADC x) =
  ( x ^. MLIL.left . MLIL.size == x ^. MLIL.right . MLIL.size )
  && ( x ^. MLIL.carry . MLIL.size == 0 )
matchADC _ = False

-- found 10 in bin0 -- only ADCs
matchOpWithCarry :: MLIL.Operation (MLIL.Expression F) -> Bool
matchOpWithCarry (MLIL.ADC _) = True
matchOpWithCarry (MLIL.SBB _) = True
matchOpWithCarry (MLIL.RLC _) = True
matchOpWithCarry (MLIL.RRC _) = True
matchOpWithCarry _ = False

matchAddOverflow :: MLIL.Operation (MLIL.Expression F) -> Bool
matchAddOverflow (MLIL.ADD_OVERFLOW _) = True
matchAddOverflow _ = False


matchDoublePrecisionOp :: MLIL.Operation (MLIL.Expression F) -> Bool
-- matchDoublePrecisionOp (MLIL.MULU_DP _) = True
-- matchDoublePrecisionOp (MLIL.MULS_DP _) = True
matchDoublePrecisionOp (MLIL.DIVU_DP _) = True
matchDoublePrecisionOp (MLIL.DIVS_DP _) = True
matchDoublePrecisionOp (MLIL.MODU_DP _) = True
matchDoublePrecisionOp (MLIL.MODS_DP _) = True
matchDoublePrecisionOp _ = False


matchCallWithMultipleReturnArgs :: MLIL.Operation (MLIL.Expression F) -> Bool
matchCallWithMultipleReturnArgs (MLIL.CALL_OUTPUT_SSA x) =
  length (x ^. MLIL.dest) > 1
matchCallWithMultipleReturnArgs _ = False


matchExternPtrWithNonZeroOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchExternPtrWithNonZeroOffset (MLIL.EXTERN_PTR x) =
  (x ^. MLIL.offset) /= 0
matchExternPtrWithNonZeroOffset _ = False

findExternPtrWithNonZeroOffset :: FilePath -> IO ()
findExternPtrWithNonZeroOffset dir = do
  objFiles <- (((dir <> "/") <>) <$>) . filter isObjectFile <$> listDirectory dir
  forM_ objFiles $ \objFile -> do
    print objFile
    xs <- getInstructionsWithOp matchExternPtrWithNonZeroOffset objFile
    unless (null xs) $ pprint xs
  where
    isObjectFile x = take 2 (reverse x) == "o."
