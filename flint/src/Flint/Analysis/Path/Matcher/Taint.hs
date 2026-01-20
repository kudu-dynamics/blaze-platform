{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Flint.Analysis.Path.Matcher.Taint where

import Flint.Prelude hiding (sym, negate, Location)
import Flint.Types.Analysis (Parameter(..), Taint(..), TaintPropagator(..))

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Size(Size))
import Blaze.Pil.Construct (var')

import qualified Data.HashSet as HashSet


-- | Transitive closure of a 'HashSet Taint'
taintTransClos :: HashSet Taint -> HashSet Taint
taintTransClos ts =
  HashSet.fromList $ do
    t1 <- HashSet.toList ts
    t2 <- HashSet.toList ts
    if t1 == t2
      then [t1]
      else case (t1, t2) of
        (Tainted src1 (Left dst1), Tainted src2 dst2)
          | Just dst1 == src2 ^? #op . #_VAR . #_VarOp -> [t1, Tainted src1 dst2]
        (Tainted src1 (Right dst1), Tainted src2 (Left dst2))
          | dst1 == src2 -> [t1, Tainted src1 (Right $ var' dst2 (coerce $ dst2 ^. #size :: Size Pil.Expression))]
        (Tainted src1 (Right dst1), Tainted src2 (Right dst2))
          | dst1 == src2 -> [t1, Tainted src1 (Right dst2)]
        _ -> [t1]

-- | Collect any taints from the expression if it matches one or more
-- 'TaintPropagator's.
mkTaintPropagatorTaintSet ::
  [TaintPropagator] ->
  Maybe Pil.PilVar ->
  Pil.ExprOp Pil.Expression ->
  HashSet Taint
mkTaintPropagatorTaintSet tps mRetVar =
  \case
    Pil.CALL (Pil.CallOp dest args) -> case Pil.destName dest of
                                         Nothing -> HashSet.empty
                                         Just name -> go name args
    _ -> HashSet.empty
  where
    go name args =
      HashSet.fromList $ do
        tps >>= \case
          FunctionCallPropagator propName (Parameter (atMay args -> Just fromExpr)) toParam
            | name == propName ->
                case toParam of
                  Parameter (atMay args -> Just toExpr) -> [Tainted fromExpr (Right toExpr)]
                  ReturnParameter ->
                    case mRetVar of
                      Just retVar -> [Tainted fromExpr (Left retVar)]
                      _ -> []
                  _ -> []
          FunctionCallPropagator {} -> []

mkStmtTaintSet :: [TaintPropagator] -> Pil.Stmt -> HashSet Taint
mkStmtTaintSet tps (Pil.Stmt _ statement) =
  case statement of
    Pil.Def (Pil.DefOp dst src) ->
      HashSet.fromList (interestingSubexpressions src <&> (`Tainted` Left dst))
        <> mkTaintPropagatorTaintSet tps (Just dst) (src ^. #op)
    Pil.Store (Pil.StoreOp dst src) ->
      HashSet.fromList (interestingSubexpressions src <&> (`Tainted` Right dst))
        <> mkTaintPropagatorTaintSet tps Nothing (src ^. #op)
    Pil.Call callOp -> mkTaintPropagatorTaintSet tps Nothing (Pil.CALL callOp)
    _ -> HashSet.empty
  where
    interestingSubexpressions :: Pil.Expression -> [Pil.Expression]
    interestingSubexpressions e =
      case e ^. #op of
        Pil.CONST _ -> []
        Pil.CONST_PTR _ -> []
        Pil.CONST_FLOAT _ -> []
        Pil.ConstStr _ -> []
        Pil.ConstFuncPtr _ -> []
        Pil.CALL _ ->
          -- Do not recurse into 'CALL' subexpressions, since 'tps' are supposed
          -- to handle these
          []
        op -> e : foldMap interestingSubexpressions (toList op)

mkTaintSet :: [TaintPropagator] -> [Pil.Stmt] -> HashSet Taint
mkTaintSet tps = taintTransClos . HashSet.unions . fmap (mkStmtTaintSet tps)
