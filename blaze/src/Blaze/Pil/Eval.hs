module Blaze.Pil.Eval where

import Blaze.Prelude

import Blaze.Types.Pil


-- | Calculates PIL expression if PIL expression is made up entirely of immediates
-- and contains no vars or other unknowns.
-- Returns error if it can't return concrete result or if types don't match.
-- Returned expression should be a const int, pointer, or float
evalPilArithmeticExpr :: Expression -> Maybe Expression
evalPilArithmeticExpr expr@(Expression sz op) = case op of
  CONST _ -> unchanged
  CONST_FLOAT _ -> unchanged
  CONST_PTR _ -> unchanged
  ConstStr _ -> unchanged
  ConstFuncPtr _ -> unchanged
  CONST_BOOL _ -> unchanged
  ADD (AddOp a b) -> binIntOp (+) a b
  SUB (SubOp a b) -> binIntOp (-) a b
  _ -> Nothing
  where
    unchanged = return expr
    binIntOp
      :: (forall a. Integral a => (a -> a -> a))
      -> Expression
      -> Expression
      -> Maybe Expression
    binIntOp intOp a b = do
      (Expression szA opA) <- evalPilArithmeticExpr a
      (Expression szB opB) <- evalPilArithmeticExpr b
      when (szA /= szB) Nothing
      case (opA, opB) of
        (CONST (ConstOp valA), CONST (ConstOp valB)) ->
          evalConst valA valB
        (CONST_PTR (ConstPtrOp valA), CONST_PTR (ConstPtrOp valB)) ->
          evalConstPtr valA valB
        (CONST_PTR (ConstPtrOp valA), CONST (ConstOp valB)) ->
          evalConstPtr valA valB
        (CONST (ConstOp valA), CONST_PTR (ConstPtrOp valB)) ->
          evalConstPtr valA valB
        _ -> Nothing
        where
          evalConst = evalBinIntOp sz intOp (Expression sz . CONST . ConstOp)
          evalConstPtr = evalBinIntOp sz intOp (Expression sz . CONST_PTR . ConstPtrOp)

evalBinIntOp
  :: forall a c.
     (Integral a)
  => Size Expression
  -> (forall b. Integral b => (b -> b -> b))
  -> (a -> c) -- constructor
  -> a
  -> a
  -> Maybe c
evalBinIntOp sz intOp cons x y  = case sz of
  1 -> construct $ intOp (fromIntegral x :: Int8) (fromIntegral y)
  2 -> construct $ intOp (fromIntegral x :: Int16) (fromIntegral y)
  4 -> construct $ intOp (fromIntegral x :: Int32) (fromIntegral y)
  8 -> construct $ intOp (fromIntegral x :: Int64) (fromIntegral y)
  _ -> Nothing
  where
    construct :: forall d. Integral d => d -> Maybe c
    construct = Just . cons . fromIntegral
