{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE RankNTypes #-}

module Blaze.Pil.Eval where

import Blaze.Prelude hiding (Bits)
import Blaze.Types.Pil
import GHC.Float
import Data.Bits


{-
  Simplifies a PIL expression as much as possible: essentially evaluates constants
  as much as possible
  Could potentially use meta-programming to make the code have less boilerplate
-}
simplifyPilExpr :: Expression -> Expression
simplifyPilExpr expr@(Expression sz ops) = case ops of
  {- Values -}
  CONST        _ -> unchanged
  CONST_PTR    _ -> unchanged
  CONST_FLOAT  _ -> unchanged
  CONST_BOOL   _ -> unchanged 
  ConstStr     _ -> unchanged
  ConstFuncPtr _ -> unchanged

  {- Operations -}
  ADC _ -> unimplemented

  ADD (AddOp expr1 expr2) ->
    evalBinIntOp' sz (+) (ADD `compose2` AddOp) expr1 expr2
  
  ADD_WILL_CARRY _ ->     unimplemented
  ADD_WILL_OVERFLOW _ ->  unimplemented

  AND (AndOp expr1 expr2) ->
    evalBinBooleanOp sz (.&.) (AND `compose2` AndOp) expr1 expr2
  ASR (AsrOp expr1 expr2) ->
    evalBinIntOp' sz binShiftROp (ASR `compose2` AsrOp) expr1 expr2
  BOOL_TO_INT (BoolToIntOp expr') ->
    evalUnBoolToIntOp sz (\b -> if b then 1 else 0) (BOOL_TO_INT . BoolToIntOp) expr'
  CEIL (CeilOp expr') ->
    evalUnFloatOp sz ((fromIntegral :: (Integer -> Double)) . ceiling) (CEIL . CeilOp) expr' -- float to float, not float to int
  CMP_E (CmpEOp expr1 expr2) ->
    evalBinEqOp sz (==) (CMP_E `compose2` CmpEOp) expr1 expr2
  CMP_NE (CmpNeOp expr1 expr2) ->
    evalBinEqOp sz (/=) (CMP_NE `compose2` CmpNeOp) expr1 expr2
  CMP_SGE (CmpSgeOp expr1 expr2) ->
    evalBinOrdOp sz (>=) (CMP_SGE `compose2` CmpSgeOp) expr1 expr2
  CMP_SGT (CmpSgtOp expr1 expr2) ->
    evalBinOrdOp sz (>) (CMP_SGT `compose2` CmpSgtOp) expr1 expr2
  CMP_SLE (CmpSleOp expr1 expr2) ->
    evalBinOrdOp sz (<=) (CMP_SLE `compose2` CmpSleOp) expr1 expr2
  CMP_SLT (CmpSltOp expr1 expr2) ->
    evalBinOrdOp sz (<) (CMP_SLT `compose2` CmpSltOp) expr1 expr2
  CMP_UGE (CmpUgeOp expr1 expr2) ->
    evalBinIntToBoolOp sz (unsignedBinIntToBoolOp (>=)) (CMP_UGE `compose2` CmpUgeOp) expr1 expr2
  CMP_UGT (CmpUgtOp expr1 expr2) ->
    evalBinIntToBoolOp sz (unsignedBinIntToBoolOp (>)) (CMP_UGT `compose2` CmpUgtOp) expr1 expr2
  CMP_ULE (CmpUleOp expr1 expr2) ->
    evalBinIntToBoolOp sz (unsignedBinIntToBoolOp (<=)) (CMP_ULE `compose2` CmpUleOp) expr1 expr2
  CMP_ULT (CmpUltOp expr1 expr2) ->
    evalBinIntToBoolOp sz (unsignedBinIntToBoolOp (<)) (CMP_ULT `compose2` CmpUltOp) expr1 expr2
  DIVS (DivsOp expr1 expr2) ->
    evalBinIntToFloatOp sz
                        (binIntToFloatAsDoubleOp ((/) `on` fromIntegral))
                        (DIVS `compose2` DivsOp)
                        expr1 expr2
  DIVS_DP (DivsDpOp expr1 expr2) ->
    evalBinIntToFloatOp sz ((/) `on` fromIntegral) (DIVS_DP `compose2` DivsDpOp) expr1 expr2

  DIVU _ -> unimplemented
  DIVU_DP _ -> unimplemented

  FABS (FabsOp expr') ->
    evalUnFloatOp sz fabsDouble (FABS . FabsOp) expr'
  FADD (FaddOp expr1 expr2) ->
    evalBinFloatOp sz (+) (FADD `compose2` FaddOp) expr1 expr2
  FCMP_E (FcmpEOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (==) (FCMP_E `compose2` FcmpEOp) expr1 expr2
  FCMP_GE (FcmpGeOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (>=) (FCMP_GE `compose2` FcmpGeOp) expr1 expr2
  FCMP_GT (FcmpGtOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (>) (FCMP_GT `compose2` FcmpGtOp) expr1 expr2
  FCMP_LE (FcmpLeOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (<=) (FCMP_LE `compose2` FcmpLeOp) expr1 expr2
  FCMP_LT (FcmpLtOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (<) (FCMP_LT `compose2` FcmpLtOp) expr1 expr2
  FCMP_NE (FcmpNeOp expr1 expr2) ->
    evalBinFloatToBoolOp sz (/=) (FCMP_NE `compose2` FcmpNeOp) expr1 expr2

  FCMP_O _ ->   unimplemented
  FCMP_UO _ ->  unimplemented

  FDIV (FdivOp expr1 expr2) ->
    evalBinFloatOp sz (/) (FDIV `compose2` FdivOp) expr1 expr2

  FLOAT_CONV _ ->   unimplemented
  FLOAT_TO_INT _ -> unimplemented

  FLOOR (FloorOp expr') ->
    evalUnFloatToIntOp sz floor (FLOOR . FloorOp) expr'
  FMUL (FmulOp expr1 expr2) ->
    evalBinFloatOp sz (*) (FMUL `compose2` FmulOp) expr1 expr2
  FNEG (FnegOp expr') ->
    evalUnFloatOp sz negate (FNEG . FnegOp) expr'
  FSQRT (FsqrtOp expr') ->
    evalUnFloatOp sz sqrt (FSQRT . FsqrtOp) expr'
  FSUB (FsubOp expr1 expr2) ->
    evalBinFloatOp sz (-) (FSUB `compose2` FsubOp) expr1 expr2
  FTRUNC (FtruncOp expr') ->
    evalUnFloatToIntOp sz truncateDouble (FTRUNC . FtruncOp) expr'
  INT_TO_FLOAT (IntToFloatOp expr') ->
    evalUnIntToFloatOp sz fromIntegral (INT_TO_FLOAT . IntToFloatOp) expr'
  
  LOAD _  ->    unimplemented
  LOW_PART _ -> unimplemented

  LSL (LslOp expr1 expr2) ->
    evalBinIntOp' sz binShiftLOp (LSL `compose2` LslOp) expr1 expr2
  LSR (LsrOp expr1 expr2) ->
    evalBinIntOp' sz binLogicalShiftROp (LSR `compose2` LsrOp) expr1 expr2

  MODS _ ->     unimplemented
  MODS_DP _ ->  unimplemented
  MODU _ ->     unimplemented
  MODU_DP _ ->  unimplemented
  MUL _ ->      unimplemented
  MULS_DP _ ->  unimplemented
  MULU_DP _ ->  unimplemented

  NEG (NegOp expr') ->
    evalUnIntOp sz negate (NEG . NegOp) expr'
  NOT (NotOp expr') ->
    evalUnBooleanOp sz complement (NOT . NotOp) expr'
  OR (OrOp expr1 expr2) ->
    evalBinBooleanOp sz (.|.) (OR `compose2` OrOp) expr1 expr2

  POPCNT _ -> unimplemented
  RLC _ ->    unimplemented
  
  ROL (RolOp expr1 expr2) ->
    evalBinIntOp' sz binRotateLOp (ROL `compose2` RolOp) expr1 expr2
  ROR (RorOp expr1 expr2) ->
    evalBinIntOp' sz binRotateROp (ROR `compose2` RorOp) expr1 expr2
  ROUND_TO_INT (RoundToIntOp expr') ->
    evalUnFloatToIntOp sz round (ROUND_TO_INT . RoundToIntOp) expr'

  RRC _ -> unimplemented
  SBB _ -> unimplemented

  SUB (SubOp expr1 expr2) ->
    evalBinIntOp' sz (-) (SUB `compose2` SubOp) expr1 expr2
    
  SUB_WILL_OVERFLOW _ -> unimplemented

  SX (SxOp expr') -> extensionOp sz False expr'
  {-
    let expr2 = simplifyPilExpr expr' 
    in if isConstant expr2 then
      case getConstType expr2 of
        (ConstNum x) -> constructConst sz x
        (ConstPtr x) -> constructConstPtr sz x
        _ -> error "sx is only defined for 2's complement integers"
       else case expr2 of
        (Expression sz' (SX (SxOp expr3))) ->
          if sz' <= sz then Expression sz (SX (SxOp expr3))
          else Expression sz (SX (SxOp expr2))
        _ -> Expression sz (SX (SxOp expr2))
  -}

  TEST_BIT _ ->   unimplemented
  UNIMPL _ ->     unimplemented
  VAR_JOIN _ ->   unimplemented
  VAR _ ->        unimplemented
  VAR_FIELD _ ->  unimplemented

  XOR (XorOp expr1 expr2) ->
    evalBinBooleanOp sz xor (XOR `compose2` XorOp) expr1 expr2
  ZX (ZxOp expr') -> extensionOp sz True expr'
  {-
    let expr2 = simplifyPilExpr expr'
    in if isConstant expr2 then
      case expr2 of
        (Expression sz' (CONST (ConstOp x))) -> (constructConst sz . castUnsignedInt sz') x
        (Expression sz' (CONST_PTR (ConstPtrOp x))) -> (constructConstPtr sz . castUnsignedInt sz') x
        _ -> error "zx is only defined for 2's complement integers"
       else case expr2 of
        (Expression sz' (ZX (ZxOp expr3))) ->
          if sz' <= sz then Expression sz (ZX (ZxOp expr3)) -- ex: zx 2 then zx 4 is equal to just zx 4
          else Expression sz (ZX (ZxOp expr2)) -- zx 4 then zx 2 is NOT equal to just zx 2, so we have to keep the expression
        _ -> Expression sz (ZX (ZxOp expr2))
  -}

  CALL _ ->             unimplemented
  Extract _ ->          unimplemented
  StrCmp _ ->           unimplemented
  StrNCmp _ ->          unimplemented
  MemCmp _ ->           unimplemented
  ExternPtr _ ->        unimplemented
  STACK_ADDR _ ->       unimplemented
  UPDATE_VAR _ ->       unimplemented
  FIELD_ADDR _ ->       unimplemented
  UNIT ->               unimplemented
  ARRAY_ADDR _ ->       unimplemented
  
  _ -> unimplemented

  where
    unchanged = expr
    unimplemented = expr 


{- ##### Higher Order Functions that Destructure PIL Types ##### -}

{- ### Unary Operations ### -}

simplifyAndCheckUn :: Expression -> Either Expression Expression
simplifyAndCheckUn expr =
  let expr' = simplifyPilExpr expr
  in if isConstant expr' then Right expr' else Left expr'

constructUnExpr :: Size Expression
                -> (Expression -> Expression)
                -> (Expression -> ExprOp Expression)
                -> Either Expression Expression
                -> Expression
constructUnExpr sz _ constructor (Left expr) = (Expression sz . constructor) expr
constructUnExpr _ op _ (Right expr) = op expr

simplifyThenConstructUn :: Size Expression
                       -> (Expression -> Expression)
                       -> (Expression -> ExprOp Expression)
                       -> Expression
                       -> Expression
simplifyThenConstructUn sz op constructor = constructUnExpr sz op constructor . simplifyAndCheckUn

evalUnFloatOp :: Size Expression
                -> (Double -> Double)
                -> (Expression -> ExprOp Expression)
                -> Expression
                -> Expression
evalUnFloatOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' = constructFloat sz . op . getDouble

evalUnIntOp :: Size Expression
               -> (Int64 -> Int64)
               -> (Expression -> ExprOp Expression)
               -> Expression
               -> Expression
evalUnIntOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' expr' = case getConstType expr' of
      (ConstNum val) -> (constructConst sz . op) val
      (ConstPtr val) -> (constructConstPtr sz . op) val

evalUnBooleanOp :: Size Expression
                -> (forall a. Bits a => a -> a)
                -> (Expression -> ExprOp Expression)
                -> Expression
                -> Expression
evalUnBooleanOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' expr' = case getConstType expr' of
      (ConstNum val) ->  (constructConst sz . op) val
      (ConstPtr val) ->  (constructConstPtr sz . op) val
      (Bool val)     ->  (constructBool sz . op) val

evalUnBoolToIntOp :: Size Expression
                  -> (Bool -> Int64)
                  -> (Expression -> ExprOp Expression)
                  -> Expression
                  -> Expression
evalUnBoolToIntOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' = constructConst sz . op . getBool

evalUnFloatToIntOp :: Size Expression
                   -> (Double -> Int64)
                   -> (Expression -> ExprOp Expression) 
                   -> Expression
                   -> Expression
evalUnFloatToIntOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' = constructConst sz . op . getDouble

evalUnIntToFloatOp :: Size Expression
                   -> (Int64 -> Double)
                   -> (Expression -> ExprOp Expression)
                   -> Expression
                   -> Expression
evalUnIntToFloatOp sz op = simplifyThenConstructUn sz op'
  where
    op' :: Expression -> Expression
    op' = constructFloat sz . op . getInt


{- ### Binary Operations ### -}

type ExprPair = (Expression, Expression)

simplifyAndCheckBin :: Expression -> Expression -> Either ExprPair ExprPair
simplifyAndCheckBin expr1 expr2 =
  let exprPair@(expr1'@(Expression sz1 _), expr2'@(Expression sz2 _)) = (simplifyPilExpr expr1, simplifyPilExpr expr2)
  in if sz1 /= sz2 then error "sizes don't match"
     else if isConstant expr1' && isConstant expr2' then Right exprPair
     else Left exprPair

constructBinExpr :: Size Expression
                 -> (Expression -> Expression -> Expression)
                 -> (Expression -> Expression -> ExprOp Expression)
                 -> Either ExprPair ExprPair
                 -> Expression
constructBinExpr sz _ constructor (Left (expr1,expr2)) = Expression sz $ constructor expr1 expr2
constructBinExpr _ op _ (Right (expr1,expr2)) = op expr1 expr2

simplifyThenConstructBin :: Size Expression
                         -> (Expression -> Expression -> Expression)
                         -> (Expression -> Expression -> ExprOp Expression)
                         -> Expression -> Expression
                         -> Expression
simplifyThenConstructBin sz op constructor expr1 expr2 =
  constructBinExpr sz op constructor $ simplifyAndCheckBin expr1 expr2

evalBinFloatOp :: Size Expression
               -> (Double -> Double -> Double)
               -> (Expression -> Expression -> ExprOp Expression)
               -> Expression
               -> Expression
               -> Expression
evalBinFloatOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = constructFloat sz $ op (getDouble expr1) (getDouble expr2)

evalBinFloatToBoolOp :: Size Expression
                     -> (Double -> Double -> Bool)
                     -> (Expression -> Expression -> ExprOp Expression)
                     -> Expression
                     -> Expression
                     -> Expression
evalBinFloatToBoolOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = constructBool sz $ op (getDouble expr1) (getDouble expr2)

evalBinIntOp' :: Size Expression
              -> (Int64 -> Int64 -> Int64)
              -> (Expression -> Expression -> ExprOp Expression)
              -> Expression 
              -> Expression 
              -> Expression
evalBinIntOp' sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 =  case (getConstType expr1, getConstType expr2) of
      {- if both values are CONST, then result will be const; if one of the values is a 
        CONST_PTR, then result will be CONST_PTR -}
      (ConstNum val1, ConstNum val2) ->
        constructConst sz $ op val1 val2
      _ -> constructConstPtr sz $ op (getInt expr1) (getInt expr2)

evalBinIntToBoolOp :: Size Expression
                   -> (Int64 -> Int64 -> Bool)
                   -> (Expression -> Expression -> ExprOp Expression)
                   -> Expression
                   -> Expression
                   -> Expression
evalBinIntToBoolOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = constructBool sz $ op (getInt expr1) (getInt expr2)

evalBinBooleanOp :: Size Expression
                 -> (forall a. Bits a => a -> a -> a)
                 -> (Expression -> Expression -> ExprOp Expression)
                 -> Expression
                 -> Expression
                 -> Expression
evalBinBooleanOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = case (getConstType expr1, getConstType expr2) of
      (Bool val1, Bool val2) ->
        constructBool sz $ op val1 val2
      (ConstNum val1, ConstNum val2) ->
        constructConst sz $ op val1 val2
      _ -> constructConstPtr sz $ op (getInt expr1) (getInt expr2)

evalBinOrdOp :: Size Expression
             -> (forall a. Ord a => a -> a -> Bool)
             -> (Expression -> Expression -> ExprOp Expression)
             -> Expression
             -> Expression
             -> Expression
evalBinOrdOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = case (getConstType expr1, getConstType expr2) of
      (Float val1, Float val2) ->
        constructBool sz $ op val1 val2
      (Bool val1, Bool val2) ->
        constructBool sz $ op val1 val2
      (ConstNum val1, ConstNum val2) ->
        constructBool sz $ op val1 val2
      _ -> constructBool sz $ op (getInt expr1) (getInt expr2)

evalBinEqOp :: Size Expression
            -> (forall a. Eq a => a -> a -> Bool)
            -> (Expression -> Expression -> ExprOp Expression)
            -> Expression
            -> Expression
            -> Expression
evalBinEqOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = constructBool sz $ op expr1 expr2

evalBinIntToFloatOp :: Size Expression
                     -> (Int64 -> Int64 -> Double)
                     -> (Expression -> Expression -> ExprOp Expression)
                     -> Expression
                     -> Expression
                     -> Expression
evalBinIntToFloatOp sz op = simplifyThenConstructBin sz op'
  where
    op' :: Expression -> Expression -> Expression
    op' expr1 expr2 = constructFloat sz $ op (getInt expr1) (getInt expr2)


{- ##### Helper Functions ##### -}

isConstant :: Expression -> Bool
isConstant (Expression _ (CONST        _))   = True
isConstant (Expression _ (CONST_PTR    _))   = True
isConstant (Expression _ (CONST_FLOAT  _))   = True
isConstant (Expression _ (IMPORT       _))   = True
isConstant (Expression _ (ConstStr     _))   = True
isConstant (Expression _ (ConstFuncPtr _))   = True
isConstant (Expression _ (CONST_BOOL   _))   = True
isConstant _ = False

{- converts Int64 to the representation given by the sz argument, but while keeping it as
   an Int64 -}
castInt :: Size Expression -> Int64 -> Int64
castInt 1 x = fromIntegral (fromIntegral x :: Int8)
castInt 2 x = fromIntegral (fromIntegral x :: Int16) 
castInt 4 x = fromIntegral (fromIntegral x :: Int32)
castInt 8 x = x

{- this is for zero extension: if we did what we did in castInt, then we would be doing sign extension. The method below
   does zero extension, or pads the upper bits with 0s -}
castUnsignedInt :: Size Expression -> Int64 -> Int64
castUnsignedInt 1 x = fromIntegral (fromIntegral (fromIntegral x :: Word8) :: Word64)
castUnsignedInt 2 x = fromIntegral (fromIntegral (fromIntegral x :: Word16) :: Word64)
castUnsignedInt 4 x = fromIntegral (fromIntegral (fromIntegral x :: Word32) :: Word64)
castUnsignedInt 8 x = x

{- same as castInt, but for Double -}
castDouble :: Size Expression -> Double -> Double
castDouble 4 x = float2Double (double2Float x)
castDouble 8 x = x

constructConst :: Size Expression -> Int64 -> Expression
constructConst sz = Expression sz . CONST . ConstOp . castInt sz

constructConstPtr :: Size Expression -> Int64 -> Expression
constructConstPtr sz = Expression sz . CONST_PTR . ConstPtrOp . castInt sz

constructFloat :: Size Expression -> Double -> Expression
constructFloat sz = Expression sz . CONST_FLOAT . ConstFloatOp . castDouble sz

constructBool :: Size Expression -> Bool -> Expression
constructBool sz = Expression sz . CONST_BOOL . ConstBoolOp

getInt :: Expression -> Int64
getInt (Expression _ (CONST (ConstOp int))) = int
getInt (Expression _ (CONST_PTR (ConstPtrOp int))) = int

getDouble :: Expression -> Double
getDouble (Expression _ (CONST_FLOAT (ConstFloatOp val))) = val

getBool :: Expression -> Bool
getBool (Expression _ (CONST_BOOL (ConstBoolOp val))) = val

data ConstType = ConstNum Int64 | ConstPtr Int64 | Float Double | Bool Bool

getConstType :: Expression -> ConstType
getConstType (Expression _ (CONST (ConstOp val))) = ConstNum val
getConstType (Expression _ (CONST_PTR (ConstPtrOp val))) = ConstPtr val
getConstType (Expression _ (CONST_FLOAT (ConstFloatOp val))) = Float val
getConstType (Expression _ (CONST_BOOL (ConstBoolOp val))) = Bool val

{- composes two functions with the first one having two arguments
   ex: f = floor `compose2` (+) -}
compose2 :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
compose2 = (.) . (.) -- (ㆆ_ㆆ)

{- performs arithmetic shift -}
binShiftROp :: Int64 -> Int64 -> Int64
binShiftROp b s = 
  let s' :: Int = fromIntegral s
  in shiftR b s'

binShiftLOp :: Int64 -> Int64 -> Int64
binShiftLOp b s =
  let s' :: Int = fromIntegral s
  in shiftL b s'

binLogicalShiftROp :: Int64 -> Int64 -> Int64
binLogicalShiftROp b s =
  let s' :: Int = fromIntegral s
      b' :: Word64 = fromIntegral b
  in fromIntegral $ shiftR b' s'

binRotateLOp :: Int64 -> Int64 -> Int64
binRotateLOp b s =
  let s' :: Int = fromIntegral s
  in rotateL b s'

binRotateROp :: Int64 -> Int64 -> Int64
binRotateROp b s =
  let s' :: Int = fromIntegral s
  in rotateR b s'

-- returns true if both numbers are not NaN
bothAreOrdered :: Double -> Double -> Bool
bothAreOrdered fp1 fp2 = isNaN fp1 && isNaN fp2

unsignedBinIntToBoolOp :: (Word64 -> Word64 -> Bool) -> Int64 -> Int64 -> Bool
unsignedBinIntToBoolOp f x y = f (fromIntegral x) (fromIntegral y)

unsignedBinIntToFloatOp :: (Word64 -> Word64 -> Double) -> Int64 -> Int64 -> Double
unsignedBinIntToFloatOp f x y = f (fromIntegral x) (fromIntegral y)

binIntToFloatAsDoubleOp :: (Int64 -> Int64 -> Float) -> Int64 -> Int64 -> Double
binIntToFloatAsDoubleOp f x y = float2Double $ f x y

binUnsignedIntToFloat :: (Word64 -> Word64 -> Float) -> Int64 -> Int64 -> Float
binUnsignedIntToFloat f x y = f (fromIntegral x) (fromIntegral y)

extensionOp :: Size Expression -> Bool -> Expression -> Expression
extensionOp sz isUnsigned expr =
  let expr' = simplifyPilExpr expr
      castFunc size x = if isUnsigned then castUnsignedInt size x else x
  in if isConstant expr' then
    case expr' of
      (Expression sz' (CONST (ConstOp x))) -> (constructConst sz . castFunc sz') x
      (Expression sz' (CONST_PTR (ConstPtrOp x))) -> (constructConstPtr sz . castFunc sz') x
      _ -> error "extension operations are only defined for 2's complement integers"
     else if isUnsigned then
      case expr' of
        (Expression sz' (ZX (ZxOp expr''))) ->
          if sz' <= sz then Expression sz (ZX (ZxOp expr'')) -- ex: zx 2 then zx 4 is equal to just zx 4
          else Expression sz (ZX (ZxOp expr')) -- zx 4 then zx 2 is NOT equal to just zx 2, so we have to keep the expression
        _ -> Expression sz (ZX (ZxOp expr'))
     else
      case expr' of
        (Expression sz' (SX (SxOp expr''))) ->
          if sz' <= sz then Expression sz (SX (SxOp expr''))
          else Expression sz (SX (SxOp expr'))
        _ -> Expression sz (SX (SxOp expr'))


{- ##### OLD CODE ##### -}

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
  IMPORT _ -> unchanged
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
