{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.Constraints where

import Blaze.Prelude hiding (Constraint, Type, bitSize, sym)
import Blaze.Types.Pil (
  Expression (Expression),
  FuncVar (FuncParam, FuncResult),
  ParamPosition (ParamPosition),
  PilVar,
  StackOffset,
  Statement,
 )
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Prelude as P

import Blaze.Pil.Function (mkCallTarget)
import Blaze.Types.Pil.Checker hiding (params, ret)


constrainStandardFunc :: Sym -> BitWidth -> Pil.CallOp SymExpression -> ConstraintGen (Maybe [(Sym, ConstraintSymType)])
constrainStandardFunc _r _resultSize (Pil.CallOp _ Nothing _) = return Nothing
constrainStandardFunc r resultSize (Pil.CallOp _ (Just name) cparams) = case name of
  "fgets" -> case cparams of
    [s, size', stream] -> do
      return . Just $
        [ (s ^. #info . #sym, ptr (CSType $ TCString Nothing))
        , (size' ^. #info . #sym, CSType $ TInt Nothing (Just True))
        , (stream ^. #info . #sym, ptr (CSType $ TChar (Just 8)))
        , (r, ptr (CSType $ TCString Nothing))
        ]
    _ -> return Nothing -- TODO: add warning about malformed params
  "asprintf" -> case cparams of
    (strp : fmt : _) -> do
      return . Just $
        [ (strp ^. #info . #sym, ptr . ptr . CSType $ TCString Nothing)
        , (fmt ^. #info . #sym, ptr . CSType $ TChar (Just 8))
        , (r, CSType $ TInt jResultSize (Just True))
        ]
    _ -> return Nothing -- TODO: add warning about malformed params
  "strcmp" -> case cparams of
    [a, b] -> do
      return . Just $
        [ (a ^. #info . #sym, ptr (CSType $ TCString Nothing))
        , (b ^. #info . #sym, ptr (CSType $ TCString Nothing))
        , (r, CSType $ TInt jResultSize (Just True))
        ]
    _ -> return Nothing -- TODO: add warning about malformed params
  "strtol" -> case cparams of
    (nptr : endptr : base : _) -> do
      return . Just $
        [ (nptr ^. #info . #sym, ptr (CSType $ TCString Nothing))
        , (endptr ^. #info . #sym, ptr . ptr . CSType $ TChar (Just 8))
        , (base ^. #info . #sym, CSType $ TInt Nothing (Just True))
        , (r, CSType $ TInt Nothing (Just True))
        ]
    _ -> return Nothing --TODO : add warning about malformed params
  "abs" -> case cparams of
    [n] -> do
      return . Just $
        [ ( n ^. #info . #sym, CSType $ TInt jResultSize (Just True))
        , (r, CSType $ TInt jResultSize (Just True))
        ]
    _ -> return Nothing --TODO : add warning about malformed params
  _ -> return Nothing
 where
  jResultSize = Just resultSize
  ptr = CSType . TPointer Nothing
--------------------------------------------------------------
------ Constraint generation phase ---------------------------

getStackOffsetSym :: StackOffset -> ConstraintGen Sym
getStackOffsetSym k = do
  m <- use #stackAddrSymMap
  case HashMap.lookup k m of
    Nothing -> do
      s <- newSym
      #stackAddrSymMap %= HashMap.insert k s
      return s
    Just s -> return s

addVarSym :: PilVar -> Sym -> ConstraintGen ()
addVarSym pv s = #varSymMap %= HashMap.insert pv s

addFuncSym :: FuncVar SymExpression -> Sym -> ConstraintGen ()
addFuncSym fv s = #funcSymMap %= HashMap.insert fv s

incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1

-- | Creates new, unused Sym
newSym :: ConstraintGen Sym
newSym = do
  x <- use #currentSym
  #currentSym %= incrementSym
  return x

-- | Get the type symbol associated with a 'PilVar'.
-- If no entry exists for the provided 'PilVar' an entry
-- is added to the map and the new 'Sym' value is returned.
lookupVarSym :: PilVar -> ConstraintGen Sym
lookupVarSym pv = do
  vsm <- use #varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> do
      s <- newSym
      addVarSym pv s
      return s
    Just s -> return s

-- | Get the type symbol associated with a 'FuncVar'.
-- If no entry exists for the provided 'FuncVar' an entry
-- is added to the map and the new 'Sym' value is returned.
lookupFuncSym :: FuncVar SymExpression -> ConstraintGen Sym
lookupFuncSym fvar = do
  fsm <- use #funcSymMap
  case HashMap.lookup fvar fsm of
    Nothing -> do
      s <- newSym
      addFuncSym fvar s
      return s
    Just s -> return s

lookupSymExpr :: Sym -> ConstraintGen SymExpression
lookupSymExpr sym' = do
  m <- use #symMap
  case HashMap.lookup sym' m of
    Nothing -> throwError CannotFindSymInSymMap
    Just x -> return x

addSymExpression :: Sym -> SymExpression -> ConstraintGen ()
addSymExpression sym' x = #symMap %= HashMap.insert sym' x

byteOffsetToBitOffset :: ByteOffset -> BitOffset
byteOffsetToBitOffset (ByteOffset n) = BitOffset $ n * 8

-- | Traverse over a 'ConstraintSymType' and into any 'PilType'
-- fields. Each 'PilType' is replaced with a 'Sym' symbol (?) and the
-- 'ConstraintGen' state is updated with a type assignment of
-- the new symbol to the type definition.
addConstraint :: SymConstraint -> ConstraintGen ()
addConstraint (s1, CSVar s2) = equals s1 s2
addConstraint (s, CSType t) = do
  t' <- traverse f t
  assignType s t'
  where
    f :: ConstraintSymType -> ConstraintGen Sym
    f (CSVar v) = return v
    f (CSType ptc) = do
      x <- newSym
      pt <- traverse f ptc
      assignType x pt
      return x

addConstraints :: [SymConstraint] -> ConstraintGen ()
addConstraints = mapM_ addConstraint

-- | Generates constraints for all immediate syms in SymExpression.
-- does not get constraints of children exprs.
addExprTypeConstraints :: SymExpression -> ConstraintGen ()
addExprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
  Pil.ADC x -> do
    additionConstraint <- integralBinOpFirstArgIsReturn Nothing True x
    add $ additionConstraint <> carryConstraint x
  Pil.ADD x -> add =<< integralBinOpFirstArgIsReturn Nothing True x

  -- should this be unsigned ret because overflow is always positive?
  Pil.ADD_OVERFLOW x -> add =<< integralBinOpFirstArgIsReturn Nothing True x

  Pil.AND x -> add $ bitVectorBinOp x

  -- Arithmetic shift right
  Pil.ASR x -> add =<< integralFirstArgIsReturn x

  Pil.BOOL_TO_INT x -> add
    [ (r, CSType $ TBitVector (Just sz))
    , (x ^. #src . #info . #sym, CSType TBool)
    ]

  -- TODO get most general type for this and args:
  Pil.CALL x -> do
    let callTgt = mkCallTarget x
        -- Create the FuncVars for parameters
        argFuncVars = FuncParam callTgt . ParamPosition <$>
          [1..(callTgt ^. #numArgs)]
    argFuncSyms <- traverse lookupFuncSym argFuncVars
    let callArgSyms = view (#info . #sym) <$> x ^. #params
    -- Set symbols for call args and function params to be equal
    zipWithM_ equals argFuncSyms callArgSyms
    -- Set constraint for return value from call target
    resultFuncSym <- lookupFuncSym $ FuncResult callTgt
    equals resultFuncSym r

    -- Use information from known functions to add constraints
    -- TODO: This currently uses a fixed set of hard-coded
    --       function type information. This could later use
    --       an external database of function type cosntraints.
    mxs <- constrainStandardFunc r sz x
    -- TODO: Should the bitvec constraints always be included?
    case mxs of
      Just xs -> addConstraints xs
      Nothing -> addConstraints [(r, CSType $ TBitVector (Just sz))]

  Pil.CEIL x -> add $ floatUnOp x
  Pil.CMP_E x -> add =<< integralBinOpReturnsBool x
  Pil.CMP_NE x -> add =<< integralBinOpReturnsBool x

  Pil.CMP_SGE x -> add =<< signedBinOpReturnsBool True x
  Pil.CMP_SGT x -> add =<< signedBinOpReturnsBool True x
  Pil.CMP_SLE x -> add =<< signedBinOpReturnsBool True x
  Pil.CMP_SLT x -> add =<< signedBinOpReturnsBool True x
  Pil.CMP_UGE x -> add =<< signedBinOpReturnsBool False x
  Pil.CMP_UGT x -> add =<< signedBinOpReturnsBool False x
  Pil.CMP_ULE x -> add =<< signedBinOpReturnsBool False x
  Pil.CMP_ULT x -> add =<< signedBinOpReturnsBool False x
  Pil.CONST _ -> add [(r, CSType $ TBitVector jSz)]
  Pil.CONST_PTR _ -> add =<< mkPointerConstraint

  -- TODO: Should the length be Text.length + 1, to account for \0 ?
  Pil.ConstStr x ->
    add [(r, CSType $ TPointer jSz
           (CSType $ TCString (Just $ fromIntegral (Text.length $ x ^. #value) + 1)))]

  -- TODO: get param cound and make this pointer point to TFunction
  Pil.ConstFuncPtr _ -> add =<< mkPointerConstraint

  -- Don't remember if this is correct, or the above
  -- Pil.ConstStr x -> return [(r, CSType $ TArray
  --                               ( CSType . TVLength . fromIntegral . Text.length
  --                                 $ x ^. Pil.value )
  --                               ( CSType TChar ))]

  Pil.CONST_BOOL _ -> add [(r, CSType TBool)]
  Pil.CONST_FLOAT _ -> add [(r, CSType $ TFloat jSz)]
  Pil.DIVS x -> add =<< integralBinOpFirstArgIsReturn (Just True) False x
  Pil.DIVS_DP x -> add =<< divOrModDP True x
  Pil.DIVU x -> add =<< integralBinOpFirstArgIsReturn (Just False) False x
  Pil.DIVU_DP x -> add =<< divOrModDP False x
  Pil.FABS x -> add $ floatUnOp x
  Pil.FADD x -> add $ floatBinOp x
  Pil.FCMP_E x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_GE x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_GT x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_LE x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_LT x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_O x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_NE x -> add =<< floatBinOpReturnsBool x
  Pil.FCMP_UO x -> add =<< floatBinOpReturnsBool x
  Pil.FDIV x -> add $ floatBinOp x

  Pil.FIELD_ADDR x -> do
    fieldType <- CSVar <$> newSym
    let recType = CSType . TRecord . HashMap.fromList $
          -- for now, assuming all offsets are positive...
          [ (byteOffsetToBitOffset $ x ^. #offset, fieldType) ]
    add [ ( r, CSType $ TPointer jSz fieldType )
        , ( x ^. #baseAddr . #info . #sym, CSType $ TPointer jSz recType )
        ]

  Pil.ExternPtr _ -> unimplError

  Pil.Extract _ -> add [(r, CSType $ TBitVector jSz)]

-- TODO: should there be a link between sz of bitvec and sz of float?
  Pil.FLOAT_CONV x -> do
    add [ ( x ^. #src . #info . #sym, CSType $ TBitVector Nothing )
        , ( r, CSType $ TFloat jSz )
        ]
  Pil.FLOAT_TO_INT x -> add =<< floatToInt x
  Pil.FLOOR x -> add $ floatUnOp x
  Pil.FMUL x -> add $ floatBinOp x
  Pil.FNEG x -> add $ floatUnOp x
  Pil.FSQRT x -> add $ floatUnOp x
  Pil.FTRUNC x -> add $ floatUnOp x
  Pil.FSUB x -> add $ floatBinOp x

--   what does IMPORT do?
--   assuming it just casts an Int to a pointer
  Pil.IMPORT _ -> add =<< mkPointerConstraint

  Pil.INT_TO_FLOAT x -> add =<< intToFloat x

  Pil.LOAD x -> do
    ptrType <- CSVar <$> newSym
    add [ ( x ^. #src . #info . #sym, CSType $ TPointer Nothing ptrType )
        , ( r, ptrType )
        , ( r, CSType $ TBitVector jSz )
        ]

    -- case x ^. Pil.src . op of
    --   Pil.FIELD_ADDR y -> do
    --     ptrWidth <- CSVar <$> newSym
    --     let fieldAddrPtrSym = x ^. Pil.src . info . sym
    --         ptrType = CSType . TRecord $ HashMap.fromList [( byteOffsetToBitOffset $ y ^. Pil.offset
    --                                                        , fieldType)]
    --     return [ ( fieldAddrPtrSym, CSType $ TPointer ptrWidth ptrType )
    --            , ( r, fieldType )
    --            , ( r, CSType $ TBitVector jSz )
    --            ]

    --   _ -> do
    --     ptrWidth <- CSVar <$> newSym
    --     fieldType <- CSVar <$> newSym
    --     let ptrType = CSType $ TZeroField fieldType
    --     return [ ( x ^. Pil.src . info . sym, CSType $ TPointer ptrWidth ptrType )
    --            , ( r, fieldType )
    --            , ( r, CSType $ TBitVector jSz )
    --            ]

  -- should _x have any influence on the type of r?
  Pil.LOW_PART _x -> add [(r, CSType $ TBitVector jSz)]

  Pil.LSL x -> add =<< integralFirstArgIsReturn x
  Pil.LSR x -> add =<< integralFirstArgIsReturn x
  Pil.MODS x -> add =<< integralBinOpFirstArgIsReturn (Just True) False x
  Pil.MODS_DP x -> add =<< divOrModDP True x
  Pil.MODU x -> add =<< integralBinOpFirstArgIsReturn (Just False) False x
  Pil.MODU_DP x -> add =<< divOrModDP False x
  Pil.MUL x -> add =<< integralBinOp Nothing x
  Pil.MULS_DP x -> add =<< mulDP True x
  Pil.MULU_DP x -> add =<< mulDP False x
  Pil.NEG x -> add =<< integralUnOp (Just True) x

  -- NOT is either Bool -> Bool or BitVec -> BitVec
  -- but no way to express that without typeclasses
  -- so it's just constrained as 'a -> a'
  Pil.NOT x -> add [(r, CSVar $ x ^. #src . #info . #sym)]
    -- bitVectorUnOp x

  Pil.OR x -> add $ bitVectorBinOp x
  Pil.RLC x -> do
    integralConstraint <- integralFirstArgIsReturn x
    add $ integralConstraint <> carryConstraint x

  Pil.ROL x -> add =<< integralFirstArgIsReturn x
  Pil.ROR x -> add =<< integralFirstArgIsReturn x
  Pil.ROUND_TO_INT x -> add =<< floatToInt x
  Pil.RRC x -> do
    integralConstraint <- integralFirstArgIsReturn x
    add $ integralConstraint <> carryConstraint x

  Pil.SBB x -> do
    integralConstraint <- integralBinOpFirstArgIsReturn (Just True) True x
    add $ integralConstraint <> carryConstraint x
  -- STORAGE _ -> unknown
  Pil.StrCmp _ -> unimplError
  Pil.StrNCmp _ -> unimplError
  Pil.MemCmp _ -> unimplError

  -- should this somehow be linked to the type of the stack var?
  -- its type could change every Store.
  Pil.STACK_LOCAL_ADDR x -> do
    s <- getStackOffsetSym $ x ^. #stackOffset
    add [ (r, CSType $ TPointer jSz (CSVar s)) ]

  Pil.SUB x -> add =<< integralBinOpFirstArgIsReturn Nothing True x
  Pil.SX x -> add =<< integralExtendOp (Just True) x

  Pil.TEST_BIT _ -> add [(r, CSType TBool)] -- ? tests if bit in int is on or off
  Pil.UNIMPL _ -> add [ (r, CSType $ TBitVector jSz ) ]
  Pil.UPDATE_VAR x -> do
    v <- lookupVarSym $ x ^. #dest
    -- How should src and dest be related?
    -- Can't express that 'offset + width(src) == width(dest)'
    --  without '+' and '==' as type level operators.
    add [ (r, CSVar v) ]

  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. #src
    add [ (r, CSVar v)
        , (v, CSType $ TBitVector jSz)
        ]
  Pil.VAR_FIELD _ ->
    -- TODO: can we know anything about src PilVar by looking at offset + result size?
    add [ (r, CSType $ TBitVector jSz) ]

  Pil.VAR_PHI _ -> unimplError

  Pil.VAR_JOIN x -> do
    low <- lookupVarSym $ x ^. #low
    high <- lookupVarSym $ x ^. #high
    add [ (r, CSType $ TBitVector jSz)
        , (low, CSType $ TBitVector jHalfSz)
        , (high, CSType $ TBitVector jHalfSz)
        ]

  Pil.XOR x -> add $ bitVectorBinOp x
  Pil.ZX x -> add =<< integralExtendOp (Just False) x

  Pil.UNIT -> add [ (r, CSType TUnit) ]

  where
    add = addConstraints
    jSz = Just sz
    jHalfSz = Just $ sz `div` 2
    jDblSz = Just $ sz * 2
    unimplError = P.error . show $ op'

    mkPointerConstraint = do
      pt <- newSym
      return [(r, CSType (TPointer jSz (CSVar pt)))]

    carryConstraint :: (HasField' "carry" x SymExpression) => x -> [SymConstraint]
    carryConstraint x = [(x ^. field' @"carry" . #info . #sym, CSType TBool)]

    bitVectorBinOp :: ( HasField' "left" x SymExpression
                      , HasField' "right" x SymExpression)
                   => x -> [SymConstraint]
    bitVectorBinOp x =
      [ (r, CSType $ TBitVector jSz)
      , (r, CSVar $ x ^. field' @"left" . #info . #sym)
      , (r, CSVar $ x ^. field' @"right" . #info . #sym)
      ]

    integralExtendOp ::
      (HasField' "src" x SymExpression) =>
      Maybe Bool ->
      x ->
      ConstraintGen [SymConstraint]
    integralExtendOp mSignedness x = do
      -- Do not assume signedness of result is shared with operand
      return
        [ (r, CSType $ TInt jSz mSignedness)
        , (x ^. field' @"src" . #info . #sym, CSType $ TInt Nothing Nothing)
        ]

    integralUnOp ::
      (HasField' "src" x SymExpression) =>
      Maybe Bool ->
      x ->
      ConstraintGen [SymConstraint]
    integralUnOp mSignedness x = do
      return
        [ (r, CSType (TInt jSz mSignedness))
        , (r, CSVar $ x ^. field' @"src" . #info . #sym)
        ]

    integralBinOp ::
      ( HasField' "left" x SymExpression
      , HasField' "right" x SymExpression
      ) =>
      Maybe Bool ->
      x ->
      ConstraintGen [SymConstraint]
    integralBinOp mSignedness x = do
      return
        -- NB: There is currently no support for symbolic constraints on
        --     type-level values. Without those, we cannot constrain
        --     the operands to have the same width. But we can constraint
        --     that the left and right operands both unify to the same type
        --     and include the type-level value check in unification of that
        --     constraint.
        -- NB: The result may not be the same size as the operands so we
        --     do not include a contraint on size between the result and
        --     operands. E.g., see MUL instruction in x86.
        [ (r, CSType (TInt jSz mSignedness))
        , (r, CSVar $ x ^. field' @"left" . #info . #sym)
        , (left ^. #info . #sym, CSType $ TInt Nothing mSignedness)
        , (right ^. #info . #sym, CSType $ TInt Nothing mSignedness)
        -- Left and right operands must unify to the same type
        , (left ^. #info . #sym, CSVar $ right ^. #info . #sym)
        ]
     where
      left :: SymExpression
      left = x ^. field' @"left"
      right :: SymExpression
      right = x ^. field' @"right"

    integralBinOpFirstArgIsReturn ::
      ( HasField' "left" x SymExpression
      , HasField' "right" x SymExpression
      ) =>
      Maybe Bool ->
      Bool ->
      x ->
      ConstraintGen [SymConstraint]
    integralBinOpFirstArgIsReturn mSignedness secondArgSameWidth x = do
      let secondArgWidth = bool Nothing jSz secondArgSameWidth
      return
        [ (r, CSType (TInt jSz mSignedness))
        , (r, CSVar $ x ^. field' @"left" . #info . #sym)
        , (left ^. #info . #sym, CSType $ TInt jSz mSignedness)
        , (right ^. #info . #sym, CSType $ TInt secondArgWidth mSignedness)
        -- Left and right operands must unify to the same type
        , (left ^. #info . #sym, CSVar $ right ^. #info . #sym)
        ]
     where
      left :: SymExpression
      left = x ^. field' @"left"
      right :: SymExpression
      right = x ^. field' @"right"

    mulDP :: ( HasField' "left" x SymExpression
             , HasField' "right" x SymExpression)
          => Bool
          -> x
          -> ConstraintGen [SymConstraint]
    mulDP signedness x = do
      let retSignednessType = Just signedness
          rightSignedness = Nothing
      return
        [ (r, CSType (TInt jSz retSignednessType))
        , (x ^. field' @"left" . #info . #sym, CSType $ TInt jHalfSz retSignednessType)
        , (x ^. field' @"right" . #info . #sym, CSType $ TInt jHalfSz rightSignedness)
        ]

    divOrModDP :: ( HasField' "left" x SymExpression
                  , HasField' "right" x SymExpression)
               => Bool
               -> x
               -> ConstraintGen [SymConstraint]
    divOrModDP signedness x = do
      let retSignedness = Just signedness
          rightSignedness = Nothing
      return
        [ (r, CSType (TInt jSz retSignedness))
        , (x ^. field' @"left" . #info . #sym, CSType $ TInt jDblSz retSignedness)
        , (x ^. field' @"right" . #info . #sym, CSType $ TInt jSz rightSignedness)
        ]

    integralBinOpReturnsBool :: ( HasField' "left" x SymExpression
                                , HasField' "right" x SymExpression )
                             => x
                             -> ConstraintGen [SymConstraint]
    integralBinOpReturnsBool x = do
      let argWidth = Nothing
          argSign = Nothing
          argType = CSType $ TInt argWidth argSign
      return
        [ (r, CSType TBool)
        , (x ^. field' @"left" . #info . #sym, argType)
        , (x ^. field' @"right" . #info . #sym, argType)
        , (x ^. field' @"left" . #info . #sym, CSVar $ x ^. field' @"right" . #info . #sym)
        ]

    signedBinOpReturnsBool :: ( HasField' "left" x SymExpression
                              , HasField' "right" x SymExpression )
                           => Bool
                           -> x
                           -> ConstraintGen [SymConstraint]
    signedBinOpReturnsBool isSignedBool x = do
      let argWidth = Nothing
          argType = CSType (TInt argWidth (Just isSignedBool))
      return
        [ (r, CSType TBool)
        , (x ^. field' @"left" . #info . #sym, argType)
        , (x ^. field' @"right" . #info . #sym, argType)
        , (x ^. field' @"left" . #info . #sym, CSVar $ x ^. field' @"right" . #info . #sym)
        ]

    intToFloat :: (HasField' "src" x SymExpression)
               => x
               -> ConstraintGen [SymConstraint]
    intToFloat x = do
      let intWidth = Nothing
          intSign = Nothing
      return
        [ (x ^. field' @"src" . #info . #sym, CSType $ TInt intWidth intSign)
        , (r, CSType $ TFloat jSz)
        ]

    floatToInt :: (HasField' "src" x SymExpression)
               => x
               -> ConstraintGen [SymConstraint]
    floatToInt x = do
      let floatWidth = Nothing
          intSign = Nothing
      return
        [ (x ^. field' @"src" . #info . #sym, CSType $ TFloat floatWidth)
        , (r, CSType $ TInt jSz intSign)
        ]

    floatBinOp :: ( HasField' "left" x SymExpression
                  , HasField' "right" x SymExpression )
               => x
               -> [SymConstraint]
    floatBinOp x =
      [ (r, CSType (TFloat jSz))
      , (r, CSVar $ x ^. field' @"left" . #info . #sym)
      , (r, CSVar $ x ^. field' @"right" . #info . #sym)
      ]

    floatUnOp :: (HasField' "src" x SymExpression)
              => x
              -> [SymConstraint]
    floatUnOp x =
      [ (r, CSType (TFloat jSz))
      , (r, CSVar $ x ^. field' @"src" . #info . #sym)
      ]

    floatBinOpReturnsBool :: (HasField' "left" x SymExpression, HasField' "right" x SymExpression)
                          => x
                          -> ConstraintGen [SymConstraint]
    floatBinOpReturnsBool x = do
      let argWidth = Nothing
          argType = CSType $ TFloat argWidth
      return
        [ (r, CSType TBool)
        , (x ^. field' @"left" . #info . #sym, argType)
        , (x ^. field' @"right" . #info . #sym, argType)
        ]

    integralFirstArgIsReturn :: (HasField' "left" x SymExpression, HasField' "right" x SymExpression)
                             => x
                             -> ConstraintGen [SymConstraint]
    integralFirstArgIsReturn x = do
      let intWidth = Nothing
          shifterWidth = Nothing
          intSign = Nothing
          shifterSign = Nothing
      let n = CSType $ TInt intWidth intSign
      return
        [ (x ^. field' @"left" . #info . #sym, n)
        , (x ^. field' @"right" . #info . #sym, CSType $ TInt shifterWidth shifterSign)
        , (r, n)
        ]

-- | Recursively adds type constraints for all expr sym's in SymExpression,
-- including nested syms.
addAllExprTypeConstraints :: SymExpression -> ConstraintGen ()
addAllExprTypeConstraints x@(InfoExpression (SymInfo _ _) op') = do
  addExprTypeConstraints x
  mapM_ addAllExprTypeConstraints op'

-- | converts expression to SymExpression (assigns symbols to all exprs), including itself
--   adds each new sym/expr pair to CheckerState
toSymExpression :: Expression -> ConstraintGen SymExpression
toSymExpression (Expression sz op') = do
  symOp <- traverse toSymExpression op'
  s <- newSym
  let bitSize = fromIntegral sz * 8
      symExpr = InfoExpression (SymInfo bitSize s) symOp
  addSymExpression s symExpr
  return symExpr

-- | Generate all rules for a statement, including sub-expressions,
-- and add them to the ConstraintGen state. Also creates a 'Statement SymExpression'.
-- Make sure to set the 'currentStmt' index before calling this function.
addStmtTypeConstraints :: Statement Expression
                       -> ConstraintGen (Statement SymExpression)
addStmtTypeConstraints (Pil.Def (Pil.DefOp pv expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. #info . #sym
  pvSym <- lookupVarSym pv
  addAllExprTypeConstraints symExpr
  equals pvSym exprSym
  return $ Pil.Def (Pil.DefOp pv symExpr)
addStmtTypeConstraints (Pil.Constraint (Pil.ConstraintOp expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. #info . #sym
  addAllExprTypeConstraints symExpr
  assignType exprSym TBool
  return $ Pil.Constraint (Pil.ConstraintOp symExpr)
addStmtTypeConstraints (Pil.Store (Pil.StoreOp addrExpr valExpr)) = do
  symAddrExpr <- toSymExpression addrExpr
  symValExpr <- toSymExpression valExpr
  let symAddr = symAddrExpr ^. #info . #sym
      symVal = symValExpr ^. #info . #sym
  addAllExprTypeConstraints symAddrExpr
  addAllExprTypeConstraints symValExpr
  let ptrWidth = Nothing
  assignType symAddr $ TPointer ptrWidth symVal
  return $ Pil.Store (Pil.StoreOp symAddrExpr symValExpr)
addStmtTypeConstraints stmt@(Pil.DefPhi (Pil.DefPhiOp pv vs)) = do
  pvSym <- lookupVarSym pv
  mapM_ (addPhiConstraint pvSym) vs
  traverse toSymExpression stmt -- does nothing but change the type
  where
    addPhiConstraint pvSym v = lookupVarSym v >>= equals pvSym
addStmtTypeConstraints (Pil.DefMemPhi x) = traverse toSymExpression $ Pil.DefMemPhi x
-- TODO: Link up args to params
addStmtTypeConstraints (Pil.Call x) = traverse toSymExpression $ Pil.Call x
addStmtTypeConstraints (Pil.TailCall x) = traverse toSymExpression $ Pil.TailCall x
addStmtTypeConstraints (Pil.BranchCond (Pil.BranchCondOp expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. #info . #sym
  addAllExprTypeConstraints symExpr
  assignType exprSym TBool
  return $ Pil.BranchCond (Pil.BranchCondOp symExpr)
addStmtTypeConstraints (Pil.UnimplInstr x) = return $ Pil.UnimplInstr x
addStmtTypeConstraints (Pil.UnimplMem x) = traverse toSymExpression $ Pil.UnimplMem x
addStmtTypeConstraints Pil.Undef = return Pil.Undef
addStmtTypeConstraints Pil.Nop = return Pil.Nop
addStmtTypeConstraints (Pil.Annotation x) = return $ Pil.Annotation x
addStmtTypeConstraints (Pil.EnterContext x) = traverse toSymExpression $ Pil.EnterContext x
addStmtTypeConstraints (Pil.ExitContext x) = traverse toSymExpression $ Pil.ExitContext x
addStmtTypeConstraints (Pil.Jump x) = traverse toSymExpression $ Pil.Jump x
addStmtTypeConstraints (Pil.JumpTo x) = traverse toSymExpression $ Pil.JumpTo x
addStmtTypeConstraints (Pil.Ret x) = traverse toSymExpression $ Pil.Ret x
addStmtTypeConstraints Pil.NoRet = return Pil.NoRet
addStmtTypeConstraints Pil.Exit = return Pil.Exit
