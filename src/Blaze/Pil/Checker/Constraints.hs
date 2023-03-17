{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.Constraints where


import Blaze.Prelude hiding (Constraint, Symbol, Type, bitSize, sym)
import Blaze.Types.Pil (
  CallDest,
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


constrainStandardFunc
  :: Sym
  -> BitWidth
  -> Pil.CallOp SymExpression
  -> ConstraintGen (Maybe [(Sym, ConstraintSymType)])
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
        , (r, CSType $ TInt Nothing Nothing)
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
      linkWhenRootFunctionParam pv s
      return s
    Just s -> return s

linkWhenRootFunctionParam :: PilVar -> Sym -> ConstraintGen ()
linkWhenRootFunctionParam pv pvSym = void . runMaybeT $ do
  rootParamInfo <- MaybeT $ view #rootFunctionParamInfo
  let paramMap = rootParamInfo ^. #rootParamMap
  pvCtx <- hoistMaybe $ pv ^. #ctx
  when (pvCtx == (rootParamInfo ^. #rootCtx)) $ do
    funcVar <- hoistMaybe $ HashMap.lookup (pv ^. #symbol) paramMap
    lift $ do
      paramSym <- lookupFuncSym funcVar
      addConstraint (paramSym, CSVar pvSym)

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

addCallOpConstraints
  :: ( HasField' "args" a [SymExpression]
     , HasField' "dest" a (CallDest SymExpression)
     )
  => a -> ConstraintGen (Pil.CallTarget SymExpression)
addCallOpConstraints x = do
  let callTgt = mkCallTarget x
      -- Create the FuncVars for parameters
      argFuncVars = FuncParam callTgt . ParamPosition <$>
        [1..(length $ x ^. #args)]
  argFuncSyms <- traverse lookupFuncSym argFuncVars
  let callArgSyms = view (#info . #sym) <$> x ^. #args
  -- Set symbols for call args and function params to be equal
  zipWithM_ equals argFuncSyms callArgSyms
  return callTgt


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
-- Will explicitly recurse into children expressions if needed.
addExprTypeConstraints :: SymExpression -> ConstraintGen ()
addExprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
  Pil.ADC x -> do
    additionConstraint <- integralBinOpFirstArgIsReturn Nothing True x
    add $ additionConstraint <> carryConstraint x
    addChildConstraints
  Pil.ADD x -> do
    add =<< integralBinOpFirstArgIsReturn Nothing True x
    addChildConstraints

  Pil.ADD_WILL_CARRY x -> do
    add =<< integralBinOpReturnsBool (Just False) x
    addChildConstraints
  Pil.ADD_WILL_OVERFLOW x -> do
    add =<< integralBinOpReturnsBool (Just True) x
    addChildConstraints

  Pil.AND x -> do
    add $ bitVectorBinOp x
    addChildConstraints

  Pil.ARRAY_ADDR x -> do
    pt <- newSym
    add
      [ (r, CSType (TPointer jSz (CSVar pt)))
      , (r, CSVar $ x ^. #base . #info . #sym)
      , (x ^. #index . #info . #sym, CSType (TInt Nothing (Just False)))
      ]
    addChildConstraints

  -- Arithmetic shift right
  Pil.ASR x -> do
    add =<< integralFirstArgIsReturn x
    addChildConstraints

  Pil.BOOL_TO_INT x -> do
    add
      [ (r, CSType $ TBitVector (Just sz))
      , (x ^. #src . #info . #sym, CSType TBool)
      ]
    addChildConstraints

  -- TODO get most general type for this and args:
  Pil.CALL x -> do
    callTgt <- addCallOpConstraints x
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

    addChildConstraints

  Pil.CEIL x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.CMP_E x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_NE x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints

  ---- Signed and unsigned comparisons perform different operations,
  ---- but the signedness of operands cannot be inferred from the
  ---- operation performed. However, using flow typing we can use
  ---- the result of the comparison to assign signededness or value
  ---- constraints in the form of refinement types.
  Pil.CMP_SGE x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_SGT x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_SLE x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_SLT x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_UGE x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_UGT x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_ULE x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CMP_ULT x -> do
    add =<< integralBinOpReturnsBool Nothing x
    addChildConstraints
  Pil.CONST _ -> do
    add [(r, CSType $ TBitVector jSz)]
  Pil.CONST_PTR _ -> do
    add =<< mkPointerConstraint

  -- TODO: Should the length be Text.length + 1, to account for \0 ?
  Pil.ConstStr x -> do
    add [(r, CSType $ TPointer jSz
           (CSType $ TCString (Just $ fromIntegral (Text.length $ x ^. #value) + 1)))]

  -- TODO: get param cound and make this pointer point to TFunction
  Pil.ConstFuncPtr _ -> do
    add =<< mkPointerConstraint

  -- Don't remember if this is correct, or the above
  -- Pil.ConstStr x -> return [(r, CSType $ TArray
  --                               ( CSType . TVLength . fromIntegral . Text.length
  --                                 $ x ^. Pil.value )
  --                               ( CSType TChar ))]

  Pil.CONST_BOOL _ -> do
    add [(r, CSType TBool)]
  Pil.CONST_FLOAT _ -> do
    add [(r, CSType $ TFloat jSz)]

  Pil.DIVS x -> do
    add =<< integralBinOpFirstArgIsReturn (Just True) False x
    addChildConstraints
  Pil.DIVS_DP x -> do
    add =<< divOrModDP True x
    addChildConstraints
  Pil.DIVU x -> do
    add =<< integralBinOpFirstArgIsReturn (Just False) False x
    addChildConstraints
  Pil.DIVU_DP x -> do
    add =<< divOrModDP False x
    addChildConstraints
  Pil.FABS x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.FADD x -> do
    add $ floatBinOp x
    addChildConstraints
  Pil.FCMP_E x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_GE x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_GT x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_LE x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_LT x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_O x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_NE x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FCMP_UO x -> do
    add =<< floatBinOpReturnsBool x
    addChildConstraints
  Pil.FDIV x -> do
    add $ floatBinOp x
    addChildConstraints

  Pil.FIELD_ADDR x -> do
    fieldType <- CSVar <$> newSym
    let recType = CSType . TRecord . HashMap.fromList $
          -- for now, assuming all offsets are positive...
          [ (byteOffsetToBitOffset $ x ^. #offset, fieldType) ]
    add [ ( r, CSType $ TPointer jSz fieldType )
        , ( x ^. #baseAddr . #info . #sym, CSType $ TPointer jSz recType )
        ]
    addChildConstraints

  Pil.ExternPtr _ -> unimplError

  Pil.Extract _ -> do
    add [(r, CSType $ TBitVector jSz)]
    addChildConstraints

-- TODO: should there be a link between sz of bitvec and sz of float?
  Pil.FLOAT_CONV x -> do
    add [ ( x ^. #src . #info . #sym, CSType $ TBitVector Nothing )
        , ( r, CSType $ TFloat jSz )
        ]
    addChildConstraints
  Pil.FLOAT_TO_INT x -> do
    add =<< floatToInt x
    addChildConstraints
  Pil.FLOOR x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.FMUL x -> do
    add $ floatBinOp x
    addChildConstraints
  Pil.FNEG x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.FSQRT x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.FTRUNC x -> do
    add $ floatUnOp x
    addChildConstraints
  Pil.FSUB x -> do
    add $ floatBinOp x
    addChildConstraints

-- TODO: What does IMPORT do? Assuming it just casts an Int to a pointer
  Pil.IMPORT _ -> do
    add =<< mkPointerConstraint
    addChildConstraints

  Pil.INT_TO_FLOAT x -> do
    add =<< intToFloat x
    addChildConstraints

  Pil.LOAD x@(Pil.LoadOp (InfoExpression _srcInfo _srcOp)) -> do
    pointeeType <- CSVar <$> newSym
    add [ ( x ^. #src . #info . #sym, CSType $ TPointer Nothing pointeeType )
        , ( r, pointeeType )
        , ( r, CSType $ TBitVector jSz )
        ]
    addChildConstraints

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
  Pil.LOW_PART _x -> do
    add [(r, CSType $ TBitVector jSz)]
    addChildConstraints

  Pil.LSL x -> do
    add =<< integralFirstArgIsReturn x
    addChildConstraints
  Pil.LSR x -> do
    add =<< integralFirstArgIsReturn x
    addChildConstraints
  Pil.MODS x -> do
    add =<< integralBinOpFirstArgIsReturn (Just True) False x
    addChildConstraints
  Pil.MODS_DP x -> do
    add =<< divOrModDP True x
    addChildConstraints
  Pil.MODU x -> do
    add =<< integralBinOpFirstArgIsReturn (Just False) False x
    addChildConstraints
  Pil.MODU_DP x -> do
    add =<< divOrModDP False x
    addChildConstraints
  Pil.MUL x -> do
    add =<< integralBinOp Nothing x
    addChildConstraints
  Pil.MULS_DP x -> do
    add =<< mulDP True x
    addChildConstraints
  Pil.MULU_DP x -> do
    add =<< mulDP False x
    addChildConstraints
  Pil.NEG x -> do
    add =<< integralUnOp (Just True) x
    addChildConstraints

  -- NOT is either Bool -> Bool or BitVec -> BitVec
  -- but no way to express that without typeclasses
  -- so it's just constrained as 'a -> a'
  Pil.NOT x -> do
    add [(r, CSVar $ x ^. #src . #info . #sym)]
    addChildConstraints
    -- bitVectorUnOp x

  Pil.OR x -> do
    add $ bitVectorBinOp x
    addChildConstraints
  Pil.POPCNT x -> do
    add
      [ (r, CSType (TInt jSz Nothing))
      , (x ^. #src . #info . #sym, CSType (TBitVector Nothing))
      ]
    addChildConstraints
  Pil.RLC x -> do
    integralConstraint <- integralFirstArgIsReturn x
    add $ integralConstraint <> carryConstraint x
    addChildConstraints

  Pil.ROL x -> do
    add =<< integralFirstArgIsReturn x
    addChildConstraints
  Pil.ROR x -> do
    add =<< integralFirstArgIsReturn x
    addChildConstraints
  Pil.ROUND_TO_INT x -> do
    add =<< floatToInt x
    addChildConstraints
  Pil.RRC x -> do
    integralConstraint <- integralFirstArgIsReturn x
    add $ integralConstraint <> carryConstraint x
    addChildConstraints

  Pil.SBB x -> do
    integralConstraint <- integralBinOpFirstArgIsReturn (Just True) True x
    add $ integralConstraint <> carryConstraint x
    addChildConstraints
  -- STORAGE _ -> unknown
  Pil.StrCmp _ -> unimplError
  Pil.StrNCmp _ -> unimplError
  Pil.MemCmp _ -> unimplError

  Pil.SUB x -> do
    add =<< integralBinOpFirstArgIsReturn Nothing True x
    addChildConstraints
  Pil.SX x -> do
    add =<< integralExtendOp (Just True) x
    addChildConstraints

  Pil.SUB_WILL_OVERFLOW x -> do
    add =<< integralBinOpReturnsBool (Just True) x
    addChildConstraints

  Pil.TEST_BIT _ -> do
    add [(r, CSType TBool)] -- ? tests if bit in int is on or off
    addChildConstraints
  Pil.UNIMPL _ -> do
    add [ (r, CSType $ TBitVector jSz ) ]
    addChildConstraints
  Pil.UPDATE_VAR x -> do
    v <- lookupVarSym $ x ^. #dest
    -- How should src and dest be related?
    -- Can't express that 'offset + width(src) == width(dest)'
    --  without '+' and '==' as type level operators.
    add [ (r, CSVar v) ]
    addChildConstraints

  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. #src
    add [ (r, CSVar v)
        , (v, CSType $ TBitVector jSz)
        ]
    addChildConstraints
  Pil.VAR_FIELD _ -> do
    -- TODO: can we know anything about src PilVar by looking at offset + result size?
    add [ (r, CSType $ TBitVector jSz) ]
    addChildConstraints

  Pil.VAR_PHI _ -> unimplError

  Pil.VAR_JOIN x -> do
    low <- lookupVarSym $ x ^. #low
    high <- lookupVarSym $ x ^. #high
    add [ (r, CSType $ TBitVector jSz)
        , (low, CSType $ TBitVector jHalfSz)
        , (high, CSType $ TBitVector jHalfSz)
        ]
    addChildConstraints

  Pil.XOR x -> do
    add $ bitVectorBinOp x
    addChildConstraints
  Pil.ZX x -> do
    add =<< integralExtendOp Nothing x
    addChildConstraints

  Pil.STACK_LOCAL_ADDR x -> do
    s <- getStackOffsetSym (x ^. #stackOffset)
    add [(r, CSType $ TPointer jSz (CSVar s))]
    addChildConstraints
  Pil.UNIT -> add [ (r, CSType TUnit) ]

  where
    add = addConstraints
    addChildConstraints = traverse_ addExprTypeConstraints op'
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
                             => Maybe Bool
                             -> x
                             -> ConstraintGen [SymConstraint]
    integralBinOpReturnsBool argSignedness x = do
      let argWidth = Nothing
          argType = CSType $ TInt argWidth argSignedness
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
  addExprTypeConstraints symExpr
  equals pvSym exprSym
  return $ Pil.Def (Pil.DefOp pv symExpr)

addStmtTypeConstraints (Pil.Constraint (Pil.ConstraintOp expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. #info . #sym
  addExprTypeConstraints symExpr
  assignType exprSym TBool
  return $ Pil.Constraint (Pil.ConstraintOp symExpr)

addStmtTypeConstraints (Pil.Store (Pil.StoreOp addrExpr valExpr)) = do
  symAddrExpr <- toSymExpression addrExpr
  symValExpr <- toSymExpression valExpr
  let symAddr = symAddrExpr ^. #info . #sym
      symVal = symValExpr ^. #info . #sym
      ptrWidth = Nothing
  addExprTypeConstraints symAddrExpr
  addExprTypeConstraints symValExpr
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
addStmtTypeConstraints (Pil.Call x) = do
  callOp <- traverse toSymExpression x
  void $ addCallOpConstraints callOp
  traverse_ addExprTypeConstraints callOp
  return $ Pil.Call callOp
addStmtTypeConstraints (Pil.TailCall x) = do
  tailCallOp <- traverse toSymExpression x
  callTgt <- addCallOpConstraints tailCallOp
  traverse_ addExprTypeConstraints tailCallOp
  case x ^. #ret of
    Nothing -> return ()
    Just (retVar, _retSize) -> do
      resultFuncSym <- lookupFuncSym $ FuncResult callTgt
      retSym <- lookupVarSym retVar
      equals retSym resultFuncSym
  return $ Pil.TailCall tailCallOp
addStmtTypeConstraints (Pil.BranchCond (Pil.BranchCondOp expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. #info . #sym
  addExprTypeConstraints symExpr
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
