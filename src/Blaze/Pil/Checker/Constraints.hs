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
constrainStandardFunc _r _sz (Pil.CallOp _ Nothing _) = return Nothing
constrainStandardFunc r sz (Pil.CallOp _ (Just name) cparams) = case name of
  "fgets" -> case cparams of
    [s, size', stream] -> do
      sizeSz <- CSVar <$> newSym
      let ptrWidth = sz'
      return . Just $
        [ ( s ^. #info . #sym, CSType $ TPointer ptrWidth (CSType TChar) )
        , ( size' ^. #info . #sym, CSType . TInt sizeSz . CSType $ TVSign True )
        , ( stream ^. #info . #sym, CSType $ TPointer ptrWidth (CSType TChar) )
        , ( r, CSType $ TPointer ptrWidth (CSType TChar) )
        ]
    _ -> return Nothing --TODO : add warning about malformed fgets params
  "asprintf" -> case cparams of
    (strp:fmt:_) -> do
      ptrWidth <- CSVar <$> newSym
      let ptr = CSType . TPointer ptrWidth
      return . Just $
        [ ( strp ^. #info . #sym, ptr . ptr . CSType $ TChar )
        , ( fmt ^. #info . #sym, ptr . CSType $ TChar)
        , ( r, CSType $ TInt sz' (CSType $ TVSign True) )
        ]
    _ -> return Nothing --TODO : add warning about malformed fgets params

  "strcmp" -> case cparams of
    [a, b] -> do
      ptrWidth <- CSVar <$> newSym
      let str = CSType . TPointer ptrWidth $ CSType TChar

      return . Just $
        [ ( a ^. #info . #sym, str )
        , ( b ^. #info . #sym, str )
        , ( r, CSType $ TInt sz' (CSType $ TVSign True) )
        ]
    _ -> return Nothing --TODO : add warning about malformed fgets params



  _ -> return Nothing

  where
    sz' = CSType $ TVBitWidth sz

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

addConstraint :: (Sym, ConstraintSymType) -> ConstraintGen ()
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

addConstraints :: [(Sym, ConstraintSymType)] -> ConstraintGen ()
addConstraints = mapM_ addConstraint

-- | Generates constraints for all immediate syms in SymExpression.
-- does not get constraints of children exprs.
addExprTypeConstraints :: SymExpression -> ConstraintGen ()
addExprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
  Pil.ADC x -> integralBinOpFirstArgIsReturn Nothing True x >> addCarryConstraint x
  Pil.ADD x -> integralBinOpFirstArgIsReturn Nothing True x

  -- should this be unsigned ret because overflow is always positive?
  Pil.ADD_OVERFLOW x -> integralBinOpFirstArgIsReturn Nothing True x

  Pil.AND x -> bitVectorBinOp x

  -- Arithmetic shift right
  Pil.ASR x -> integralFirstArgIsReturn x

  Pil.BOOL_TO_INT x -> ret
    [ (r, CSType $ TBitVector sz')
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
    --       function type information. This should later use
    --       the BN type library and/or a source that can be 
    --       updated. 
    mxs <- constrainStandardFunc r sz x
    -- TODO: Should the bitvec constraints always be included?
    case mxs of
      Just xs -> addConstraints xs
      Nothing -> addConstraints [(r, CSType $ TBitVector sz')]

  Pil.CEIL x -> floatUnOp x
  Pil.CMP_E x -> integralBinOpReturnsBool x
  Pil.CMP_NE x -> integralBinOpReturnsBool x

  Pil.CMP_SGE x -> signedBinOpReturnsBool True x
  Pil.CMP_SGT x -> signedBinOpReturnsBool True x
  Pil.CMP_SLE x -> signedBinOpReturnsBool True x
  Pil.CMP_SLT x -> signedBinOpReturnsBool True x
  Pil.CMP_UGE x -> signedBinOpReturnsBool False x
  Pil.CMP_UGT x -> signedBinOpReturnsBool False x
  Pil.CMP_ULE x -> signedBinOpReturnsBool False x
  Pil.CMP_ULT x -> signedBinOpReturnsBool False x
  Pil.CONST _ -> ret [(r, CSType $ TBitVector sz')]
  Pil.CONST_PTR _ -> retPointer

  -- TODO: Should the length be Text.length + 1, to account for \0 ?
  Pil.ConstStr x ->
    ret [(r, CSType . TPointer sz'
           . CSType $ TArray
           ( CSType . TVLength . fromIntegral . Text.length
             $ x ^. #value )
           ( CSType TChar ))]

  -- TODO: get param cound and make this pointer point to TFunction
  Pil.ConstFuncPtr _ -> retPointer 

  -- Don't remember if this is correct, or the above
  -- Pil.ConstStr x -> return [(r, CSType $ TArray
  --                               ( CSType . TVLength . fromIntegral . Text.length
  --                                 $ x ^. Pil.value )
  --                               ( CSType TChar ))]

  Pil.CONST_BOOL _ -> retBool
  Pil.DIVS x -> integralBinOpFirstArgIsReturn (Just True) False x
  Pil.DIVS_DP x -> divOrModDP True x
  Pil.DIVU x -> integralBinOpFirstArgIsReturn (Just False) False x
  Pil.DIVU_DP x -> divOrModDP False x
  Pil.FABS x -> floatUnOp x
  Pil.FADD x -> floatBinOp x
  Pil.FCMP_E x -> floatBinOpReturnsBool x
  Pil.FCMP_GE x -> floatBinOpReturnsBool x
  Pil.FCMP_GT x -> floatBinOpReturnsBool x
  Pil.FCMP_LE x -> floatBinOpReturnsBool x
  Pil.FCMP_LT x -> floatBinOpReturnsBool x
  Pil.FCMP_O x -> floatBinOpReturnsBool x
  Pil.FCMP_NE x -> floatBinOpReturnsBool x
  Pil.FCMP_UO x -> floatBinOpReturnsBool x
  Pil.FDIV x -> floatBinOp x

  Pil.FIELD_ADDR x -> do
    fieldType <- CSVar <$> newSym
    let recType = CSType . TRecord . HashMap.fromList $
          -- for now, assuming all offsets are positive...
          [ (byteOffsetToBitOffset $ x ^. #offset, fieldType) ]
    ret [ ( r, CSType $ TPointer sz' fieldType )
        , ( x ^. #baseAddr . #info . #sym, CSType $ TPointer sz' recType )
        ]

  Pil.CONST_FLOAT _ -> retFloat

  Pil.Extract _ -> ret [(r, CSType $ TBitVector sz')]

-- TODO: should there be a link between sz of bitvec and sz of float?
  Pil.FLOAT_CONV x -> do
    bvWidth <- CSVar <$> newSym
    ret [ ( x ^. #src . #info . #sym, CSType $ TBitVector bvWidth )
        , ( r, CSType $ TFloat sz' )
        ]
  Pil.FLOAT_TO_INT x -> floatToInt x
  Pil.FLOOR x -> floatUnOp x
  Pil.FMUL x -> floatBinOp x
  Pil.FNEG x -> floatUnOp x
  Pil.FSQRT x -> floatUnOp x
  Pil.FTRUNC x -> floatUnOp x
  Pil.FSUB x -> floatBinOp x

--   what does IMPORT do?
--   assuming it just casts an Int to a pointer
  Pil.IMPORT _ -> retPointer

  Pil.INT_TO_FLOAT x -> intToFloat x

  Pil.LOAD x -> do
    ptrWidth <- CSVar <$> newSym
    ptrType <- CSVar <$> newSym
    ret [ ( x ^. #src . #info . #sym, CSType $ TPointer ptrWidth ptrType )
        , ( r, ptrType )
        , ( r, CSType $ TBitVector sz' )
        ]

    -- case x ^. Pil.src . op of
    --   Pil.FIELD_ADDR y -> do
    --     ptrWidth <- CSVar <$> newSym
    --     let fieldAddrPtrSym = x ^. Pil.src . info . sym
    --         ptrType = CSType . TRecord $ HashMap.fromList [( byteOffsetToBitOffset $ y ^. Pil.offset
    --                                                        , fieldType)]
    --     return [ ( fieldAddrPtrSym, CSType $ TPointer ptrWidth ptrType )
    --            , ( r, fieldType )
    --            , ( r, CSType $ TBitVector sz' )
    --            ]

    --   _ -> do
    --     ptrWidth <- CSVar <$> newSym
    --     fieldType <- CSVar <$> newSym
    --     let ptrType = CSType $ TZeroField fieldType
    --     return [ ( x ^. Pil.src . info . sym, CSType $ TPointer ptrWidth ptrType )
    --            , ( r, fieldType )
    --            , ( r, CSType $ TBitVector sz' )
    --            ]

  -- should _x have any influence on the type of r?
  Pil.LOW_PART _x -> ret [(r, CSType $ TBitVector sz')]

  Pil.LSL x -> integralFirstArgIsReturn x
  Pil.LSR x -> integralFirstArgIsReturn x
  Pil.MODS x -> integralBinOpFirstArgIsReturn (Just True) False x
  Pil.MODS_DP x -> divOrModDP True x
  Pil.MODU x -> integralBinOpFirstArgIsReturn (Just False) False x
  Pil.MODU_DP x -> divOrModDP False x
  Pil.MUL x -> integralBinOpFirstArgIsReturn Nothing True x
  Pil.MULS_DP x -> mulDP True x
  Pil.MULU_DP x -> mulDP False x
  Pil.NEG x -> integralUnOp (Just True) x

  -- NOT is either Bool -> Bool or BitVec -> BitVec
  -- but no way to express that without typeclasses
  -- so it's just constrained as `a -> a`
  Pil.NOT x -> ret [(r, CSVar $ x ^. #src . #info . #sym)]
    -- bitVectorUnOp x

  Pil.OR x -> bitVectorBinOp x
  Pil.RLC x -> integralFirstArgIsReturn x >> addCarryConstraint x

  Pil.ROL x -> integralFirstArgIsReturn x
  Pil.ROR x -> integralFirstArgIsReturn x
  Pil.ROUND_TO_INT x -> floatToInt x
  Pil.RRC x -> integralFirstArgIsReturn x >> addCarryConstraint x

  Pil.SBB x -> integralBinOpFirstArgIsReturn (Just True) True x >> addCarryConstraint x
--   -- STORAGE _ -> unknown
--   StrCmp _ -> intRet
--   StrNCmp _ -> intRet
--   MemCmp _ -> intRet

  -- should this somehow be linked to the type of the stack var?
  -- its type could change every Store.
  Pil.STACK_LOCAL_ADDR x -> do
    s <- getStackOffsetSym $ x ^. #stackOffset
    ret [ (r, CSType $ TPointer sz' (CSVar s)) ]

  Pil.SUB x -> integralBinOpFirstArgIsReturn (Just True) True x
  Pil.SX x -> integralExtendOp x

  Pil.TEST_BIT _ -> retBool -- ? tests if bit in int is on or off
  Pil.UNIMPL _ -> ret [ (r, CSType $ TBitVector sz' ) ]
  Pil.UPDATE_VAR x -> do
    v <- lookupVarSym $ x ^. #dest
    -- How should src and dest be related?
    -- Can't express that `offset + width(src) == width(dest)`
    --  without `+` and `==` as type level operators.
    ret [ (r, CSVar v) ]

  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. #src
    ret [ (r, CSVar v)
        , (v, CSType $ TBitVector sz')
        ]
  Pil.VAR_FIELD _ ->
    -- TODO: can we know anything about src PilVar by looking at offset + result size?
    ret [ (r, CSType $ TBitVector sz') ]

  Pil.VAR_JOIN x -> do
    low <- lookupVarSym $ x ^. #low
    high <- lookupVarSym $ x ^. #high
    ret [ (r, CSType $ TBitVector sz')
        , (low, CSType $ TBitVector halfsz')
        , (high, CSType $ TBitVector halfsz')
        ]

  Pil.XOR x -> bitVectorBinOp x
  Pil.ZX x -> integralExtendOp x

  Pil.UNIT -> ret [ (r, CSType TUnit) ]

  _ -> P.error . show $ op'
    --throwError UnhandledExpr
  where
    ret = addConstraints
    sz' = CSType $ TVBitWidth sz
    halfsz' = CSType . TVBitWidth $ sz `div` 2
    sz2x' = CSType . TVBitWidth $ sz * 2
    getBoolRet = return $ CSType TBool -- TODO: doesn't need to be a monad anymore...
    retBool = addConstraints [ (r, CSType TBool) ]

    retFloat = ret [ (r, CSType $ TFloat sz') ]

    retPointer = do
      pt <- newSym
      ret [ (r, CSType (TPointer sz' (CSVar pt))) ]

    addCarryConstraint :: (HasField' "carry" x SymExpression) => x -> ConstraintGen ()
    addCarryConstraint x =
      ret [ (x ^. field' @"carry" . #info . #sym, CSType TBool) ]

    bitVectorBinOp :: ( HasField' "left" x SymExpression
                      , HasField' "right" x SymExpression)
                   => x -> ConstraintGen ()
    bitVectorBinOp x =
      ret [ (r, CSType $ TBitVector sz')
          , (r, CSVar $ x ^. field' @"left" . #info . #sym)
          , (r, CSVar $ x ^. field' @"right" . #info . #sym)
          ]

    integralExtendOp :: (HasField' "src" x SymExpression) => x -> ConstraintGen ()
    integralExtendOp x = do
      argSizeType <- CSVar <$> newSym
      signednessType <- CSVar <$> newSym
      ret [ (r, CSType $ TInt sz' signednessType)
          , (x ^. field' @"src" . #info . #sym, CSType $ TInt argSizeType signednessType)
          ]

    integralUnOp :: (HasField' "src" x SymExpression)
                 => Maybe Bool
                 -> x
                 -> ConstraintGen ()
    integralUnOp mSignedness x = do
      signednessType <- case mSignedness of
        Nothing -> CSVar <$> newSym
        Just b -> return . CSType . TVSign $ b
      ret [ (r, CSType (TInt sz' signednessType))
          , (r, CSVar $ x ^. field' @"src" . #info . #sym)
          ]

    integralBinOpFirstArgIsReturn :: ( HasField' "left" x SymExpression
                                     , HasField' "right" x SymExpression)
                                  => Maybe Bool -> Bool -> x
                                  -> ConstraintGen ()
    integralBinOpFirstArgIsReturn mSignedness secondArgSameWidth x = do
      signednessType <- case mSignedness of
        Nothing -> CSVar <$> newSym
        Just b -> return . CSType . TVSign $ b
      arg2Sign <- CSVar <$> newSym
      secondArgWidth <- bool (CSVar <$> newSym) (return sz') secondArgSameWidth
      ret [ (r, CSType (TInt sz' signednessType))
          , (r, CSVar $ x ^. field' @"left" . #info . #sym)
          , (x ^. field' @"right" . #info . #sym, CSType $ TInt secondArgWidth arg2Sign)
          ]

    mulDP :: ( HasField' "left" x SymExpression
             , HasField' "right" x SymExpression)
          => Bool
          -> x
          -> ConstraintGen ()
    mulDP signedness x = do
      let retSignednessType = CSType . TVSign $ signedness
      rightSignedness <- CSVar <$> newSym
      ret [ (r, CSType (TInt sz' retSignednessType))
          , (x ^. field' @"left" . #info . #sym, CSType $ TInt halfsz' retSignednessType)
          , (x ^. field' @"right" . #info . #sym, CSType $ TInt halfsz' rightSignedness)
          ]

    divOrModDP :: ( HasField' "left" x SymExpression
                  , HasField' "right" x SymExpression)
               => Bool
               -> x
               -> ConstraintGen ()
    divOrModDP signedness x = do
      let retSignednessType = CSType . TVSign $ signedness
      rightSignedness <- CSVar <$> newSym
      ret [ (r, CSType (TInt sz' retSignednessType))
          , (x ^. field' @"left" . #info . #sym, CSType $ TInt sz2x' retSignednessType)
          , (x ^. field' @"right" . #info . #sym, CSType $ TInt sz' rightSignedness)
          ]

    integralBinOpReturnsBool :: ( HasField' "left" x SymExpression
                                , HasField' "right" x SymExpression )
                             => x
                             -> ConstraintGen ()
    integralBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      argSignSym <- newSym
      let argType = CSType (TInt (CSVar argWidthSym) (CSVar argSignSym))
      ret [ ( r, b )
          , ( x ^. field' @"left" . #info . #sym, argType )
          , ( x ^. field' @"right". #info . #sym, argType )
          ]

    signedBinOpReturnsBool :: ( HasField' "left" x SymExpression
                              , HasField' "right" x SymExpression )
                           => Bool
                           -> x
                           -> ConstraintGen ()
    signedBinOpReturnsBool isSignedBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      ret [ (r, b)
          , (x ^. field' @"left". #info . #sym, CSType (TInt (CSVar argWidthSym) (CSType $ TVSign isSignedBool)))
          , (x ^. field' @"right" . #info . #sym, CSType (TInt (CSVar argWidthSym) (CSType $ TVSign isSignedBool)))
          ]

    intToFloat :: (HasField' "src" x SymExpression)
               => x
               -> ConstraintGen ()
    intToFloat x = do
      intWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      ret [ ( x ^. field' @"src" . #info . #sym, CSType $ TInt intWidth intSign )
          , ( r, CSType $ TFloat sz' )
          ]


    floatToInt :: (HasField' "src" x SymExpression)
               => x
               -> ConstraintGen ()
    floatToInt x = do
      floatWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      ret [ ( x ^. field' @"src" . #info . #sym, CSType $ TFloat floatWidth )
          , ( r, CSType $ TInt sz' intSign )
          ]

    floatBinOp :: ( HasField' "left" x SymExpression
                  , HasField' "right" x SymExpression )
               => x
               -> ConstraintGen ()
    floatBinOp x = do
      ret [ (r, CSType (TFloat sz'))
          , (r, CSVar $ x ^. field' @"left" . #info . #sym)
          , (r, CSVar $ x ^. field' @"right" . #info . #sym)
          ]

    floatUnOp :: (HasField' "src" x SymExpression)
              => x
              -> ConstraintGen ()
    floatUnOp x = do
      ret [ (r, CSType (TFloat sz'))
          , (r, CSVar $ x ^. field' @"src" . #info . #sym)
          ]

    floatBinOpReturnsBool :: (HasField' "left" x SymExpression, HasField' "right" x SymExpression)
                          => x
                          -> ConstraintGen ()
    floatBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      let argType = CSType (TFloat $ CSVar argWidthSym)
      ret [ ( r, b )
          , ( x ^. field' @"left" . #info . #sym, argType )
          , ( x ^. field' @"right" . #info . #sym, argType )
          ]

    integralFirstArgIsReturn :: (HasField' "left" x SymExpression, HasField' "right" x SymExpression)
                             => x
                             -> ConstraintGen ()
    integralFirstArgIsReturn x = do
      intWidth <- CSVar <$> newSym
      shifterWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      shifterSign <- CSVar <$> newSym
      let n = CSType $ TInt intWidth intSign
      ret [ ( x ^. field' @"left". #info . #sym, n )
          , ( x ^. field' @"right" . #info . #sym
            , CSType $ TInt shifterWidth shifterSign )
          , ( r, n )
          ]

-- | Generates constraints for all syms in SymExpression and adds them to state.
-- does NOT recurse down sub-expressions like addAllExprTypeConstraints
-- addExprTypeConstraints :: SymExpression -> ConstraintGen ()
-- addExprTypeConstraints = mapM_ addConstraint' <=< exprTypeConstraints

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
      sexpr = InfoExpression (SymInfo bitSize s) symOp
  addSymExpression s sexpr
  return sexpr

-- | Generate all rules for a stmt, including subexpressions
-- and add them to state
-- also create a `Statement SymExpression`
-- make sure to set the `currentStmt` index before calling this
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
  ptrWidth <- newSym
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
