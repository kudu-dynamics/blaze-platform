{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.Checker.Constraints where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Prelude as P
import Blaze.Types.Pil ( Expression(Expression)
                       , Statement
                       , PilVar
                       , StackOffset
                       )
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Blaze.Pil.Analysis as Analysis
import qualified Data.Text as Text
import Blaze.Types.Pil.Checker


constrainStandardFunc :: Sym -> BitWidth -> Pil.CallOp SymExpression -> ConstraintGen (Maybe [(Sym, ConstraintSymType)])
constrainStandardFunc _r _sz (Pil.CallOp _ Nothing _) = return Nothing
constrainStandardFunc r sz (Pil.CallOp _ (Just name) cparams) = case name of
  "fgets" -> case cparams of
    [s, size', stream] -> do
      sizeSz <- CSVar <$> newSym
      let ptrWidth = sz'
      return . Just $
        [ ( s ^. info . sym, CSType $ TPointer ptrWidth (CSType TChar) )
        , ( size' ^. info . sym, CSType . TInt sizeSz . CSType $ TVSign True )
        , ( stream ^. info . sym, CSType $ TPointer ptrWidth (CSType TChar) )
        , ( r, CSType $ TPointer ptrWidth (CSType TChar) )
        ]
    _ -> return Nothing --TODO : add warning about malformed fgets params
  "asprintf" -> case cparams of
    (strp:fmt:_) -> do
      ptrWidth <- CSVar <$> newSym
      let ptr = CSType . TPointer ptrWidth
      return . Just $
        [ ( strp ^. info . sym, ptr . ptr $ CSType TChar )
        , ( fmt ^. info . sym, ptr $ CSType TChar )
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
  m <- use stackAddrSymMap
  case HashMap.lookup k m of
    Nothing -> do
      s <- newSym
      stackAddrSymMap %= HashMap.insert k s
      return s
    Just s -> return s

addVarSym :: PilVar -> Sym -> ConstraintGen ()
addVarSym pv sym' = varSymMap %= HashMap.insert pv sym'

-- | Create mapping of each PilVar to a symbol
createVarSymMap :: [Statement Expression] -> ConstraintGen ()
createVarSymMap stmts' = do
  let vars = Analysis.getAllVars stmts'
  mapM_ f $ HashSet.toList vars
  where
    f var = newSym >>= addVarSym var

incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1

-- | Creates new, unused Sym
newSym :: ConstraintGen Sym
newSym = do
  x <- use currentSym
  currentSym %= incrementSym
  return x

-- | get pilvar's cooresponding Sym from state
lookupVarSym :: PilVar -> ConstraintGen Sym
lookupVarSym pv = do
  vsm <- use varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> throwError $ CannotFindPilVarInVarSymMap pv
    Just s -> return s

lookupSymExpr :: Sym -> ConstraintGen SymExpression
lookupSymExpr sym' = do
  m <- use symMap
  case HashMap.lookup sym' m of
    Nothing -> throwError $ CannotFindSymInSymMap
    Just x -> return x

addSymExpression :: Sym -> SymExpression -> ConstraintGen ()
addSymExpression sym' x = symMap %= HashMap.insert sym' x

byteOffsetToBitOffset :: ByteOffset -> BitOffset
byteOffsetToBitOffset (ByteOffset n) = BitOffset $ n * 8

addConstraint' :: (Sym, ConstraintSymType) -> ConstraintGen ()
addConstraint' (s1, CSVar s2) = addConstraint_ s1 $ SVar s2
addConstraint' (s, CSType t) = do
  t' <- traverse f t
  addConstraint_ s $ SType t'
  where
    f :: ConstraintSymType -> ConstraintGen Sym
    f (CSVar v) = return v
    f (CSType ptc) = do
      x <- newSym
      pt <- traverse f ptc
      addConstraint_ x . SType $ pt
      return x


-- | Generates constraints for all syms in SymExpression.
-- TODO: Convert this to use regular ConstraintSymType and add constraints directly
exprTypeConstraints :: SymExpression -> ConstraintGen [(Sym, ConstraintSymType)]
exprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
  Pil.ADC x -> (<>) <$> integralBinOpFirstArgIsReturn Nothing True x
                    <*> carryConstraint x
  Pil.ADD x -> integralBinOpFirstArgIsReturn Nothing True x

  -- should this be unsigned ret because overflow is always positive?
  Pil.ADD_OVERFLOW x -> integralBinOpFirstArgIsReturn Nothing True x

  Pil.AND x -> bitVectorBinOp x

  --   shift right...?
  Pil.ASR x -> integralFirstArgIsReturn x

--   BOOL_TO_INT _ -> 

  -- TODO get most general type for this and args:
  Pil.CALL x -> do
    mxs <- constrainStandardFunc r sz x
    -- should it always return additional bitvec constraint for ret?
    maybe (return [ (r, CSType $ TBitVector sz') ]) return mxs

    -- this one always includes bitvec width constraint on ret
    -- ((r, CSType $ TBitVector sz') :) <$> maybe (return []) return mxs
  
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
  Pil.CONST _ -> return [(r, CSType $ TBitVector sz')]
  Pil.CONST_PTR _ -> retPointer
  Pil.ConstStr x -> return [(r, CSType $ TArray
                                ( CSType . TVLength . fromIntegral . Text.length
                                  $ x ^. Pil.value )
                                ( CSType TChar ))]
  Pil.DIVS x -> integralBinOpFirstArgIsReturn (Just True) False x
  Pil.DIVS_DP x -> integralBinOpDP (Just True) x
  Pil.DIVU x -> integralBinOpFirstArgIsReturn (Just False) False x
  Pil.DIVU_DP x -> integralBinOpDP (Just False) x
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
          [ (byteOffsetToBitOffset $ x ^. Pil.offset, fieldType) ]
    return [ ( r, CSType $ TPointer sz' fieldType )
           , ( x ^. Pil.baseAddr . info . sym, CSType $ TPointer sz' recType )
           ]

  Pil.FLOAT_CONST _ -> retFloat

-- TODO: should there be a link between sz of bitvec and sz of float?
  Pil.FLOAT_CONV x -> do
    bvWidth <- CSVar <$> newSym
    return [ ( x ^. Pil.src . info . sym, CSType $ TBitVector bvWidth )
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
    return [ ( x ^. Pil.src . info . sym, CSType $ TPointer ptrWidth ptrType )
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
  Pil.LOW_PART _x -> return [(r, CSType $ TBitVector sz')]

  Pil.LSL x -> integralFirstArgIsReturn x
  Pil.LSR x -> integralFirstArgIsReturn x
  Pil.MODS x -> integralBinOpFirstArgIsReturn (Just True) False x
  Pil.MODS_DP x -> integralBinOpDP (Just True) x
  Pil.MODU x -> integralBinOpFirstArgIsReturn (Just False) False x
  Pil.MODU_DP x -> integralBinOpDP (Just False) x
  Pil.MUL x -> integralBinOpFirstArgIsReturn Nothing True x
  Pil.MULS_DP x -> integralBinOpDP (Just True) x
  Pil.MULU_DP x -> integralBinOpDP (Just False) x
  Pil.NEG x -> integralUnOp (Just True) x

  -- NOT is either Bool -> Bool or BitVec -> BitVec
  -- but no way to express that without typeclasses
  -- so it's just constrained as `a -> a`
  Pil.NOT x -> return [(r, CSVar $ x ^. Pil.src . info . sym)]
    -- bitVectorUnOp x

  Pil.OR x -> bitVectorBinOp x
  Pil.RLC x -> (<>) <$> integralFirstArgIsReturn x
                    <*> carryConstraint x
  
  Pil.ROL x -> integralFirstArgIsReturn x
  Pil.ROR x -> integralFirstArgIsReturn x
  Pil.ROUND_TO_INT x -> floatToInt x
  Pil.RRC x -> (<>) <$> integralFirstArgIsReturn x
                    <*> carryConstraint x

  Pil.SBB x -> (<>) <$> integralBinOpFirstArgIsReturn (Just True) True x
                    <*> carryConstraint x
--   -- STORAGE _ -> unknown
--   StrCmp _ -> intRet
--   StrNCmp _ -> intRet
--   MemCmp _ -> intRet

  -- should this somehow be linked to the type of the stack var?
  -- its type could change every Store.
  Pil.STACK_LOCAL_ADDR x -> do
    s <- getStackOffsetSym $ x ^. Pil.stackOffset
    return [ (r, CSType $ TPointer sz' (CSVar s)) ]

  Pil.SUB x -> integralBinOpFirstArgIsReturn (Just True) True x
  Pil.SX x -> integralExtendOp x

  Pil.TEST_BIT _ -> retBool -- ? tests if bit in int is on or off
  Pil.UNIMPL _ -> return [ (r, CSType $ TBitVector sz' ) ]
  Pil.UPDATE_VAR x -> do
    v <- lookupVarSym $ x ^. Pil.dest
    -- How should src and dest be related?
    -- Can't express that `offset + width(src) == width(dest)`
    --  without `+` and `==` as type level operators.
    return [ (r, CSVar v) ]

  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. Pil.src
    return [ (r, CSVar v)
           , (v, CSType $ TBitVector sz')
           ]
  Pil.VAR_FIELD _ ->
    -- TODO: can we know anything about src PilVar by looking at offset + result size?
    return [ (r, CSType $ TBitVector sz') ]

  -- binja is wrong about the size of VarSplit.
  Pil.VAR_SPLIT x -> do
    low <- lookupVarSym $ x ^. Pil.low
    high <- lookupVarSym $ x ^. Pil.high
    return [ (r, CSType $ TBitVector sz2x')
           , (low, CSType $ TBitVector sz')
           , (high, CSType $ TBitVector sz')
           ]

  Pil.XOR x -> bitVectorBinOp x
  Pil.ZX x -> integralExtendOp x
--   -- _ -> unknown

--   Extract _ -> bitvecRet
  _ -> P.error . show $ op'
    --throwError UnhandledExpr
  where
    sz' = CSType $ TVBitWidth sz
    sz2x' = CSType . TVBitWidth $ sz * 2
    getBoolRet = return $ CSType TBool -- TODO: doesn't need to be a monad anymore...
    retBool = return [ (r, CSType TBool) ]

    retFloat :: ConstraintGen [(Sym, ConstraintSymType)]
    retFloat = return [ (r, CSType $ TFloat sz') ]

    retPointer = do
      pt <- newSym
      return [ (r, CSType (TPointer sz' (CSVar pt))) ]

    carryConstraint :: (Pil.HasCarry x SymExpression) => x -> ConstraintGen [(Sym, ConstraintSymType)]
    carryConstraint x =
      return [ (x ^. Pil.carry . info . sym, CSType $ TBool) ]


    -- bitVectorUnOp :: (Pil.HasSrc x SymExpression) => x -> ConstraintGen [(Sym, ConstraintSymType)]
    -- bitVectorUnOp x =
    --   return [ (r, CSType $ TBitVector sz')
    --          , (r, CSVar $ x ^. Pil.src . info . sym)
    --          ]

    bitVectorBinOp :: ( Pil.HasLeft x SymExpression
                      , Pil.HasRight x SymExpression)
                   => x -> ConstraintGen [(Sym, ConstraintSymType)]
    bitVectorBinOp x =
      return [ (r, CSType $ TBitVector sz')
             , (r, CSVar $ x ^. Pil.left . info . sym)
             , (r, CSVar $ x ^. Pil.right . info . sym)
             ]

    integralExtendOp :: (Pil.HasSrc x SymExpression) => x -> ConstraintGen [(Sym, ConstraintSymType)]
    integralExtendOp x = do
      argSizeType <- CSVar <$> newSym
      signednessType <- CSVar <$> newSym
      return [ (r, CSType $ TInt sz' signednessType)
             , (x ^. Pil.src . info . sym, CSType $ TInt argSizeType signednessType)
             ]


    integralUnOp :: (Pil.HasSrc x SymExpression)
                 => Maybe Bool
                 -> x
                 -> ConstraintGen [(Sym, ConstraintSymType)]
    integralUnOp mSignedness x = do
      signednessType <- case mSignedness of
        Nothing -> CSVar <$> newSym
        Just b -> return . CSType . TVSign $ b
      return [ (r, CSType (TInt sz' signednessType))
             , (r, CSVar $ x ^. Pil.src . info . sym)
             ]

    integralBinOpFirstArgIsReturn :: ( Pil.HasLeft x SymExpression
                                     , Pil.HasRight x SymExpression)
                                  => Maybe Bool -> Bool -> x
                                  -> ConstraintGen [(Sym, ConstraintSymType)]
    integralBinOpFirstArgIsReturn mSignedness secondArgSameWidth x = do
      signednessType <- case mSignedness of
        Nothing -> CSVar <$> newSym
        Just b -> return . CSType . TVSign $ b
      arg2Sign <- CSVar <$> newSym
      secondArgWidth <- bool (CSVar <$> newSym) (return sz') secondArgSameWidth
      return [ (r, CSType (TInt sz' signednessType))
             , (r, CSVar $ x ^. Pil.left . info . sym)
             , (x ^. Pil.right . info . sym, CSType $ TInt secondArgWidth arg2Sign)
             ]


    -- integralBinOpUnrelatedArgs :: ( Pil.HasLeft x SymExpression
    --                               , Pil.HasRight x SymExpression)
    --                            => Maybe Bool
    --                            -> x
    --                            -> ConstraintGen [(Sym, ConstraintSymType)]
    -- integralBinOpUnrelatedArgs mSignedness x = do
    --   signednessType <- case mSignedness of
    --     Nothing -> CSVar <$> newSym
    --     Just b -> return . CSType . TVSign $ b
    --   arg1Sign <- CSVar <$> newSym
    --   arg2Sign <- CSVar <$> newSym
    --   arg1Width <- CSVar <$> newSym
    --   arg2Width <- CSVar <$> newSym
    --   return [ (r, CSType (TInt sz' signednessType))
    --          , (x ^. Pil.left . info . sym, CSType $ TInt arg1Width arg1Sign)
    --          , (x ^. Pil.right . info . sym, CSType $ TInt arg2Width arg2Sign)
    --          ]

    -- integralBinOp :: ( Pil.HasLeft x SymExpression
    --                  , Pil.HasRight x SymExpression)
    --               => Maybe Bool
    --               -> x
    --               -> ConstraintGen [(Sym, ConstraintSymType)]
    -- integralBinOp mSignedness x = do
    --   signednessType <- case mSignedness of
    --     Nothing -> CSVar <$> newSym
    --     Just b -> return . CSType . TVSign $ b
    --   return [ (r, CSType (TInt sz' signednessType))
    --          , (r, CSVar $ x ^. Pil.left . info . sym)
    --          , (r, CSVar $ x ^. Pil.right . info . sym)
    --          ]

    -- first arg is double-precision of second and return
    -- signedness of args can apparently be anything
    integralBinOpDP :: ( Pil.HasLeft x SymExpression
                       , Pil.HasRight x SymExpression)
                    => Maybe Bool
                    -> x
                    -> ConstraintGen [(Sym, ConstraintSymType)]
    integralBinOpDP mSignedness x = do
      retSignednessType <- case mSignedness of
        Nothing -> CSVar <$> newSym
        Just b -> return . CSType . TVSign $ b
      return [ (r, CSType (TInt sz' retSignednessType))
             , (x ^. Pil.left . info . sym, CSType $ TInt sz2x' retSignednessType)
             , (r, CSVar $ x ^. Pil.right . info . sym)
             ]


    integralBinOpReturnsBool :: ( Pil.HasLeft x SymExpression
                                , Pil.HasRight x SymExpression ) 
                             => x
                             -> ConstraintGen [(Sym, ConstraintSymType)]
    integralBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      argSignSym <- newSym
      let argType = CSType (TInt (CSVar argWidthSym) (CSVar argSignSym))
      return [ ( r, b )
             , ( x ^. Pil.left . info . sym, argType )
             , ( x ^. Pil.right . info . sym, argType )
             ]

    signedBinOpReturnsBool :: ( Pil.HasLeft x SymExpression
                              , Pil.HasRight x SymExpression )
                           => Bool
                           -> x
                           -> ConstraintGen [(Sym, ConstraintSymType)]
    signedBinOpReturnsBool isSignedBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      return [ (r, b)
             , (x ^. Pil.left . info . sym, CSType (TInt (CSVar argWidthSym) (CSType $ TVSign isSignedBool)))
             , (x ^. Pil.right . info . sym, CSType (TInt (CSVar argWidthSym) (CSType $ TVSign isSignedBool)))
             ]

    intToFloat :: (Pil.HasSrc x SymExpression)
               => x
               -> ConstraintGen [(Sym, ConstraintSymType)]
    intToFloat x = do
      intWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      return [ ( x ^. Pil.src . info . sym, CSType $ TInt intWidth intSign )
             , ( r, CSType $ TFloat sz' )
             ]


    floatToInt :: (Pil.HasSrc x SymExpression)
               => x
               -> ConstraintGen [(Sym, ConstraintSymType)]
    floatToInt x = do
      floatWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      return [ ( x ^. Pil.src . info . sym, CSType $ TFloat floatWidth )
             , ( r, CSType $ TInt sz' intSign )
             ]

    floatBinOp :: ( Pil.HasLeft x SymExpression
                  , Pil.HasRight x SymExpression )
               => x
               -> ConstraintGen [(Sym, ConstraintSymType)]
    floatBinOp x = do
      return [ (r, CSType (TFloat sz'))
             , (r, CSVar $ x ^. Pil.left . info . sym)
             , (r, CSVar $ x ^. Pil.right . info . sym)
             ]

    floatUnOp :: (Pil.HasSrc x SymExpression)
              => x
              -> ConstraintGen [(Sym, ConstraintSymType)]
    floatUnOp x = do
      return [ (r, CSType (TFloat sz'))
             , (r, CSVar $ x ^. Pil.src . info . sym)
             ]

    floatBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                          => x
                          -> ConstraintGen [(Sym, ConstraintSymType)]
    floatBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      let argType = CSType (TFloat $ CSVar argWidthSym)
      return [ ( r, b )
             , ( x ^. Pil.left . info . sym, argType )
             , ( x ^. Pil.right . info . sym, argType )
             ]

    integralFirstArgIsReturn :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                             => x
                             -> ConstraintGen [(Sym, ConstraintSymType)]
    integralFirstArgIsReturn x = do
      intWidth <- CSVar <$> newSym
      shifterWidth <- CSVar <$> newSym
      intSign <- CSVar <$> newSym
      shifterSign <- CSVar <$> newSym
      let n = CSType $ TInt intWidth intSign
      return [ ( x ^. Pil.left . info . sym, n )
             , ( x ^. Pil.right . info . sym
               , CSType $ TInt shifterWidth shifterSign )
             , ( r, n )
             ]


-- | Generates constraints for all syms in SymExpression and adds them to state.
-- does NOT recurse down sub-expressions like addAllExprTypeConstraints
addExprTypeConstraints :: SymExpression -> ConstraintGen ()
addExprTypeConstraints = mapM_ addConstraint' <=< exprTypeConstraints


-- | Recursively adds type constraints for all expr sym's in SymExpression,
-- including nested syms.
addAllExprTypeConstraints :: SymExpression -> ConstraintGen ()
addAllExprTypeConstraints x@(InfoExpression (SymInfo _ _thisExprSym) op') = do
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


-- | get all rules for a stmt, including subexpressions
-- and add them to state
-- also create a `Statement SymExpression`
-- make sure to set the `currentStmt` index before calling this
addStmtTypeConstraints :: Statement Expression
                       -> ConstraintGen (Statement SymExpression)
addStmtTypeConstraints (Pil.Def (Pil.DefOp pv expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. info . sym
  pvSym <- lookupVarSym pv
  addAllExprTypeConstraints symExpr
  addConstraint_ pvSym $ SVar exprSym
  return $ Pil.Def (Pil.DefOp pv symExpr)
addStmtTypeConstraints (Pil.Constraint (Pil.ConstraintOp expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. info . sym
  addAllExprTypeConstraints symExpr
  addConstraint_ exprSym $ SType TBool
  return $ Pil.Constraint (Pil.ConstraintOp symExpr)
addStmtTypeConstraints (Pil.Store (Pil.StoreOp addrExpr valExpr)) = do
  symAddrExpr <- toSymExpression addrExpr
  symValExpr <- toSymExpression valExpr
  let symAddr = symAddrExpr ^. info . sym
      symVal = symValExpr ^. info . sym
  addAllExprTypeConstraints symAddrExpr
  addAllExprTypeConstraints symValExpr
  ptrWidth <- newSym
  addConstraint_ symAddr . SType $ TPointer ptrWidth symVal
  return $ Pil.Store (Pil.StoreOp symAddrExpr symValExpr)


  -- symAddrExpr <- toSymExpression addrExpr
  -- symValExpr <- toSymExpression valExpr
  -- let symAddr = symAddrExpr ^. info . sym
  --     symVal = symValExpr ^. info . sym
  -- addAllExprTypeConstraints symAddrExpr
  -- addAllExprTypeConstraints symValExpr
  -- ptrWidth <- newSym
  -- addConstraint' (symAddr, CSType $ TPointer (CSVar ptrWidth)
  --                          (CSType . TZeroField . CSVar $ symVal))
  -- return $ Pil.Store (Pil.StoreOp symAddrExpr symValExpr)
addStmtTypeConstraints s = traverse toSymExpression s
