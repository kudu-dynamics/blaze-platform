{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Blaze.Solver where

import Blaze.Prelude

import qualified Prelude as P
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Inference as Inference
import Blaze.Types.Pil ( Expression( Expression )
                       , Stmt
                       , PilVar
                       , TypeEnv(TypeEnv)
                       , HasLeft
                       , HasRight
                       )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Solver
import qualified Data.SBV.Dynamic as D
import Data.SBV (SWord, SInt, fromSized, toSized, FromSized, ToSized)
--import qualified Data.SBV as SBV (fromSized, toSized)
import Data.SBV.Internals (SBV(SBV, unSBV))
import Data.SBV.Dynamic (SVal)
import qualified Data.SBV.Trans as SBV
import Data.SBV.Trans (Symbolic)
import Data.SBV.String as SS
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text as Text
import qualified Binja.Function as Func
import qualified Blaze.Solver.Op as Op
import qualified Z3.Monad as Z3
import Z3.Monad (Z3, MonadZ3)

add5 :: SWord16 -> SWord16
add5 n = n + 5

pilVarName :: PilVar -> Text
pilVarName pv = pv ^. Pil.symbol
  <> maybe "" (("@"<>) . view Func.name) (pv ^. Pil.func)
  <> maybe "" (("."<>) . show . f) (pv ^. Pil.ctxIndex)
  where
    f (Pil.CtxIndex n) = n



makeSymVar :: PilVar -> Pil.Type -> Solver SymExpr
makeSymVar pv pt = case pt of
  Pil.TBool -> SymBool <$> exists nm
  (Pil.TBitVec x) -> createWord $ x ^. Pil.width
  (Pil.TInt x) -> bool (createWord w) (createInt w) $ x ^. Pil.signed
    where w = x ^. Pil.width
  -- Float is ignoring the width. Could also do a SDouble...
  (Pil.TFloat _) -> SymFloat <$> exists nm
  -- I think we need dependent types to store
  -- and recall an Array SymExpr
  (Pil.TArray _) -> err ArrayTypeNotYetSupported

  -- TODO: base on size_t instead of always 64bit.
  -- Ptr's should be turned into other things (like Arrays or Strings)
  -- eventually
  (Pil.TPtr _) -> SymWord64 <$> exists nm
  (Pil.TField _) -> err FieldTypeNotYetSupported
  Pil.TString -> SymString <$> exists nm
  (Pil.TObs _) -> err EncounteredObsType
  (Pil.TFunc _) -> err FuncTypeNotYetSupported
  
  where
    err = throwError . SymVarConversionError pv pt

    createWord 1 = SymWord8 <$> exists nm
    createWord 2 = SymWord16 <$> exists nm
    createWord 4 = SymWord32 <$> exists nm
    createWord 8 = SymWord64 <$> exists nm
    createWord n = err $ UnrecognizedWordWidth n

    createInt :: Int -> Solver SymExpr
    createInt 1 = SymInt8 <$> exists nm
    createInt 2 = SymInt16 <$> exists nm
    createInt 4 = SymInt32 <$> exists nm
    createInt 8 = SymInt64 <$> exists nm
    createInt n = err $ UnrecognizedIntWidth n
      
    nm = Text.unpack $ pilVarName pv

checkSat :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError SolverResult)
checkSat s m = runSolver s $ do
  initVarMap
  _ <- m
  initVarMap
  _ <- m
  liftSolverT . query $ do
    csat <- SBV.checkSat
    case csat of
      SBV.Unsat -> return Unsat
      SBV.Unk -> return Unk
      SBV.Sat -> return Sat

checkSat_ :: Solver () -> IO (Either SolverError SolverResult)
checkSat_ = checkSat (emptyState, emptyCtx)

checkSatWithSolution :: (SolverState, SolverCtx) -> Solver () -> IO (Either SolverError SolutionResult)
checkSatWithSolution s m = runSolver s $ do
  initVarMap
  _ <- m
  vm <- use varMap
  liftSolverT . query $ do
    csat <- SBV.checkSat
    case csat of
      SBV.Unsat -> return SUnsat
      SBV.Unk -> return SUnk
      SBV.Sat -> do
        SSat <$> getSolutions vm

initVarMap :: Solver ()
initVarMap = do
  (TypeEnv te) <- typeEnv <$> ask
  let pvts = HashMap.toList te
  vars <- mapM (\ (pv, pt) -> (pv,) <$> makeSymVar pv pt) pvts
  varMap .= HashMap.fromList vars
  return ()



literalToSymExpr :: Integral a => Pil.Type -> a -> Maybe SymExpr
literalToSymExpr et n = do
  w <- Pil.getTypeWidth et
  s <- Pil.getSignedness et
  case (w, s) of
    (1, False) -> return . SymWord8 . SBV.literal . fromIntegral $ n
    (2, False) -> return . SymWord16 . SBV.literal . fromIntegral $ n
    (4, False) -> return . SymWord32 . SBV.literal . fromIntegral $ n
    (8, False) -> return . SymWord64 . SBV.literal . fromIntegral $ n
    (1, True) -> return . SymInt8 . SBV.literal . fromIntegral $ n
    (2, True) -> return . SymInt16 . SBV.literal . fromIntegral $ n
    (4, True) -> return . SymInt32 . SBV.literal . fromIntegral $ n
    (8, True) -> return . SymInt64 . SBV.literal . fromIntegral $ n
    _ -> Nothing


-- binOp :: (HasLeft a Expression, HasRight a Expression) => 

-- getExprType :: Solver SymExpr

-- because Expression is opaque and doesn't have specific return type
-- the Solver return type has to be the opaque SymExpr
-- maybe Expression should be converted to some type correct GADT
-- first...
solveExpr :: Expression -> Solver SymExpr
solveExpr expr@(Expression sz xop) = do
  tenv <- typeEnv <$> ask
  et <- maybe (throwError $ UknownExpectedType expr) return
        $ Inference.getExprType tenv expr

  let error :: SolverError -> Solver a
      error = throwError . ExpressionConversionError expr et

      mapError :: Solver a -> Solver a
      mapError = flip catchError error
      
      todo = error OpNotYetSupported

      binFloat :: (forall a. IEEEFloating a => SBV a -> SBV a -> SBV a)
               -> SymExpr -> SymExpr -> Solver SymExpr
      binFloat f a b = do
        bool (error $ ArgsAndRetNotTheSameType (symType a) (symType b)) (return ())
          $ sameType a b && sameType a et
        case (a, b) of
          ((SymFloat a'), (SymFloat b')) -> return . SymFloat $ f a' b'
          _ -> error UnexpectedArgType

      unFloat :: (forall a. IEEEFloating a => SBV a -> SBV a)
              -> SymExpr -> Solver SymExpr
      unFloat f a = do
        bool (error $ ArgAndRetNotTheSameType (symType a)) (return ())
          $ sameType a et
        case a of
          (SymFloat a') -> return . SymFloat $ f a'
          _ -> error UnexpectedArgType

  
      binIntegral :: (forall a. (SDivisible (SBV a), SIntegral a) => SBV a -> SBV a -> SBV a)
                  -> SymExpr -> SymExpr -> Solver SymExpr
      binIntegral f a b = do
        bool (error $ ArgsAndRetNotTheSameType (symType a) (symType b)) (return ())
          $ sameType a b && sameType a et
        let h :: (SIntegral c, SDivisible (SBV c)) => Solver (SBV c)
            h = f <$> getIntegral a <*> getIntegral b
        case a of
          (SymWord8 _) -> SymWord8 <$> h
          (SymWord16 _) -> SymWord16 <$> h
          (SymWord32 _) -> SymWord32 <$> h
          (SymWord64 _) -> SymWord64 <$> h
          (SymInt8 _) -> SymInt8 <$> h
          (SymInt16 _) -> SymInt16 <$> h
          (SymInt32 _) -> SymInt32 <$> h
          (SymInt64 _) -> SymInt64 <$> h          
          _ -> error UnexpectedReturnType


      unIntegral :: (forall a. (SDivisible (SBV a), SIntegral a) => SBV a -> SBV a)
                 -> SymExpr -> Solver SymExpr
      unIntegral f a = do
        bool (error $ ArgAndRetNotTheSameType (symType a)) (return ())
          $ sameType a et
        let h :: (SIntegral c, SDivisible (SBV c)) => Solver (SBV c)
            h = f <$> getIntegral a
        case a of
          (SymWord8 _) -> SymWord8 <$> h
          (SymWord16 _) -> SymWord16 <$> h
          (SymWord32 _) -> SymWord32 <$> h
          (SymWord64 _) -> SymWord64 <$> h
          (SymInt8 _) -> SymInt8 <$> h
          (SymInt16 _) -> SymInt16 <$> h
          (SymInt32 _) -> SymInt32 <$> h
          (SymInt64 _) -> SymInt64 <$> h          
          _ -> error UnexpectedReturnType

      binBiIntegral :: (forall a b. (SDivisible (SBV a), SIntegral a, SDivisible (SBV b), SIntegral b) => SBV a -> SBV b -> SBV a)
                  -> SymExpr -> SymExpr -> Solver SymExpr
      binBiIntegral f a b = do
        bool (error $ ArgAndRetNotTheSameType (symType a)) (return ())
          $ sameType a et
        let h :: (SIntegral a, SDivisible (SBV a)) => SBV a -> Solver (SBV a)
            h m = case b of
              (SymWord8 n) -> return $ f m n
              (SymWord16 n) -> return $ f m n
              (SymWord32 n) -> return $ f m n
              (SymWord64 n) -> return $ f m n
              (SymInt8 n) -> return $ f m n
              (SymInt16 n) -> return $ f m n
              (SymInt32 n) -> return $ f m n
              (SymInt64 n) -> return $ f m n
              _ -> error UnexpectedReturnType
        case a of
          (SymWord8 m) -> SymWord8 <$> h m
          (SymWord16 m) -> SymWord16 <$> h m
          (SymWord32 m) -> SymWord32 <$> h m
          (SymWord64 m) -> SymWord64 <$> h m
          (SymInt8 m) -> SymInt8 <$> h m
          (SymInt16 m) -> SymInt16 <$> h m
          (SymInt32 m) -> SymInt32 <$> h m
          (SymInt64 m) -> SymInt64 <$> h m
          _ -> error UnexpectedReturnType

  
      binIntegralToBool :: (forall a. SIntegral a => SBV a -> SBV a -> SBool)
                        -> SymExpr -> SymExpr -> Solver SymExpr
      binIntegralToBool f a b = do
        let h :: forall a . SIntegral a => SBV a -> SBV a -> Solver SymExpr
            h x y = return . SymBool $ f x y
        case (a, b) of
          ((SymWord8 x), (SymWord8 y)) -> h x y
          ((SymWord16 x), (SymWord16 y)) -> h x y
          ((SymWord32 x), (SymWord32 y)) -> h x y
          ((SymWord64 x), (SymWord64 y)) -> h x y
          ((SymInt8 x), (SymInt8 y)) -> h x y
          ((SymInt16 x), (SymInt16 y)) -> h x y
          ((SymInt32 x), (SymInt32 y)) -> h x y
          ((SymInt64 x), (SymInt64 y)) -> h x y
          _ -> error UnexpectedArgType

      binSignedToBool :: (forall a. SIntegral a => SBV a -> SBV a -> SBool)
                        -> SymExpr -> SymExpr -> Solver SymExpr
      binSignedToBool f a b = do
        let h :: forall a . SIntegral a => SBV a -> SBV a -> Solver SymExpr
            h x y = return . SymBool $ f x y
        case (a, b) of
          ((SymInt8 x), (SymInt8 y)) -> h x y
          ((SymInt16 x), (SymInt16 y)) -> h x y
          ((SymInt32 x), (SymInt32 y)) -> h x y
          ((SymInt64 x), (SymInt64 y)) -> h x y
          _ -> error UnexpectedArgType

      binUnsignedToBool :: (forall a. SIntegral a => SBV a -> SBV a -> SBool)
                        -> SymExpr -> SymExpr -> Solver SymExpr
      binUnsignedToBool f a b = do
        let h :: forall a . SIntegral a => SBV a -> SBV a -> Solver SymExpr
            h x y = return . SymBool $ f x y
        case (a, b) of
          ((SymWord8 x), (SymWord8 y)) -> h x y
          ((SymWord16 x), (SymWord16 y)) -> h x y
          ((SymWord32 x), (SymWord32 y)) -> h x y
          ((SymWord64 x), (SymWord64 y)) -> h x y
          _ -> error UnexpectedArgType

  
      binBool :: (SBool -> SBool -> SBool) -> SymExpr -> SymExpr -> Solver SymExpr
      binBool f (SymBool a) (SymBool b) = case et of
        Pil.TBool -> return . SymBool $ f a b
        _ -> error $ UnexpectedReturnType'Expected Pil.TBool
      binBool _ _ _ = error UnexpectedArgType

      fromSrc :: (Pil.HasSrc x Expression)
              => x -> (SymExpr -> Solver SymExpr)
              -> Solver SymExpr
      fromSrc x f = solveExpr (x ^. Pil.src) >>= f
  
      lr :: (HasLeft x Expression, HasRight x Expression)
         => x -> (SymExpr -> SymExpr -> Solver SymExpr)
         -> Solver SymExpr
      lr x f = do
        a <- solveExpr (x ^. Pil.left)
        b <- solveExpr (x ^. Pil.right)
        f a b

      mkConst :: Int64 -> Solver SymExpr
      mkConst n = maybe (error UnexpectedArgType) return $ literalToSymExpr et n

      mkFloatConst :: Double -> Solver SymExpr
      mkFloatConst = return . SymFloat . SBV.literal

  case xop of
    -- asumes left, right, and carry are all the same type.
    (Pil.ADC x) -> do
      r <- lr x $ binIntegral (+)
      c <- solveExpr (x ^. Pil.carry)
      binIntegral (+) r c

    (Pil.ADD x) -> lr x $ binIntegral (+)

    (Pil.ADDRESS_OF x) -> todo
    (Pil.ADDRESS_OF_FIELD x) -> todo

    (Pil.ADD_OVERFLOW x) -> todo
    (Pil.AND x) -> lr x $ binIntegral (.&.)

    -- sShiftRight preserves the sign bit if its arg is signed
    (Pil.ASR x) -> lr x $ binBiIntegral (sShiftRight)

    -- is x really an SBool type?
    (Pil.BOOL_TO_INT x) -> case (literalToSymExpr et 0, literalToSymExpr et 1) of
      (Just sf, Just st) -> do
        solveExpr (x ^. Pil.src) >>= \case
          (SymBool b) -> case (sf, st) of
            ((SymWord8 f), (SymWord8 t)) -> return . SymWord8 $ SBV.ite b t f
            ((SymWord16 f), (SymWord16 t)) -> return . SymWord16 $ SBV.ite b t f
            ((SymWord32 f), (SymWord32 t)) -> return . SymWord32 $ SBV.ite b t f
            ((SymWord64 f), (SymWord64 t)) -> return . SymWord64 $ SBV.ite b t f
            ((SymInt8 f), (SymInt8 t)) -> return . SymInt8 $ SBV.ite b t f
            ((SymInt16 f), (SymInt16 t)) -> return . SymInt16 $ SBV.ite b t f
            ((SymInt32 f), (SymInt32 t)) -> return . SymInt32 $ SBV.ite b t f
            ((SymInt64 f), (SymInt64 t)) -> return . SymInt64 $ SBV.ite b t f
            _ -> error UnexpectedArgType
          _ -> error UnexpectedArgType
      _ -> error UnexpectedArgType


    -- floats
    (Pil.CEIL x) -> fromSrc x $ unFloat (fpRoundToIntegral sRoundTowardPositive)
    
    (Pil.CMP_E x) -> lr x $ binIntegralToBool (.==)
    (Pil.CMP_NE x) -> lr x $ binIntegralToBool (./=)
    (Pil.CMP_SGE x) -> lr x $ binSignedToBool (.>=) 
    (Pil.CMP_SGT x) -> lr x $ binSignedToBool (.>)
    (Pil.CMP_SLE x) -> lr x $ binSignedToBool (.<=) 
    (Pil.CMP_SLT x) -> lr x $ binSignedToBool (.<) 
    (Pil.CMP_UGE x) -> lr x $ binUnsignedToBool (.>=) 
    (Pil.CMP_UGT x) -> lr x $ binUnsignedToBool (.>) 
    (Pil.CMP_ULE x) -> lr x $ binUnsignedToBool (.<=) 
    (Pil.CMP_ULT x) -> lr x $ binUnsignedToBool (.<) 

    (Pil.CONST x) -> mkConst $ x ^. Pil.constant
    (Pil.CONST_PTR x) -> mkConst $ x ^. Pil.constant

    (Pil.DIVS x) -> lr x $ binIntegral (sDiv)
    (Pil.DIVS_DP x) -> lr x $ binIntegral (sDiv)
    (Pil.DIVU x) -> lr x $ binIntegral (sDiv)
    (Pil.DIVU_DP x) -> lr x $ binIntegral (sDiv)

    -- note: all floats are Doubles for now, regardless of binja type
    (Pil.FABS x) -> fromSrc x $ unFloat fpAbs
    (Pil.FADD x) -> lr x $ binFloat (fpAdd sRoundNearestTiesToAway)
    (Pil.FCMP_E x) -> todo
    (Pil.FCMP_GE x) -> todo
    (Pil.FCMP_GT x) -> todo
    (Pil.FCMP_LE x) -> todo
    (Pil.FCMP_LT x) -> todo
    (Pil.FCMP_NE x) -> todo
    (Pil.FCMP_O x) -> todo
    (Pil.FCMP_UO x) -> todo
    (Pil.FDIV x) -> lr x $ binFloat (fpDiv sRoundNearestTiesToAway)
    (Pil.FLOAT_CONST x) -> mkFloatConst $ x ^. Pil.constant
    
    (Pil.FLOAT_CONV x) -> solveExpr (x ^. Pil.src) >>= \case
      (SymWord32 w) -> return . SymFloat . toSDouble sRoundNearestTiesToAway $ sWord32AsSFloat w
      (SymWord64 w) -> return . SymFloat $ sWord64AsSDouble w
      _ -> error UnexpectedArgType

    (Pil.FLOAT_TO_INT x) -> do
      m <- solveExpr (x ^. Pil.src)
      case m of
        (SymFloat n) -> do
          let rm = sRoundNearestTiesToAway
          (w, s) <- maybe (error UnexpectedArgType) return $ (,) <$> Pil.getTypeWidth et <*> Pil.getSignedness et
          case (w, s) of
            (1, False) -> return . SymWord8 . fromSDouble rm $ n
            (2, False) -> return . SymWord16 . fromSDouble rm $ n
            (4, False) -> return . SymWord32 . fromSDouble rm $ n
            (8, False) -> return . SymWord64 . fromSDouble rm $ n
            (1, True) -> return . SymInt8 . fromSDouble rm $ n
            (2, True) -> return . SymInt16 . fromSDouble rm $ n
            (4, True) -> return . SymInt32 . fromSDouble rm $ n
            (8, True) -> return . SymInt64 . fromSDouble rm $ n
            _ -> error UnexpectedReturnType

    (Pil.FLOOR x) -> fromSrc x $ unFloat (fpRoundToIntegral sRoundTowardNegative)
    (Pil.FMUL x) -> lr x $ binFloat (fpMul sRoundNearestTiesToAway)
    (Pil.FNEG x) -> fromSrc x $ unFloat fpNeg
    (Pil.FSQRT x) -> fromSrc x $ unFloat (fpSqrt sRoundNearestTiesToAway)
    (Pil.FSUB x) -> lr x $ binFloat (fpSub sRoundNearestTiesToAway)
    (Pil.FTRUNC x) -> fromSrc x $ unFloat (fpRoundToIntegral sRoundTowardZero)

    (Pil.IMPORT x) -> mkConst $ x ^. Pil.constant

    (Pil.INT_TO_FLOAT x) -> do
      r <- solveExpr $ x ^. Pil.src
      let h :: forall a. IEEEFloatConvertible a => SBV a -> Solver SymExpr
          h = return . SymFloat . toSDouble sRoundNearestTiesToAway
      case r of
        (SymWord8 m) -> h m
        (SymWord16 m) -> h m
        (SymWord32 m) -> h m
        (SymWord64 m) -> h m
        (SymInt8 m) -> h m
        (SymInt16 m) -> h m
        (SymInt32 m) -> h m
        (SymInt64 m) -> h m
        _ -> error UnexpectedArgType

    (Pil.LOAD x) -> todo
    (Pil.LOAD_SSA x) -> todo
    (Pil.LOAD_STRUCT x) -> todo
    (Pil.LOAD_STRUCT_SSA x) -> todo

    (Pil.LOW_PART x) -> mapError . fromSrc x $ Op.handleLowPart et

    (Pil.LSL x) -> lr x $ binBiIntegral (sShiftLeft)
    (Pil.LSR x) -> lr x $ binBiIntegral (sShiftRight)
    (Pil.MODS x) -> lr x $ binIntegral (sMod)
    (Pil.MODS_DP x) -> lr x $ binIntegral (sMod)
    (Pil.MODU x) -> lr x $ binIntegral (sMod)
    (Pil.MODU_DP x) -> lr x $ binIntegral (sMod)
    (Pil.MUL x) -> lr x $ binIntegral (*)
    (Pil.MULS_DP x) -> lr x $ binIntegral (*)
    (Pil.MULU_DP x) -> lr x $ binIntegral (*)
    (Pil.NEG x) -> todo
    (Pil.NOT x) -> todo
    (Pil.OR x) -> lr x $ binIntegral (.|.)
    (Pil.RLC x) -> todo
    (Pil.ROL x) -> todo
    (Pil.ROR x) -> todo
    (Pil.ROUND_TO_INT x) -> todo
    (Pil.RRC x) -> todo
    (Pil.SBB x) -> todo
    (Pil.SUB x) -> todo
    
    (Pil.SX x) -> mapError . fromSrc x $ Op.handleSx et
    
    (Pil.TEST_BIT x) -> todo
    Pil.UNIMPL -> todo
  --  (Pil.VAR (VarOp x) -> todo
    (Pil.VAR_ALIASED x) -> todo
    (Pil.VAR_ALIASED_FIELD x) -> todo
  --  (Pil.VAR_FIELD (VarFieldOp x) -> todo
    (Pil.VAR_PHI x) -> todo
  --  (Pil.VAR_SPLIT (VarSplitOp x) -> todo
    (Pil.VAR_SPLIT x) -> todo
    (Pil.VAR x) -> do
      vm <- use varMap
      case HashMap.lookup (x ^. Pil.src) vm of
        Nothing -> error CannotFindPilVarInVarMap
        Just v -> bool (error UnexpectedArgType) (return v) $ sameType v et

    (Pil.VAR_FIELD x) -> todo
    (Pil.XOR x) -> lr x $ binIntegral xor
    
    (Pil.ZX x) -> mapError . fromSrc x $ Op.handleZx et
  
    (Pil.CALL x) -> todo

    (Pil.StrCmp x) -> todo
    (Pil.StrNCmp x) -> todo
    (Pil.MemCmp x) -> todo
    (Pil.ConstStr x) -> todo
    _ -> todo


getIntegral :: forall a. (SIntegral a) => SymExpr -> Solver (SBV a)
getIntegral = \case
  (SymWord8 x) -> r x
  (SymWord16 x) -> r x
  (SymWord32 x) -> r x
  (SymWord64 x) -> r x
  (SymInt8 x) -> r x
  (SymInt16 x) -> r x
  (SymInt32 x) -> r x
  (SymInt64 x) -> r x
  _ -> throwError IntegralConversionError
  where
    r :: forall m n. (SIntegral m, SIntegral n) => SBV m -> Solver (SBV n)
    r x = return . sFromIntegral $ x


solveStmt :: Stmt -> Solver ()
solveStmt = undefined
