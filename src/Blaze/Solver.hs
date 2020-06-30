{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Blaze.Solver where

import Blaze.Prelude

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
import Data.SBV.Tools.Overflow (ArithOverflow, bvAddO)
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text as Text
import qualified Binja.Function as Func
import qualified Blaze.Solver.Op as Op

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
  (Pil.TStruct _) -> err StructTypeNotYetSupported
  
  where
    err = throwError . SymVarConversionError pv pt

    createWord 1 = SymWord8 <$> exists nm
    createWord 2 = SymWord16 <$> exists nm
    createWord 4 = SymWord32 <$> exists nm
    createWord 8 = SymWord64 <$> exists nm
    createWord 16 = SymWord128 <$> exists nm
    createWord n = err $ UnrecognizedWordWidth n

    createInt :: Int -> Solver SymExpr
    createInt 1 = SymInt8 <$> exists nm
    createInt 2 = SymInt16 <$> exists nm
    createInt 4 = SymInt32 <$> exists nm
    createInt 8 = SymInt64 <$> exists nm
    createInt 16 = SymInt128 <$> exists nm
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
  w <- Pil.getTypeByteWidth et
  s <- Pil.getSignedness et
  case (w, s) of
    (1, False) -> return . SymWord8 . SBV.literal . fromIntegral $ n
    (2, False) -> return . SymWord16 . SBV.literal . fromIntegral $ n
    (4, False) -> return . SymWord32 . SBV.literal . fromIntegral $ n
    (8, False) -> return . SymWord64 . SBV.literal . fromIntegral $ n
    (16, False) -> return . SymWord128 . SBV.literal . fromIntegral $ n
    (1, True) -> return . SymInt8 . SBV.literal . fromIntegral $ n
    (2, True) -> return . SymInt16 . SBV.literal . fromIntegral $ n
    (4, True) -> return . SymInt32 . SBV.literal . fromIntegral $ n
    (8, True) -> return . SymInt64 . SBV.literal . fromIntegral $ n
    (16, True) -> return . SymInt128 . SBV.literal . fromIntegral $ n
    _ -> Nothing


-- because Expression is opaque and doesn't have specific return type
-- the Solver return type has to be the opaque SymExpr
-- maybe Expression should be converted to some type correct GADT
-- first...
solveExpr :: Expression -> Solver SymExpr
solveExpr expr@(Expression _sz xop) = do
  tenv <- typeEnv <$> ask
  -- et = expected type
  et <- maybe (throwError $ UknownExpectedType expr) return
        $ Inference.getExprType tenv expr

  let error :: SolverError -> Solver a
      error = throwError . ExpressionConversionError expr et

      mapError :: Solver a -> Solver a
      mapError = flip catchError error
      
      todo = error OpNotYetSupported

      binFloatBool :: (forall a. IEEEFloating a => SBV a -> SBV a -> SBool)
               -> SymExpr -> SymExpr -> Solver SymExpr
      binFloatBool f a b = do
        bool (error $ ArgsAndRetNotTheSameType (symType a) (symType b)) (return ())
          $ sameType a b
        case (a, b) of
          ((SymFloat a'), (SymFloat b')) -> return . SymBool $ f a' b'
          _ -> error UnexpectedArgType

  
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
          (SymWord128 _) -> SymWord128 <$> h
          (SymInt8 _) -> SymInt8 <$> h
          (SymInt16 _) -> SymInt16 <$> h
          (SymInt32 _) -> SymInt32 <$> h
          (SymInt64 _) -> SymInt64 <$> h
          (SymInt128 _) -> SymInt128 <$> h
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
          (SymWord128 _) -> SymWord128 <$> h
          (SymInt8 _) -> SymInt8 <$> h
          (SymInt16 _) -> SymInt16 <$> h
          (SymInt32 _) -> SymInt32 <$> h
          (SymInt64 _) -> SymInt64 <$> h
          (SymInt128 _) -> SymInt128 <$> h
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
              (SymWord128 n) -> return $ f m n
              (SymInt8 n) -> return $ f m n
              (SymInt16 n) -> return $ f m n
              (SymInt32 n) -> return $ f m n
              (SymInt64 n) -> return $ f m n
              (SymInt128 n) -> return $ f m n
              _ -> error UnexpectedReturnType
        case a of
          (SymWord8 m) -> SymWord8 <$> h m
          (SymWord16 m) -> SymWord16 <$> h m
          (SymWord32 m) -> SymWord32 <$> h m
          (SymWord64 m) -> SymWord64 <$> h m
          (SymWord128 m) -> SymWord128 <$> h m
          (SymInt8 m) -> SymInt8 <$> h m
          (SymInt16 m) -> SymInt16 <$> h m
          (SymInt32 m) -> SymInt32 <$> h m
          (SymInt64 m) -> SymInt64 <$> h m
          (SymInt128 m) -> SymInt128 <$> h m
          _ -> error UnexpectedReturnType

      binBiIntegralToBool :: (forall a b. (SFiniteBits a, SDivisible (SBV a), SIntegral a, SDivisible (SBV b), SIntegral b) => SBV a -> SBV b -> SBool)
                  -> SymExpr -> SymExpr -> Solver SymExpr
      binBiIntegralToBool f a b = do
        let h :: (SDivisible (SBV a), SFiniteBits a, SIntegral a)
              => SBV a -> Solver SBool
            h m = case b of
              (SymWord8 n) -> return $ f m n
              (SymWord16 n) -> return $ f m n
              (SymWord32 n) -> return $ f m n
              (SymWord64 n) -> return $ f m n
              (SymWord128 n) -> return $ f m n
              (SymInt8 n) -> return $ f m n
              (SymInt16 n) -> return $ f m n
              (SymInt32 n) -> return $ f m n
              (SymInt64 n) -> return $ f m n
              (SymInt128 n) -> return $ f m n
              _ -> error UnexpectedReturnType
        case a of
          (SymWord8 m) -> SymBool <$> h m
          (SymWord16 m) -> SymBool <$> h m
          (SymWord32 m) -> SymBool <$> h m
          (SymWord64 m) -> SymBool <$> h m
          (SymWord128 m) -> SymBool <$> h m
          (SymInt8 m) -> SymBool <$> h m
          (SymInt16 m) -> SymBool <$> h m
          (SymInt32 m) -> SymBool <$> h m
          (SymInt64 m) -> SymBool <$> h m
          (SymInt128 m) -> SymBool <$> h m
          _ -> error UnexpectedReturnType

  
      binIntegralToBool :: (forall a. (ArithOverflow (SBV a), SIntegral a) => SBV a -> SBV a -> SBool)
                        -> SymExpr -> SymExpr -> Solver SymExpr
      binIntegralToBool f a b = do
        let h :: forall a . (ArithOverflow (SBV a), SIntegral a)
              => SBV a -> SBV a -> Solver SymExpr
            h x y = return . SymBool $ f x y
        case (a, b) of
          ((SymWord8 x), (SymWord8 y)) -> h x y
          ((SymWord16 x), (SymWord16 y)) -> h x y
          ((SymWord32 x), (SymWord32 y)) -> h x y
          ((SymWord64 x), (SymWord64 y)) -> h x y
          ((SymWord128 x), (SymWord128 y)) -> h x y
          ((SymInt8 x), (SymInt8 y)) -> h x y
          ((SymInt16 x), (SymInt16 y)) -> h x y
          ((SymInt32 x), (SymInt32 y)) -> h x y
          ((SymInt64 x), (SymInt64 y)) -> h x y
          ((SymInt128 x), (SymInt128 y)) -> h x y
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
          ((SymWord128 x), (SymWord128 y)) -> h x y
          _ -> error UnexpectedArgType

  
      -- binBool :: (SBool -> SBool -> SBool) -> SymExpr -> SymExpr -> Solver SymExpr
      -- binBool f (SymBool a) (SymBool b) = case et of
      --   Pil.TBool -> return . SymBool $ f a b
      --   _ -> error $ UnexpectedReturnType'Expected Pil.TBool
      -- binBool _ _ _ = error UnexpectedArgType

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
  
      lrc :: ( HasLeft x Expression
             , HasRight x Expression
             , Pil.HasCarry x Expression)
          => x -> (SymExpr -> SymExpr -> SymExpr -> Solver SymExpr)
          -> Solver SymExpr
      lrc x f = do
        a <- solveExpr (x ^. Pil.left)
        b <- solveExpr (x ^. Pil.right)
        c <- solveExpr (x ^. Pil.carry)
        f a b c

      mkConst :: Int64 -> Solver SymExpr
      mkConst n = maybe (error UnexpectedArgType) return $ literalToSymExpr et n

      mkFloatConst :: Double -> Solver SymExpr
      mkFloatConst = return . SymFloat . SBV.literal

      lookupPilVar :: PilVar -> Solver SymExpr
      lookupPilVar pv = use varMap
        >>= maybe (error CannotFindPilVarInVarMap) return . HashMap.lookup pv
 
  case xop of
    -- asumes left, right, and carry are all the same type.
    (Pil.ADC x) -> do
      r <- lr x $ binIntegral (+)
      c <- solveExpr (x ^. Pil.carry)
      binIntegral (+) r c

    (Pil.ADD x) -> lr x $ binIntegral (+)

    (Pil.ADD_OVERFLOW x) ->
      lr x $ binIntegralToBool (\a b -> uncurry (.||) $ bvAddO a b)
          
    (Pil.AND x) -> lr x $ binIntegral (.&.)

    -- sShiftRight preserves the sign bit if its arg is signed
    (Pil.ASR x) -> lr x $ binBiIntegral (sShiftRight)

    -- is x really an SBool type?
    (Pil.BOOL_TO_INT x) -> case (literalToSymExpr et (0 :: Int), literalToSymExpr et (1 :: Int)) of
      (Just sf, Just st) -> do
        solveExpr (x ^. Pil.src) >>= \case
          (SymBool b) -> case (sf, st) of
            ((SymWord8 f), (SymWord8 t)) -> return . SymWord8 $ SBV.ite b t f
            ((SymWord16 f), (SymWord16 t)) -> return . SymWord16 $ SBV.ite b t f
            ((SymWord32 f), (SymWord32 t)) -> return . SymWord32 $ SBV.ite b t f
            ((SymWord64 f), (SymWord64 t)) -> return . SymWord64 $ SBV.ite b t f
            ((SymWord128 f), (SymWord128 t)) -> return . SymWord128 $ SBV.ite b t f
            ((SymInt8 f), (SymInt8 t)) -> return . SymInt8 $ SBV.ite b t f
            ((SymInt16 f), (SymInt16 t)) -> return . SymInt16 $ SBV.ite b t f
            ((SymInt32 f), (SymInt32 t)) -> return . SymInt32 $ SBV.ite b t f
            ((SymInt64 f), (SymInt64 t)) -> return . SymInt64 $ SBV.ite b t f
            ((SymInt128 f), (SymInt128 t)) -> return . SymInt128 $ SBV.ite b t f
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
    (Pil.FCMP_E x) -> lr x $ binFloatBool (.==)
    (Pil.FCMP_GE x) -> lr x $ binFloatBool (.>=)
    (Pil.FCMP_GT x) -> lr x $ binFloatBool (.>)
    (Pil.FCMP_LE x) -> lr x $ binFloatBool (.<=)
    (Pil.FCMP_LT x) -> lr x $ binFloatBool (.<)
    (Pil.FCMP_NE x) -> lr x $ binFloatBool (./=)
    (Pil.FCMP_O x) -> mapError . lr x $ binFloatBool Op.neitherIsNaN
    (Pil.FCMP_UO x) -> mapError . lr x $ binFloatBool Op.atLeastOneIsNaN
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
          (w, s) <- maybe (error UnexpectedArgType) return $ (,) <$> Pil.getTypeBitWidth et <*> Pil.getSignedness et
          case (w, s) of
            (8, False) -> return . SymWord8 . fromSDouble rm $ n
            (16, False) -> return . SymWord16 . fromSDouble rm $ n
            (32, False) -> return . SymWord32 . fromSDouble rm $ n
            (64, False) -> return . SymWord64 . fromSDouble rm $ n
            (8, True) -> return . SymInt8 . fromSDouble rm $ n
            (16, True) -> return . SymInt16 . fromSDouble rm $ n
            (32, True) -> return . SymInt32 . fromSDouble rm $ n
            (64, True) -> return . SymInt64 . fromSDouble rm $ n
            _ -> error UnexpectedReturnType
        _ -> error UnexpectedArgType
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

    (Pil.LOAD _x) -> todo
    -- (Pil.LOAD_SSA _x) -> todo
    -- (Pil.LOAD_STRUCT _x) -> todo
    -- (Pil.LOAD_STRUCT_SSA _x) -> todo

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

    -- this will still 'negate` unsigned by adding the sign bit
    -- maybe it should throw an error instead?
    (Pil.NEG x) -> mapError . fromSrc x $ unIntegral Op.integralNeg

    (Pil.NOT x) -> fromSrc x $ unIntegral complement
    (Pil.OR x) -> lr x $ binIntegral (.|.)
    (Pil.RLC x) -> mapError . lrc x $ Op.handleRLC et
    (Pil.ROL x) -> lr x $ binBiIntegral (sRotateLeft)
    (Pil.ROR x) -> lr x $ binBiIntegral (sRotateRight)
    (Pil.ROUND_TO_INT x) -> fromSrc x $ unFloat (fpRoundToIntegral sRoundNearestTiesToAway)
    (Pil.RRC x) -> lrc x $ Op.handleRRC et
    (Pil.SBB x) -> do
      a <- solveExpr (x ^. Pil.left)
      b <- solveExpr (x ^. Pil.right)
      cf <- solveExpr (x ^. Pil.carry)
      c <- binIntegral (+) b cf
      binIntegral subtract a c

    (Pil.SUB x) -> lr x $ binIntegral subtract
    
    (Pil.SX x) -> mapError . fromSrc x $ Op.handleSx et
    
    (Pil.TEST_BIT x) -> lr x $ binBiIntegralToBool Op.testSBit
    (Pil.UNIMPL _) -> todo
  --  (Pil.VAR (VarOp x) -> todo
  --  (Pil.VAR_FIELD (VarFieldOp x) -> todo
    (Pil.VAR_PHI _x) -> todo
  --  (Pil.VAR_SPLIT (VarSplitOp x) -> todo
    (Pil.VAR_SPLIT x) -> mapError $ Op.handleVarSplit et (x ^. Pil.high) (x ^. Pil.low)

    (Pil.VAR x) -> do
      v <- lookupPilVar (x ^. Pil.src) 
      bool (error UnexpectedArgType) (return v) $ sameType v et

    (Pil.VAR_FIELD x) -> do
      v <- lookupPilVar (x ^. Pil.src)
      mapError $ Op.extract' et (x ^. Pil.offset) v
      
    (Pil.XOR x) -> lr x $ binIntegral xor
    
    (Pil.ZX x) -> mapError . fromSrc x $ Op.handleZx et
  
    (Pil.CALL _x) -> todo

    (Pil.StrCmp _x) -> todo
    (Pil.StrNCmp _x) -> todo
    (Pil.MemCmp _x) -> todo
    (Pil.ConstStr _x) -> todo

    (Pil.Extract x) -> mapError . fromSrc x $ Op.extract' et (x ^. Pil.offset)


solveStmt :: Stmt -> Solver ()
solveStmt = undefined
