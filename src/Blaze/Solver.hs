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

    createWord 8 = SymWord8 <$> exists nm
    createWord 16 = SymWord16 <$> exists nm
    createWord 32 = SymWord32 <$> exists nm
    createWord 64 = SymWord64 <$> exists nm
    createWord n = err $ UnrecognizedWordWidth n

    createInt :: Int -> Solver SymExpr
    createInt 8 = SymInt8 <$> exists nm
    createInt 16 = SymInt16 <$> exists nm
    createInt 32 = SymInt32 <$> exists nm
    createInt 64 = SymInt64 <$> exists nm
    createInt n = err $ UnrecognizedIntWidth n
      
    nm = Text.unpack $ pilVarName pv


initVarMap :: Solver ()
initVarMap = do
  (TypeEnv te) <- typeEnv <$> ask
  let pvts = HashMap.toList te
  vars <- mapM (\ (pv, pt) -> (pv,) <$> makeSymVar pv pt) pvts
  varMap .= HashMap.fromList vars
  return ()

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
      
      todo = error OpNotYetSupported

      binIntegral :: (forall a. SIntegral a => SBV a -> SBV a -> SBV a)
                  -> SymExpr -> SymExpr -> Solver SymExpr
      binIntegral f a b = do
        bool (error $ ArgsAndRetNotTheSameType (symType a) (symType b)) (return ())
          $ sameType a b && sameType a et
        let h :: SIntegral c => Solver (SBV c)
            h = f <$> getIntegral a <*> getIntegral b
        case et of
          (Pil.TInt x) -> case (x ^. Pil.signed, x ^. Pil.width) of
            (False, 1) -> SymWord8 <$> h
            (False, 2) -> SymWord16 <$> h
            (False, 4) -> SymWord32 <$> h
            (False, 8) -> SymWord64 <$> h
            (True, 1) -> SymInt8 <$> h
            (True, 2) -> SymInt16 <$> h
            (True, 4) -> SymInt32 <$> h
            (True, 8) -> SymInt64 <$> h
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
  
      lr :: (HasLeft x Expression, HasRight x Expression)
         => x -> (SymExpr -> SymExpr -> Solver SymExpr)
         -> Solver SymExpr
      lr x f = do
        a <- solveExpr (x ^. Pil.left)
        b <- solveExpr (x ^. Pil.right)
        f a b

      mkConst :: Int64 -> Solver SymExpr
      mkConst n = do
        case Pil.getTypeWidth et of
          (Just 1) -> return . SymWord8 . SBV.literal . fromIntegral $ n
          (Just 2) -> return . SymWord16 . SBV.literal . fromIntegral $ n
          (Just 4) -> return . SymWord32 . SBV.literal . fromIntegral $ n
          (Just 8) -> return . SymWord64 . SBV.literal . fromIntegral $ n
          _ -> error UnexpectedArgType
      
  case xop of
    (Pil.ADC x) -> todo
    (Pil.ADD x) -> lr x $ binIntegral (+)
    (Pil.ADDRESS_OF x) -> todo
    (Pil.ADDRESS_OF_FIELD x) -> todo
    (Pil.ADD_OVERFLOW x) -> todo
    (Pil.AND x) -> lr x $ binIntegral (.&.)
    (Pil.ASR x) -> todo
    (Pil.BOOL_TO_INT x) -> todo
    (Pil.CEIL x) -> todo
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

    (Pil.CONST_PTR x) -> todo
    (Pil.DIVS x) -> todo
    (Pil.DIVS_DP x) -> todo
    (Pil.DIVU x) -> todo
    (Pil.DIVU_DP x) -> todo
    (Pil.FABS x) -> todo
    (Pil.FADD x) -> todo
    (Pil.FCMP_E x) -> todo
    (Pil.FCMP_GE x) -> todo
    (Pil.FCMP_GT x) -> todo
    (Pil.FCMP_LE x) -> todo
    (Pil.FCMP_LT x) -> todo
    (Pil.FCMP_NE x) -> todo
    (Pil.FCMP_O x) -> todo
    (Pil.FCMP_UO x) -> todo
    (Pil.FDIV x) -> todo
    (Pil.FLOAT_CONST x) -> todo
    (Pil.FLOAT_CONV x) -> todo
    (Pil.FLOAT_TO_INT x) -> todo
    (Pil.FLOOR x) -> todo
    (Pil.FMUL x) -> todo
    (Pil.FNEG x) -> todo
    (Pil.FSQRT x) -> todo
    (Pil.FSUB x) -> todo
    (Pil.FTRUNC x) -> todo
    (Pil.IMPORT x) -> todo
    (Pil.INT_TO_FLOAT x) -> todo
    (Pil.LOAD x) -> todo
    (Pil.LOAD_SSA x) -> todo
    (Pil.LOAD_STRUCT x) -> todo
    (Pil.LOAD_STRUCT_SSA x) -> todo
    (Pil.LOW_PART x) -> todo
    (Pil.LSL x) -> todo
    (Pil.LSR x) -> todo
    (Pil.MODS x) -> todo
    (Pil.MODS_DP x) -> todo
    (Pil.MODU x) -> todo
    (Pil.MODU_DP x) -> todo
    (Pil.MUL x) -> todo
    (Pil.MULS_DP x) -> todo
    (Pil.MULU_DP x) -> todo
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
    
    (Pil.SX x) -> Op.handleSx et =<< solveExpr (x ^. Pil.src)
    
    (Pil.TEST_BIT x) -> todo
    Pil.UNIMPL -> todo
  --  (Pil.VAR (VarOp x) -> todo
    (Pil.VAR_ALIASED x) -> todo
    (Pil.VAR_ALIASED_FIELD x) -> todo
  --  (Pil.VAR_FIELD (VarFieldOp x) -> todo
    (Pil.VAR_PHI x) -> todo
  --  (Pil.VAR_SPLIT (VarSplitOp x) -> todo
    (Pil.VAR_SPLIT x) -> todo
    (Pil.VAR x) -> todo
    (Pil.VAR_FIELD x) -> todo
    (Pil.XOR x) -> lr x $ binIntegral xor
    
    (Pil.ZX x) -> Op.handleZx et =<< solveExpr (x ^. Pil.src)
  
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

-- uadd32 :: (SIntegral a, SIntegral b) => SBV a -> SBV b -> SBV Word32
-- uadd32 = Op.add

-- uadd64 :: (SIntegral a, SIntegral b) => SBV a -> SBV b -> SBV Word64
-- uadd64 = Op.add

-- smalltest :: SWord8 -> Symbolic SBool
-- smalltest x = do
--   return $ x `shiftL` 3 .== 4 * (x :: SWord8)

-- smalltest2 :: Symbolic SBool
-- smalltest2 = do
--   x <- exists "x"
--   return $ x `shiftL` 3 .== 4 * (x :: SWord8)

-- constrainTest :: Symbolic SatResult
-- constrainTest = do
--   constrain sFalse
--   r <- sat (const sTrue)
--   return r

-- bigtest :: Symbolic ()
-- bigtest = do
--   x <- exists "x"
--   y <- exists "y"
--   z <- exists "z" :: Symbolic (SBV Word32)
--   constrain $ z .== x `Op.add` y
--   constrain $ add5 x .== (y :: SWord16)
--   query $ do
--     csat <- SBV.checkSat
--     case csat of
--       SBV.Sat -> do
--         xv <- getValue x
--         io . putText $ "This is Jimmy: " <> show xv
--       _ -> io $ putText "sorry Jim"
--   return ()
--   -- return $ add5 x .== y

-- bigtest2 :: Symbolic SBool
-- bigtest2 = do
--   x <- exists "x"
--   y <- exists "y"
--   constrain $ add5 x .== (y :: SWord16)
--   -- SBV.constrain $ add5 x .== x
--   return sFalse

-- opy :: SWord16 -> SWord16 -> SWord32 -> Symbolic SBool
-- opy x y z = return $ z .== x `Op.add` y

-- bigtest8 :: Solver SBool
-- bigtest8 = return sFalse

-- bigtest' :: Symbolic SVal
-- bigtest' = do
--   x <- exists "x"
--   y <- exists "y"
--   z <- exists "z" :: Symbolic (SBV Word32)
--   s <- exists "s" :: Symbolic (SBV String)
--   --constrain $ s .== "hello"
--   constrain $ SS.isPrefixOf "hello" s
--   constrain $ SS.length s .== 18
--   constrain =<< opy x y z
--   constrain $ add5 x .== (y :: SWord16)
-- --   -- query $ do
-- --   --   cs <- checkSat
-- --   --   case cs of
-- --   --     Sat -> do
-- --   --       xv <- getValue x
-- --   --       io . putText $ "This is Jimmy: " <> show xv
-- --   --     _ -> io $ putText "sorry Jim"
--   return D.svTrue
--   -- return $ add5 x .== y


-- -- trydrop :: Int -> SVal -> SVal
-- -- trydrop n bv =
-- --   case SBV.kindOf bv of
-- --     KBounded signed width ->
-- --       case someNatVal (fromIntegral width) of
-- --         Nothing -> P.error "sorry"
-- --         (Just (SomeNat (_ :: Proxy m))) ->
-- --           unSBV $ SBV.bvTake (Proxy :: Proxy m) (SBV bv :: SWord m)
-- --     _ -> P.error "oh well"

-- svalDrop :: Int -> SVal -> SVal
-- svalDrop n inp
--    | sz <= n = P.error $ "Trying to drop too much: " ++ show (n, k)
--    | True    = D.svExtract (sz - n - 1) 0 inp
--   where k  = kindOf inp
--         sz = case k of
--                KBounded _ v -> v
--                _            -> P.error $ "getSignSize: Not a bitvector: " ++ show k
