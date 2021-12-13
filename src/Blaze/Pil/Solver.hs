{- HLINT ignore "Use if" -}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Blaze.Pil.Solver
  ( module Blaze.Pil.Solver
  , module Blaze.Types.Pil.Solver
  , module Exports
  ) where

import Blaze.Prelude hiding (zero, natVal, isSigned)
import qualified Prelude as P
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil ( Expression
                       , PilVar
                       , Statement
                       )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Solver
import qualified Blaze.Pil.Solver.List as BSList
import Data.SBV.Tools.Overflow (bvAddO)
import qualified Data.SBV.Trans as Exports (z3, cvc4)
import qualified Data.SBV.Trans as SBV
import Data.SBV.Trans ( (.==)
                      , (./=)
                      , (.>)
                      , (.>=)
                      , (.<)
                      , (.<=)
                      , (.||)
                      , (.~|)
                      , SInteger
                      , SInt8
                      , SInt16
                      , SInt32
                      , SInt64
                      , SInt
                      , SWord8
                      , SWord16
                      , SWord32
                      , SWord64
                      , SWord
                      )
import qualified Data.Text as Text
import Data.SBV.Dynamic as D hiding (Solver)
import qualified Blaze.Types.Pil.Checker as Ch
import qualified Blaze.Pil.Checker as Ch
import Blaze.Types.Pil.Checker ( DeepSymType )
import Data.SBV.Internals (SBV(SBV), unSBV)

stubbedFunctionConstraintGen :: HashMap Text (SVal -> [SVal] -> Solver ())
stubbedFunctionConstraintGen = HashMap.fromList
  [ ( "memcpy"
    , \_r args -> case args of
        [dest, src, n] -> do
          guardList dest
          guardList src
          guardIntegral n
          n' <- boundedToSInteger n
          constrain_ $ BSList.length dest .>= n'
          constrain_ $ BSList.length src .>= n'
          -- constrain_ $ r .== (SList.take n' src .++ SList.drop n' dest)
        xs -> throwError . StubbedFunctionArgError "memcpy" 3 $ length xs
    )
  , ( "abs"
    , \r args -> case args of
        [n] -> do
          guardIntegral n
          constrain $ r `svEqual` svAbs n
        xs -> throwError . StubbedFunctionArgError "abs" 1 $ length xs
    ) 
  ]
  
pilVarName :: PilVar -> Text
pilVarName pv = pv ^. #symbol
  <> maybe "" (("@"<>) . view (#func . #name)) mCtx
  <> maybe "" (("."<>) . show . f . view #ctxId) mCtx
  where
    f (Pil.CtxId n) = n
    mCtx :: Maybe Pil.Ctx
    mCtx = pv ^. #ctx

-- | convert a DeepSymType to an SBV Kind
-- any un-inferred Sign types resolve to False
deepSymTypeToKind :: DeepSymType -> Solver Kind
deepSymTypeToKind t = case t of
  Ch.DSVar v -> err $ "Can't convert DSVar " <> show v
  Ch.DSRecursive _s pt -> deepSymTypeToKind (Ch.DSType pt)
                          -- ignore recursion, and hope for the best

  Ch.DSType pt -> case pt of
    Ch.TArray _alen _etype -> err "Array should be handled only when wrapped in Ptr"
    Ch.TBool -> return KBool
    Ch.TChar -> return $ KBounded False 8
    Ch.TInt bwt st -> KBounded <$> (getSigned st <|> pure False) <*> getBitWidth bwt
    Ch.TFloat _ -> return KDouble
                   -- SBV only has float or double, so we'll just pick double

    Ch.TBitVector bwt -> KBounded <$> pure False <*> getBitWidth bwt
    Ch.TPointer bwt ptrElemType -> case ptrElemType of
      Ch.DSType (Ch.TArray _alen arrayElemType) ->
        -- alen constraint is handled at sym var creation
        KList <$> deepSymTypeToKind arrayElemType
      -- TODO: structs. good luck
      _ -> KBounded <$> pure False <*> getBitWidth bwt
    Ch.TCString _ -> return KString
    Ch.TRecord _ -> err "Can't handle Record type"
    Ch.TUnit -> return $ KTuple []
    Ch.TBottom s -> err $ "TBottom " <> show s
    Ch.TFunction _ _ -> err "Can't handle Function type"

    -- these all should be handled as nested in above type conversions
    Ch.TVBitWidth _ -> err "Out of place TVBitWidth"
    Ch.TVLength _ -> err "Out of place TVLength"
    Ch.TVSign _ -> err "Out of place TVSign"
  where
    getBitWidth (Ch.DSType (Ch.TVBitWidth bw)) = return $ fromIntegral bw
    getBitWidth badbw = err $ "Can't get bitwidth of: " <> show badbw

    getSigned (Ch.DSType (Ch.TVSign s)) = return s
    getSigned bads = err $ "Can't get signed of: " <> show bads

    err :: forall a. Text -> Solver a
    err = throwError . DeepSymTypeConversionError t

makeSymVar :: Maybe Text -> DeepSymType -> Kind -> Solver SVal
makeSymVar nm dst k = do
  v <- case cs <$> nm of
    Just n -> D.svNewVar k n
    Nothing -> D.svNewVar_ k
  case dst of
    Ch.DSType (Ch.TPointer _ (Ch.DSType (Ch.TArray (Ch.DSType (Ch.TVLength n)) _))) -> do
      constrain_ $ fromIntegral n .== BSList.length v
    _ -> return ()
  return v

makeSymVarOfType :: Maybe Text -> DeepSymType -> Solver SVal
makeSymVarOfType nm dst = deepSymTypeToKind dst >>= makeSymVar nm dst

declarePilVars :: Solver ()
declarePilVars = ask >>= mapM_ f . HashMap.toList . typeEnv
  where
    f (pv, dst) = do
      sval <- makeSymVarOfType (Just nm) dst
      #varNames %= HashMap.insert pv nm
      #varMap %= HashMap.insert pv sval
      where
        nm = pilVarName pv

constInt :: Bits -> Integer -> SVal
constInt w = svInteger (KBounded True $ fromIntegral w)

constWord :: Bits -> Integer -> SVal
constWord w = svInteger (KBounded False $ fromIntegral w)

constFloat :: Double -> SVal
constFloat = svDouble

constInteger :: Integral a => a -> SVal
constInteger = svInteger KUnbounded . fromIntegral

-- | requires: targetWidth >= bv
--           : kindOf bv is bounded
zeroExtend :: Bits -> SVal -> SVal
zeroExtend targetWidth bv = case kindOf bv of
  (KBounded s w)
    | tw == w -> bv
    | tw > w -> svJoin ext bv
    | otherwise -> P.error "zeroExtend: target width less than bitvec width"
    where
      ext = svInteger (KBounded s $ fromIntegral targetWidth - w) 0
  _ -> P.error "zeroExtend: arg not bitvec"
  where
    tw = fromIntegral targetWidth

-- | most significant bit
-- requires: kindOf bv is Bounded
--         : width bv > 0
msb :: SVal -> SVal
msb bv = case kindOf bv of
  (KBounded _ w)
    | w == 0 -> P.error "msb: bv has zero width"
    | otherwise -> svTestBit bv (w - 1)
  _ -> P.error "msb: bv must be Bounded kind"

-- | requires: targetWidth >= bv
--           : kindOf bv is bounded
--           : width bv > 0
signExtend :: Bits -> SVal -> SVal
signExtend targetWidth bv = case kindOf bv of
  (KBounded s w)
    | tw == w -> bv
    | tw > w -> svJoin ext bv
    | otherwise -> P.error "signExtend: target width less than bitvec width"
    where
      tw = fromIntegral targetWidth
      zero = svInteger (KBounded s $ fromIntegral targetWidth - w) 0
      ones = svNot zero
      ext  = svIte (msb bv) ones zero
  _ -> P.error "signExtend: bv must be Bounded kind"

signExtendSVal :: SVal -> SVal -> SVal
signExtendSVal tw bv = case kindOf bv of
  (KBounded _s w) -> svIte (tw `svGreaterEq` constWord 32 (toInteger w)) (svJoin ext bv) bv
    where
      -- subtract an additional 1 off since we are zero inclusive in createExtendBuf
      extWidth = tw `svMinus` constWord 32 (toInteger w) `svMinus` constWord 32 1
      zeros = buf
      ones = svNot buf
      ext = svIte (msb bv) ones zeros
      buf = createExtendBuf extWidth
      createExtendBuf :: SVal -> SVal
      createExtendBuf width = svIte (width `svEqual` constWord 32 0) 
                                      (constInt 1 0) 
                                      $ svJoin (createExtendBuf $ width `svMinus` constWord 32 1) $ constInt 1 0
  _ -> P.error "signExtend: bv must be Bounded kind"

zeroExtendSVal :: SVal -> SVal -> SVal
zeroExtendSVal tw bv = case kindOf bv of
  (KBounded _s w) -> svIte (tw `svGreaterEq` constWord 32 (toInteger w)) (svJoin buf bv) bv
    where
      extWidth = tw `svMinus` constWord 32 (toInteger w) `svMinus` constWord 32 1
      buf = createExtendBuf extWidth
      createExtendBuf :: SVal -> SVal
      createExtendBuf width = svIte (width `svEqual` constWord 32 0)
                                      (constWord 1 0)
                                      $ svJoin (createExtendBuf $ width `svMinus` constWord 32 1) $ constWord 1 0
  _ -> P.error "signExtend: bv must be Bounded kind"

-- | Extends b to match a's width.
-- | requires: width a >= width b
--           : kindOf a, b are bounded
--           : widths a, b > 0
matchBoundedWidth :: HasKind a => a -> SVal -> SVal
matchBoundedWidth a b = case (kindOf a, kindOf b) of
  (KBounded _ w1, KBounded s w2)
    | w1 == w2 -> b
    | w1 > w2 -> if s then signExtend w1' b else zeroExtend w1' b
    | otherwise -> lowPart w1' b
    where
      w1' = fromIntegral w1
  _ -> P.error "matchBoundedWidth: a and b must be kind Bounded"

-- | Matches second to first bounded integral sign
-- error if either not bounded.
matchSign :: HasKind a => a -> SVal -> SVal
matchSign a b = case (kindOf a, kindOf b) of
  (KBounded s1 _, KBounded s2 _)
    | s1 == s2 -> b
    | otherwise -> if s1 then svSign b else svUnsign b
  _ -> P.error "matchSign: a and b must be kind Bounded"

matchIntegral :: HasKind a => a -> SVal -> SVal
matchIntegral a b = matchBoundedWidth a (matchSign a b)

-- if x is signed, converts to unsigned, then runs f, then converts result to signed
runAsUnsigned :: (SVal -> SVal) -> SVal -> SVal
runAsUnsigned f x = case kindOf x of
  KBounded True _ -> svSign (f (svUnsign x))
  KBounded False _ -> f x
  _ -> P.error "runAsSigned: expected KBounded"


isSigned :: SVal -> Bool
isSigned x = case kindOf x of
  KBounded s _ -> s
  k            -> P.error $ "isSigned expected KBounded, got " <> show k

-- this is pretty much just copied out of Data.SBV
sSignedShiftArithRight :: SVal -> SVal -> SVal
sSignedShiftArithRight x i
  | isSigned i = error "sSignedShiftArithRight: shift amount should be unsigned"
  | isSigned x = svShiftRight x i
  | otherwise  = svIte (msb x)
                       (svNot (svShiftRight (svNot x) i))
                       (svShiftRight x i)

-- TODO: convert SVals to unsigned.
-- the svJoin gets messed up if these are signed
-- TODO: has the above TODO been done? check to make sure
updateBitVec :: BitOffset -> SVal -> SVal -> SVal
updateBitVec boff src dest = case (kindOf dest, kindOf src) of
  (KBounded destSign wdest, KBounded _ wsrc)
    | wsrc + off > wdest -> P.error "updateBitVec: src width + offset must be less than dest width"
    | otherwise -> bool svUnsign svSign destSign
                   $ destHighPart `svJoin` src' `svJoin` destLowPart
    where
      dest' = svUnsign dest
      src' = svUnsign src
      destHighPart = highPart (fromIntegral $ wdest - (off + wsrc)) dest'
      destLowPart = lowPart (fromIntegral boff) dest'
      off = fromIntegral boff
  _ -> P.error "updateBitVec: both args must be KBounded"
  

-- TODO: guard that n is greater than 0
-- and that w is big enough
lowPart :: Bits -> SVal -> SVal
lowPart n src = case kindOf src of
  KBounded _ _w -> svExtract (fromIntegral n - 1) 0 src
  _ -> P.error "lowPart: src must be KBounded"

-- TODO: guard that n is greater than 0
-- and that w is big enough
highPart :: Bits -> SVal -> SVal
highPart n src = case kindOf src of
  KBounded _ w -> svExtract (w - 1) (w - fromIntegral n) src
  _ -> P.error "lowPart: src must be KBounded"

rotateWithCarry :: (SVal -> SVal -> SVal) -> SVal -> SVal -> SVal -> SVal
rotateWithCarry rotFunc src rot c = case kindOf src of
  KBounded _ w -> svExtract w 1 $ rotFunc (svJoin src c) rot
  _ -> P.error "rotateWithCarry: src is not KBounded"

-- with carry works like regular rotate with carry appended to the end
rotateRightWithCarry :: SVal -> SVal -> SVal -> SVal
rotateRightWithCarry = rotateWithCarry svRotateRight

rotateLeftWithCarry :: SVal -> SVal -> SVal -> SVal
rotateLeftWithCarry = rotateWithCarry svRotateLeft

toSFloat :: SVal -> Solver SBV.SDouble
toSFloat x = guardFloat x >> return (SBV x)

toSFloat' :: SVal -> SBV.SDouble
toSFloat' x = case kindOf x of
  KDouble -> SBV x
  _ -> P.error "toSFloat: x is not KDouble kind"

toSBool' :: SVal -> SBool
toSBool' x = case kindOf x of
  KBool -> SBV x
  _ -> P.error "toSBool: x is not KBool kind"

toSBool :: SVal -> Solver SBool
toSBool x = guardBool x >> return (SBV x)

-- toSList :: HasKind a => SVal -> Solver (SList a)
-- toSList x 

-- sizeOf :: SVal -> Solver SInteger
-- sizeOf x = case k of
--   KBool -> throwError $ SizeOfError k
--   KBounded _ w -> constInteger s
--   KUnbounded    -> error "SBV.HasKind.intSizeOf((S)Integer)"
--   KReal         -> error "SBV.HasKind.intSizeOf((S)Real)"
--   KFloat        -> 32
--   KDouble       -> 64
--   KFP i j       -> i + j
--   KRational     -> error "SBV.HasKind.intSizeOf((S)Rational)"
--   KUserSort s _ -> error $ "SBV.HasKind.intSizeOf: Uninterpreted sort: " ++ s
--   KString       -> error "SBV.HasKind.intSizeOf((S)Double)"
--   KChar         -> error "SBV.HasKind.intSizeOf((S)Char)"
--   KList ek      -> error $ "SBV.HasKind.intSizeOf((S)List)" ++ show ek
--   KSet  ek      -> error $ "SBV.HasKind.intSizeOf((S)Set)"  ++ show ek
--   KTuple tys    -> error $ "SBV.HasKind.intSizeOf((S)Tuple)" ++ show tys
--   KMaybe k      -> error $ "SBV.HasKind.intSizeOf((S)Maybe)" ++ show k
--   KEither k1 k2 -> error $ "SBV.HasKind.intSizeOf((S)Either)" ++ show (k1, k2)
--   where
--     k = kindOf x

boolToInt' :: SVal -> SVal
boolToInt' b = svIte b (svInteger k 1) (svInteger k 0)
  where
    k = KBounded False 1

-- converts bool to 0 or 1 integral of Kind k
boolToInt :: Kind -> SVal -> Solver SVal
boolToInt (KBounded s w) b = do
  guardBool b
  return $ svIte b (svInteger (KBounded s w) 1) (svInteger (KBounded s w) 0)
boolToInt t _ = throwError . ErrorMessage $ "boolToInt expected KBounded, got " <> show t

constrain_ :: SBool -> Solver ()
constrain_ b = do
  ctx <- ask
  case ctx ^. #useUnsatCore of
    False -> SBV.constrain b
    True -> do
      i <- use #currentStmtIndex
      SBV.namedConstraint ("stmt_" <> show i) b

constrain :: SVal -> Solver ()
constrain = constrain_ <=< toSBool

newSymVar :: Text -> Kind -> Solver SVal
newSymVar name' k = D.svNewVar k (cs name')

-------------------------------

guardBool :: HasKind a => a -> Solver ()
guardBool x = case k of
  KBool -> return ()
  _ -> throwError $ GuardError "guardBool" [k] "Not Bool"
  where
    k = kindOf x

svBoolNot :: SVal -> SVal
svBoolNot = unSBV . SBV.sNot . toSBool'

guardIntegral :: HasKind a => a -> Solver ()
guardIntegral x = case k of
  KBounded _ _ -> return ()
  _ -> throwError $ GuardError "guardIntegral" [k] "Not integral"
  where
    k = kindOf x

boundedToSInteger :: SVal -> Solver SInteger
boundedToSInteger x = do
  guardIntegral x
  case kindOf x of
    KBounded True 8 -> return $ SBV.sFromIntegral (SBV x :: SInt8)
    KBounded True 16 -> return $ SBV.sFromIntegral (SBV x :: SInt16)
    KBounded True 32 -> return $ SBV.sFromIntegral (SBV x :: SInt32)
    KBounded True 64 -> return $ SBV.sFromIntegral (SBV x :: SInt64)
    KBounded True 128 -> return $ SBV.sFromIntegral (SBV x :: SInt 128)

    KBounded False 8 -> return $ SBV.sFromIntegral (SBV x :: SWord8)
    KBounded False 16 -> return $ SBV.sFromIntegral (SBV x :: SWord16)
    KBounded False 32 -> return $ SBV.sFromIntegral (SBV x :: SWord32)
    KBounded False 64 -> return $ SBV.sFromIntegral (SBV x :: SWord64)
    KBounded False 128 -> return $ SBV.sFromIntegral (SBV x :: SWord 128)

    t -> throwError . ConversionError $ "Cannot convert type " <> show t

guardFloat :: HasKind a => a -> Solver ()
guardFloat x = case k of
  KDouble -> return ()
  _ -> throwError $ GuardError "guardFloat" [k] "Not Double"
  where
    k = kindOf x

guardIntegralFirstWidthNotSmaller :: (HasKind a, HasKind b)
                                  => a -> b -> Solver ()
guardIntegralFirstWidthNotSmaller x y = case (kx, ky) of
  (KBounded _ w1, KBounded _ w2)
    | w1 >= w2 -> return ()
    | otherwise -> throwError $ GuardError "guardIntegralFirstWidthNotSmaller"
                   [kx, ky]
                   "Second arg width is greater than first"
  _ -> throwError $ GuardError "guardIntegralFirstWidthNotSmaller"
       [kx, ky]
       "Both must be KBounded"
  where
    kx = kindOf x
    ky = kindOf y

guardSameKind :: (HasKind a, HasKind b) => a -> b -> Solver ()
guardSameKind x y = if kindOf x == kindOf y
  then return ()
  else throwError $ GuardError "guardSameKind" [kindOf x, kindOf y] "not same kind"

guardList :: (HasKind a) => a -> Solver ()
guardList x = case kindOf x of
  KList _ -> return ()
  _ -> throwError $ GuardError "guardList" [kindOf x] "not a list"

lookupVarSym :: PilVar -> Solver SVal
lookupVarSym pv = do
  vm <- use #varMap
  maybe err return $ HashMap.lookup pv vm
  where
    err = throwError . ErrorMessage
          $ "lookupVarSym failed for var '" <> pilVarName pv <> "'"

bitsToOperationSize :: Bits -> Pil.OperationSize
bitsToOperationSize = Pil.OperationSize . (`div` 8) . fromIntegral

dstToExpr :: DSTExpression -> Expression
dstToExpr (Ch.InfoExpression (info, _) op) = Pil.Expression (bitsToOperationSize $ info ^. #size) $ dstToExpr <$> op

catchAndWarnStmtDef :: a -> Solver a -> Solver a
catchAndWarnStmtDef def m = catchError m $ \e -> do  
  si <- use #currentStmtIndex
  warn $ StmtError si e
  return def

catchAndWarnStmt :: Solver () -> Solver ()
catchAndWarnStmt m = catchError m $ \e -> do  
  si <- use #currentStmtIndex
  warn $ StmtError si e

warn :: SolverError -> Solver ()
warn e = #errors %= (e :)

solveStmt :: Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))
          -> Solver ()
solveStmt = solveStmt_ solveExpr

-- | Generates consraints for statement, using provided expr solver
solveStmt_ :: (DSTExpression -> Solver SVal)
           -> Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))
           -> Solver ()
solveStmt_ solveExprFunc stmt = catchAndWarnStmt $ case stmt of
  Pil.Def x -> do
    pv <- lookupVarSym $ x ^. #var
    expr <- solveExprFunc $ x ^. #value
    guardSameKind pv expr
    constrain $ pv `svEqual` expr
  Pil.Constraint x ->
    solveExprFunc (x ^. #condition) >>= constrain
  Pil.Store x -> do
    let exprAddr = dstToExpr $ x ^. #addr
    sValue <- solveExprFunc $ x ^. #value
    -- modify (\s -> s { mem = HashMap.insert exprAddr sValue $ s ^. #mem } )
    let insertStoreVar Nothing = Just [sValue]
        insertStoreVar (Just xs) = Just $ sValue : xs
    modify (\s -> s { stores = HashMap.alter insertStoreVar exprAddr $ s ^. #stores } )
    return ()
  Pil.DefPhi x -> do
    pv <- lookupVarSym $ x ^. #dest
    eqs <- mapM (f pv) $ x ^. #src
    constrain_ $ SBV.sOr eqs
    where
      f pv y = do
        pv2 <- lookupVarSym y
        guardSameKind pv pv2
        toSBool $ pv `svEqual` pv2

  _ -> return ()

solveExpr :: DSTExpression -> Solver SVal
solveExpr = solveExpr_ solveExpr

-- | Creates SVal that represents expression.
--   This type of InfoExpression is in a TypeReport
solveExpr_ :: (DSTExpression -> Solver SVal) -> DSTExpression -> Solver SVal
-- solveExpr (Ch.InfoExpression ((Ch.SymInfo _ xsym), Nothing) _) = \
--   solverError $ "No type for sym " <> show xsym
solveExpr_ solveExprRec (Ch.InfoExpression (Ch.SymInfo sz xsym, mdst) op) = catchFallbackAndWarn $ case op of
  Pil.ADC x -> integralBinOpWithCarry x $ \a b c -> a `svPlus` b `svPlus` c

  Pil.ADD x -> integralBinOpMatchSecondArgToFirst x svPlus

  Pil.ADD_OVERFLOW x ->
    integralBinOpMatchSecondArgToFirst x $ \a b -> unSBV $ uncurry (.||) $ bvAddO a b

  Pil.AND x -> integralBinOpMatchSecondArgToFirst x svAnd
  Pil.ASR x -> integralBinOpUnrelatedArgs x sSignedShiftArithRight
  Pil.BOOL_TO_INT x -> do
    b <- solveExprRec $ x ^. #src
    guardBool b
    k <- getRetKind
    guardIntegral k
    return $ svIte b (svInteger k 1) (svInteger k 0)

  -- TODO: stub standard libs here
  Pil.CALL x -> do
    fcg <- view #funcConstraintGen <$> ask
    case (x ^. #name) >>= flip HashMap.lookup fcg of
      Nothing -> fallbackAsFreeVar
      Just gen -> do
        args <- mapM solveExprRec $ x ^. #params
        r <- fallbackAsFreeVar
        gen r args
        return r

  Pil.CEIL x -> floatUnOp x $ SBV.fpRoundToIntegral SBV.sRoundTowardPositive
  Pil.CMP_E x -> binOpEqArgsReturnsBool x svEqual
  Pil.CMP_NE x -> binOpEqArgsReturnsBool x svNotEqual
  Pil.CMP_SGE x -> binOpEqArgsReturnsBool x svGreaterEq
  Pil.CMP_SGT x -> binOpEqArgsReturnsBool x svGreaterThan
  Pil.CMP_SLE x -> binOpEqArgsReturnsBool x svLessEq
  Pil.CMP_SLT x -> binOpEqArgsReturnsBool x svLessThan

  -- the signed and unsigned versions use the same smt func
  -- the type checker should guarantee that the args are correctly signed or unsigned
  -- but maybe TODO should be to convert signed SVal to unsigned SVal if necessary
  Pil.CMP_UGE x -> binOpEqArgsReturnsBool x svGreaterEq
  Pil.CMP_UGT x -> binOpEqArgsReturnsBool x svGreaterThan
  Pil.CMP_ULE x -> binOpEqArgsReturnsBool x svLessEq
  Pil.CMP_ULT x -> binOpEqArgsReturnsBool x svLessThan

  Pil.CONST x -> do
    k <- getRetKind
    guardIntegral k
    return . svInteger k . fromIntegral $ x ^. #constant
  Pil.CONST_BOOL x -> return . svBool $ x ^. #constant
  Pil.CONST_FLOAT x -> return . svDouble $ x ^. #constant
  Pil.CONST_PTR x ->
    return . svInteger (KBounded False $ fromIntegral sz)
    . fromIntegral $ x ^. #constant

  Pil.ConstStr x -> return . unSBV $ SBV.literal (cs $ x ^. #value :: String)
  Pil.ConstFuncPtr x -> return . svInteger (KBounded False $ fromIntegral sz)
    . fromIntegral $ x ^. #address

  -- TODO: do we need to do anything special for the DP versions?
  Pil.DIVS x -> integralBinOpMatchSecondArgToFirst x svDivide
  Pil.DIVS_DP x -> divOrModDP True x svDivide
  Pil.DIVU x -> integralBinOpMatchSecondArgToFirst x svDivide
  Pil.DIVU_DP x -> divOrModDP False x svDivide

  Pil.FABS x -> floatUnOp x SBV.fpAbs
  Pil.FADD x -> floatBinOp x $ SBV.fpAdd SBV.sRoundNearestTiesToAway
  Pil.FDIV x -> floatBinOp x $ SBV.fpDiv SBV.sRoundNearestTiesToAway
  Pil.FCMP_E x -> floatBinOpReturnsBool x (.==)
  Pil.FCMP_GE x -> floatBinOpReturnsBool x (.>=)
  Pil.FCMP_GT x -> floatBinOpReturnsBool x (.>)
  Pil.FCMP_LE x -> floatBinOpReturnsBool x (.<=)
  Pil.FCMP_LT x -> floatBinOpReturnsBool x (.<)
  Pil.FCMP_O x -> floatBinOpReturnsBool x $ \a b -> SBV.fpIsNaN a .~| SBV.fpIsNaN b
  Pil.FCMP_NE x -> floatBinOpReturnsBool x (./=)
  Pil.FCMP_UO x -> floatBinOpReturnsBool x $ \a b -> SBV.fpIsNaN a .|| SBV.fpIsNaN b

  -- TODO: a FIELD_ADDR should only be used inside a LOAD or Store, and hence
  -- should never be "solved". But maybe field addrs could be added to?
--   Pil.FIELD_ADDR x -> do

  Pil.FLOAT_CONV x -> do
    y <- solveExprRec $ x ^. #src
    case kindOf y of
      (KBounded False 32) -> return
        . unSBV
        . SBV.toSDouble SBV.sRoundNearestTiesToAway
        . SBV.sWord32AsSFloat
        . SBV $ y
      (KBounded False 64) -> return
        . unSBV
        . SBV.sWord64AsSDouble
        . SBV $ y

      k -> throwError . ErrorMessage
           $ "FLOAT_CONV expecting Unsigned integral of 32 or 64 bit width, got"
           <> show k

  Pil.FLOAT_TO_INT x -> do
    k <- getRetKind
    y <- solveExprRec $ x ^. #src
    guardFloat y
    case k of
      (KBounded False 64) -> unSBV <$> (f y :: Solver SBV.SWord64)
      (KBounded False 32) -> unSBV <$> (f y :: Solver SBV.SWord32)
      (KBounded False 16) -> unSBV <$> (f y :: Solver SBV.SWord16)
      (KBounded False 8) -> unSBV <$> (f y :: Solver SBV.SWord8)

      (KBounded True 64) -> unSBV <$> (f y :: Solver SBV.SInt64)
      (KBounded True 32) -> unSBV <$> (f y :: Solver SBV.SInt32)
      (KBounded True 16) -> unSBV <$> (f y :: Solver SBV.SInt16)
      (KBounded True 8) -> unSBV <$> (f y :: Solver SBV.SInt8)
      _ -> throwError . ErrorMessage
           $ "FLOAT_TO_INT: unsupported return type: " <> show k
      where
        f :: forall a. (SBV.IEEEFloatConvertible a) => SVal -> Solver (SBV a)
        f = return . SBV.fromSDouble SBV.sRoundNearestTiesToAway . SBV

  Pil.FLOOR x -> floatUnOp x $ SBV.fpRoundToIntegral SBV.sRoundTowardNegative
  Pil.FMUL x -> floatBinOp x $ SBV.fpMul SBV.sRoundNearestTiesToAway
  Pil.FNEG x -> floatUnOp x SBV.fpNeg
  Pil.FSQRT x -> floatUnOp x $ SBV.fpSqrt SBV.sRoundNearestTiesToAway
  Pil.FTRUNC x -> floatUnOp x $ SBV.fpRoundToIntegral SBV.sRoundTowardZero
  Pil.FSUB x -> floatBinOp x $ SBV.fpSub SBV.sRoundNearestTiesToAway
  Pil.IMPORT x -> return
    . svInteger (KBounded False $ fromIntegral sz)
    . fromIntegral
    $ x ^. #constant

  Pil.INT_TO_FLOAT x -> do
    y <- solveExprRec $ x ^. #src
    let f :: forall a. SBV.IEEEFloatConvertible a => SBV a -> Solver SVal
        f = return . unSBV . SBV.toSDouble SBV.sRoundNearestTiesToAway
    case kindOf y of
      (KBounded True 8) -> f (SBV y :: SBV.SInt8)
      (KBounded True 16) -> f (SBV y :: SBV.SInt16)
      (KBounded True 32) -> f (SBV y :: SBV.SInt32)
      (KBounded True 64) -> f (SBV y :: SBV.SInt64)

      (KBounded False 8) -> f (SBV y :: SBV.SWord8)
      (KBounded False 16) -> f (SBV y :: SBV.SWord16)
      (KBounded False 32) -> f (SBV y :: SBV.SWord32)
      (KBounded False 64) -> f (SBV y :: SBV.SWord64)
      k -> throwError . ErrorMessage
           $ "INT_TO_FLOAT: unsupported return type: " <> show k

  Pil.LOAD x -> do
    m <- use #mem
    let key = dstToExpr $ x ^. #src
    maybe (createFreeVar key) return $ HashMap.lookup key m
    where
      createFreeVar k = do
        freeVar <- fallbackAsFreeVar
        #mem %= HashMap.insert k freeVar
        return freeVar

  Pil.LOW_PART x -> integralUnOp x (lowPart sz)
  Pil.LSL x -> integralBinOpUnrelatedArgs x svShiftLeft
  Pil.LSR x -> integralBinOpUnrelatedArgs x svShiftRight
  Pil.MODS x -> integralBinOpMatchSecondArgToFirst x svRem
  Pil.MODS_DP x -> divOrModDP True x svRem
  Pil.MODU x -> integralBinOpMatchSecondArgToFirst x svRem
  Pil.MODU_DP x -> divOrModDP False x svRem
  Pil.MUL x -> integralBinOpMatchSecondArgToFirst x svTimes
  Pil.MULS_DP x -> mulDP True x
  Pil.MULU_DP x -> mulDP False x
  Pil.NEG x -> integralUnOp x svUNeg

  Pil.NOT x -> do
    y <- solveExprRec $ x ^. #src
    let k = kindOf y
    case k of
      KBool -> return $ unSBV . SBV.sNot . toSBool' $ y
      (KBounded _ _) -> return $ svNot y
      _ -> throwError . ErrorMessage $ "NOT expecting Bool or Integral, got " <> show k

  Pil.OR x -> integralBinOpMatchSecondArgToFirst x svOr
  Pil.RLC x -> rotateBinOpWithCarry x rotateLeftWithCarry
  Pil.ROL x -> integralBinOpUnrelatedArgs x svRotateLeft
  Pil.ROR x -> integralBinOpUnrelatedArgs x svRotateRight
--   Pil.ROUND_TO_INT x -> floatToInt x
  Pil.RRC x -> rotateBinOpWithCarry x rotateRightWithCarry
  Pil.SBB x -> integralBinOpWithCarry x $ \a b c -> (a `svMinus` b) `svMinus` c
-- --   -- STORAGE _ -> unknown
-- --   StrCmp _ -> intRet
-- --   StrNCmp _ -> intRet
-- --   MemCmp _ -> intRet
--   Pil.STACK_LOCAL_ADDR _ -> retPointer

  Pil.SUB x -> integralBinOpMatchSecondArgToFirst x svMinus

  Pil.SX x -> bitVectorUnOp x (signExtend sz)

  Pil.TEST_BIT x -> integralBinOpUnrelatedArgs x $ \a b -> case kindOf a of
    KBounded _ w -> (a `svAnd` svExp (constWord (Bits w') 2) b)
                    `svGreaterThan` constWord (Bits w') 0
      where
        w' = fromIntegral w
    -- TODO: Throw error if not KBounded
    _ -> svFalse

  Pil.UNIMPL _ -> throwError . ErrorMessage $ "UNIMPL"

  Pil.UPDATE_VAR x -> do
    dest <- lookupVarSym $ x ^. #dest
    src <- solveExprRec $ x ^. #src
    guardIntegral dest
    guardIntegral src
    --TODO: convert dest and src to unsigned and convert them back if needed
    --TODO: the above TODO might already happen in updateBitVec. find out.
    return $ updateBitVec (toBitOffset $ x ^. #offset) src dest
    
  --   -- How should src and dest be related?
  --   -- Can't express that `offset + width(src) == width(dest)`
  --   --  without `+` and `==` as type level operators.
  --   return [ (r, CSVar v) ]

  Pil.VAR x -> lookupVarSym $ x ^. #src

  -- TODO: add VAR_FIELD test
  -- also, maybe convert the offset to bits?
  Pil.VAR_FIELD x -> do
    v <- lookupVarSym $ x ^. #src
    return $ svExtract (off + w - 1) off v 
    where
      off = fromIntegral . toBitOffset $ x ^. #offset
      w = fromIntegral sz

  -- this should really be called VAR_JOIN
  Pil.VAR_JOIN x -> do
    low <- lookupVarSym $ x ^. #low
    high <- lookupVarSym $ x ^. #high
    guardIntegral low
    guardIntegral high
    return $ svJoin high low

  Pil.XOR x -> integralBinOpUnrelatedArgs x svXOr
  Pil.ZX x -> bitVectorUnOp x (zeroExtend sz)
-- --   Extract _ -> bitvecRet
        -- expr offset sz --> expr 0 sz -> takes low sz of expr
  _ -> throwError . ErrorMessage $ "unhandled PIL op: "
       <> Text.takeWhile (/= ' ') (show op)
  where
    fallbackAsFreeVar :: Solver SVal
    fallbackAsFreeVar = case mdst of
      Nothing -> throwError . ExprError xsym . ErrorMessage $ "missing DeepSymType"
      Just dst -> catchError (makeSymVarOfType Nothing dst) $ \e ->
          throwError $ ExprError xsym e

    getDst :: Solver DeepSymType
    getDst = maybe e return mdst
      where e = throwError . ErrorMessage $ "missing DeepSymType"

    getRetKind = getDst >>= deepSymTypeToKind

    catchFallbackAndWarn :: Solver SVal -> Solver SVal
    catchFallbackAndWarn m = catchError m $ \e -> do  
      si <- use #currentStmtIndex
      warn $ StmtError si e
      fallbackAsFreeVar
      
    binOpEqArgsReturnsBool :: ( HasField' "left" x DSTExpression
                              , HasField' "right" x DSTExpression)
                              => x -> (SVal -> SVal -> SVal) -> Solver SVal
    binOpEqArgsReturnsBool x f = do
      lx <- solveExprRec (x ^. #left)
      rx <- solveExprRec (x ^. #right)
      guardSameKind lx rx
      return $ f lx rx

    -- | doesn't match second arg to first
    integralBinOpUnrelatedArgs :: ( HasField' "left" x DSTExpression
                                  , HasField' "right" x DSTExpression)
                               => x -> (SVal -> SVal -> SVal) -> Solver SVal
    integralBinOpUnrelatedArgs x f = do
      lx <- solveExprRec (x ^. #left)
      rx <- solveExprRec (x ^. #right)
      guardIntegral lx
      guardIntegral rx
      return $ f lx rx

    -- | assumes first arg width >= second arg width
    -- matches second args sign and width to equal first
    integralBinOpMatchSecondArgToFirst
      :: ( HasField' "left" x DSTExpression
         , HasField' "right" x DSTExpression)
      => x -> (SVal -> SVal -> SVal) -> Solver SVal
    integralBinOpMatchSecondArgToFirst x f = do
      lx <- solveExprRec (x ^. #left)
      rx <- solveExprRec (x ^. #right)
      guardIntegralFirstWidthNotSmaller lx rx
      let rx' = matchSign lx (matchBoundedWidth lx rx)
      return $ f lx rx'

    {- HLINT ignore "Reduce duplication" -}
    floatBinOp :: ( HasField' "left" x DSTExpression
                  , HasField' "right" x DSTExpression )
               => x
               -> (SBV.SDouble -> SBV.SDouble -> SBV.SDouble) -> Solver SVal
    floatBinOp x f = do
      lx <- toSFloat =<< solveExprRec (x ^. #left)
      rx <- toSFloat =<< solveExprRec (x ^. #right)
      return . unSBV $ f lx rx

    floatBinOpReturnsBool :: ( HasField' "left" x DSTExpression
                             , HasField' "right" x DSTExpression )
                          => x
                          -> (SBV.SDouble -> SBV.SDouble -> SBool) -> Solver SVal
    floatBinOpReturnsBool x f = do
      lx <- toSFloat =<< solveExprRec (x ^. #left)
      rx <- toSFloat =<< solveExprRec (x ^. #right)
      return . unSBV $ f lx rx


    bitVectorUnOp :: HasField' "src" x DSTExpression
               => x
               -> (SVal -> SVal) -> Solver SVal
    bitVectorUnOp = integralUnOp

    integralUnOp :: HasField' "src" x DSTExpression
               => x
               -> (SVal -> SVal) -> Solver SVal
    integralUnOp x f = do
      lx <- solveExprRec (x ^. #src)
      guardIntegral lx
      return $ f lx

    floatUnOp :: HasField' "src" x DSTExpression
              => x
              -> (SBV.SDouble -> SBV.SDouble) -> Solver SVal
    floatUnOp x f = do
      lx <- solveExprRec (x ^. #src)
      unSBV . f <$> toSFloat lx

    -- | return is double width, so we double the args
    mulDP :: ( HasField' "left" x DSTExpression
             , HasField' "right" x DSTExpression )
          => Bool
          -> x
          -> Solver SVal
    mulDP signedness x = do
      lx <- solveExprRec (x ^. #left)
      rx <- solveExprRec (x ^. #right)
      let retKind = KBounded signedness $ fromIntegral sz
      guardIntegralFirstWidthNotSmaller lx rx
      guardIntegralFirstWidthNotSmaller retKind lx
      let lx' = matchIntegral retKind lx
          rx' = matchIntegral lx' rx
      return $ svTimes lx' rx'


    -- | first arg is double width of second and return arg
    -- so we have to increase width of second, then shrink result by half
    divOrModDP :: ( HasField' "left" x DSTExpression
                  , HasField' "right" x DSTExpression )
                    => Bool
                    -> x
                    -> (SVal -> SVal -> SVal) -> Solver SVal
    divOrModDP signedness x f = do
      lx <- solveExprRec (x ^. #left)
      rx <- solveExprRec (x ^. #right)
      let retKind = KBounded signedness $ fromIntegral sz
      guardIntegralFirstWidthNotSmaller lx rx
      let rx' = matchIntegral lx rx
          res = f lx rx'
          res' = matchIntegral retKind res -- make result size of original rx
      return res'

    integralBinOpWithCarry :: ( HasField' "left" x DSTExpression
                              , HasField' "right" x DSTExpression
                              , HasField' "carry" x DSTExpression)
                           => x
                           -> (SVal -> SVal -> SVal -> SVal) -> Solver SVal
    integralBinOpWithCarry x f = do
      a <- solveExprRec (x ^. #left)
      b <- solveExprRec (x ^. #right)
      c <- solveExprRec (x ^. #carry)
      guardIntegralFirstWidthNotSmaller a b
      cAsInt <- boolToInt (kindOf a) c
      
      let b' = matchIntegral a b
      return $ f a b' cAsInt

    rotateBinOpWithCarry :: ( HasField' "left" x DSTExpression
                            , HasField' "right" x DSTExpression
                            , HasField' "carry" x DSTExpression)
                         => x
                         -> (SVal -> SVal -> SVal -> SVal) -> Solver SVal
    rotateBinOpWithCarry x f = do
      a <- solveExprRec (x ^. #left)
      b <- solveExprRec (x ^. #right)
      c <- solveExprRec (x ^. #carry)
      guardIntegral a
      guardIntegral b
      guardBool c
      cAsInt <- boolToInt (KBounded False 1) c
      return $ runAsUnsigned (\y -> f y b cAsInt) a

solveTypedStmtsWith :: SMTConfig
                    -> HashMap PilVar DeepSymType
                    -> [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
                    -> IO (Either SolverError SolverReport)
solveTypedStmtsWith solverCfg vartypes stmts = do
  er <- runSolverWith solverCfg run ( emptyState
                                    , SolverCtx vartypes stubbedFunctionConstraintGen True
                                    )
  return $ toSolverReport <$> er
  where
    toSolverReport :: (SolverResult, SolverState) -> SolverReport
    toSolverReport (r, s) = SolverReport r (s ^. #errors)
    run = do
      declarePilVars
      mapM_ f stmts
      querySolverResult
    f (ix, stmt) = do
      #currentStmtIndex .= ix
      solveStmt stmt

-- | runs type checker first, then solver
solveStmtsWith :: SMTConfig
               -> [Statement Expression]
               -> IO (Either
                      (Either
                       Ch.ConstraintGenError
                       (SolverError, Ch.TypeReport))
                      (SolverReport, Ch.TypeReport))
solveStmtsWith solverCfg stmts = do
  -- should essential analysis steps be included here?
  -- let stmts' = Analysis.substFields stmts
  let er = Ch.checkStmts stmts
  case er of
    Left e -> return $ Left (Left e)
    Right tr -> solveTypedStmtsWith solverCfg (tr ^. #varSymTypeMap) (tr ^. #symTypeStmts) >>= \case
      Left e -> return $ Left (Right (e, tr))
      Right sr -> return $ Right (sr, tr)

-- | convenience function for checking statements.
-- any errors in Type Checker or Solver result in Unk
-- warnings are ignored
solveStmtsWith_ :: SMTConfig
                -> [Statement Expression]
                -> IO SolverResult
solveStmtsWith_ solverCfg stmts = solveStmtsWith solverCfg stmts >>= \case
  Left _ -> return Unk
  Right (r, _) -> return $ r ^. #result
