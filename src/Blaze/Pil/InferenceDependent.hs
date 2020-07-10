{-# LANGUAGE TemplateHaskell #-}
module Blaze.Pil.InferenceDependent where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Prelude as P
import Blaze.Types.Pil ( Expression(Expression)
                       , ExprOp
                       , OperationSize
                       , Statement
                       , PilVar
                       )
import qualified Blaze.Types.Pil as Pil
import qualified Data.Map as Map
-- import Data.HashMap.Strict (HashMap)
import qualified Binja.Variable as V
import qualified Binja.C.Enums as E
import qualified Binja.MLIL as MLIL
-- import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
-- import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Blaze.Pil.Analysis as Analysis
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified Data.STRef as ST
import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as GA
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NG
import qualified Data.List.NonEmpty as NE

type BitWidth = Bits
type ByteWidth = Bytes

charSize :: BitWidth
charSize = 8

-- NOTE: I got rid of non-concrete types, like TNumber,
--       but for function signatures they should be added back in
data PilType t = TArray { len :: t, elemType :: t }
               | TChar
               | TInt { bitWidth :: t, signed :: t }
               | TFloat { bitWidth :: t }
               | TBitVector { bitWidth :: t }
               | TPointer { bitWidth :: t, pointeeType :: t }
               | TRecord (HashMap BitWidth -- todo: change bitwidth to 't'?
                                  t -- type
                         )
               | TBottom
               | TFunction { ret :: t, params :: [t] }

               -- class constraint (t should be TVBitWidth)
               | THasWidth t
               
               -- type level values for some dependent-type action
               | TVBitWidth BitWidth
               | TVLength Word64
               | TVSign Bool
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data T = T (PilType T)
  deriving (Eq, Ord, Read, Show)

unT :: T -> PilType T
unT (T pt) = pt

newtype Sym = Sym Int
            deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Sym

data SymType = SVar Sym
             | SType (PilType SymType)
             deriving (Eq, Ord, Read, Show, Generic)


-- XVars are existential vars used for function sigs
type XVar = Text
data ExistentialType = XVar Text
                     | XType (PilType ExistentialType)
                     deriving (Eq, Ord, Read, Show, Generic)

data CheckerError = CannotFindPilVarInVarSymMap PilVar
                  | CannotFindSymInSymMap
                  | UnhandledExpr
                  | UnhandledStmt
  deriving (Eq, Ord, Show)

incrementSym :: Sym -> Sym
incrementSym (Sym n) = Sym $ n + 1

data InfoExpression a = InfoExpression
  { _info :: a
  , _op :: ExprOp (InfoExpression a)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
$(makeFieldsNoPrefix ''InfoExpression)

data SymInfo = SymInfo
  { _size :: BitWidth
  , _sym :: Sym
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''SymInfo)

type SymExpression = InfoExpression SymInfo

type SymTypeExpression = InfoExpression SymType

-- | Goal is to generate statements with TypedExpressions
type TypedExpression = InfoExpression (PilType T)

data UnifyError = IncompatibleTypes (PilType SymType) (PilType SymType)
                | OverlappingRecordField { recordFields :: (HashMap BitWidth SymType)
                                         , offendingOffset :: BitWidth
                                         }
                deriving (Eq, Ord, Read, Show, Generic)

-- | The final report of the type checker, which contains types and errors.
data TypeReport = TypeReport
  { _symTypeStmts :: [Statement SymTypeExpression]
--  , _typedStmts :: [Statement TypedExpression]
  , _varSymTypeMap :: HashMap PilVar SymType
--  , _varTypeMap :: HashMap PilVar (PilType T)
--  , _unresolvedStmts :: [Statement SymExpression]
  -- , _unresolvedSyms :: [(Sym, Sym)]
  -- , _unresolvedTypes :: [(Sym, PilType SymType, PilType SymType)]
  , _errors :: [UnifyError]
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''TypeReport)


-- | The "Checker" monad is actually currently just used to generate symbols and
--   constraints for every expression and var. it should be renamed
data CheckerState = CheckerState
  { _currentSym :: Sym
  , _symMap :: HashMap Sym SymExpression
  , _varSymMap :: HashMap PilVar Sym
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''CheckerState)

emptyCheckerState :: CheckerState
emptyCheckerState = CheckerState (Sym 0) HashMap.empty HashMap.empty

newtype Checker a = Checker
  { _runChecker :: ExceptT CheckerError (StateT CheckerState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError CheckerError
           , MonadState CheckerState
           )

runChecker :: Checker a -> CheckerState -> (Either CheckerError a, CheckerState)
runChecker m ss = runIdentity . flip runStateT ss . runExceptT . _runChecker $ m

runChecker_ :: Checker a -> (Either CheckerError a, CheckerState)
runChecker_ m = runChecker m emptyCheckerState

-- | get pilvar's cooresponding Sym from state
lookupVarSym :: (MonadState CheckerState m, MonadError CheckerError m)
             => PilVar -> m Sym
lookupVarSym pv = do
  vsm <- use varSymMap
  case HashMap.lookup pv vsm of
    Nothing -> throwError $ CannotFindPilVarInVarSymMap pv
    Just s -> return s

lookupSymExpr :: (MonadState CheckerState m, MonadError CheckerError m)
             => Sym -> m SymExpression
lookupSymExpr sym' = do
  m <- use symMap
  case HashMap.lookup sym' m of
    Nothing -> throwError $ CannotFindSymInSymMap
    Just x -> return x

addSymExpression :: MonadState CheckerState m => Sym -> SymExpression -> m ()
addSymExpression sym' x = symMap %= HashMap.insert sym' x

addVarSym :: MonadState CheckerState m => PilVar -> Sym -> m ()
addVarSym pv sym' = varSymMap %= HashMap.insert pv sym'

-- | Create mapping of each PilVar to a symbol
createVarSymMap :: MonadState CheckerState m => [Statement Expression] -> m ()
createVarSymMap stmts = do
  let vars = Analysis.getAllVars stmts
  mapM_ f $ HashSet.toList vars
  where
    f var = newSym >>= addVarSym var

-- | Creates new, unused Sym
newSym :: MonadState CheckerState m => m Sym
newSym = do
  x <- use currentSym
  currentSym %= incrementSym
  return x

-- | converts expression to SymExpression (assigns symbols to all exprs), including itself
--   adds each new sym/expr pair to CheckerState
toSymExpression :: MonadState CheckerState m => Expression -> m SymExpression
toSymExpression (Expression sz op') = do
  symOp <- traverse toSymExpression op'
  s <- newSym
  let bitSize = fromIntegral sz * 8
      sexpr = InfoExpression (SymInfo bitSize s) symOp
  addSymExpression s sexpr
  return sexpr


-- maybe TODO: make a function for succinctly writing PIL expr op constraint generators

-- example for signed-greater-than
-- funcSig ["a", "b"] [(a, 

-- funcSig :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
--         => [XVar] -> [(XVar, ExistentialType)] -> [XVar]
--         -> m ( [SymType] -> m [(Sym, SymType)] )
-- funcSig existentials classBindings funcTypes = undefined

byteOffsetToBitWidth :: ByteOffset -> BitWidth
byteOffsetToBitWidth n
  -- For now, assume all offsets are positive.
  -- negative offsets will require thinking about how to deal with a type's size
  -- and how to check if nested records with negative fields conflict with
  -- the parent record's other fields.
  | n < 0 = P.error "Unexpected negative offset"
  | otherwise = fromIntegral $ n * 8

-- | Generates constraints for all syms in SymExpression.
exprTypeConstraints :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
                    => SymExpression -> m [(Sym, SymType)]
exprTypeConstraints (InfoExpression (SymInfo sz r) op') = case op' of
  Pil.ADC x -> integralBinOp Nothing x
  Pil.ADD x -> integralBinOpUnrelatedArgs Nothing x

  -- should this be unsigned ret because overflow is always positive?
  Pil.ADD_OVERFLOW x -> integralBinOp Nothing x

  Pil.AND x -> bitVectorBinOp x

  --   shift right...?
  Pil.ASR x -> integralFirstArgIsReturn x

--   BOOL_TO_INT _ -> 

  -- TODO get most general type for this and args:
  Pil.CALL _ -> return [ (r, SType $ THasWidth sz') ]
  
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
  Pil.CONST _ -> do
    unknownSignednessSym <- newSym
    return [(r, SType $ TInt sz' (SVar unknownSignednessSym))]
  Pil.CONST_PTR _ -> retPointer
  Pil.ConstStr x -> return [(r, SType $ TArray
                                ( SType . TVLength . fromIntegral . Text.length
                                  $ x ^. Pil.value )
                                ( SType TChar ))]
  Pil.DIVS x -> integralBinOp (Just True) x
  Pil.DIVS_DP x -> integralBinOpDP (Just True) x
  Pil.DIVU x -> integralBinOp (Just False) x
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
    fieldType <- SVar <$> newSym
    let recType = SType . TRecord . HashMap.fromList $
          -- for now, assuming all offsets are positive...
          [ (byteOffsetToBitWidth $ x ^. Pil.offset, fieldType) ]
    return [ ( r, SType $ TPointer sz' fieldType )
           , ( x ^. Pil.baseAddr . info . sym, SType $ TPointer sz' recType )
           ]

  Pil.FLOAT_CONST _ -> retFloat

-- TODO: should there be a link between sz of bitvec and sz of float?
  Pil.FLOAT_CONV x -> do
    bvWidth <- SVar <$> newSym
    return [ ( x ^. Pil.src . info . sym, SType $ TBitVector bvWidth )
           , ( r, SType $ TFloat sz' )
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
    ptrWidth <- SVar <$> newSym
    ptrType <- SVar <$> newSym
    return [ ( x ^. Pil.src . info . sym, SType $ TPointer ptrWidth ptrType )
           , ( r, ptrType )
           , ( r, SType $ THasWidth sz' )
           ]

  -- should _x have any influence on the type of r?
  Pil.LOW_PART _x -> return [(r, SType $ THasWidth sz')]

  Pil.LSL x -> integralFirstArgIsReturn x
  Pil.LSR x -> integralFirstArgIsReturn x
  Pil.MODS x -> integralBinOp (Just True) x
  Pil.MODS_DP x -> integralBinOpDP (Just True) x
  Pil.MODU x -> integralBinOp (Just False) x
  Pil.MODU_DP x -> integralBinOpDP (Just False) x
  Pil.MUL x -> integralBinOp Nothing x
  Pil.MULS_DP x -> integralBinOpDP (Just True) x
  Pil.MULU_DP x -> integralBinOpDP (Just False) x
  Pil.NEG x -> integralUnOp (Just True) x
  Pil.NOT x -> bitVectorUnOp x
  Pil.OR x -> bitVectorBinOp x
  Pil.RLC x -> integralFirstArgIsReturn x
  Pil.ROL x -> integralFirstArgIsReturn x
  Pil.ROR x -> integralFirstArgIsReturn x
  Pil.ROUND_TO_INT x -> floatToInt x
  Pil.RRC x -> integralFirstArgIsReturn x
  Pil.SBB x -> signedBinOpReturnsBool True x
--   -- STORAGE _ -> unknown
--   StrCmp _ -> intRet
--   StrNCmp _ -> intRet
--   MemCmp _ -> intRet

  -- should this somehow be linked to the type of the stack var?
  -- its type could change every Store.
  Pil.STACK_LOCAL_ADDR _ -> retPointer

  Pil.SUB x -> integralBinOpUnrelatedArgs (Just True) x
  Pil.SX x -> integralExtendOp x

--   TEST_BIT _ -> boolRet -- ? tests if bit in int is on or off
--   UNIMPL _ -> bitvecRet -- should this be unknown?
  Pil.UPDATE_VAR x -> do
    v <- lookupVarSym $ x ^. Pil.dest
    -- How should src and dest be related?
    -- Can't express that `offset + width(src) == width(dest)`
    --  without `+` and `==` as type level operators.
    return [ (r, SVar v) ]

  Pil.VAR x -> do
    v <- lookupVarSym $ x ^. Pil.src
    return [ (r, SVar v)
           , (v, SType $ THasWidth sz')
           ]
  Pil.VAR_FIELD _ ->
    -- TODO: can we know anything about src PilVar by looking at offset + result size?
    return [ (r, SType $ TBitVector sz') ]

--   VAR_SPLIT _ -> bitvecRet
  Pil.XOR x -> bitVectorBinOp x
  Pil.ZX x -> integralExtendOp x
--   -- _ -> unknown

--   VAR_PHI _ -> unknown -- should be removed by analysis

--   Extract _ -> bitvecRet
  _ -> P.error . show $ op'
    --throwError UnhandledExpr
  where
    sz' = SType $ TVBitWidth sz
    sz2x' = SType . TVBitWidth $ sz * 2
    getBoolRet = SType . TInt sz' . SVar <$> newSym
  
    retBool = do
      b <- getBoolRet
      return [(r, b)]

    retFloat :: m [(Sym, SymType)]
    retFloat = return [ (r, SType $ TFloat sz') ]

    retPointer = do
      pt <- newSym
      return [ (r, SType (TPointer sz' (SVar pt))) ]

    bitVectorUnOp :: (Pil.HasSrc x SymExpression) => x -> m [(Sym, SymType)]
    bitVectorUnOp x =
      return [ (r, SType $ TBitVector  sz')
             , (r, SVar $ x ^. Pil.src . info . sym)
             ]

    bitVectorBinOp :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => x -> m [(Sym, SymType)]
    bitVectorBinOp x =
      return [ (r, SType $ TBitVector sz')
             , (r, SVar $ x ^. Pil.left . info . sym)
             , (r, SVar $ x ^. Pil.right . info . sym)
             ]

    integralExtendOp :: (Pil.HasSrc x SymExpression) => x -> m [(Sym, SymType)]
    integralExtendOp x = do
      argSizeType <- SVar <$> newSym
      signednessType <- SVar <$> newSym
      return [ (r, SType $ TInt sz' signednessType)
             , (x ^. Pil.src . info . sym, SType $ TInt argSizeType signednessType)
             ]


    integralUnOp :: (Pil.HasSrc x SymExpression) => Maybe Bool -> x -> m [(Sym, SymType)]
    integralUnOp mSignedness x = do
      signednessType <- case mSignedness of
        Nothing -> SVar <$> newSym
        Just b -> return . SType . TVSign $ b
      return [ (r, SType (TInt sz' signednessType))
             , (r, SVar $ x ^. Pil.src . info . sym)
             ]

    integralBinOpUnrelatedArgs :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => Maybe Bool -> x -> m [(Sym, SymType)]
    integralBinOpUnrelatedArgs mSignedness x = do
      signednessType <- case mSignedness of
        Nothing -> SVar <$> newSym
        Just b -> return . SType . TVSign $ b
      arg1Sign <- SVar <$> newSym
      arg2Sign <- SVar <$> newSym
      arg1Width <- SVar <$> newSym
      arg2Width <- SVar <$> newSym
      return [ (r, SType (TInt sz' signednessType))
             , (x ^. Pil.left . info . sym, SType $ TInt arg1Width arg1Sign)
             , (x ^. Pil.right . info . sym, SType $ TInt arg2Width arg2Sign)
             ]

    integralBinOp :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => Maybe Bool -> x -> m [(Sym, SymType)]
    integralBinOp mSignedness x = do
      signednessType <- case mSignedness of
        Nothing -> SVar <$> newSym
        Just b -> return . SType . TVSign $ b
      return [ (r, SType (TInt sz' signednessType))
             , (r, SVar $ x ^. Pil.left . info . sym)
             , (r, SVar $ x ^. Pil.right . info . sym)
             ]

    -- first arg is double-precision of second and return
    -- signedness of args can apparently be anything
    integralBinOpDP :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => Maybe Bool -> x -> m [(Sym, SymType)]
    integralBinOpDP mSignedness x = do
      retSignednessType <- case mSignedness of
        Nothing -> SVar <$> newSym
        Just b -> return . SType . TVSign $ b
      return [ (r, SType (TInt sz' retSignednessType))
             , (x ^. Pil.left . info . sym, SType $ TInt sz2x' retSignednessType)
             , (r, SVar $ x ^. Pil.right . info . sym)
             ]


    integralBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                             => x -> m [(Sym, SymType)]
    integralBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      argSignSym <- newSym
      let argType = SType (TInt (SVar argWidthSym) (SVar argSignSym))
      return [ ( r, b )
             , ( x ^. Pil.left . info . sym, argType )
             , ( x ^. Pil.right . info . sym, argType )
             ]

    signedBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                             => Bool -> x -> m [(Sym, SymType)]
    signedBinOpReturnsBool isSignedBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      return [ (r, b)
             , (x ^. Pil.left . info . sym, SType (TInt (SVar argWidthSym) (SType $ TVSign isSignedBool)))
             , (x ^. Pil.right . info . sym, SType (TInt (SVar argWidthSym) (SType $ TVSign isSignedBool)))
             ]

    intToFloat :: (Pil.HasSrc x SymExpression)
               => x -> m [(Sym, SymType)]
    intToFloat x = do
      intWidth <- SVar <$> newSym
      intSign <- SVar <$> newSym
      return [ ( x ^. Pil.src . info . sym, SType $ TInt intWidth intSign )
             , ( r, SType $ TFloat sz' )
             ]


    floatToInt :: (Pil.HasSrc x SymExpression)
               => x -> m [(Sym, SymType)]
    floatToInt x = do
      floatWidth <- SVar <$> newSym
      intSign <- SVar <$> newSym
      return [ ( x ^. Pil.src . info . sym, SType $ TFloat floatWidth )
             , ( r, SType $ TInt sz' intSign )
             ]

    floatBinOp :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression) => x -> m [(Sym, SymType)]
    floatBinOp x = do
      return [ (r, SType (TFloat sz'))
             , (r, SVar $ x ^. Pil.left . info . sym)
             , (r, SVar $ x ^. Pil.right . info . sym)
             ]

    floatUnOp :: (Pil.HasSrc x SymExpression) => x -> m [(Sym, SymType)]
    floatUnOp x = do
      return [ (r, SType (TFloat sz'))
             , (r, SVar $ x ^. Pil.src . info . sym)
             ]

    floatBinOpReturnsBool :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                             => x -> m [(Sym, SymType)]
    floatBinOpReturnsBool x = do
      b <- getBoolRet
      argWidthSym <- newSym
      let argType = SType (TFloat $ SVar argWidthSym)
      return [ ( r, b )
             , ( x ^. Pil.left . info . sym, argType )
             , ( x ^. Pil.right . info . sym, argType )
             ]

    integralFirstArgIsReturn :: (Pil.HasLeft x SymExpression, Pil.HasRight x SymExpression)
                  => x -> m [(Sym, SymType)]
    integralFirstArgIsReturn x = do
      intWidth <- SVar <$> newSym
      shifterWidth <- SVar <$> newSym
      intSign <- SVar <$> newSym
      shifterSign <- SVar <$> newSym
      let n = SType $ TInt intWidth intSign
      return [ ( x ^. Pil.left . info . sym, n )
             , ( x ^. Pil.right . info . sym
               , SType $ TInt shifterWidth shifterSign )
             , ( r, n )
             ]


-- | recursively generates type constraints for all expr sym's in SymExpression
getAllExprTypeConstraints :: forall m. (MonadState CheckerState m, MonadError CheckerError m)
                    => SymExpression -> m [(Sym, SymType)]
getAllExprTypeConstraints x@(InfoExpression (SymInfo _ _thisExprSym) op') = do
  constraintsForThisExpr <- exprTypeConstraints x
  constraintsForChildren <- foldM f  [] op'
  return $ constraintsForThisExpr <> constraintsForChildren
  where
    f :: [(Sym, SymType)] -> SymExpression -> m [(Sym, SymType)]
    f constraints sexpr = (<> constraints) <$> getAllExprTypeConstraints sexpr

-- | get all rules for a stmt
--   create `Statement SymExpression`
stmtTypeConstraints :: (MonadState CheckerState m, MonadError CheckerError m)
                    => Statement Expression -> m (Statement SymExpression, [(Sym, SymType)])
stmtTypeConstraints (Pil.Def (Pil.DefOp pv expr)) = do
  symExpr <- toSymExpression expr
  let exprSym = symExpr ^. info . sym
  pvSym <- lookupVarSym pv
  exprConstraints <- getAllExprTypeConstraints symExpr
  return ( Pil.Def (Pil.DefOp pv symExpr)
         , [ (pvSym, SVar exprSym) ]
           <> exprConstraints )
stmtTypeConstraints (Pil.Constraint (Pil.ConstraintOp expr)) = do
  symExpr <- toSymExpression expr
  exprConstraints <- getAllExprTypeConstraints symExpr
  return ( Pil.Constraint (Pil.ConstraintOp symExpr)
         , exprConstraints )
stmtTypeConstraints (Pil.Store (Pil.StoreOp addrExpr valExpr)) = do
  symAddrExpr <- toSymExpression addrExpr
  symValExpr <- toSymExpression valExpr
  let symAddr = symAddrExpr ^. info . sym
      symVal = symValExpr ^. info . sym
  addrExprConstraints <- getAllExprTypeConstraints symAddrExpr
  valExprConstraints <- getAllExprTypeConstraints symValExpr
  ptrWidth <- SVar <$> newSym
  return ( Pil.Store (Pil.StoreOp symAddrExpr symValExpr)
         , addrExprConstraints <> valExprConstraints
           <> [ ( symAddr, SType $ TPointer ptrWidth (SVar symVal) ) ]
         )
stmtTypeConstraints s = (,[]) <$> traverse toSymExpression s


-- TODO
--toTypedExpression :: SymExpression -> TypedExpression

-- TODO
-- symStatementToTypedStatement :: Statement SymExpression -> Statement TypedExpression
-- symStatementToTypedStatement 


-- | if a symbol type cannot be inferred, it will be in the [(Sym, Sym)] | --
splitSVarsAndSTypes :: [(Sym, SymType)] -> ([(Sym, Sym)], [(Sym, PilType SymType)])
splitSVarsAndSTypes xs = (mapMaybe getSVar xs, mapMaybe getSType xs)
  where
    getSVar :: (Sym, SymType) -> Maybe (Sym, Sym)
    getSVar (sym', SVar x) = Just (sym', x)
    getSVar _ = Nothing

    getSType :: (Sym, SymType) -> Maybe (Sym, PilType SymType)
    getSType (sym', SType x) = Just (sym', x)
    getSType _ = Nothing

-- | replaces Sym's with expressions, fails if Sym missing in map |--
symInfoToPilType :: HashMap Sym (PilType a) -> SymInfo -> Maybe (PilType a)
symInfoToPilType m si = HashMap.lookup (si ^. sym) m

-- | replaces Sym's with expressions, fails if Sym missing in map |--
toTypedExpression :: HashMap Sym (PilType T) -> SymExpression -> Maybe TypedExpression
toTypedExpression m = traverse (symInfoToPilType m)


------------------- unification --------------

class IsType a where
  getTypeWidth :: a -> Maybe BitWidth
  getTypeLength :: a -> Maybe Word64
  getTypeSign :: a -> Maybe Bool

instance IsType T where
  getTypeWidth (T pt) = getTypeWidth pt
  getTypeLength (T pt) = getTypeLength pt
  getTypeSign (T pt) = getTypeSign pt

instance IsType SymType where
  getTypeWidth (SVar _) = Nothing
  getTypeWidth (SType pt) = getTypeWidth pt

  getTypeLength (SVar _) = Nothing
  getTypeLength (SType pt) = getTypeLength pt

  getTypeSign (SVar _) = Nothing
  getTypeSign (SType pt) = getTypeSign pt


instance IsType a => IsType (PilType a) where
  getTypeWidth (TArray len' et) = (*) <$> (fromIntegral <$> getTypeLength len')
                                      <*> getTypeWidth et
  getTypeWidth (TVBitWidth bw) = Just bw
  getTypeWidth TChar = Just charSize
  getTypeWidth (TInt w _) = getTypeWidth w
  getTypeWidth (TFloat w) = getTypeWidth w
  getTypeWidth (TBitVector w) = getTypeWidth w
  getTypeWidth (TPointer w _) = getTypeWidth w
  getTypeWidth (TRecord m) = Just $ getMinimumRecordWidth m
  getTypeWidth (THasWidth t) = getTypeWidth t
  getTypeWidth _ = Nothing

  getTypeLength (TVLength n) = Just n
  getTypeLength (TArray len' _) = getTypeLength len'
  getTypeLength _ = Nothing

  -- maybe should be TVSign instead of TVSign
  getTypeSign (TVSign b) = Just b
  getTypeSign (TInt _ s) = getTypeSign s
  getTypeSign _ = Nothing


-- | True if second is at the same level or below in the type lattice
--   doesn't check recursive types or type sizes/signs.
--   Used to order args for unification
isTypeDescendent :: PilType a -> PilType a -> Bool
isTypeDescendent (TArray _ _) t = case t of
  TArray _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TInt _ _) t = case t of
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TFloat _) t = case t of
  TFloat _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TBitVector _) t = case t of
  TBitVector _ -> True
  -- I think these should be descendents
  TInt _ _ -> True
  TPointer _ _ -> True
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TPointer _ _) t = case t of
  TPointer _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TChar t = case t of
  TChar -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TFunction _ _) t = case t of
  (TFunction _ _) -> True
  TBottom -> True
  _ -> False
isTypeDescendent (TRecord _) t = case t of
  TRecord _ -> True
  TPointer _ _ -> True
  TBottom -> True
  _ -> False
isTypeDescendent TBottom t = case t of
  TBottom -> True
  _ -> False
isTypeDescendent (THasWidth _) t = case t of
  THasWidth _ -> True
  TChar -> True
  TInt _ _ -> True
  TFloat _ -> True
  TPointer _ _ -> True
  _ -> False
isTypeDescendent (TVBitWidth _) t = case t of
  TVBitWidth _ -> True
  _ -> False
isTypeDescendent (TVLength _) t = case t of
  TVLength _ -> True
  _ -> False
isTypeDescendent (TVSign _) t = case t of
  TVSign _ -> True
  _ -> False



-- | given the fields in the hashmap, find the greatest (offset + known width)
--   This doesn't consider padding or error on overlapping fields.
getMinimumRecordWidth :: IsType a => HashMap BitWidth a -> BitWidth
getMinimumRecordWidth m = max maxOffset maxOffsetPlusKnownWidth
  where
    -- for if a field has a big offset, but an unkown width
    maxOffset = foldr max 0 . HashMap.keys $ m
    maxOffsetPlusKnownWidth = foldr max 0
                              . mapMaybe fieldReach
                              . HashMap.toList $ m
    fieldReach (off, pt) = (+ off) <$> getTypeWidth pt


-- | if field has offset 32 and width 16, its range is (32, 48)
--   if field has offset 32, but unknown width, it's range is (32, 33) | --
getFieldRange :: IsType a => BitWidth -> a-> (BitWidth, BitWidth)
getFieldRange off = (off,) . (+off) . maybe 1 identity . getTypeWidth

getFieldRanges :: IsType a => HashMap BitWidth a -> [(BitWidth, BitWidth)]
getFieldRanges m = uncurry getFieldRange <$> HashMap.toList m

doFieldRangesOverlap :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
doFieldRangesOverlap (start, end) (start', end') = 
  start >= start' && start < end'
  || end > start' && end <= end'
  || start' >= start && start' < end
  || end' > start && end' <= end

secondRangeContainsFirst :: (BitWidth, BitWidth) -> (BitWidth, BitWidth) -> Bool
secondRangeContainsFirst (start, end) (start', end') =
  start >= start' && start < end'
  && end >= start' && end <= end'


---------- unification and constraint solving ----------------------

data Constraint = Constraint (Sym, SymType)
  deriving (Eq, Ord, Read, Show, Generic)

-- | solutions should be the "final unification" for any sym.
-- | complex types might still contain SVars subject to substitution
-- | but the type structure shouldn't change.
newtype Solution = Solution (Sym, SymType)
  deriving (Eq, Ord, Read, Show, Generic)



data UnifyWithSubsState = UnifyWithSubsState
                          { _accSubs :: [Constraint]
                            -- , _solutions :: [(Sym, SymType)]
                             -- , _errors :: [UnifyError]
                          } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyWithSubsState)

data UnifyResult = UnifyResult { _solutions :: [(Sym, SymType)]
                               , _errors :: [UnifyError]
                               } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyResult)

-- | monad just used for unifyWithSubs function and its helpers
newtype UnifyWithSubs a = UnifyWithSubs { _runUnifyWithSubs :: ExceptT UnifyError (StateT UnifyWithSubsState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError UnifyError
           , MonadState UnifyWithSubsState
           )

runUnifyWithSubs :: UnifyWithSubs a -> UnifyWithSubsState -> (Either UnifyError a, UnifyWithSubsState)
runUnifyWithSubs m s = runIdentity . flip runStateT s . runExceptT . _runUnifyWithSubs $ m


addSubs :: MonadState UnifyWithSubsState m => [Constraint] -> m ()
addSubs subs = accSubs %= (<> subs)


unifyWithSubs :: SymType -> SymType -> (Either UnifyError SymType, [Constraint])
unifyWithSubs t1 t2 =
  let (er, ustate) = runUnifyWithSubs (unifyWithSubsM t1 t2) (UnifyWithSubsState []) in
    (er, ustate ^. accSubs)

-- | returns unification to most specific and saves substitutions in state
unifyWithSubsM :: ( MonadError UnifyError m
                  , MonadState UnifyWithSubsState m
                  )
               => SymType -> SymType -> m SymType
unifyWithSubsM (SVar a) (SVar b) = addSubs [Constraint (b, SVar a)] >> pure (SVar a)
unifyWithSubsM (SVar a) (SType pt) = addSubs [Constraint (a, SType pt)] >> pure (SType pt)
unifyWithSubsM a@(SType _) b@(SVar _) = unifyWithSubsM b a
unifyWithSubsM (SType pt1) (SType pt2) =
  case (isTypeDescendent pt1 pt2, isTypeDescendent pt2 pt1) of
    (False, False) -> err
    (False, True) -> unifyWithSubsM (SType pt2) (SType pt1)
    _ -> case pt1 of
      TArray len1 et1 -> case pt2 of
        (TArray len2 et2)
          | len1 == len2 -> stype $ TArray len1 <$> unifyWithSubsM et1 et2
          | otherwise -> err -- array length mismatch
        (TPointer _w et2) -> stype $ TArray len1 <$> unifyWithSubsM et1 et2
        _ -> err
      TInt w1 sign1 -> case pt2 of
        TInt w2 sign2 -> stype $ TInt <$> unifyBitWidth w1 w2
                                      <*> unifySign sign1 sign2
        TPointer w2 pointeeType1 -> do
          void . unifyWithSubsM sign1 . SType $ TVSign False
          stype $ flip TPointer pointeeType1 <$> unifyBitWidth w1 w2
        TChar -> do
          void . unifyWithSubsM w1 . SType $ TVBitWidth charSize
          void . unifyWithSubsM sign1 . SType $ TVSign False
          solo TChar
        _ -> err

      -- TUnsigned w1 -> case pt2 of
      --   TUnsigned w2 -> soloW w1 w2 $ TUnsigned w1
      --   TPointer w2 ptype -> soloW w1 w2 $ TPointer w2 ptype
      --   TChar -> soloW w1 charSize TChar
      --   _ -> err
      TChar -> case pt2 of
        TChar -> solo TChar
        _ -> err
      TFloat w1 -> case pt2 of
        TFloat w2 -> stype $ TFloat <$> unifyBitWidth w1 w2
        _ -> err
      TBitVector w1 -> case pt2 of
        TBitVector w2 -> stype $ TBitVector <$> unifyBitWidth w1 w2
        TInt w2 s -> stype $ TInt <$> unifyBitWidth w1 w2 <*> pure s
        _ -> err
      TPointer w1 pointeeType1 -> case pt2 of
        TPointer w2 pointeeType2 ->
          stype $ TPointer <$> unifyBitWidth w1 w2
                           <*> unifyWithSubsM pointeeType1 pointeeType2
        _ -> err
      TFunction ret1 params1 -> err -- don't know how to unify at the moment...
      TRecord m1 -> case pt2 of
        TRecord m2 -> stype $ TRecord <$> mergeRecords m1 m2
        TPointer _ t -> stype . fmap TRecord . mergeRecords m1 . HashMap.fromList $ [(0, t)]
        _ -> err

      THasWidth w1 -> case pt2 of
        TChar -> unifyBitWidth w1 (SType $ TVBitWidth 8)
        TInt w2 s -> stype $ TInt <$> unifyBitWidth w1 w2 <*> pure s
        TFloat w2 -> stype $ TFloat <$> unifyBitWidth w1 w2
        TPointer w2 et -> stype $ TPointer <$> unifyBitWidth w1 w2 <*> pure et
        _ -> err

      TVBitWidth bw1 -> case pt2 of
        TVBitWidth bw2
          | bw1 == bw2 -> solo $ TVBitWidth bw1
          | otherwise -> err
        _ -> err

      TVLength len1 -> case pt2 of
        TVLength len2
          | len1 == len2 -> solo $ TVLength len1
          | otherwise -> err
        _ -> err

      TVSign s1 -> case pt2 of
        TVSign s2
          | s1 == s2 -> solo $ TVSign s1
          | otherwise -> err
        _ -> err

      _ -> err
  where
    stype = (SType <$>)
    err = throwError $ IncompatibleTypes pt1 pt2

    unifyBitWidth w1 w2 = guardBitWidth =<< unifyWithSubsM w1 w2
    unifySign s1 s2 = guardSign =<< unifyWithSubsM s1 s2

    guardBitWidth x@(SVar _) = pure x
    guardBitWidth x@(SType (TVBitWidth _)) = pure x
    guardBitWidth _ = err -- maybe need better error here

    guardSign x@(SVar _) = pure x
    guardSign x@(SType (TVSign _)) = pure x
    guardSign _ = err -- maybe need better error here

    guardLength x@(SVar _) = pure x
    guardLength x@(SType (TVLength _)) = pure x
    guardLength _ = err -- maybe need better error here
    
    solo = pure . SType
    --duo a b = unifyWithSubsM


-- | returns Nothing if there is a known conflict.
--   TODO: allow some overlapping fields, like an Int16 in an Int32 | --
addFieldToRecord :: ( MonadError UnifyError m
                    , MonadState UnifyWithSubsState m
                    )
                 => HashMap BitWidth SymType
                 -> BitWidth
                 -> SymType
                 -> m (HashMap BitWidth SymType)
addFieldToRecord m off pt = case HashMap.lookup off m of
  Just pt' -> do
    x <- unifyWithSubsM pt pt'
    checkOverlap x $ HashMap.delete off m
    return $ HashMap.insert off x m
  Nothing -> do
    checkOverlap pt m
    return $ HashMap.insert off pt m
  where
    checkOverlap pt' m'
      | (not . any (doFieldRangesOverlap $ getFieldRange off pt') . getFieldRanges $ m') = return ()
      | otherwise = throwError $ OverlappingRecordField (HashMap.insert off pt' m') off

mergeRecords :: ( MonadError UnifyError m
                , MonadState UnifyWithSubsState m
                )
             => HashMap BitWidth SymType
             -> HashMap BitWidth SymType
             -> m (HashMap BitWidth SymType)
mergeRecords m1 = foldM (uncurry . addFieldToRecord) m1 . HashMap.toList

substitute_ :: (Sym -> Maybe SymType) -> SymType -> SymType
substitute_ f (SVar v) = maybe (SVar v) identity $ f v
substitute_ f (SType pt) = SType $ substitute_ f <$> pt


-- | applies subs to sym type
substitute' :: (Sym, SymType) -> SymType -> SymType
substitute' (v, t) (SVar v')
  | v == v' = t
  | otherwise = SVar v'
substitute' x (SType pt) = SType $ substitute' x <$> pt

class Substitute a where
  substitute :: Solution -> a -> a

instance Substitute Constraint where
  substitute (Solution (v, t)) (Constraint (v', t')) =
    Constraint . (v',) $ substitute' (v, t) t'

instance Substitute Solution where
  substitute (Solution (v, t)) (Solution (v', t')) =
    Solution . (v',) $ substitute' (v, t) t'


data UnifyConstraintsResult = UnifyConstraintsResult
  { _constraints :: [(Sym, SymType)]
  , _solutions :: [(Sym, SymType)]
  , _errors :: [UnifyError]
  } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyConstraintsResult)



-- | unifies (v, t) with any (v', t') where v == v'
-- | returns most unified type for v, plus any extra subs
-- | (which should be appended to end of constraints list)
getMostUnifiedConstraintAndSubs :: Constraint
                                -> [Constraint]
                                -> (Solution, [Constraint], [UnifyError])
getMostUnifiedConstraintAndSubs (Constraint cx) cxs = foldr f (Solution cx, [], []) cxs
  where
    f (Constraint (cv, ct)) ((Solution (sv, st)), newConstraints, errs)
      | cv /= sv = (Solution (sv, st), (Constraint (cv,ct)):newConstraints, errs)
      | otherwise = let (er, subs) = unifyWithSubs st ct in
          case er of
            Left uerr -> (Solution (sv, st), subs <> newConstraints, uerr:errs)
            Right ut -> (Solution (sv, ut), subs <> newConstraints, errs)


-- | cx - constraint "at bat"
-- | cxs - non-solution constraint list, minus cx
-- | sols - solutions (the final unification for any var, subject to substitution)
-- | outputs (updated constraints, updated solutions, errors)
unifyConstraintWithAll :: Constraint
                       -> [Constraint]
                       -> [Solution]
                       -> ([Constraint], [Solution], [UnifyError])
unifyConstraintWithAll cx cxs sols  =
  ( substitute sol <$> constraintsWithSubs
  , sol : (substitute sol <$> sols)
  , errs'
  )
  where
    (sol, constraintsWithSubs, errs') = getMostUnifiedConstraintAndSubs cx cxs
    

unifyConstraints' :: [Constraint]
                  -> [Solution]
                  -> [UnifyError]
                  -> ([Solution], [UnifyError])
unifyConstraints' [] sols errs = (sols, errs)
unifyConstraints' (cx:cxs) sols errs =
  unifyConstraints' cxs' sols' (errs <> errs')
  where
      (cxs', sols', errs') = unifyConstraintWithAll cx cxs sols

unifyConstraints :: [Constraint] -> ([Solution], [UnifyError])
unifyConstraints cxs = (revertVarCopiedSolution restoreMap sols, errs)
  where
    (cxs', restoreMap) = varCopyConstraints cxs
    (sols, errs) = unifyConstraints' cxs' [] []
    

-------------------------

-- for debugging...
stmtsConstraints :: [Statement Expression]
                 -> Either CheckerError ([Statement SymExpression], [Constraint], CheckerState)
stmtsConstraints stmts = case er of
  Left err -> Left err
  Right (symStmts, cxs) -> Right (symStmts, cxs, s)
  where
    (er, s) = runChecker_ $ do
      createVarSymMap stmts
      (symStmts, cxs) <- foldM getStmtConstraints ([], []) stmts
      return (symStmts, Constraint <$> cxs)
      
    getStmtConstraints :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> Checker ([Statement SymExpression], [(Sym, SymType)])
    getStmtConstraints (symStmts, cxs) stmt = do
      (sstmt, cxs') <- stmtTypeConstraints stmt
      return ( symStmts <> [sstmt] -- maybe should use a Vector
             , cxs' <> cxs)


stmtSolutions :: [Statement Expression]
              -> Either CheckerError ([Statement SymExpression], [Solution], [UnifyError], CheckerState)
stmtSolutions stmts = case er of
  Left err -> Left err
  Right (symStmts, cxs) -> Right (symStmts, sols, errs, s)
    where
      (sols, errs) = unifyConstraints cxs
  where
    (er, s) = runChecker_ $ do
      createVarSymMap stmts
      (symStmts, cxs) <- foldM getStmtConstraints ([], []) stmts
      return (symStmts, Constraint <$> cxs)
      
    getStmtConstraints :: ([Statement SymExpression], [(Sym, SymType)]) -> Statement Expression -> Checker ([Statement SymExpression], [(Sym, SymType)])
    getStmtConstraints (symStmts, cxs) stmt = do
      (sstmt, cxs') <- stmtTypeConstraints stmt
      return ( symStmts <> [sstmt] -- maybe should use a Vector
             , cxs' <> cxs)

-- | main function to type check / infer statements
--   currently only returning types of pilvars in stmts for testing.
checkStmts :: [Statement Expression] -> Either CheckerError TypeReport
checkStmts = fmap toReport . stmtSolutions
  where
    toReport :: ([Statement SymExpression], [Solution], [UnifyError], CheckerState)
             -> TypeReport
    toReport (stmts, sols, errs, s) = TypeReport
      { _symTypeStmts = []
      , _varSymTypeMap = pilVarMap
      , _errors = errs
      }
      where
        solutionsMap :: HashMap Sym SymType
        solutionsMap = HashMap.fromList . fmap coerce $ sols

        pilVarMap :: HashMap PilVar SymType
        pilVarMap = fmap f $ s ^. varSymMap
          where
            f :: Sym -> SymType
            f sv = maybe (SVar sv) identity $ HashMap.lookup sv solutionsMap


--- constraint copy prop

getVarPair :: Constraint -> Maybe (Sym, Sym)
getVarPair (Constraint (s1, (SVar s2))) = Just (s1, s2)
getVarPair _ = Nothing

type EqualityMap a = HashMap a (HashSet a)

addVarPairToEqualityMap :: (Eq a, Hashable a)
                        => (a, a) -> EqualityMap a -> EqualityMap a
addVarPairToEqualityMap (v1, v2) = HashMap.unionWith HashSet.union m
  where
    s = HashSet.fromList [v1, v2]
    m = HashMap.fromList [ (v1, s)
                         , (v2, s)
                         ]

-- | [(0, {3, 1}), (1, {8})] becomes [(0, {0,1,3,8}), (1, {0,1,3,8}) ...]
-- TODO: make this more efficient
unionEqualityMap :: forall a. (Eq a, Hashable a) => EqualityMap a -> EqualityMap a
unionEqualityMap m = foldr f m $ HashMap.toList m
  where
    f :: (a, HashSet a) -> EqualityMap a -> EqualityMap a
    f (x, s) m' = HashMap.insert x s' . foldr (HashMap.alter g) m' $ HashSet.toList s
      where
        s' = HashSet.insert x s

        g Nothing = Just s'
        g (Just s'') = Just $ HashSet.union s s''

varPairsToEqualityMap :: (Eq a, Hashable a, Foldable t)
                     => t (a, a) -> EqualityMap a
varPairsToEqualityMap = foldr f HashMap.empty
  where
    f (v1, v2) = HashMap.alter g v1
      where
        g Nothing = Just $ HashSet.singleton v2
        g (Just s) = Just $ HashSet.insert v2 s

pairsToGroups :: (Hashable a, Ord a) => [(a, a)] -> [NonEmpty a]
pairsToGroups =
  fmap NG.vertexList1
  . G.vertexList
  . GA.scc
  . G.edges
  . concatMap dup
  where
    dup (a, b) = [(a, b), (b, a)]

-- | To subst many equivalent vars to one
substMapFromGroups :: (Hashable a, Eq a) => [NonEmpty a] -> HashMap a a
substMapFromGroups groups = HashMap.fromList $ do
  (rep :| xs) <- groups
  x <- xs
  return (x, rep)

restorationMapFromGroups :: (Hashable a, Eq a)
                         => [NonEmpty a] -> HashMap a (HashSet a)
restorationMapFromGroups groups = HashMap.fromList $ do
  g <- groups
  let xs = NE.toList g
      s = HashSet.fromList xs
  x <- xs
  return (x, s)


substVarsInConstraint :: HashMap Sym Sym -> Constraint -> Constraint
substVarsInConstraint m (Constraint (v, t)) =
  Constraint ( maybe v identity $ HashMap.lookup v m
             , substitute_ (flip HashMap.lookup $ SVar <$> m) t)

-- | returns copy-propped constraints and an equality map, which can
--   be used after constraint solving to duplicate constraints for
--   equal vars.
varCopyConstraints :: [Constraint] -> ([Constraint], EqualityMap Sym)
varCopyConstraints cxs =
  ( substVarsInConstraint substMap <$> cxsWithoutVarVars
  , restorationMap )
  where
    cxsWithoutVarVars = filter (not . isVarVar) cxs

    isVarVar (Constraint (_, SVar _)) = True
    isVarVar _ = False

    groups = pairsToGroups $ mapMaybe getVarPair cxs

    substMap = substMapFromGroups groups

    restorationMap = restorationMapFromGroups groups


-- | adds back in all the vars excised from copy prop
--   i.e. [(a, b), (b, c), (c, T)] copy props to [(a, T)]
--   this generates [(a, T), (b, T), (c, T)]
revertVarCopiedSolution :: EqualityMap Sym -> [Solution] -> [Solution]
revertVarCopiedSolution m = concatMap f
  where
    f s@(Solution (v, t)) =
      maybe [s] (fmap (Solution . (,t)) . HashSet.toList) $ HashMap.lookup v m


