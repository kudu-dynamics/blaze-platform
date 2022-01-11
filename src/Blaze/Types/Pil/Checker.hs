module Blaze.Types.Pil.Checker where

import Blaze.Prelude hiding (Constraint, Type, bitSize, sym)
import Blaze.Types.Pil (
  ExprOp,
  FuncVar,
  PilVar,
  StackOffset,
  Statement,
 )
import qualified Data.HashMap.Strict as HashMap

type BitWidth = Bits
type ByteWidth = Bytes

charSize :: BitWidth
charSize = 8

type SymConstraint = (Sym, ConstraintSymType)

newtype Sym = Sym Int
  deriving (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable)

data TypeTag = TagDirty
             | TagSanitized
             | TagAllocedMemory
             | TagFreedMemory
             | TagNullPtr
             | TagNonNull
             deriving (Eq, Ord, Read, Show, Generic)

data PilType t = TBool
               | TChar
               
               | TInt { bitWidth :: t, signed :: t }
               | TFloat { bitWidth :: t }
               | TBitVector { bitWidth :: t }
               | TPointer { bitWidth :: t, pointeeType :: t }

               | TCString { len :: t }

               | TArray { len :: t, elemType :: t }
               | TRecord (HashMap BitOffset -- todo: change bitwidth to 't'?
                           -- TODO: change bitwidth to signed offset
                                  t -- type
                         )

               -- first record field or array index, or itself
               -- t is type of first thing

               | TUnit
               
               -- Bottom is labeled with error info
               -- it only results from a unification error
               | TBottom Sym

               | TFunction { ret :: t, params :: [t] }
              
               -- type level values for some dependent-type action
               | TVBitWidth BitWidth
               | TVLength Word64
               | TVSign Bool    
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic, Hashable)

newtype T = T (PilType T)
  deriving (Eq, Ord, Read, Show, Generic)

unT :: T -> PilType T
unT (T pt) = pt

data SymType = SVar Sym
             | SType (PilType Sym)
             deriving (Eq, Ord, Read, Show, Generic)

data ConstraintSymType = CSVar Sym
                       | CSType (PilType ConstraintSymType)
                       deriving (Eq, Ord, Read, Show, Generic)

data DeepSymType = DSVar Sym
                 | DSRecursive Sym (PilType DeepSymType)
                 | DSType (PilType DeepSymType)
               deriving (Eq, Ord, Read, Show, Generic, Hashable)

--joinDeepSymTypes :: DeepSymType -> DeepSymType -> DeepSymType

data Constraint = Constraint
  { stmtOrigin :: Int -- probably need (func, instructionIndex) eventually
  , sym :: Sym
  , symType :: SymType
  } deriving (Eq, Ord, Show, Generic)

-- | solutions should be the "final unification" for any sym.
-- | complex types might still contain SVars subject to substitution
-- | but the type structure shouldn't change.

newtype Solution = Solution (Sym, PilType Sym)
  deriving (Eq, Ord, Show, Generic)

-- XVars are existential vars used for function sigs
type XVar = Text
data ExistentialType = XVar Text
                     | XType (PilType ExistentialType)
                     deriving (Eq, Ord, Read, Show, Generic)

data ConstraintGenError = CannotFindPilVarInVarSymMap PilVar
                        | CannotFindSymInSymMap
                        | UnhandledExpr
                        | UnhandledStmt
                        | BadStubCallArgCount { funcName :: Text
                                              , stmtIndex :: Int
                                              , expected :: Int
                                              , got :: Int
                                              }
                        deriving (Eq, Ord, Show, Generic)

data InfoExpression a = InfoExpression
  { info :: a
  , op :: ExprOp (InfoExpression a)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable, FromJSON, ToJSON)

instance Hashable a => Hashable (InfoExpression a)

data SymInfo = SymInfo
  { size :: BitWidth
  , sym :: Sym
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass Hashable

type SymExpression = InfoExpression SymInfo

-- | Goal is to generate statements with TypedExpressions
type TypedExpression = InfoExpression (PilType T)

data UnifyError t = UnifyError (PilType t) (PilType t) (UnifyError t)
                  | IncompatibleTypes (PilType t) (PilType t)
                  | OverlappingRecordField { recordFields :: HashMap BitWidth t
                                           , offendingOffset :: BitWidth
                                           }
                  deriving (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)

data UnifyConstraintsError t = UnifyConstraintsError
                               { stmtOrigin  :: Int
                               --index in list of pil stmts for now
                               , sym :: Sym
                               , error :: UnifyError t
                               } deriving (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)

type EqualityMap a = HashMap a (HashSet a)

type VarEqMap = EqualityMap Sym

-- | The final report of the type checker, which contains types and errors.
data TypeReport = TypeReport
  { symTypeStmts :: [(Int, Statement (InfoExpression (SymInfo, Maybe DeepSymType)))]
  , symStmts :: [(Int, Statement SymExpression)]
  --  , _typedStmts :: [Statement TypedExpression]
  , varSymTypeMap :: HashMap PilVar DeepSymType
  , varSymMap :: HashMap PilVar Sym
--  , _unresolvedStmts :: [Statement SymExpression]
  -- , _unresolvedSyms :: [(Sym, Sym)]
  -- , _unresolvedTypes :: [(Sym, PilType SymType, PilType SymType)]
  , varEqMap :: VarEqMap
  , funcSymTypeMap :: HashMap (FuncVar SymExpression) DeepSymType
  , funcSymMap :: HashMap (FuncVar SymExpression) Sym
  , errors :: [UnifyConstraintsError DeepSymType]
  , flatSolutions :: HashMap Sym (PilType Sym)
  , solutions :: HashMap Sym DeepSymType
  , originMap :: HashMap Sym Sym -- from UnifyState
  , errorConstraints :: HashMap Sym [Constraint] -- original constraints
  , ogConstraints :: [Constraint]
  } deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------
------ Constraint generation phase ---------------------------

type CallConstraintGenerator = Sym -> [Sym] -> ConstraintGen ()

newtype ConstraintGenCtx = ConstraintGenCtx
  { -- hashmap index is (stmt index from IndexedStmts, function name)
    callConstraintGenerators :: HashMap (Int, Text) CallConstraintGenerator
  } deriving (Generic)

emptyConstraintGenCtx :: ConstraintGenCtx
emptyConstraintGenCtx = ConstraintGenCtx HashMap.empty

data ConstraintGenState = ConstraintGenState
  { currentSym :: Sym
  , symMap :: HashMap Sym SymExpression
  , varSymMap :: HashMap PilVar Sym
  , funcSymMap :: HashMap (FuncVar SymExpression) Sym
  , constraints :: [Constraint]
  , currentStmt :: Int
  , stackAddrSymMap :: HashMap StackOffset Sym
  } deriving (Eq, Ord, Show, Generic)


emptyConstraintGenState :: ConstraintGenState
emptyConstraintGenState = ConstraintGenState (Sym 0) HashMap.empty HashMap.empty HashMap.empty [] 0 HashMap.empty

newtype ConstraintGen a = ConstraintGen
  { _runConstraintGen :: ExceptT ConstraintGenError (ReaderT ConstraintGenCtx (StateT ConstraintGenState Identity)) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadError ConstraintGenError
                   , MonadState ConstraintGenState
                   , MonadReader ConstraintGenCtx
                   )


runConstraintGen :: ConstraintGen a -> (ConstraintGenCtx, ConstraintGenState) -> (Either ConstraintGenError a, ConstraintGenState)
runConstraintGen m (ctx, ss) = runIdentity . flip runStateT ss . flip runReaderT ctx . runExceptT . _runConstraintGen $ m

runConstraintGen_ :: ConstraintGen a -> (Either ConstraintGenError a, ConstraintGenState)
runConstraintGen_ m = runConstraintGen m (emptyConstraintGenCtx, emptyConstraintGenState)

--------------------------------------------------------------------
---------- unification and constraint solving ----------------------

data UnifyState = UnifyState
                  { constraints :: [Constraint]

                  -- solution key syms should all be origins in originMap
                  , solutions :: HashMap Sym (PilType Sym)

                  , errors :: [UnifyConstraintsError Sym]

                  -- this is a map of syms to their "original" sym
                  -- like if you add (a, b), (b, c)
                  -- it should store a | b  -> c
                  , originMap :: HashMap Sym Sym
                  , currentStmt :: Int
                  } deriving (Eq, Ord, Show, Generic)


addConstraint_ :: ( HasField' "constraints" s [Constraint]
                  , HasField' "currentStmt" s Int
                  , MonadState s m )
                  => Sym -> SymType -> m ()
addConstraint_ s st = do
  i <- use #currentStmt
  #constraints %= (Constraint i s st :)

assignType :: ( HasField' "constraints" s [Constraint]
              , HasField' "currentStmt" s Int
              , MonadState s m)
               => Sym -> PilType Sym -> m ()
assignType s t = addConstraint_ s (SType t)

equals :: ( HasField' "constraints" s [Constraint]
          , HasField' "currentStmt" s Int
          , MonadState s m)
            => Sym -> Sym -> m ()
equals x y = addConstraint_ x (SVar y)

data UnifyResult = UnifyResult { solutions :: [(Sym, SymType)]
                               , errors :: [UnifyError Sym]
                               } deriving (Eq, Ord, Read, Show)

-- | monad just used for unifyWithSubs function and its helpers
newtype Unify a = Unify { _runUnify :: ExceptT (UnifyError Sym) (StateT UnifyState Identity) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadError (UnifyError Sym)
                   , MonadState UnifyState
                   )

runUnify :: Unify a -> UnifyState -> (Either (UnifyError Sym) a, UnifyState)
runUnify m s = runIdentity . flip runStateT s . runExceptT . _runUnify $ m

popConstraint :: Unify (Maybe Constraint)
popConstraint = use #constraints >>= \case
  [] -> return Nothing
  (cx:cxs) -> do
    #constraints .= cxs
    return $ Just cx


class VarSubst a where
  varSubst :: HashMap Sym Sym -> a -> a

instance VarSubst Sym where
  varSubst m v = maybe v identity $ HashMap.lookup v m

instance VarSubst SymType where
  varSubst m (SVar v) = SVar $ varSubst m v
  varSubst m (SType t) = SType $ varSubst m t

-- shouldn't really need to do this because DeepSymTypes are constructed
-- using the `Solutions` map, which already should only have origin vars
instance VarSubst DeepSymType where
  varSubst m (DSVar v) = DSVar $ varSubst m v
  varSubst m (DSType t) = DSType $ varSubst m t
  varSubst m (DSRecursive s t) = DSRecursive (varSubst m s) (varSubst m t)

instance VarSubst Constraint where
  varSubst m (Constraint i v t) = Constraint i (varSubst m v) (varSubst m t)

instance VarSubst a => VarSubst (Statement a) where
  varSubst m = fmap (varSubst m)

instance VarSubst a => VarSubst (PilType a) where
  varSubst m = fmap (varSubst m)

instance VarSubst a => VarSubst (InfoExpression a) where
  varSubst m = fmap (varSubst m)

instance VarSubst a => VarSubst [a] where
  varSubst m = fmap (varSubst m)

instance VarSubst SymInfo where
  varSubst m (SymInfo sz s) = SymInfo sz (varSubst m s)

instance VarSubst a => VarSubst (UnifyError a) where
  varSubst m (UnifyError pt1 pt2 uerr) =
    UnifyError (varSubst m pt1) (varSubst m pt2) (varSubst m uerr)
  varSubst m (IncompatibleTypes pt1 pt2) =
    IncompatibleTypes (varSubst m pt1) (varSubst m pt2)
  varSubst m (OverlappingRecordField fields off) =
    OverlappingRecordField (fmap (varSubst m) fields) off

