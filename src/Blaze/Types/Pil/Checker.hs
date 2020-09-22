{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Pil.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Types.Pil ( ExprOp
                       , Statement
                       , PilVar
                       , StackOffset
                       )
import Blaze.Types.Pil.Function ( FuncVar )
import qualified Data.HashMap.Strict as HashMap


type BitWidth = Bits
type ByteWidth = Bytes

charSize :: BitWidth
charSize = 8

newtype Sym = Sym Int
            deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Sym

data TypeTag = TagDirty
             | TagSanitized
             | TagAllocedMemory
             | TagFreedMemory
             | TagNullPtr
             | TagNonNull
             deriving (Eq, Ord, Read, Show, Generic)

data PilType t = TBool
               | TChar
               -- | TQueryChar
               
               | TInt { bitWidth :: t, signed :: t }
               | TFloat { bitWidth :: t }
               | TBitVector { bitWidth :: t }
               | TPointer { bitWidth :: t, pointeeType :: t }

--               | TCString { len :: t }

               | TArray { len :: t, elemType :: t }
               | TRecord (HashMap BitOffset -- todo: change bitwidth to 't'?
                           -- TODO: change bitwidth to signed offset
                                  t -- type
                         )

               -- | TFirstOf t
               -- first record field or array index, or itself
               
               -- Bottom is labeled with error info
               | TBottom Sym
               | TFunction { ret :: t, params :: [t] }
              
               -- type level values for some dependent-type action
               | TVBitWidth BitWidth
               | TVLength Word64
               | TVSign Bool
               
               -- TTagged (HashSet TypeTag) t
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

newtype T = T (PilType T)
  deriving (Eq, Ord, Read, Show)

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
               deriving (Eq, Ord, Read, Show, Generic)


data Constraint = Constraint
  { _stmtOrigin :: Int -- probably need (func, instructionIndex) eventually
  , _sym ::Sym
  , _symType :: SymType
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
                        deriving (Eq, Ord, Show)

data InfoExpression a = InfoExpression
  { _info :: a
  , _op :: ExprOp (InfoExpression a)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
$(makeFieldsNoPrefix ''InfoExpression)

instance Hashable a => Hashable (InfoExpression a)

data SymInfo = SymInfo
  { _size :: BitWidth
  , _sym :: Sym
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''SymInfo)

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
                               { _stmtOrigin  :: Int
                               --index in list of pil stmts for now

                               , _sym :: Sym
                               , _error :: UnifyError t
                               } deriving (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)
$(makeFieldsNoPrefix ''UnifyConstraintsError)

type EqualityMap a = HashMap a (HashSet a)

type VarEqMap = EqualityMap Sym

-- | The final report of the type checker, which contains types and errors.
data TypeReport = TypeReport
  { _symTypeStmts :: [(Int, Statement (InfoExpression (SymInfo, Maybe DeepSymType)))]
  , _symStmts :: [Statement SymExpression]
  --  , _typedStmts :: [Statement TypedExpression]
  , _varSymTypeMap :: HashMap PilVar DeepSymType
  , _varSymMap :: HashMap PilVar Sym
--  , _unresolvedStmts :: [Statement SymExpression]
  -- , _unresolvedSyms :: [(Sym, Sym)]
  -- , _unresolvedTypes :: [(Sym, PilType SymType, PilType SymType)]
  , _varEqMap :: VarEqMap
  , _funcSymTypeMap :: HashMap FuncVar DeepSymType
  , _funcSymMap :: HashMap FuncVar Sym
  , _errors :: [UnifyConstraintsError DeepSymType]
  , _flatSolutions :: HashMap Sym (PilType Sym)
  , _solutions :: HashMap Sym DeepSymType
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''TypeReport)


--------------------------------------------------------------
------ Constraint generation phase ---------------------------

data ConstraintGenState = ConstraintGenState
  { _currentSym :: Sym
  , _symMap :: HashMap Sym SymExpression
  , _varSymMap :: HashMap PilVar Sym
  , _funcSymMap :: HashMap FuncVar Sym
  , _constraints :: [Constraint]
  , _currentStmt :: Int
  , _stackAddrSymMap :: HashMap StackOffset Sym
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''ConstraintGenState)

emptyConstraintGenState :: ConstraintGenState
emptyConstraintGenState = ConstraintGenState (Sym 0) HashMap.empty HashMap.empty HashMap.empty [] 0 HashMap.empty

newtype ConstraintGen a = ConstraintGen
  { _runConstraintGen :: ExceptT ConstraintGenError (StateT ConstraintGenState Identity) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadError ConstraintGenError
                   , MonadState ConstraintGenState
                   )

runConstraintGen :: ConstraintGen a -> ConstraintGenState -> (Either ConstraintGenError a, ConstraintGenState)
runConstraintGen m ss = runIdentity . flip runStateT ss . runExceptT . _runConstraintGen $ m

runConstraintGen_ :: ConstraintGen a -> (Either ConstraintGenError a, ConstraintGenState)
runConstraintGen_ m = runConstraintGen m emptyConstraintGenState

--------------------------------------------------------------------
---------- unification and constraint solving ----------------------

data UnifyState = UnifyState
                  { _constraints :: [Constraint]

                  -- solution key syms should all be origins in originMap
                  , _solutions :: HashMap Sym (PilType Sym)

                  , _errors :: [UnifyConstraintsError Sym]

                  -- this is a map of syms to their "original" sym
                  -- like if you add (a, b), (b, c)
                  -- it should store a | b  -> c
                  , _originMap :: HashMap Sym Sym
                  , _currentStmt :: Int
                  } deriving (Eq, Ord, Show)
$(makeFieldsNoPrefix ''UnifyState)


addConstraint :: (HasConstraints s [Constraint], MonadState s m)
              => Constraint -> m ()
addConstraint cx = constraints %= (cx:)

addConstraint_ :: ( HasConstraints s [Constraint]
                  , HasCurrentStmt s Int
                  , MonadState s m)
               => Sym -> SymType -> m ()
addConstraint_ s st = do
  i <- use currentStmt
  constraints %= (Constraint i s st :)

addConstraints :: (HasConstraints s [Constraint], MonadState s m)
               => [Constraint] -> m ()
addConstraints = mapM_ addConstraint

addConstraints_ :: ( HasConstraints s [Constraint]
                   , HasCurrentStmt s Int
                   , MonadState s m)
                => [(Sym, SymType)] -> m ()
addConstraints_ = mapM_ $ uncurry addConstraint_

data UnifyResult = UnifyResult { _solutions :: [(Sym, SymType)]
                               , _errors :: [UnifyError Sym]
                               } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyResult)

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
popConstraint = use constraints >>= \case
  [] -> return Nothing
  (cx:cxs) -> do
    constraints .= cxs
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

