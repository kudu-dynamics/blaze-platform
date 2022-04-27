{-# LANGUAGE UndecidableInstances #-}
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
  deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

data TypeTag = TagDirty
             | TagSanitized
             | TagAllocedMemory
             | TagFreedMemory
             | TagNullPtr
             | TagNonNull
             deriving (Eq, Ord, Read, Show, Generic)

data PilType t
  = TBool
  -- | Represents a character from some character set. Please use TInt {bitWidth = Bits 8, ...} for
  -- the C-type `char`
  | TChar {bitWidth :: Maybe Bits}
  | TInt {bitWidth :: Maybe Bits, signed :: Maybe Bool}
  | TFloat {bitWidth :: Maybe Bits}
  | TBitVector {bitWidth :: Maybe Bits}
  | TPointer {bitWidth :: Maybe Bits, pointeeType :: t}
  | TCString {len :: Maybe Word64}
  | TArray {len :: Maybe Word64, elemType :: t}
  -- | First record field or array index, or itself t is type of first thing
  | TRecord (HashMap BitOffset t)
  -- TODO: Consider adding a recursive type constructor
  -- TRecursive Sym (PilType t)
  | TUnit
    -- | Bottom is labeled with error info,
    -- it only results from a unification error
  | TBottom Sym
  | TFunction {ret :: PilType t, params :: [PilType t]}
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic, Hashable, FromJSON, ToJSON)

newtype T = T (PilType T)
  deriving (Eq, Ord, Read, Show, Generic)

unT :: T -> PilType T
unT (T pt) = pt

-- | A flat representation of PIL types where composite types are referenced through
-- `Sym` values rather than directly referencing a PIL type.
type FlatPilType = PilType Sym

data SymType = SVar Sym
             | SType FlatPilType
             deriving (Eq, Ord, Read, Show, Generic, Hashable, FromJSON, ToJSON)

-- TODO: Can we mege SymType with ConstraintSymType? It's looking like "yes".
-- | Used to represent shallow/flat types where nested types are looked up
-- via symbol.
data ConstraintSymType = CSVar Sym
                       | CSType (PilType ConstraintSymType)
                       deriving (Eq, Ord, Read, Show, Generic)

-- TODO: Can rescursive types be nested? If so, is this supported?
data DeepSymType = DSVar Sym
                 | DSRecursive Sym (PilType DeepSymType)
                 | DSType (PilType DeepSymType)
               deriving (Eq, Ord, Read, Show, Generic, Hashable, FromJSON, ToJSON)

data Constraint = Constraint
  { stmtOrigin :: Int -- probably need (func, instructionIndex) eventually
  , sym :: Sym
  , symType :: SymType
  } deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, ToJSON)

-- | Solutions should be the "final unification" for any sym.
-- Complex types might still contain SVars subject to substitution
-- but the type structure shouldn't change.
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

type TypedExpression = InfoExpression (PilType T)

data UnifyError t
  = UnifyError (PilType t) (PilType t) (UnifyError t)
  | IncompatibleTypes (PilType t) (PilType t)
  | IncompatibleTypeLevelValues
  | OverlappingRecordField
      { recordFields :: HashMap BitOffset t
      , offendingOffset :: BitOffset
      }
  deriving (Eq, Ord, Read, Show, Functor, Generic, Hashable, FromJSON, ToJSON)

data UnifyConstraintsError t = UnifyConstraintsError
  { stmtOrigin :: Int -- ^ Index in list of pil stmts for now
  , sym :: Sym
  , error :: UnifyError t
  }
  deriving (Eq, Ord, Read, Show, Generic, Functor, Hashable, FromJSON, ToJSON)

type EqualityMap a = HashMap a (HashSet a)

type VarEqMap = EqualityMap Sym

type VarSymMap = HashMap PilVar Sym

-- | The final report of the type checker, which contains types and errors.
data TypeReport = TypeReport
  { symTypeStmts :: [(Int, Statement (InfoExpression (SymInfo, Maybe DeepSymType)))]
  , symStmts :: [(Int, Statement SymExpression)]
  , varSymTypeMap :: HashMap PilVar DeepSymType
  , varSymMap :: VarSymMap
  , varEqMap :: VarEqMap
  , funcSymTypeMap :: HashMap (FuncVar SymExpression) DeepSymType
  , funcSymMap :: HashMap (FuncVar SymExpression) Sym
  , errors :: [UnifyConstraintsError DeepSymType]
  , flatSolutions :: HashMap Sym (PilType Sym)
  , solutions :: HashMap Sym DeepSymType
  , originMap :: HashMap Sym Sym -- ^ from UnifyState
  , errorConstraints :: HashMap Sym [Constraint] -- ^ original constraints
  , ogConstraints :: [Constraint]
  } deriving (Eq, Ord, Show, Generic, Hashable)

--------------------------------------------------------------
------ Constraint generation phase ---------------------------

type CallConstraintGenerator = Sym -> [Sym] -> ConstraintGen ()

newtype ConstraintGenCtx = ConstraintGenCtx
  { -- | Hashmap index is (stmt index from IndexedStmts, function name)
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

                  -- This is a map of syms to their "original" sym.
                  -- E.g., if you add (a, b), (b, c)
                  -- the map should contain the entries:
                  -- (a, c) and (b, c)
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
               => Sym -> FlatPilType -> m ()
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
newtype Unify a = Unify
  { _runUnify :: ExceptT (UnifyError Sym) (StateT UnifyState Identity) a
  }
  deriving newtype
    ( Functor
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

-- | Type class used to provide `varSubst` method which can be used to perform symbol substitutions.
-- I.e., if a `Sym` in some structure is found as a key in the map, then the corresponding `Sym` value is
-- used to replace the original symbol.
class VarSubst a where
  varSubst :: HashMap Sym Sym -> a -> a

instance VarSubst a => VarSubst (PilType a) where
  varSubst m = fmap (varSubst m)

instance VarSubst Sym where
  varSubst m v = maybe v identity $ HashMap.lookup v m

instance VarSubst SymType where
  varSubst m (SVar v) = SVar $ varSubst m v
  varSubst m (SType t) = SType $ varSubst m t

-- -- shouldn't really need to do this because DeepSymTypes are constructed
-- -- using the `Solutions` map, which already should only have origin vars
-- instance VarSubst DeepSymType where
--   varSubst m (DSVar v) = DSVar $ varSubst m v
--   varSubst m (DSType t) = DSType $ varSubst m t
--   varSubst m (DSRecursive s t) = DSRecursive (varSubst m s) (varSubst m t)

instance VarSubst Constraint where
  varSubst m (Constraint i v t) = Constraint i (varSubst m v) (varSubst m t)

instance VarSubst a => VarSubst (Statement a) where
  varSubst m = fmap (varSubst m)

-- instance
--   VarSubst f
--   =>
--   VarSubst (PilType f)
--   where
--   varSubst m pt = case pt of
--     TBool -> pt
--     TChar -> pt
--     TInt bw sign ->
--       TInt
--         { bitWidth = varSubst m bw
--         , signed = varSubst m sign
--         }
--     TFloat bw -> TFloat{bitWidth = varSubst m bw}
--     TBitVector bw -> TBitVector{bitWidth = varSubst m bw}
--     TPointer bw pnt ->
--       TPointer
--         { bitWidth = varSubst m bw
--         , pointeeType = varSubst m pnt
--         }
--     TCString l -> TCString{len = varSubst m l}
--     TArray l et ->
--       TArray
--         { len = varSubst m l
--         , elemType = varSubst m et
--         }
--     TRecord _ -> pt
--     TRecursive _ _ -> pt
--     TUnit -> pt
--     TBottom _ -> pt
--     TFunction _ _ -> pt
--     TVBitWidth _ -> pt
--     TVLength _ -> pt
--     TVSign _ -> pt

-- instance VarSubst (Either Sym a) where
--   varSubst m x = case x of
--     Left s -> Left $ HashMap.findWithDefault s s m
--     Right _ -> x

-- -- TODO: Do we need this definition? Do variable substitutions only
-- --       occur in a PilType that contains symbols?
-- instance VarSubst (Identity a) where
--   varSubst _m x = x

-- instance VarSubst PilType where
--   varSubst m x = PType (varSubst m $ unPType x)

instance VarSubst a => VarSubst (InfoExpression a) where
  varSubst m = fmap (varSubst m)

-- instance VarSubst a => VarSubst [a] where
--   varSubst m = fmap (varSubst m)

instance VarSubst SymInfo where
  varSubst m (SymInfo sz s) = SymInfo sz (varSubst m s)

-- instance VarSubst UnifyError where
--   varSubst m (UnifyError pt1 pt2 uerr) =
--     UnifyError (varSubst m pt1) (varSubst m pt2) (varSubst m uerr)
--   varSubst m (IncompatibleTypes pt1 pt2) =
--     IncompatibleTypes (varSubst m pt1) (varSubst m pt2)
--   varSubst m (OverlappingRecordField fields off) =
--     OverlappingRecordField (fmap (varSubst m) fields) off

instance VarSubst a => VarSubst (UnifyError a) where
  varSubst m (UnifyError pt1 pt2 uerr) =
    UnifyError (varSubst m pt1) (varSubst m pt2) (varSubst m uerr)
  varSubst m (IncompatibleTypes pt1 pt2) =
    IncompatibleTypes (varSubst m pt1) (varSubst m pt2)
  varSubst _m e@IncompatibleTypeLevelValues = e
  varSubst m (OverlappingRecordField fields off) =
    OverlappingRecordField (fmap (varSubst m) fields) off
