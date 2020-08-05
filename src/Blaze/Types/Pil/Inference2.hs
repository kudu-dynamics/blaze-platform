{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Pil.Inference2 where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Blaze.Types.Pil ( ExprOp
                       , Statement
                       , PilVar
                       )
-- import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


type BitWidth = Bits
type ByteWidth = Bytes

charSize :: BitWidth
charSize = 8

data Sym = Sym Int
         deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Sym


data PilType t = TArray { len :: t, elemType :: t }
               | TChar
               | TInt { bitWidth :: t, signed :: t }
               | TFloat { bitWidth :: t }
               | TBitVector { bitWidth :: t }
               | TPointer { bitWidth :: t, pointeeType :: t }
               | TRecord (HashMap BitWidth -- todo: change bitwidth to 't'?
                                  t -- type
                         )
               -- Bottom is labeled with error info
               | TBottom
               | TFunction { ret :: t, params :: [t] }
              
               -- type level values for some dependent-type action
               | TVBitWidth BitWidth
               | TVLength Word64
               | TVSign Bool
               deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

data T = T (PilType T)
  deriving (Eq, Ord, Read, Show)

unT :: T -> PilType T
unT (T pt) = pt

data SymType = SVar Sym
             | SType (PilType Sym)
             deriving (Eq, Ord, Read, Show, Generic)

data DeepSymType = DSVar Sym
                 | DSRecursive Sym (PilType DeepSymType)
                 | DSType (PilType DeepSymType)
               deriving (Eq, Ord, Read, Show, Generic)

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

data UnifyError = UnifyError (PilType Sym) (PilType Sym) UnifyError
                | IncompatibleTypes (PilType Sym) (PilType Sym)
                | OverlappingRecordField { recordFields :: (HashMap BitWidth Sym)
                                         , offendingOffset :: BitWidth
                                         }
                deriving (Eq, Ord, Read, Show, Generic)

data UnifyConstraintsError = UnifyConstraintsError
                           { _sym :: Sym
                           , _error :: UnifyError
                           } deriving (Eq, Ord, Read, Show, Generic)
$(makeFieldsNoPrefix ''UnifyConstraintsError)

-- kind of pointless, I guess... could just be a tuple
data WithMeta meta a = WithMeta meta a
  deriving (Eq, Ord, Read, Show, Generic, Functor, Foldable, Traversable)

mapMeta :: (meta -> meta') -> WithMeta meta a -> WithMeta meta' a
mapMeta f (WithMeta meta x) = WithMeta (f meta) x

type EqualityMap a = HashMap a (HashSet a)

type VarEqMap = EqualityMap Sym

-- | The final report of the type checker, which contains types and errors.
data TypeReport = TypeReport
  { _symTypeStmts :: [Statement (InfoExpression (SymInfo, Maybe SymType))]
  , _symStmts :: [Statement SymExpression]
  --  , _typedStmts :: [Statement TypedExpression]
  , _varSymTypeMap :: HashMap PilVar SymType
  , _varSymMap :: HashMap PilVar Sym
--  , _unresolvedStmts :: [Statement SymExpression]
  -- , _unresolvedSyms :: [(Sym, Sym)]
  -- , _unresolvedTypes :: [(Sym, PilType SymType, PilType SymType)]
  , _varEqMap :: VarEqMap
  , _errors :: [UnifyConstraintsError]
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''TypeReport)


--------------------------------------------------------------
------ Constraint generation phase ---------------------------

data ConstraintGenState = ConstraintGenState
  { _currentSym :: Sym
  , _symMap :: HashMap Sym SymExpression
  , _varSymMap :: HashMap PilVar Sym
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''ConstraintGenState)

emptyConstraintGenState :: ConstraintGenState
emptyConstraintGenState = ConstraintGenState (Sym 0) HashMap.empty HashMap.empty

newtype ConstraintGen a = ConstraintGen
  { _runConstraintGen :: ExceptT ConstraintGenError (StateT ConstraintGenState Identity) a }
  deriving ( Functor
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

data ConstraintMeta = ConstraintMeta
  { _stmts :: HashSet (Statement SymExpression)
  , _syms :: HashSet Sym
  } deriving (Eq, Ord, Show, Generic)
$(makeFieldsNoPrefix ''ConstraintMeta)

newtype Constraint = Constraint (Sym, SymType)
  deriving (Eq, Ord, Show, Generic)

-- | solutions should be the "final unification" for any sym.
-- | complex types might still contain SVars subject to substitution
-- | but the type structure shouldn't change.

newtype Solution = Solution (Sym, PilType Sym)
  deriving (Eq, Ord, Show, Generic)


data UnifyState = UnifyState
                  { _constraints :: [Constraint]

                  -- solution key syms should all be origins in originMap
                  , _solutions :: HashMap Sym (PilType Sym)

                  , _errors :: [UnifyError]
                            
                  -- this is a map of syms to their "original" sym
                  -- like if you add (a, b), (b, c)
                  -- it should store a | b  -> c
                  , _originMap :: HashMap Sym Sym
                  } deriving (Eq, Ord, Show)
$(makeFieldsNoPrefix ''UnifyState)


addConstraint :: MonadState UnifyState m => Constraint -> m ()
addConstraint cx = constraints %= (cx:)

-- | Creates a map of "origins" that vars are equal to.
-- The "origin" for vars remains the same, i.e. if you add (a, b)
-- to a map where multiple vars map to `a`, it just adds (b, a) to map
-- instead of adding (a, b) and updating all the `a`s to `b`.
-- returns updated map and "origin" var that 'a' and 'b' are pointing to
addToOriginMap :: Sym -> Sym -> HashMap Sym Sym -> (Sym, Maybe Sym, HashMap Sym Sym)
addToOriginMap a b m = case (HashMap.lookup a m, HashMap.lookup b m) of
  (Nothing, Nothing) -> (b, Nothing, HashMap.insert a b (HashMap.insert b b m))
  (Just c, Nothing) -> (c, Nothing, HashMap.insert b c m)
  (Nothing, Just c) -> (c, Nothing, HashMap.insert a c m)
  (Just c, Just d)
    | c == d -> (c, Nothing, m)
    | otherwise -> (d, Just c, fmap (\x -> if x == c then d else x) m)

-- | Adds new var equality, returning the origin sym.
-- If the equality merges two groups, it picks the origin associated
-- with the second symbol and changes the origins of the first group to
-- the second origin. It also removes the solution associated with the
-- first origin and adds it as constraint to be later merged as solution.
addVarEq :: MonadState UnifyState m => Sym -> Sym -> m Sym
addVarEq a b = do
  m <- use originMap
  let (v, mr, m') = addToOriginMap a b m
  case mr of
    Nothing -> return ()
    Just retiredSym -> do
      sols <- use solutions
      case HashMap.lookup retiredSym sols of
        Nothing -> return ()
        Just rt -> do
          addConstraint $ Constraint (retiredSym, SType rt)
          solutions %= HashMap.delete retiredSym
  originMap .= m'
  return (v)

originMapToGroupMap :: HashMap Sym Sym -> HashMap Sym (HashSet Sym) 
originMapToGroupMap = foldr f HashMap.empty . HashMap.toList
  where
    f (a, b) m = HashMap.alter g b m
      where
        g Nothing = Just $ HashSet.singleton a
        g (Just s) = Just $ HashSet.insert a s

originMapToGroups :: HashMap Sym Sym -> HashSet (HashSet Sym)
originMapToGroups = HashSet.fromList . HashMap.elems . originMapToGroupMap

data UnifyResult = UnifyResult { _solutions :: [(Sym, SymType)]
                               , _errors :: [UnifyError]
                               } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyResult)

-- | monad just used for unifyWithSubs function and its helpers
newtype Unify a = Unify { _runUnify :: ExceptT UnifyError (StateT UnifyState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError UnifyError
           , MonadState UnifyState
           )

runUnify :: Unify a -> UnifyState -> (Either UnifyError a, UnifyState)
runUnify m s = runIdentity . flip runStateT s . runExceptT . _runUnify $ m

popConstraint :: Unify (Maybe Constraint)
popConstraint = use constraints >>= \case
  [] -> return Nothing
  (cx:cxs) -> do
    constraints .= cxs
    return $ Just cx


