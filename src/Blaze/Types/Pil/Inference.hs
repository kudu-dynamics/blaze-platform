{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Pil.Inference where

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


-- NOTE: I got rid of non-concrete types, like TNumber,
--       but for function signatures they should be added back in
data PilType t = TArray { len :: t, elemType :: t }
               | TChar
               | TInt { classVar :: Sym, bitWidth :: t, signed :: t }
               | TFloat { bitWidth :: t }
               | TBitVector { classVar :: Sym, bitWidth :: t }
               | TPointer { bitWidth :: t, pointeeType :: t }
               | TRecord (HashMap BitWidth -- todo: change bitwidth to 't'?
                                  t -- type
                         )
               -- Bottom is labeled with error info
               | TBottom { symId :: HashSet Sym }
               | TFunction { ret :: t, params :: [t] }

               -- class constraint (t should be TVBitWidth)
               -- maybe should just be BitVector?
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

type SymTypeExpression = InfoExpression SymType

-- | Goal is to generate statements with TypedExpressions
type TypedExpression = InfoExpression (PilType T)

data UnifyError = UnifyError (PilType SymType) (PilType SymType) UnifyError
                | IncompatibleTypes (PilType SymType) (PilType SymType)
                | OverlappingRecordField { recordFields :: (HashMap BitWidth SymType)
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

newtype Solution = Solution (Sym, SymType)
  deriving (Eq, Ord, Show, Generic)


data UnifyWithSubsState = UnifyWithSubsState
                          { _accSubs :: [Constraint]
                            -- , _solutions :: [(Sym, SymType)]
                             -- , _errors :: [UnifyError]

                          -- this is a map of syms to their "original" sym
                          -- like if you add (a, b), (b, c)
                          -- it should store a | b  -> c
                          , _originMap :: HashMap Sym Sym
                          } deriving (Eq, Ord, Show)
$(makeFieldsNoPrefix ''UnifyWithSubsState)

-- | Creates a map of "origins" that vars are equal to.
-- The "origin" for vars remains the same, i.e. if you add (a, b)
-- to a map where multiple vars map to `a`, it just adds (b, a) to map
-- instead of adding (a, b) and updating all the `a`s to `b`.
addToOriginMap :: Sym -> Sym -> HashMap Sym Sym -> HashMap Sym Sym
addToOriginMap a b m =
  case ((a,) <$> HashMap.lookup b m) <|> ((b,) <$> HashMap.lookup a m) of
    Nothing -> HashMap.insert a b (HashMap.insert b b m)
    Just (c, d)
      | c == d -> m
      | otherwise -> HashMap.insert c d m

addVarEq :: MonadState UnifyWithSubsState m => Sym -> Sym -> m ()
addVarEq a b = originMap %= addToOriginMap a b

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
newtype UnifyWithSubs a = UnifyWithSubs { _runUnifyWithSubs :: ExceptT UnifyError (StateT UnifyWithSubsState Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError UnifyError
           , MonadState UnifyWithSubsState
           )

runUnifyWithSubs :: UnifyWithSubs a -> UnifyWithSubsState -> (Either UnifyError a, UnifyWithSubsState)
runUnifyWithSubs m s = runIdentity . flip runStateT s . runExceptT . _runUnifyWithSubs $ m

data UnifyConstraintsResult = UnifyConstraintsResult
  { _constraints :: [(Sym, SymType)]
  , _solutions :: [(Sym, SymType)]
  , _errors :: [UnifyError]
  } deriving (Eq, Ord, Read, Show)
$(makeFieldsNoPrefix ''UnifyConstraintsResult)


