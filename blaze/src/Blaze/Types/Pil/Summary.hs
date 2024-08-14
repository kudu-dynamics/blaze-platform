module Blaze.Types.Pil.Summary (
    module Blaze.Types.Pil.Summary,
) where

import Blaze.Prelude

import Blaze.Types.Pil (Expression, PilVar, Stmt)
import Blaze.Types.Pil.Analysis (LoadExpr)
import Data.HashSet as HashSet

-- | Specification of the origin of the data being copied
data InputLocation =
  -- | A memory location that is fixed as a constant by the callee
    ConcreteInputLocation Address
  -- | A memory location that is passed by reference from the caller
  | SymbolicInputLocation PilVar
  -- | A pure expression that is specified by the callee
  | PureExpression Expression
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

-- | Specification of the destination of the data being copied
data OutputLocation =
  -- | A memory location that is fixed as a constant by the callee
    ConcreteOutputLocation Address
  -- | A memory location that is passed by reference from the caller
  | SymbolicOutputLocation PilVar
  -- | The data is returned by value from the callee to the caller
  | Returned
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

data Capability =
    CopyCapability OutputLocation InputLocation
  | AddressLeak
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

data CodeSummary = CodeSummary
    { inputVars :: [PilVar]
    , inputLoads :: [LoadExpr]
    , results :: [Expression]
    , effects :: [Effect]
    , capabilities :: [Capability]
    } deriving (Eq, Ord, Show, Generic)

data ReadsWrites = ReadsWrites 
  { readsFrom :: HashSet PilVar 
  , writesTo :: HashSet PilVar 
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup ReadsWrites where
  x <> y = ReadsWrites reads' writes'
    where
      reads' = x ^. #readsFrom <> y ^. #readsFrom
      writes' = x ^. #writesTo <> y ^. #writesTo

instance Monoid ReadsWrites where
  mempty = ReadsWrites HashSet.empty HashSet.empty

data Effect
  = EffectWrite Stmt
  | EffectAlloc Stmt
  | EffectDealloc Stmt
  | EffectCall Stmt
  deriving (Eq, Ord, Show, Generic)
