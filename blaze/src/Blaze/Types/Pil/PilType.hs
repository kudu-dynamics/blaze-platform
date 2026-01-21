module Blaze.Types.Pil.PilType where

import Blaze.Prelude

-- Originaly the following were in Blaze.Types.Pil.Checker, but there was a circular dependency issue and so I moved it here.

-- | Type symbols. Both type variables and metavariables are represented as
-- 'Sym's
newtype Sym = Sym Int
  deriving (Eq, Ord, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

-- | PIL types, as a functor over the type of nested types. 'ConcretePilType',
-- 'DeepSymType', and 'RecursiveSymType' all use instances of this parameterized
-- type
data PilType t
  = TBool
  -- | Represents a character from some character set. Please use TInt {bitWidth = Bits 8, ...} for
  -- the C-type `char`
  | TChar {bitWidth :: Maybe Bits}
  | TInt {bitWidth :: Maybe Bits, signed :: Maybe Bool}
  | TFloat {bitWidth :: Maybe Bits}
  | TBitVector {bitWidth :: Maybe Bits}
  | TPointer {bitWidth :: Maybe Bits, pointeeType :: t}
  | TCString {strLen :: Maybe Bytes}
  | TArray {len :: Maybe Word64, elemType :: t}
  -- | First record field or array index, or itself t is type of first thing
  | TRecord (HashMap BitOffset t)
  -- TODO: Consider adding a recursive type constructor
  -- TRecursive Sym (PilType t)
  | TUnit
    -- | Bottom is labeled with error info,
    -- it only results from a unification error
  | TBottom Sym
  | TFunction {ret :: t, params :: [t]}
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic, Hashable, FromJSON, ToJSON)

