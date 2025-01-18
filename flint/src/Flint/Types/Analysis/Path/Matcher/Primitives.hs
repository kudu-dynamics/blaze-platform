module Flint.Types.Analysis.Path.Matcher.Primitives where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher (Symbol)
import qualified Flint.Types.Analysis.Path.Matcher as Matcher

import Blaze.Types.Pil (Size)
import qualified Blaze.Types.Pil as Pil


data Location

-- | The type of primitive and names for input and output vars.
-- These are any vars both input and output, that will be
-- bound to args/globals/ret-vars/immediates during a concrete pattern match
data PrimType = PrimType
  { name :: Text
  , vars :: HashSet (Symbol Pil.Expression)
  -- | important locations in the primitive
  , locationLabels :: HashSet (Symbol Location) 
  } deriving (Eq, Ord, Show, Hashable, Generic)

-- | The primitive vars bound in CallablePrimitive will be written in terms of these
data FuncVar
  = Arg Word64
  | Ret
  | Global Pil.Expression -- expr is address of store or load
  deriving (Eq, Ord, Show, Hashable, Generic)

data FuncVarExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf FuncVar
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | This is like a BoundExpr, except it can refer to FuncVars
data FuncVarExpr
  = PrimVar (Symbol Pil.Expression) -- can refer to other vars bound by the primitive
  | FuncVar FuncVar
  | FuncVarExpr (FuncVarExprSize) (Pil.ExprOp FuncVarExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | This represents a concrete primitive, callable through a function,
-- where the prim inputs are accessible through function inputs.
-- The point of this is so we can match on PrimTypes in the Matcher
-- and have it link up the correct concrete arg inputs and output exprs to
-- the Syms in the PrimType, which can be referred to in a pattern.
data CallablePrimitive = CallablePrimitive
  { prim :: PrimType
  , callDest :: Matcher.Func -- func pattern
  , varMapping :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
  -- |constraints to reach prim, and constraints on outputs
  , constraints :: HashSet (FuncVarExpr, HashSet FuncVar)
  , locations :: HashMap (Symbol Location) (HashSet Address)
  -- | Vars that need to link up to the outside (used inside varMapping and constraints)
  -- if you can't control them, maybe the primitive isn't useful
  , linkedVars :: HashSet FuncVar
  } deriving (Eq, Ord, Show, Hashable, Generic)
