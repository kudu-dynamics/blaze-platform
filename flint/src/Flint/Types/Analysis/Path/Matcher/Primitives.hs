module Flint.Types.Analysis.Path.Matcher.Primitives where

import Flint.Prelude hiding (Location, sym)

import Flint.Types.Analysis.Path.Matcher.Func (Func)
import Flint.Types.Symbol (Symbol)

import qualified Blaze.Pil.Display as Disp
import Blaze.Pil.Construct (ExprConstructor(mkExpr))
import Blaze.Pretty (tt, tokenize, (<++>))
import qualified Blaze.Pretty as Pretty
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Size)
import qualified Blaze.Types.Pil as Pil


-- | The type of primitive and names for input and output vars.
-- These are any vars both input and output, that will be
-- bound to args/globals/ret-vars/immediates during a concrete pattern match
data PrimType = PrimType
  { name :: Text
  , vars :: HashSet (Symbol Pil.Expression)
  -- | important locations in the primitive
  , locations :: HashSet (Symbol Address) 
  } deriving (Eq, Ord, Show, Hashable, Generic)

-- | The primitive vars bound in CallablePrimitive will be written in terms of these
data FuncVar
  = Arg Word64
  | Ret Pil.Expression
  | Global Pil.Expression -- expr is address of store or load
  deriving (Eq, Ord, Show, Hashable, Generic)

instance Pretty.Tokenizable FuncVar where
  -- TODO: MAYBE WE SHOULDN'T 1-index this on the printout and 0-index when using huh??
  tokenize (Arg n) = return [tt $ "ARG_" <> show (n + 1)]
  tokenize (Global x) = tt "GLOBAL(" <++> tokenize x <++> tt ")"
  tokenize (Ret x) = tt "RET(" <++> tokenize x <++> tt ")"

-- | This is like a BoundExpr, except it can refer to FuncVars
data FuncVarExpr
  = FuncVar FuncVar
  | FuncVarExpr (Size Pil.Expression) (Pil.ExprOp FuncVarExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

instance Disp.NeedsParens FuncVarExpr where
  needsParens (FuncVar _) = False
  needsParens (FuncVarExpr _ op) = Disp.needsParens op

instance Pretty.Tokenizable FuncVarExpr where
  tokenize (FuncVar v) = tokenize v
  tokenize (FuncVarExpr sz op) = Pretty.tokenizeExprOp Nothing op sz

instance ExprConstructor (Size Pil.Expression) FuncVarExpr where
  mkExpr =  FuncVarExpr

-- | This represents a concrete primitive, callable through a function,
-- where the prim inputs are accessible through function inputs.
-- The point of this is so we can match on PrimTypes in the Matcher
-- and have it link up the correct concrete arg inputs and output exprs to
-- the Syms in the PrimType, which can be referred to in a pattern.
data CallablePrimitive = CallablePrimitive
  { prim :: PrimType
  , func :: Function
  , callDest :: Func -- func pattern
  , varMapping :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
  -- |constraints to reach prim, and constraints on outputs
  , constraints :: [(FuncVarExpr, HashSet FuncVar)]
  , locations :: HashMap (Symbol Address) (HashSet Address)
  -- | Vars that need to link up to the outside (used inside varMapping and constraints)
  -- if you can't control them, maybe the primitive isn't useful
  , linkedVars :: HashSet FuncVar
  } deriving (Eq, Ord, Show, Hashable, Generic)
