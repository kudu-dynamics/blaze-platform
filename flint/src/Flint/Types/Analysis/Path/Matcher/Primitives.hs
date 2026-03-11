module Flint.Types.Analysis.Path.Matcher.Primitives where

import Flint.Prelude hiding (Location, sym)

import qualified Flint.Types.Analysis.Path.Matcher.Func as M
import Flint.Types.Symbol (Symbol)

import qualified Blaze.Pil.Display as Disp
import Blaze.Pil.Construct (ExprConstructor(mkExpr))
import Blaze.Pretty (tt, tokenize, showHex)
import qualified Blaze.Pretty as Pretty
import Blaze.Types.Function (ExternFunction, Func)
import Blaze.Types.Pil (Size(Size))
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashSet as HashSet

-- | The type of primitive and names for input and output vars.
-- These are any vars both input and output, that will be
-- bound to args/globals/ret-vars/immediates during a concrete pattern match
data PrimSpec = PrimSpec
  { name :: Text
  , vars :: HashSet (Symbol Pil.Expression)
  -- | important locations in the primitive
  , locations :: HashSet (Symbol Address)
  } deriving (Eq, Ord, Show, Hashable, Generic, ToJSON, FromJSON)

-- | The primitive vars bound in CallableWMI will be written in terms of these
data FuncVar
  = Arg Word64
  | Ret
  | Global Pil.Expression -- expr is address of store or load
  deriving (Eq, Ord, Show, Hashable, Generic)

instance Pretty.Tokenizable FuncVar where
  -- TODO: MAYBE WE SHOULDN'T 1-index this on the printout and 0-index when using huh??
  tokenize (Arg n) = return [tt $ "ARG_" <> show (n + 1)]
  tokenize (Global (Pil.Expression _ (Pil.GLOBAL_PTR op))) =
    return [tt $ "(GLOBAL " <> showHex addr <> maybe "" (" " <>) (op ^. #symbol) <> ")"]
    where addr = fromIntegral (op ^. #constant) :: Word64
  tokenize (Global x) = tt "(GLOBAL " Pretty.<++> tokenize x Pretty.<++> tt ")"
  tokenize Ret = return [tt "RET"]

data FuncVarExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf FuncVar  -- looks up symbol to get size of expr
  deriving (Eq, Ord, Show, Hashable, Generic)

-- | This is like a BoundExpr, except it can refer to FuncVars
data FuncVarExpr
  = FuncVar FuncVar
  | FuncVarExpr FuncVarExprSize (Pil.ExprOp FuncVarExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

extractFuncVars :: FuncVarExpr -> HashSet FuncVar
extractFuncVars (FuncVar v) = HashSet.singleton v
extractFuncVars (FuncVarExpr _ op) = foldMap extractFuncVars op
  
instance Disp.NeedsParens FuncVarExpr where
  needsParens (FuncVar _) = False
  needsParens (FuncVarExpr _ op) = Disp.needsParens op

instance Pretty.Tokenizable FuncVarExpr where
  tokenize (FuncVar v) = tokenize v
  tokenize (FuncVarExpr (ConstSize (Size size)) op) = Pretty.tokenizeExprOp Nothing op (Size size)
  tokenize (FuncVarExpr (SizeOf _) op) = Pretty.tokenizeExprOp Nothing op (Size 0)

instance ExprConstructor FuncVarExprSize FuncVarExpr where
  mkExpr = FuncVarExpr

data MkCallableWMIError
  = MkCallableWMIError
  { primSpec :: PrimSpec
  , missingVars :: HashSet (Symbol Pil.Expression)
  , missingLocations :: HashSet (Symbol Address)
  } deriving (Eq, Ord, Show, Generic)

-- | This represents a concrete primitive, callable through a function,
-- where the prim inputs are accessible through function inputs.
-- The point of this is so we can match on PrimSpecs in the Matcher
-- and have it link up the correct concrete arg inputs and output exprs to
-- the Syms in the PrimSpec, which can be referred to in a pattern.
data CallableWMI = CallableWMI
  { prim :: PrimSpec
  , func :: Func
  , callDest :: M.Func -- func pattern
  , varMapping :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
  -- |constraints to reach prim, and constraints on outputs
  , constraints :: [(FuncVarExpr, HashSet FuncVar)]
  , locations :: HashMap (Symbol Address) (Either ExternFunction Address)
  -- | Vars that need to link up to the outside (used inside varMapping and constraints)
  -- if you can't control them, maybe the primitive isn't useful
  , linkedVars :: HashSet FuncVar
  } deriving (Eq, Ord, Show, Hashable, Generic)

-- | Describes a known function whose arg semantics are understood.
-- Used to match calls to known functions (e.g. stdlib) and map their
-- arguments to primitive variables.
data KnownFunc = KnownFunc
  { prim :: PrimSpec
  , funcName :: Text
  , varMapping :: HashMap (Symbol Pil.Expression) FuncVarExpr
  , constraints :: [FuncVarExpr]
  } deriving (Eq, Ord, Show, Hashable, Generic)

-- | Deprecated alias
type StdLibPrimitive = KnownFunc
