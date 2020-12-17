module Blaze.Types.Pil.Function where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil (CallDest, PilVar)
import Data.BinaryAnalysis (Symbol)

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Parameter positions start at position 1.
newtype ParamPosition = ParamPosition Int
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data PositionalParameter = PositionalParameter
  { var :: PilVar
  , access :: Access
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data VarArgParameter
  = VarArgParameter
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data Function = Function
  { name :: Symbol
  , start :: Address
  , params :: [PositionalParameter]
  , varparam :: Maybe VarArgParameter
  , locals :: [PilVar]
  , globals :: [PilVar]
  , result :: Maybe PilVar
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | Used to reference a function by symbol and address.
 In particular, useful when the function body is unavailable
 or awkward to include.
-}
data FuncRef = FuncRef
  { name :: Symbol
  , start :: Address
  , params :: [PositionalParameter]
  , varparam :: Maybe VarArgParameter
  , hasResult :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | Used for type inference of functions to assign
 shared type variables to function parameters.
-}
data FuncVar expr
  = FuncParam (CallTarget expr) ParamPosition
  | FuncResult (CallTarget expr)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

{- | A call target has an expression destination
 along with parameter and result information.
 Used when a function cannot be resolved (e.g., indirect call)
 or when it simple has not been resolved.
 A CallTarget can be used to group call sites that share
 the same call destination and expectations around
 call arguments and results.
-}
data CallTarget expr = CallTarget
  { dest :: CallDest expr
  , numArgs :: Int
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype ResultInfo = ResultInfo
  { name :: Text }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

{- | Describe basic information about a function.
 Because this describe known functions, we are matching
 according to symbol. This will work for C, but not C++.
 Can try matching according to simple name from Symbol and
 parameter list.
-}
data FuncInfo = FuncInfo
  { name :: Symbol
  , params :: [PositionalParameter]
  , varparam :: Maybe VarArgParameter
  , result :: ResultInfo
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- TODO: Consider extending this to support
--       the call expression as the result.
newtype CallResult = CallResult
  { var :: PilVar }
