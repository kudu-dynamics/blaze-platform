{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Function where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil (PilVar, CallDest)
import Data.BinaryAnalysis (Symbol)

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Parameter positions start at position 1.
newtype ParamPosition = ParamPosition Int
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data PositionalParameter
  = PositionalParameter
      { _var :: PilVar,
        _position :: ParamPosition,
        _access :: Access
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data Parameter
  = Parameter PositionalParameter
  | VarArgParmeter
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data Function
  = Function
      { _name :: Symbol,
        _start :: Address,
        _params :: [Parameter],
        _locals :: [PilVar],
        _globals :: [PilVar],
        _result :: Maybe PilVar
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Used to reference a function by symbol and address.
-- In particular, useful when the function body is unavailable
-- or awkward to include.
data FuncRef
  = FuncRef
      { _name :: Symbol,
        _start :: Address,
        _params :: [Parameter],
        _hasResult :: Bool
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Used for type inference of functions to assign
-- shared type variables to function parameters.
data FuncVar expr
  = FuncParam (CallTarget expr) ParamPosition
  | FuncResult (CallTarget expr)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | A call target has an expression destination
-- along with parameter and result information.
-- Used when a function cannot be resolved (e.g., indirect call)
-- or when it simple has not been resolved.
-- A CallTarget can be used to group call sites that share
-- the same call destination and expectations around 
-- call arguments and results.
data CallTarget expr
  = CallTarget
      { _dest :: CallDest expr,
        _numArgs :: Int
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype ResultInfo
  = ResultInfo
      {_name :: Text}
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | Describe basic information about a function.
-- Because this describe known functions, we are matching
-- according to symbol. This will work for C, but not C++.
-- Can try matching according to simple name from Symbol and
-- parameter list.
data FuncInfo
  = FuncInfo
      { _name :: Symbol,
        _params :: [Parameter],
        _result :: ResultInfo
      }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- TODO: Consider extending this to support
--       the call expression as the result.
newtype CallResult
  = CallResult 
      {_var :: PilVar}

$(makePrisms ''Access)

$(makeFieldsNoPrefix ''Parameter)

$(makeFieldsNoPrefix ''Function)

$(makePrisms ''FuncVar)

$(makePrisms ''Parameter)

$(makeFieldsNoPrefix ''ResultInfo)

$(makeFieldsNoPrefix ''FuncInfo)

$(makeFieldsNoPrefix ''CallResult)

$(makeFieldsNoPrefix ''CallTarget)