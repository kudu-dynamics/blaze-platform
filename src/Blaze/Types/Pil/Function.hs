{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Pil.Function where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil (PilVar, StmtIndex)
import Data.BinaryAnalysis (Symbol)

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

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
data FuncVar
  = FuncParam FuncRef Parameter
  | FuncResult FuncRef
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

data CallSite
  = CallSite
      { _caller :: FuncRef,
        _stmtIndex :: StmtIndex,
        _callee :: FuncRef
      }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

$(makePrisms ''Access)

$(makeFieldsNoPrefix ''Parameter)

$(makeFieldsNoPrefix ''Function)

$(makePrisms ''FuncVar)

$(makePrisms ''Parameter)

$(makeFieldsNoPrefix ''ResultInfo)

$(makeFieldsNoPrefix ''FuncInfo)
