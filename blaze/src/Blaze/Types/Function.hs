{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

-- todo: most this to binja converter dir
module Blaze.Types.Function where

import Blaze.Prelude hiding (Symbol)
import Data.BinaryAnalysis (Symbol (Symbol))
import Blaze.Types.Graph (NodeId (NodeId), Identifiable (getNodeId))

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

data ParamInfo = ParamInfo
  { name :: Text
  , size :: Maybe Bytes
  , access :: Access
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

-- | Parameter positions start at position 1.
newtype ParamPosition = ParamPosition Int
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data FuncParamInfo
  = FuncParamInfo ParamInfo
  | FuncVarArgInfo ParamInfo
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

data VarArgParameter
  = VarArgParameter
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype ResultInfo = ResultInfo
  {name :: Text}
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
  , params :: [FuncParamInfo]
  , result :: ResultInfo
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

mkFuncInfo :: Text -> Text -> [FuncParamInfo] -> ResultInfo -> FuncInfo
mkFuncInfo name' rawName =
  FuncInfo (Symbol name' rawName)

-- | Convenient type for things that return either
data Func
  = Internal Function
  | External ExternFunction
  deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, ToJSON, Serialize)

-- | lenses for common fields in Func
-- I tried really hard to get #name etc to work for Func, but it seems impossible.

_symbol :: Lens' Func (Maybe Symbol)
_symbol = lens
  (\case Internal f -> f ^. #symbol
         External e -> e ^. #symbol)
  (\s newA -> case s of
     Internal f -> Internal (f & #symbol .~ newA)
     External e -> External (e & #symbol .~ newA))

_name :: Lens' Func Text
_name = lens
  (\case Internal f -> f ^. #name
         External e -> e ^. #name)
  (\s newA -> case s of
     Internal f -> Internal (f & #name .~ newA)
     External e -> External (e & #name .~ newA))

_params :: Lens' Func [FuncParamInfo]
_params = lens
  (\case Internal f -> f ^. #params
         External e -> e ^. #params)
  (\s newA -> case s of
     Internal f -> Internal (f & #params .~ newA)
     External e -> External (e & #params .~ newA))

  
instance Identifiable Func Int where
  -- TODO: aren't we going to maybe have some collisions if we just use the hash?
  getNodeId f = NodeId $ hash f

-- | These are functions internally defined in a binary
data Function = Function
  { symbol :: Maybe Symbol
  , name :: Text
  , address :: Address
  , params :: [FuncParamInfo]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Hash by address only — unique per function, avoids traversing params list.
instance Hashable Function where
  hashWithSalt salt f = hashWithSalt salt (f ^. #address)

instance Identifiable Function Int where
  getNodeId f = NodeId $ hash f

-- | Lightweight version of 'Function' without params.
-- Getting params requires decompilation which is expensive, so this type
-- is used for function lists and call graphs where we just need to mark
-- where a function is and its name.
data FunctionRef = FunctionRef
  { symbol :: Maybe Symbol
  , name :: Text
  , address :: Address
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- | Hash by address only, consistent with Function.
instance Hashable FunctionRef where
  hashWithSalt salt f = hashWithSalt salt (f ^. #address)

instance Identifiable FunctionRef Int where
  getNodeId f = NodeId $ hash f

toFunctionRef :: Function -> FunctionRef
toFunctionRef f = FunctionRef
  { symbol = f ^. #symbol
  , name = f ^. #name
  , address = f ^. #address
  }

-- | These are functions externally defined and referred to in a binary
-- Important note, for Ghidra the offset field in address refers to the externalIndex
data ExternFunction = ExternFunction
  { symbol :: Maybe Symbol
  , name :: Text
  , library :: Maybe Text
  , address :: Address
  , params :: [FuncParamInfo]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialize)

instance Hashable ExternFunction where
  hashWithSalt salt f = hashWithSalt salt (f ^. #address)

instance Identifiable ExternFunction Int where
  getNodeId f = NodeId $ hash f

toExternFunctionRef :: ExternFunction -> FunctionRef
toExternFunctionRef f = FunctionRef
  { symbol = f ^. #symbol
  , name = f ^. #name
  , address = f ^. #address
  }

-- | Lightweight version of 'Func' without params, for use in call graphs
-- and function lists where decompilation is not needed.
data FuncRef
  = InternalRef FunctionRef
  | ExternalRef FunctionRef
  deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, ToJSON, Serialize)

instance Identifiable FuncRef Int where
  getNodeId f = NodeId $ hash f

toFuncRef :: Func -> FuncRef
toFuncRef = \case
  Internal f -> InternalRef $ toFunctionRef f
  External f -> ExternalRef $ toExternFunctionRef f

funcRefAddress :: FuncRef -> Address
funcRefAddress = \case
  InternalRef f -> f ^. #address
  ExternalRef f -> f ^. #address

funcRefName :: FuncRef -> Text
funcRefName = \case
  InternalRef f -> f ^. #name
  ExternalRef f -> f ^. #name
