{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

-- todo: most this to binja converter dir
module Blaze.Types.Function where

import Blaze.Prelude hiding (Symbol)
import Data.BinaryAnalysis (Symbol (Symbol))
import Blaze.Types.Graph (NodeId (NodeId), Identifiable (getNodeId))

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data ParamInfo = ParamInfo
  { name :: Text
  , access :: Access
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

data FuncParamInfo
  = FuncParamInfo ParamInfo
  | FuncVarArgInfo ParamInfo
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

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
  deriving (Eq, Ord, Show, Generic, Hashable, FromJSON, ToJSON)

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

-- TODO: Consider moving Function to Data.BinaryAnalysis
-- | These are functions internally defined in a binary
data Function = Function
  { symbol :: Maybe Symbol
  , name :: Text
  , address :: Address
  , params :: [FuncParamInfo]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

dummy :: Function
dummy = Function Nothing "jones" 0x1000 []

instance Identifiable Function Int where
  getNodeId f = NodeId $ hash f

-- | Since the External address space doesn't really exist, this is just a
-- way for the backend to uniqely refer to each function.
-- In Ghidra, this would be an Address in EXTERNAL with offset == externalIndex
-- TODO: Add AddressSpace to our Address type and we can just make this Address
newtype ExternAddress = ExternAddress { externalIndex :: Word64 }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

-- | These are functions externally defined and referred to in a binary
data ExternFunction = ExternFunction
  { symbol :: Maybe Symbol
  , name :: Text
  , library :: Maybe Text
  , address :: ExternAddress
  , params :: [FuncParamInfo]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Identifiable ExternFunction Int where
  getNodeId f = NodeId $ hash f
