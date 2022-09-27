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

-- TODO: Consider moving Function to Data.BinaryAnalysis
data Function = Function
  { symbol :: Maybe Symbol
  , name :: Text
  , address :: Address
  , params :: [FuncParamInfo]
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON)

instance Identifiable Function Int where
  getNodeId f = NodeId $ hash f
