module Blaze.Types.VTable where
  

import Binja.Core (BNBinaryReader, BNBinaryView)
import Blaze.Function (Function)
import Blaze.Prelude

type Ctx = ReaderT VTContext IO


data VTContext
  = VTContext
    { width :: AddressWidth,
      reader :: BNBinaryReader,
      bv :: BNBinaryView
    }
  deriving (Eq, Ord, Show, Generic)

data VTable
  = VTable
      { topOffset :: Maybe Bytes,
        typeInfo :: Maybe TypeInfo,
        vptrAddress :: Address,
        vFunctions :: [Function],
        parents :: Maybe [VTable]
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable VTable

data TypeInfo
  = TypeInfo
      { helperClass :: Maybe Address,
        name :: Maybe Text,
        parentsTypeInfo :: Maybe TypeInfo
      }
  deriving (Eq, Ord, Show, Generic)

instance Hashable TypeInfo
