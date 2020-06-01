{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.VTable
  (module Blaze.Types.VTable) where
  

import Binja.Core (BNBinaryReader, BNBinaryView)
import Blaze.CallGraph (Function)
import Blaze.Prelude

type Ctx = ReaderT VTContext IO

data VTContext
  = VTContext
    { _width :: AddressWidth,
      _reader :: BNBinaryReader,
      _bv :: BNBinaryView
    }

data VTable
  = VTable
      { _topOffset :: Maybe Word64,
        _typeInfo :: Maybe TypeInfo,
        _vptrAddress :: Address,
        _vFunctions :: [Function],
        _parents :: Maybe [VTable]
      }
  deriving (Eq, Ord, Show)

data TypeInfo
  = TypeInfo
      { _helperClass :: Maybe Address,
        _name :: Maybe Text,
        _parentsTypeInfo :: Maybe TypeInfo
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''VTable)
$(makeFieldsNoPrefix ''TypeInfo)
$(makeFieldsNoPrefix ''VTContext)
