{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.BinaryAnalysis where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Text.Encoding as TE

import Data.Generics.Labels ()
import Data.Generics.Product.Fields ()

import Data.Hashable
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import qualified Numeric

-- Orphan Serialize instance for Text (cereal doesn't provide one)
instance Serialize Text where
  put = Serialize.put . TE.encodeUtf8
  get = TE.decodeUtf8 <$> Serialize.get

newtype Bytes = Bytes Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

newtype Bits = Bits Word64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

toBits :: Bytes -> Bits
toBits (Bytes n) = Bits (8*n)

toBytes :: Bits -> Bytes
toBytes (Bits n) = Bytes (n `div` 8)

newtype ByteOffset = ByteOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

newtype BitOffset = BitOffset Int64
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Serialize)

toBitOffset :: ByteOffset -> BitOffset
toBitOffset (ByteOffset n) = BitOffset (8*n)

toByteOffset :: BitOffset -> ByteOffset
toByteOffset (BitOffset n) = ByteOffset (n `div` 8)

newtype AddressWidth = AddressWidth {bits :: Bits}
  deriving (Eq, Ord, Read, Show, Generic, Enum)
  deriving newtype (Real, Integral, Num)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)

data AddressSpaceName
  = EXTERNAL
  | HASH
  | Const
  | Ram
  | Register
  | Stack
  | Unique
  | Other Text
  deriving (Eq, Ord, Read, Show, Generic, Hashable)
  deriving anyclass (FromJSON, ToJSON, Serialize)

data AddressSpace = AddressSpace
  { ptrSize :: Bytes
  , addressableUnitSize :: Bytes
  , name :: AddressSpaceName
  }
  deriving (Eq, Ord, Read, Show, Generic, Hashable)
  deriving anyclass (FromJSON, ToJSON, Serialize)

-- newtype Address = Address Bytes
--   deriving (Eq, Ord, Read, Generic, Enum)
--   deriving newtype (Real, Integral, Num)
--   deriving anyclass (Hashable, FromJSON, ToJSON)

data Address = Address
  { space :: AddressSpace
  , offset :: Int64 -- ^ multiply by addressableUnitSize to get byte offset
  }
  deriving (Eq, Ord, Read, Generic, Hashable)
  deriving anyclass (FromJSON, ToJSON, Serialize)

addrToInt :: Address -> Int64
addrToInt addr = addr ^. #offset * fromIntegral (addr ^. #space . #addressableUnitSize)

intToAddr :: Int64 -> Address
intToAddr addr = Address
  { space = AddressSpace
    { ptrSize = Bytes 8
    , addressableUnitSize = Bytes 1
    , name = Ram
    }
  , offset = addr
  }

instance Show Address where
  show addr = prefix ++ Numeric.showHex (addrToInt addr) ""
    where
      prefix = case addr ^. #space . #name of
        Ram -> "Address 0x"
        spaceName -> "Address " ++ show spaceName ++ ":0x"

data Symbol
  = Symbol
      { _symbolName :: Text,
        _symbolRawName :: Text
      }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON, Serialize)
