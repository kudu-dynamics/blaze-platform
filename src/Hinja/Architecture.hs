module Hinja.Architecture where

import Hinja.Prelude hiding (onException)

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers

newtype SemClasses = SemClasses (Map Word32 Text)
  deriving (Eq, Ord, Show)

newtype SemGroups = SemGroups (Map Word32 Text)
  deriving (Eq, Ord, Show)

data Architecture = Architecture
  { _handle :: BNArchitecture
  , _name :: Text
  , _semClasses :: SemClasses
  , _semGroups :: SemGroups
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''Architecture)

getSemClasses :: BNArchitecture -> IO SemClasses
getSemClasses archPtr = fmap (SemClasses . Map.fromList) $
  BN.getAllArchitectureSemanticFlagClasses archPtr >>= traverse f
  where
    f :: Word32 -> IO (Word32, Text)
    f n = (n,) . Text.pack <$> BN.getArchitectureSemanticFlagClassName archPtr n

getSemGroups :: BNArchitecture -> IO SemGroups
getSemGroups archPtr = fmap (SemGroups . Map.fromList) $
  BN.getAllArchitectureSemanticFlagGroups archPtr >>= traverse f
  where
    f :: Word32 -> IO (Word32, Text)
    f n = (n,) . Text.pack <$> BN.getArchitectureSemanticFlagGroupName archPtr n

create :: BNArchitecture -> IO Architecture
create ptr = Architecture ptr
             <$> (Text.pack <$> BN.getArchitectureName ptr)
             <*> getSemClasses ptr
             <*> getSemGroups ptr
