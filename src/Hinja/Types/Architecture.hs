module Hinja.Types.Architecture where

import Hinja.Prelude

import Hinja.C.Pointers (BNArchitecture)

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
