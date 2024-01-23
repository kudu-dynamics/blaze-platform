{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Architecture where

import Binja.Prelude

import Binja.C.Pointers (BNArchitecture)

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
