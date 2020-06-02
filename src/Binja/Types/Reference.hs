{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Reference where

import Binja.Prelude

import Binja.Types.Architecture (Architecture)
import Binja.Types.Function (Function)
import Binja.C.Pointers (BNArchitecture, BNFunction)


data ReferenceSource = ReferenceSource
  { _func :: Function
  , _arch :: Architecture
  , _addr :: Address
  } deriving (Eq, Ord, Show)


data BNReferenceSource = BNReferenceSource
  { _func :: BNFunction
  , _arch :: BNArchitecture
  , _addr :: Address
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNReferenceSource)
$(makeFieldsNoPrefix ''ReferenceSource)
