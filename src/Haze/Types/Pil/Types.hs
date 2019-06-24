{-# LANGUAGE TemplateHaskell #-}
module Haze.Types.Pil.Types where

import Hinja.Prelude

data JimDog = JimDog
  { _jimDogName :: Text
  , _jimDogAge :: Int
  } deriving (Eq, Show, Ord)

$(makeFields ''JimDog)
