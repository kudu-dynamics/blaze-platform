{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Hinja.Types.LLIL.Reg where

import Hinja.Prelude

data Reg = Reg
  { _index  :: Int
  , _isTemp :: Bool
  , _name   :: Text
  } deriving (Eq, Read, Show)

makeLenses ''Reg
