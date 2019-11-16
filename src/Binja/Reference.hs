{-# LANGUAGE TemplateHaskell #-}

module Binja.Reference
  ( module Exports
  , getCodeReferences
  ) where

import Binja.Prelude

import Binja.Types.Reference as Exports
import Binja.Function (createFunction)
import qualified Binja.Architecture as Architecture
import Binja.C.Helpers (getCodeReferences')
import Binja.C.Types (Address)
import Binja.C.Pointers (BNBinaryView)


getCodeReferences :: BNBinaryView -> Address -> IO [ReferenceSource]
getCodeReferences bv addr' = getCodeReferences' bv addr' >>= mapM f
  where
    f x = ReferenceSource
          <$> createFunction (x ^. func)
          <*> Architecture.create (x ^. arch)
          <*> pure (x ^. addr)

