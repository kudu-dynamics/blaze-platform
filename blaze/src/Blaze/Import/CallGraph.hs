module Blaze.Import.CallGraph where

import Blaze.Types.CallGraph (CallSite)
import Blaze.Types.Function (Function)
import Blaze.Prelude

class CallGraphImporter a where
  getFunction :: a -> Address -> IO (Maybe Function)
  getFunctions :: a -> IO [Function]
  getCallSites :: a -> Function -> IO [CallSite]
