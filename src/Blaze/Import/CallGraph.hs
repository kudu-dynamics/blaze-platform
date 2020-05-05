module Blaze.Import.CallGraph where

import Blaze.CallGraph (CallSite, Function)
import Blaze.Prelude

class CallGraphImporter a where
  getFunctions :: a -> IO [Function]
  getCallSites :: a -> Function -> IO [CallSite]
