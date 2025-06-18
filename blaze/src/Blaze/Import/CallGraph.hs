module Blaze.Import.CallGraph where

import Blaze.Types.CallGraph (CallSite)
import Blaze.Types.Function (Func)
import Blaze.Prelude


class CallGraphImporter a where
  getFunction :: a -> Address -> IO (Maybe Func)
  getFunctions :: a -> IO [Func]
  -- | getCallSites returns sites that call Function or ExternFunction
  getCallSites :: a -> Func -> IO [CallSite]
