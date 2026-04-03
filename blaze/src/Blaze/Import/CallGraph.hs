module Blaze.Import.CallGraph where

import Blaze.Types.CallGraph (CallSite)
import Blaze.Types.Function (Func, FuncRef)
import Blaze.Prelude


class CallGraphImporter a where
  getFunction :: a -> Address -> IO (Maybe Func)
  getFunctions :: a -> IO [FuncRef]
  -- | getCallSites returns sites that call a function (internal or extern)
  getCallSites :: a -> FuncRef -> IO [CallSite]
