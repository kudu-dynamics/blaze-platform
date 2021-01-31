module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Cfg

class CfgImporter a b c | a -> b c where
  getCfg :: a -> Function -> IO (Maybe (Cfg b c))
