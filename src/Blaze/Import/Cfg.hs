module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Cfg

-- TODO: (Cfg a) should reall be (Cfg ImporterSpecificMetadata)
class CfgImporter a where
  getCfg :: a -> Function -> IO (Maybe (Cfg a))
