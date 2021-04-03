module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.Cfg
import Blaze.Types.Import (ImportResult)
import Blaze.Types.Pil (Ctx)

class CfgImporter a where
  type NodeDataType a
  type NodeMapType a
  getCfg
    :: a
    -> Ctx
    -> IO (Maybe (ImportResult (Cfg (NodeDataType a)) (NodeMapType a)))
