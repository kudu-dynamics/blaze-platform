module Flint.Types.Cfg.InterBinaryStore where

import Flint.Prelude

import Blaze.Types.Function (Function)

import Blaze.Types.Cfg (PilCfg)


-- | Mapping of function name to its cfg.
-- Could be used for externs or regular calls
-- TODO: make this into sqlite db
type CfgStore = HashMap Text [(Function, PilCfg)]
