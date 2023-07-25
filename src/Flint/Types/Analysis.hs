module Flint.Types.Analysis
  ( module Flint.Types.Analysis
  ) where

import Flint.Prelude


type AnalysisCtx = ()

type BndbFilePath = FilePath

-- -- | An analysis cache for a single db.
-- -- Eventually this should handle multiple dbs.
-- -- TODO: used CachedCalc from blaze-ui
-- data AnalysisState = AnalysisState
--   { funcs :: Maybe [Function]
--   , cfgs :: HashMap Function (Maybe PilCfg)
--   , funcStatements :: Maybe 
--   }

-- newtype Analysis a = Analysis { runAnalysisMonad_ :: ReaderT AnalysisCtx
