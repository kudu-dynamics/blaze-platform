module Blaze.Types.Pil.Summary (
    module Blaze.Types.Pil.Summary,
) where

import Blaze.Prelude

import Blaze.Types.Pil (Expression, PilVar, Stmt)
import Blaze.Types.Pil.Analysis (LoadExpr)

data CodeSummary = CodeSummary
    { inputVars :: [PilVar]
    , inputLoads :: [LoadExpr]
    , results :: [Expression]
    , effects :: [Stmt]
    } deriving (Eq, Ord, Show, Generic)
