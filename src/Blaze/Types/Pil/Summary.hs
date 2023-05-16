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
    , effects :: [Effect]
    } deriving (Eq, Ord, Show, Generic)

data Effect
  = EffectWrite Stmt
  | EffectAlloc Stmt
  | EffectDealloc Stmt
  | EffectCall Stmt
  deriving (Eq, Ord, Show, Generic)
