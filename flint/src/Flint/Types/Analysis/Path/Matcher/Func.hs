module Flint.Types.Analysis.Path.Matcher.Func where

import Flint.Prelude


data Func
  = FuncName Text
  | FuncNames (HashSet Text)
  | FuncAddr Address
  | FuncNameRegex Text
  deriving (Eq, Ord, Show, Hashable, Generic)
