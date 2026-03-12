module Flint.Analysis.LibC where

import Flint.Prelude()

import Flint.Types.Analysis (Parameter(..), TaintPropagator(..))

taintPropagators :: [TaintPropagator]
taintPropagators =
    [ FunctionCallPropagator "fgets" (Parameter 2) (Parameter 0)
    , FunctionCallPropagator "memcpy" (Parameter 1) (Parameter 0)
    , FunctionCallPropagator "memchr" (Parameter 0) ReturnParameter
        -- TODO: Should handle more C standard functions of interset
    ]
