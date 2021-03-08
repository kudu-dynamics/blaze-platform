module Blaze.Pil.Function where

import Blaze.Prelude
import Blaze.Types.Pil (
  CallOp,
  CallTarget (CallTarget, dest, numArgs),
  FuncRef (FuncRef, name, hasResult, params, start, varparam),
  FunctionInfo,
 )

funcRefFromFunc :: FunctionInfo -> FuncRef
funcRefFromFunc func =
  FuncRef
    { name = func ^. #name
    , start = func ^. #start
    , params = func ^. #params
    , varparam = func ^. #varparam
    , hasResult = isJust $ func ^. #result
    }

mkCallTarget :: CallOp expr -> CallTarget expr
mkCallTarget call =
  CallTarget
    { dest = call ^. #dest
    , numArgs = length $ call ^. #params
    }
