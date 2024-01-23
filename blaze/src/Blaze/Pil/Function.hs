module Blaze.Pil.Function where

import Blaze.Prelude
import Blaze.Types.Pil (
  CallDest,
  CallTarget (CallTarget, dest),
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

mkCallTarget
  :: HasField' "dest" a (CallDest expr)
  => a
  -> CallTarget expr
mkCallTarget call =
  CallTarget
    { dest = call ^. #dest
    }
