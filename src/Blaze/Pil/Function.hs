module Blaze.Pil.Function where

import Blaze.Prelude
import Blaze.Types.Pil (CallOp)
import Blaze.Types.Pil.Function (
  CallTarget (CallTarget),
  FuncRef (FuncRef),
  Function,
 )
import qualified Blaze.Types.Pil.Function as Func

funcRefFromFunc :: Function -> FuncRef
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
