module Blaze.Pil.Function where

import Blaze.Prelude
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (CallOp)
import qualified Blaze.Types.Pil.Function as Func
import Blaze.Types.Pil.Function
  (CallTarget (CallTarget),
    FuncRef
      ( FuncRef,
        _hasResult,
        _name,
        _params,
        _start
      ),
    Function
  )

funcRefFromFunc :: Function -> FuncRef
funcRefFromFunc func =
  FuncRef
    { _name = func ^. Func.name,
      _start = func ^. Func.start,
      _params = func ^. Func.params,
      _hasResult = isJust $ func ^. Func.result
    }

mkCallTarget :: CallOp expr -> CallTarget expr
mkCallTarget call =
  CallTarget 
    { _dest = call ^. Pil.dest,
      _numArgs = length $ call ^. Pil.params
    }