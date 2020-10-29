module Blaze.Pil.Function where

import Blaze.Prelude
import Blaze.Types.Pil
  ( CallDest,
    Expression,
    Stmt,
    StmtIndex,
    getCallDest,
    mkCallStatement
  )
import Blaze.Types.Pil.Function
  ( CallSite
      ( CallSite,
        _callee,
        _caller,
        _stmtIndex
      ),
    FuncRef
      ( FuncRef,
        _hasResult,
        _name,
        _params,
        _start
      ),
    Function,
    name,
    params,
    result,
    start,
  )

mkCallSite :: FuncRef -> StmtIndex -> Stmt -> Maybe CallSite
mkCallSite caller stmtIndex stmt = do
  callStmt <- mkCallStatement stmt
  let callDest = getCallDest callStmt
  return CallSite
    { _caller = caller,
      _stmtIndex = stmtIndex,
      _callee = funcRefFromCallDest callDest
    }

funcRefFromFunc :: Function -> FuncRef
funcRefFromFunc func =
  FuncRef
    { _name = func ^. name,
      _start = func ^. start,
      _params = func ^. params,
      _hasResult = isJust $ func ^. result
    }

funcRefFromCallDest :: CallDest Expression -> FuncRef
funcRefFromCallDest dest =
  undefined
