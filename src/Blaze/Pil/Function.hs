module Blaze.Pil.Function where

import Blaze.Prelude
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil
  (Expression, CallOp,  CallStatement,
  )
import qualified Blaze.Types.Pil.Function as Func
import Blaze.Types.Pil.Function
  (CallArg (CallArg),
    CallInfo
      ( CallInfo,
        _args,
        _dest,
        _result
      ),
    CallResult (CallResult),
    CallTarget (CallTarget),
    FuncRef
      ( FuncRef,
        _hasResult,
        _name,
        _params,
        _start
      ),
    Function
  )

-- mkCallSite :: FuncRef -> StmtIndex -> Stmt -> Maybe CallSite
-- mkCallSite caller stmtIndex stmt = do
--   callStmt <- mkCallStatement stmt
--   let callDest = getCallDest callStmt
--   return
--     CallSite
--       { _caller = caller,
--         _stmtIndex = stmtIndex,
--         _callee = funcRefFromCallDest callDest
--       }

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

-- mkCallInfo :: CallStatement -> CallInfo
-- mkCallInfo callStmt =
--   CallInfo
--     { _dest = callStmt ^. (Pil.callOp . Pil.dest),
--       _args = CallArg <$> callStmt ^. (Pil.callOp . Pil.params),
--       _result = case callStmt ^. Pil.stmt of
--         Pil.Def (Pil.DefOp var _) -> Just $ CallResult var
--         _ -> Nothing
--     }
      
-- mkCallInfoFromCallOp :: CallOp a -> CallInfo a
-- mkCallInfoFromCallOp callOp =
--  CallInfo
--     { _dest = callOp ^. Pil.dest,
--       _args = CallArg <$> callOp ^. Pil.params,
--       -- Without additional context we don't know if a result 
--       -- from the call is used.
--       _result = Nothing
--     }