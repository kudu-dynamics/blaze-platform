module Blaze.Pil.Function where

import Blaze.Prelude
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil
  ( CallStatement,
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

mkCallTarget :: CallInfo -> CallTarget
mkCallTarget info =
  CallTarget 
    { _dest = info ^. Func.dest,
      _numArgs = length $ info ^. Func.args,
      _hasResult = isJust $ info ^. Func.result
    }

mkCallInfo :: CallStatement -> CallInfo
mkCallInfo callStmt =
  CallInfo
    { _dest = callStmt ^.(Pil.callOp . Pil.dest),
      _args = args,
      _result = result
    }
  where
    args :: [CallArg]
    args = CallArg <$> callStmt ^. (Pil.callOp . Pil.params)
    result :: Maybe CallResult
    result = case callStmt ^. Pil.stmt of
      Pil.Def (Pil.DefOp var _) -> Just $ CallResult var
      _ -> Nothing
      
