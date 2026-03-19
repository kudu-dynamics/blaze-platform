module Flint.Shell.Commands.Xrefs
  ( functionsCallingCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store

import Blaze.Types.CallGraph (CallSite)
import qualified Blaze.Types.Function as Func

import qualified Data.Text as Text
import Numeric (showHex)


functionsCallingCommand :: ShellCommand
functionsCallingCommand = ShellCommand
  { cmdName = "calls"
  , cmdAliases = ["xrefs", "functions-calling"]
  , cmdHelp = "Find functions that call a given function (internal or extern)"
  , cmdUsage = "calls <func_name>"
  , cmdAction = functionsCallingAction
  }

functionsCallingAction :: ShellState -> [Text] -> IO CommandResult
functionsCallingAction st args = case args of
  [] -> return $ ResultError "Usage: calls <func_name>"
  (funcName : _) -> do
    let store = st ^. #cfgStore
        lowerName = Text.toLower funcName
    -- Search both extern and internal functions
    externs <- Store.getExternalFuncs store
    internals <- Store.getInternalFuncs store
    let matchingExterns = Func.External <$>
          filter (\e -> Text.toLower (e ^. #name) == lowerName) externs
        matchingInternals = Func.Internal <$>
          filter (\f -> Text.toLower (f ^. #name) == lowerName) internals
        matchingFuncs = matchingExterns <> matchingInternals
    case matchingFuncs of
      [] -> return $ ResultOk $ "No function found matching: " <> funcName
      _ -> do
        results <- fmap concat . forM matchingFuncs $
          fmap (fmap formatCallSite) . Store.getCallSitesToFunc store
        case results of
          [] -> return $ ResultOk $ "No call sites found for: " <> funcName
          _  -> return $ ResultText $ Text.unlines results

formatCallSite :: CallSite -> Text
formatCallSite callSite =
  let callerName = callSite ^. #caller . #name
      callAddr = callSite ^. #address
  in callerName <> " @ " <> showAddr callAddr

showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")
