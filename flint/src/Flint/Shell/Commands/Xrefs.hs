module Flint.Shell.Commands.Xrefs
  ( functionsCallingCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store
import qualified Flint.Types.CachedCalc as CC

import Blaze.Types.CallGraph (CallSite)
import qualified Blaze.Types.Function as Func

import qualified Data.Text as Text
import Numeric (showHex)


functionsCallingCommand :: ShellCommand
functionsCallingCommand = ShellCommand
  { cmdName = "functions-calling"
  , cmdAliases = ["xrefs"]
  , cmdHelp = "Find internal functions that call an extern  (e.g. functions-calling system)"
  , cmdUsage = "functions-calling <extern_name>"
  , cmdAction = functionsCallingAction
  }

functionsCallingAction :: ShellState -> [Text] -> IO CommandResult
functionsCallingAction st args = case args of
  [] -> return $ ResultError "Usage: functions-calling <extern_name>"
  (externName : _) -> do
    let store = st ^. #cfgStore
    -- Find matching extern functions
    externs <- Store.getExternalFuncs store
    let matchingExterns = filter (\e -> Text.toLower (e ^. #name) == Text.toLower externName) externs
    case matchingExterns of
      [] -> return $ ResultOk $ "No extern function found matching: " <> externName
      _ -> do
        -- Get all internal functions and their call sites
        internalFuncs <- Store.getInternalFuncs store
        results <- fmap concat . forM internalFuncs $ \func -> do
          mCallSites <- CC.get func (store ^. #callSitesCache)
          case mCallSites of
            Nothing -> return []
            Just callSites -> do
              let matching = filter (isCallingExtern matchingExterns) callSites
              return $ fmap formatCallSite matching
        case results of
          [] -> return $ ResultOk $ "No call sites found for: " <> externName
          _  -> return $ ResultText $ Text.unlines results

isCallingExtern :: [Func.ExternFunction] -> CallSite -> Bool
isCallingExtern externs callSite = case callSite ^. #dest of
  Func.External e -> e `elem` externs
  _ -> False

formatCallSite :: CallSite -> Text
formatCallSite callSite =
  let callerName = callSite ^. #caller . #name
      callAddr = callSite ^. #address
  in callerName <> " @ " <> showAddr callAddr

showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")
