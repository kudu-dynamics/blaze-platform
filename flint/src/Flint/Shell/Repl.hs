module Flint.Shell.Repl
  ( runShell
  , allCommands
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command
import Flint.Shell.Commands.Functions (functionsCommand)
import Flint.Shell.Commands.Paths (sampleCommand, showCommand, pshowCommand, freeCommand, pathsCommand, tagCommand, freeUntaggedCommand, expandCommand)
import Flint.Shell.Commands.Solver (solveCommand)
import Flint.Shell.Commands.WMI (wmisCommand, checkWMICommand)
import Flint.Shell.Commands.TypeCheck (typecheckCommand)
import Flint.Shell.Commands.Strings (stringsCommand, stringXrefsCommand)
import Flint.Shell.Commands.Xrefs (functionsCallingCommand)
import Flint.Shell.Commands.Input (inputGenesisCommand)
import Flint.Shell.Commands.Taint (taintAddCommand, taintListCommand, taintRemoveCommand, taintResetCommand)
import Flint.Shell.Commands.PrimDef (primDefCommand, primListCommand, primRemoveCommand, primResetCommand)
import Flint.Shell.Commands.StdLib (stdlibAddCommand, stdlibListCommand, stdlibRemoveCommand)
import Flint.Shell.Commands.Inspect (inspectCommand, saveCommand)
import Flint.Shell.Commands.DumpLift (dumpLiftCommand)
import Flint.Shell.Commands.Analyze (analyzeAllCommand)
import Flint.Shell.Commands.Psum (psumCommand)
import Flint.Shell.Commands.GlobalXrefs (globalXrefsCommand)
import Flint.Shell.Commands.Decomp (decompCommand)
import Flint.Shell.Commands.CAst
  ( checkCAstCommand, castScanCommand
  , castPatternAddCommand, castPatternListCommand
  , castPatternRemoveCommand, castPatternResetCommand
  , castPatternSaveCommand, castPatternLoadCommand
  , castPatternShowCommand
  )
import Flint.Shell.Commands.Dataflow (dataflowSummaryCommand, dataflowScanCommand, dataflowQueryCommand)

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.IO (hIsTerminalDevice, hSetBuffering, BufferMode(..), hFlush, isEOF)


-- | Print to real stdout. Used by the interactive shell for user-facing
-- output so it lands on the terminal (and on piped stdout). The library's
-- 'putText' writes to stderr (see Flint.Prelude) to keep flint-mcp's
-- JSON-RPC channel clean — the shell has to opt back into stdout explicitly
-- via this helper.
shellOut :: Text -> IO ()
shellOut = TIO.putStrLn


allCommands :: [ShellCommand]
allCommands =
  [ functionsCommand
  , sampleCommand
  , showCommand
  , pshowCommand
  , solveCommand
  , wmisCommand
  , checkWMICommand
  , typecheckCommand
  , freeCommand
  , pathsCommand
  , tagCommand
  , freeUntaggedCommand
  , expandCommand
  , functionsCallingCommand
  , stringsCommand
  , stringXrefsCommand
  , inputGenesisCommand
  , taintAddCommand
  , taintListCommand
  , taintRemoveCommand
  , taintResetCommand
  , primDefCommand
  , primListCommand
  , primRemoveCommand
  , primResetCommand
  , stdlibAddCommand
  , stdlibListCommand
  , stdlibRemoveCommand
  , inspectCommand
  , dumpLiftCommand
  , decompCommand
  , saveCommand
  , analyzeAllCommand
  , psumCommand
  , globalXrefsCommand
  , checkCAstCommand
  , castScanCommand
  , castPatternAddCommand
  , castPatternListCommand
  , castPatternRemoveCommand
  , castPatternResetCommand
  , castPatternSaveCommand
  , castPatternLoadCommand
  , castPatternShowCommand
  , dataflowSummaryCommand
  , dataflowScanCommand
  , dataflowQueryCommand
  ]

runShell :: ShellState -> IO ()
runShell st = do
  isTerm <- hIsTerminalDevice stdin
  if isTerm
    then runWithHaskeline st
    else runWithBasicIO st

runWithHaskeline :: ShellState -> IO ()
runWithHaskeline st = do
  let settings = Settings
        { complete = completeWord Nothing " \t" (completionFunc st)
        , historyFile = Just ".flint_history"
        , autoAddHistory = True
        }
  runInputT settings (haskelineLoop st)

haskelineLoop :: ShellState -> InputT IO ()
haskelineLoop st = do
  minput <- getInputLine "flint> "
  case minput of
    Nothing -> liftIO $ shellOut "Goodbye."
    Just input -> do
      let trimmed = Text.strip $ Text.pack input
      if Text.null trimmed
        then haskelineLoop st
        else do
          shouldContinue <- liftIO $ handleInput st trimmed
          when shouldContinue $ haskelineLoop st

-- | Fallback REPL for non-terminal stdin (e.g. stack run, pipes)
runWithBasicIO :: ShellState -> IO ()
runWithBasicIO st = do
  hSetBuffering stdout LineBuffering
  basicLoop st

basicLoop :: ShellState -> IO ()
basicLoop st = do
  putStr ("flint> " :: Text)
  hFlush stdout
  eof <- isEOF
  if eof
    then shellOut "Goodbye."
    else do
      input <- Text.strip <$> getLine
      if Text.null input
        then basicLoop st
        else do
          shouldContinue <- handleInput st input
          when shouldContinue $ basicLoop st

handleInput :: ShellState -> Text -> IO Bool
handleInput st input
  | input `elem` ["quit", "exit", ":q"] = do
      shellOut "Goodbye."
      return False
  | input == "help" || input == "?" = do
      shellOut $ formatHelp allCommands
      return True
  | "set " `Text.isPrefixOf` input = do
      handleSet st (Text.words $ Text.drop 4 input)
      return True
  | otherwise = do
      result <- catch
        (dispatchCommand allCommands st input)
        (\(e :: SomeException) -> return $ ResultError $ "Error: " <> show e)
      renderResult result
      return True

handleSet :: ShellState -> [Text] -> IO ()
handleSet st ["solver", val]
  | val `elem` ["on", "true", "1"] = do
      writeIORef (st ^. #useSolver) True
      shellOut "Solver enabled."
  | val `elem` ["off", "false", "0"] = do
      writeIORef (st ^. #useSolver) False
      shellOut "Solver disabled."
  | otherwise = shellOut "Usage: set solver on|off"
handleSet _st _ = shellOut "Usage: set solver on|off"

renderResult :: CommandResult -> IO ()
renderResult = \case
  ResultText t -> shellOut t
  ResultOk t -> shellOut t
  ResultError t -> shellOut $ "Error: " <> t
  ResultFunctions internals externs -> do
    unless (null internals) $ do
      shellOut "Internal Functions:"
      forM_ internals $ \(name, addr) ->
        shellOut $ "  " <> padAddr addr <> "  " <> name
    unless (null externs) $ do
      unless (null internals) $ shellOut ""
      shellOut "External Functions:"
      forM_ externs $ \(name, mLib) ->
        let libText = maybe "" (\l -> " (" <> l <> ")") mLib
        in shellOut $ "  " <> name <> libText
  ResultTextAndPaths header paths -> do
    shellOut header
    forM_ paths $ \(pid, summary) ->
      shellOut $ "  [" <> show pid <> "] " <> summary
  ResultPaths paths -> do
    forM_ paths $ \(pid, summary) ->
      shellOut $ "  [" <> show pid <> "] " <> summary
  ResultSolver results -> do
    forM_ results $ \(pid, res) ->
      shellOut $ "  [" <> show pid <> "] " <> res
  ResultWMIs results -> do
    forM_ results $ \(pid, msgs) -> do
      shellOut $ "  [" <> show pid <> "]"
      forM_ msgs shellOut

padAddr :: Address -> Text
padAddr = show

completionFunc :: ShellState -> String -> IO [Completion]
completionFunc _st prefix = do
  let prefixT = Text.pack prefix
      cmdNames = concatMap (\c -> (c ^. #cmdName) : (c ^. #cmdAliases)) allCommands
      builtins = ["help", "quit", "exit", "set"]
      allNames = cmdNames <> builtins
      matches = filter (Text.isPrefixOf prefixT) allNames
  return $ fmap (\t -> Completion (Text.unpack t) (Text.unpack t) True) matches
