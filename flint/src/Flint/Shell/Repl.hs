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

import qualified Data.Text as Text
import System.Console.Haskeline
import System.IO (hIsTerminalDevice, hSetBuffering, BufferMode(..), hFlush, isEOF)


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
  , saveCommand
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
    Nothing -> liftIO $ putText "Goodbye."
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
    then putText "Goodbye."
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
      putText "Goodbye."
      return False
  | input == "help" || input == "?" = do
      putText $ formatHelp allCommands
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
      putText "Solver enabled."
  | val `elem` ["off", "false", "0"] = do
      writeIORef (st ^. #useSolver) False
      putText "Solver disabled."
  | otherwise = putText "Usage: set solver on|off"
handleSet _st _ = putText "Usage: set solver on|off"

renderResult :: CommandResult -> IO ()
renderResult = \case
  ResultText t -> putText t
  ResultOk t -> putText t
  ResultError t -> putText $ "Error: " <> t
  ResultFunctions internals externs -> do
    unless (null internals) $ do
      putText "Internal Functions:"
      forM_ internals $ \(name, addr) ->
        putText $ "  " <> padAddr addr <> "  " <> name
    unless (null externs) $ do
      unless (null internals) $ putText ""
      putText "External Functions:"
      forM_ externs $ \(name, mLib) ->
        let libText = maybe "" (\l -> " (" <> l <> ")") mLib
        in putText $ "  " <> name <> libText
  ResultTextAndPaths header paths -> do
    putText header
    forM_ paths $ \(pid, summary) ->
      putText $ "  [" <> show pid <> "] " <> summary
  ResultPaths paths -> do
    forM_ paths $ \(pid, summary) ->
      putText $ "  [" <> show pid <> "] " <> summary
  ResultSolver results -> do
    forM_ results $ \(pid, res) ->
      putText $ "  [" <> show pid <> "] " <> res
  ResultWMIs results -> do
    forM_ results $ \(pid, msgs) -> do
      putText $ "  [" <> show pid <> "]"
      forM_ msgs putText

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
