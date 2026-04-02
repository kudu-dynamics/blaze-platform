module Flint.Shell.Commands.StdLib
  ( stdlibAddCommand
  , stdlibListCommand
  , stdlibRemoveCommand
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis.Path.Matcher.Primitives (KnownFunc(..), FuncVar(..), FuncVarExpr(..), PrimSpec)
import Flint.Types.Symbol (Symbol(..))
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.StdLib as StdLibPrims
import qualified Flint.Cfg.Store as Store

import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a FuncVar mapping: "ret", "arg:N"
parseFuncVar :: Text -> Maybe FuncVar
parseFuncVar "ret" = Just Ret
parseFuncVar "return" = Just Ret
parseFuncVar t
  | Just n <- Text.stripPrefix "arg:" t = Arg <$> readMaybe (Text.unpack n)
  | otherwise = Nothing

showFuncVar :: FuncVar -> Text
showFuncVar Ret = "ret"
showFuncVar (Arg n) = "arg:" <> show n
showFuncVar (Global _) = "global"

-- | Parse "var=mapping" pairs from args.
parseVarMappings :: [Text] -> Either Text (HashMap (Symbol expr) FuncVarExpr)
parseVarMappings args = do
  mappingPairs <- traverse parseOne args
  return $ HashMap.fromList mappingPairs
  where
    parseOne arg = case Text.breakOn "=" arg of
      (_, "") -> Left $ "Expected var=mapping in: " <> arg
      (varName, rest) -> do
        let mappingText = Text.drop 1 rest
        case parseFuncVar mappingText of
          Nothing -> Left $ "Invalid mapping: " <> mappingText <> ". Use 'ret' or 'arg:N'"
          Just fv -> Right (coerce (Text.strip varName), FuncVar fv)

validateVarMappings :: PrimSpec -> HashMap (Symbol Pil.Expression) FuncVarExpr -> Either Text ()
validateVarMappings primSpec varMap
  | HashSet.null missing && HashSet.null unknown = Right ()
  | otherwise = Left . Text.unlines . filter (not . Text.null) $
      [ "Mappings do not match primitive spec: " <> primSpec ^. #name
      , if HashSet.null missing then "" else "Missing vars: " <> fmt missing
      , if HashSet.null unknown then "" else "Unknown vars: " <> fmt unknown
      , "Expected vars: " <> fmt expected
      ]
  where
    expected = primSpec ^. #vars
    provided = HashSet.fromList $ HashMap.keys varMap
    missing = HashSet.difference expected provided
    unknown = HashSet.difference provided expected
    fmt = Text.intercalate ", " . fmap (view #unSymbol) . HashSet.toList

-- ---------------------------------------------------------------------------
-- stdlib-add
-- ---------------------------------------------------------------------------

stdlibAddCommand :: ShellCommand
stdlibAddCommand = ShellCommand
  { cmdName = "stdlib-add"
  , cmdAliases = ["sa"]
  , cmdHelp = "Register a function as a known primitive"
  , cmdUsage = "stdlib-add <func> <primType> <var>=<mapping> ..."
  , cmdAction = stdlibAddAction
  }

stdlibAddAction :: ShellState -> [Text] -> IO CommandResult
stdlibAddAction _st [] = return $ ResultError
  "Usage: stdlib-add <func> <primType> <var>=<mapping> ...\n  e.g. stdlib-add myAlloc allocHeap ptr=ret size=arg:0"
stdlibAddAction _st [_] = return $ ResultError "Missing <primType> and mappings."
stdlibAddAction _st [_, _] = return $ ResultError "Missing variable mappings."
stdlibAddAction st (funcName : primTypeName : mappingArgs) = do
  case HashMap.lookup primTypeName PrimLib.primSpecRegistry of
    Nothing -> return $ ResultError $ "Unknown prim type: " <> primTypeName
      <> "\nAvailable: " <> Text.intercalate ", " (HashMap.keys PrimLib.primSpecRegistry)
    Just primSpec -> case parseVarMappings mappingArgs of
      Left err -> return $ ResultError err
      Right varMap -> case validateVarMappings primSpec varMap of
        Left err -> return $ ResultError err
        Right () -> do
          let kf = KnownFunc
                { prim = primSpec
                , funcName = funcName
                , varMapping = varMap
                , constraints = []
                }
          modifyIORef' (st ^. #userKnownFuncs) (<> [kf])
          -- Merge into callable prims cache
          Store.addKnownFuncs [kf] (st ^. #cfgStore)
          return $ ResultOk $ "Registered " <> funcName <> " as " <> primTypeName

-- ---------------------------------------------------------------------------
-- stdlib-list
-- ---------------------------------------------------------------------------

stdlibListCommand :: ShellCommand
stdlibListCommand = ShellCommand
  { cmdName = "stdlib-list"
  , cmdAliases = ["sl"]
  , cmdHelp = "List all known function mappings"
  , cmdUsage = "stdlib-list"
  , cmdAction = stdlibListAction
  }

stdlibListAction :: ShellState -> [Text] -> IO CommandResult
stdlibListAction st _args = do
  user <- readIORef (st ^. #userKnownFuncs)
  let defaults = StdLibPrims.allStdLibPrims
      defaultLines = fmap (\kf -> formatKnownFunc kf <> "  (built-in)") defaults
      userLines = fmap formatKnownFunc user
  return $ ResultText $ Text.unlines $ defaultLines <> userLines

formatKnownFunc :: KnownFunc -> Text
formatKnownFunc kf =
  let vars = Text.intercalate ", "
           . fmap (\(k, v) -> k ^. #unSymbol <> "=" <> showFuncVarExpr v)
           . HashMap.toList
           $ kf ^. #varMapping
  in "  " <> kf ^. #funcName <> " -> " <> (kf ^. #prim . #name) <> "(" <> vars <> ")"

showFuncVarExpr :: FuncVarExpr -> Text
showFuncVarExpr (FuncVar fv) = showFuncVar fv
showFuncVarExpr _ = "<expr>"

-- ---------------------------------------------------------------------------
-- stdlib-remove
-- ---------------------------------------------------------------------------

stdlibRemoveCommand :: ShellCommand
stdlibRemoveCommand = ShellCommand
  { cmdName = "stdlib-remove"
  , cmdAliases = ["sr"]
  , cmdHelp = "Remove a user-registered known function"
  , cmdUsage = "stdlib-remove <func>"
  , cmdAction = stdlibRemoveAction
  }

stdlibRemoveAction :: ShellState -> [Text] -> IO CommandResult
stdlibRemoveAction _st [] = return $ ResultError "Usage: stdlib-remove <func>"
stdlibRemoveAction st (func : _) = do
  old <- readIORef (st ^. #userKnownFuncs)
  let new = filter (\kf -> kf ^. #funcName /= func) old
      oldKnownFuncs = StdLibPrims.allStdLibPrims <> old
      newKnownFuncs = StdLibPrims.allStdLibPrims <> new
      removed = length old - length new
  writeIORef (st ^. #userKnownFuncs) new
  if removed > 0
    then do
      Store.replaceKnownFuncsPreservingLearned oldKnownFuncs newKnownFuncs (st ^. #cfgStore)
      return $ ResultOk $ "Removed: " <> func
    else return $ ResultError $ "No user-registered function found: " <> func
