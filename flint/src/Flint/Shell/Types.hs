module Flint.Shell.Types where

import Flint.Prelude

import Data.IORef

import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep)
import Flint.Analysis.Path.Matcher (TypedStmt)
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Types.Function (Function)
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text



type PathId = Int

-- | Parse path IDs from command arguments. Supports:
--   Individual args:   1 2 3
--   Bracket lists:     [1, 2, 3]
--   Ranges:            1..5  or  1-5  (expands to [1,2,3,4,5])
--   Mixed:             [1, 3..7, 10]
parsePathIds :: [Text] -> [PathId]
parsePathIds args =
  let joined = Text.intercalate " " args
      stripped = Text.filter (\c -> c /= '[' && c /= ']') joined
      parts = filter (not . Text.null)
            . fmap Text.strip
            . Text.split (\c -> c == ',' || isSpace c)
            $ stripped
  in concatMap parsePart parts

parsePart :: Text -> [PathId]
parsePart t
  -- Try "a..b" range first
  | (a, rest) <- Text.breakOn ".." t
  , not (Text.null rest)
  , Just lo <- readMaybe (Text.unpack $ Text.strip a)
  , Just hi <- readMaybe (Text.unpack . Text.strip $ Text.drop 2 rest)
  = [lo..hi]
  -- Try "a-b" range (only if both sides are digits, to avoid negative numbers)
  | Just (a, b) <- splitRange t
  , Just lo <- readMaybe (Text.unpack a)
  , Just hi <- readMaybe (Text.unpack b)
  = [lo..hi]
  -- Single number
  | Just n <- readMaybe (Text.unpack t)
  = [n]
  | otherwise = []
  where
    -- Split "3-10" but not "-3" (negative number)
    splitRange :: Text -> Maybe (Text, Text)
    splitRange s =
      let digits = Text.takeWhile isDigit s
      in if Text.null digits
         then Nothing
         else case Text.uncons (Text.drop (Text.length digits) s) of
           Just ('-', rest) | not (Text.null rest) -> Just (digits, rest)
           _ -> Nothing

data CachedPath = CachedPath
  { pilPath     :: [Pil.Stmt]
  , sourceFunc  :: Function
  , pathPrep    :: Maybe (PathPrep TypedStmt)
  } deriving (Generic)

data ShellState = ShellState
  { cfgStore    :: CfgStore
  , pathCache   :: IORef (HashMap PathId CachedPath)
  , nextPathId  :: IORef PathId
  , useSolver   :: IORef Bool
  , baseOffset  :: Address
  } deriving (Generic)

data CommandResult
  = ResultText Text
  | ResultPaths [(PathId, Text)]
  | ResultSolver [(PathId, Text)]
  | ResultWMIs [(PathId, [Text])]
  | ResultFunctions [(Text, Address)]
  | ResultOk Text
  | ResultError Text
  deriving (Eq, Show, Generic)

initShellState :: CfgStore -> Address -> Bool -> IO ShellState
initShellState store base solver = do
  cache <- newIORef HashMap.empty
  nextId <- newIORef 0
  solverRef <- newIORef solver
  return ShellState
    { cfgStore = store
    , pathCache = cache
    , nextPathId = nextId
    , useSolver = solverRef
    , baseOffset = base
    }

allocPathId :: ShellState -> IO PathId
allocPathId st = do
  pid <- readIORef (st ^. #nextPathId)
  modifyIORef' (st ^. #nextPathId) (+ 1)
  return pid

insertPath :: ShellState -> CachedPath -> IO PathId
insertPath st cp = do
  pid <- allocPathId st
  modifyIORef' (st ^. #pathCache) (HashMap.insert pid cp)
  return pid

lookupPath :: ShellState -> PathId -> IO (Maybe CachedPath)
lookupPath st pid = HashMap.lookup pid <$> readIORef (st ^. #pathCache)

deletePath :: ShellState -> PathId -> IO Bool
deletePath st pid = do
  cache <- readIORef (st ^. #pathCache)
  if HashMap.member pid cache
    then do
      modifyIORef' (st ^. #pathCache) (HashMap.delete pid)
      return True
    else return False

allPaths :: ShellState -> IO (HashMap PathId CachedPath)
allPaths st = readIORef (st ^. #pathCache)
