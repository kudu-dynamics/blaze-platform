module Flint.Shell.Commands.CAst
  ( checkCAstCommand
  , castScanCommand
  , castPatternAddCommand
  , castPatternListCommand
  , castPatternRemoveCommand
  , castPatternResetCommand
  , castPatternSaveCommand
  , castPatternLoadCommand
  , castPatternShowCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (findFunction)
import Flint.Types.Analysis.CAst.Finding (CAstFinding(..), Severity(..))
import Flint.Types.Analysis.CAst.Matcher (CAstCheck(..))
import Flint.Analysis.CAst.Matcher (CheckResult(..), runCheckCapped, maxFindingsPerCheck)
import qualified Flint.Analysis.CAst.PatternStore as PS
import Flint.Util (timeIt)
import qualified Flint.Cfg.Store as Store

import Blaze.Types.Function (FunctionRef)
import Blaze.Types.CAst (AddrRange(..), CStmt)

import Data.Aeson (eitherDecodeStrict)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (NominalDiffTime)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Numeric
import Text.Printf (printf)


-- ===================================================================
-- Existing commands (check-cast, cast-scan)
-- ===================================================================

-- | check-cast: run C AST vulnerability checks on a function.
checkCAstCommand :: ShellCommand
checkCAstCommand = ShellCommand
  { cmdName = "check-cast"
  , cmdAliases = ["cc"]
  , cmdHelp = "Check function for C AST vulnerability patterns"
  , cmdUsage = "check-cast <pattern|all> <func_name_or_addr>"
  , cmdAction = checkCAstAction
  }

-- | cast-scan: scan functions for C AST vulnerability patterns.
castScanCommand :: ShellCommand
castScanCommand = ShellCommand
  { cmdName = "cast-scan"
  , cmdAliases = ["cs"]
  , cmdHelp = "Scan functions for C AST vulnerability patterns (analyzed functions by default)"
  , cmdUsage = "cast-scan [--all] [pattern_name]"
  , cmdAction = castScanAction
  }


-- ---------------------------------------------------------------------------
-- Actions: check-cast, cast-scan
-- ---------------------------------------------------------------------------

checkCAstAction :: ShellState -> [Text] -> IO CommandResult
checkCAstAction st [patName, funcRef] = do
  case st ^. #decompFuncAst of
    Nothing -> return $ ResultError "C AST analysis not available (no live backend)"
    Just decompAst -> do
      mFunc <- findFunction st funcRef
      case mFunc of
        Nothing -> return $ ResultError $ "Function not found: " <> funcRef
        Just func -> do
          (mStmts, decompTime) <- timeIt $ decompAst (func ^. #address)
          case mStmts of
            Nothing -> return $ ResultError $ "Could not decompile: " <> funcRef
            Just stmts -> do
              all_ <- PS.getAllChecks (st ^. #userCAstChecks)
              case resolveChecksFrom all_ patName of
                Nothing -> return $ ResultError $ "Unknown pattern: " <> patName
                    <> ". Available: " <> T.intercalate ", " (fmap (^. #checkName) all_)
                Just resolved -> do
                  ((fs, truncatedChecks), matchTime) <- timeIt $ do
                    let results = fmap (\c -> (c ^. #checkName, runCheckCapped c stmts)) resolved
                        allFs = concatMap (findings . snd) results
                        truncNames = HashSet.fromList
                          [ n | (n, r) <- results, truncated r ]
                    _ <- evaluate (length allFs)
                    return (allFs, truncNames)
                  let checkMap = mkCheckMap all_
                      timing = "Decompile: " <> fmtTime decompTime
                        <> ", Match: " <> fmtTime matchTime
                  return $ ResultText
                    (formatFindings checkMap truncatedChecks funcRef fs <> "\n" <> timing)
checkCAstAction _ _ = return $ ResultError "Usage: check-cast <pattern|all> <func_name_or_addr>"


-- | Per-function timing result. 'ftTruncated' holds the names of checks
-- whose findings were capped for this function — surfaced as a warning
-- in the scan output.
data FuncTiming = FuncTiming
  { ftRef       :: FunctionRef
  , ftDecompMs  :: NominalDiffTime
  , ftMatchMs   :: NominalDiffTime
  , ftFindings  :: [(FunctionRef, CAstFinding)]
  , ftTruncated :: HashSet Text
  } deriving (Generic)

castScanAction :: ShellState -> [Text] -> IO CommandResult
castScanAction st args = do
  case st ^. #decompFuncAst of
    Nothing -> return $ ResultError "C AST analysis not available (no live backend)"
    Just decompAst -> do
      let (scanAll, restArgs) = case args of
            ("--all" : rest) -> (True, rest)
            other            -> (False, other)
      all_ <- PS.getAllChecks (st ^. #userCAstChecks)
      let mChecks = case restArgs of
            [patName] -> resolveChecksFrom all_ patName
            []        -> Just all_
            _         -> Nothing
      case mChecks of
        Nothing -> return $ ResultError "Usage: cast-scan [--all] [pattern_name]"
        Just resolved -> do
          funcRefs <- getFunctionRefsForScan st scanAll
          let scopeLabel = if scanAll then "all" else "analyzed"
              checkMap = mkCheckMap all_
          when (null funcRefs && not scanAll) $
            putText "  (No analyzed functions found. Run analyze-all first, or use cast-scan --all.)"
          (timings, totalTime) <- timeIt $ forM funcRefs $ \fref -> do
            (mStmts, dt) <- timeIt $ decompAst (fref ^. #address)
            case mStmts of
              Nothing -> return FuncTiming
                { ftRef = fref, ftDecompMs = dt, ftMatchMs = 0
                , ftFindings = [], ftTruncated = HashSet.empty }
              Just stmts -> do
                ((fs, tr), mt) <- timeIt $ runChecks resolved fref stmts
                return FuncTiming
                  { ftRef = fref, ftDecompMs = dt, ftMatchMs = mt
                  , ftFindings = fs, ftTruncated = tr }
          let taggedFindings = concatMap ftFindings timings
              truncatedChecks = foldl' HashSet.union HashSet.empty (fmap ftTruncated timings)
              totalDecomp = sum (fmap ftDecompMs timings)
              totalMatch = sum (fmap ftMatchMs timings)
              timingReport = formatTimingReport timings totalTime totalDecomp totalMatch scopeLabel
          return $ ResultText
            (formatScanResults checkMap truncatedChecks taggedFindings <> "\n" <> timingReport)


-- ===================================================================
-- Pattern management commands
-- ===================================================================

castPatternAddCommand :: ShellCommand
castPatternAddCommand = ShellCommand
  { cmdName = "cast-pattern-add"
  , cmdAliases = ["cpa"]
  , cmdHelp = "Add a C AST pattern from JSON"
  , cmdUsage = "cast-pattern-add <json_string>"
  , cmdAction = castPatternAddAction
  }

castPatternListCommand :: ShellCommand
castPatternListCommand = ShellCommand
  { cmdName = "cast-pattern-list"
  , cmdAliases = ["cpl"]
  , cmdHelp = "List C AST patterns (user-defined by default, --all for built-in too)"
  , cmdUsage = "cast-pattern-list [--all]"
  , cmdAction = castPatternListAction
  }

castPatternRemoveCommand :: ShellCommand
castPatternRemoveCommand = ShellCommand
  { cmdName = "cast-pattern-remove"
  , cmdAliases = ["cpr"]
  , cmdHelp = "Remove a user-defined C AST pattern"
  , cmdUsage = "cast-pattern-remove <name>"
  , cmdAction = castPatternRemoveAction
  }

castPatternResetCommand :: ShellCommand
castPatternResetCommand = ShellCommand
  { cmdName = "cast-pattern-reset"
  , cmdAliases = []
  , cmdHelp = "Remove all user-defined C AST patterns"
  , cmdUsage = "cast-pattern-reset"
  , cmdAction = castPatternResetAction
  }

castPatternSaveCommand :: ShellCommand
castPatternSaveCommand = ShellCommand
  { cmdName = "cast-pattern-save"
  , cmdAliases = ["cps"]
  , cmdHelp = "Save user-defined C AST patterns to a JSON file"
  , cmdUsage = "cast-pattern-save <filepath>"
  , cmdAction = castPatternSaveAction
  }

castPatternLoadCommand :: ShellCommand
castPatternLoadCommand = ShellCommand
  { cmdName = "cast-pattern-load"
  , cmdAliases = ["cpload"]
  , cmdHelp = "Load C AST patterns from a JSON file"
  , cmdUsage = "cast-pattern-load <filepath>"
  , cmdAction = castPatternLoadAction
  }

castPatternShowCommand :: ShellCommand
castPatternShowCommand = ShellCommand
  { cmdName = "cast-pattern-show"
  , cmdAliases = ["cpshow"]
  , cmdHelp = "Show a pattern's JSON definition"
  , cmdUsage = "cast-pattern-show <name>"
  , cmdAction = castPatternShowAction
  }


-- ---------------------------------------------------------------------------
-- Pattern management actions
-- ---------------------------------------------------------------------------

castPatternAddAction :: ShellState -> [Text] -> IO CommandResult
castPatternAddAction st args = do
  let jsonText = T.intercalate " " args
  case eitherDecodeStrict (TE.encodeUtf8 jsonText) of
    Left err -> return $ ResultError $ "Invalid JSON: " <> cs err
    Right newCheck -> do
      PS.addUserCheck (st ^. #userCAstChecks) newCheck
      return $ ResultOk $ "Added pattern: " <> newCheck ^. #checkName

castPatternListAction :: ShellState -> [Text] -> IO CommandResult
castPatternListAction st args = do
  let showAll = "--all" `elem` args
  if showAll
    then do
      all_ <- PS.getAllChecks (st ^. #userCAstChecks)
      user <- PS.getUserChecks (st ^. #userCAstChecks)
      let userNames = HashSet.fromList $ fmap (^. #checkName) user
          tag c = if HashSet.member (c ^. #checkName) userNames then " [user]" else ""
          fmt c = "  " <> c ^. #checkName
                <> " [" <> showSeverity (c ^. #severity) <> "]"
                <> maybe "" (\n -> " (CWE-" <> show n <> ")") (c ^. #cwe)
                <> tag c
                <> " -- " <> c ^. #description
      return $ ResultText $ "All patterns (" <> show (length all_) <> "):\n"
        <> T.unlines (fmap fmt all_)
    else do
      user <- PS.getUserChecks (st ^. #userCAstChecks)
      if null user
        then return $ ResultText "No user-defined patterns. Use cast-pattern-add or cast-pattern-load.\nUse cast-pattern-list --all to see built-in patterns."
        else do
          let fmt c = "  " <> c ^. #checkName
                <> " [" <> showSeverity (c ^. #severity) <> "]"
                <> maybe "" (\n -> " (CWE-" <> show n <> ")") (c ^. #cwe)
                <> " -- " <> c ^. #description
          return $ ResultText $ "User-defined patterns (" <> show (length user) <> "):\n"
            <> T.unlines (fmap fmt user)

castPatternRemoveAction :: ShellState -> [Text] -> IO CommandResult
castPatternRemoveAction st [name] = do
  found <- PS.removeUserCheck (st ^. #userCAstChecks) name
  if found
    then return $ ResultOk $ "Removed: " <> name
    else return $ ResultError $ "No user pattern named: " <> name
castPatternRemoveAction _ _ = return $ ResultError "Usage: cast-pattern-remove <name>"

castPatternResetAction :: ShellState -> [Text] -> IO CommandResult
castPatternResetAction st _ = do
  PS.clearUserChecks (st ^. #userCAstChecks)
  return $ ResultOk "All user-defined C AST patterns cleared."

castPatternSaveAction :: ShellState -> [Text] -> IO CommandResult
castPatternSaveAction st [path] = do
  result <- PS.saveUserChecks (st ^. #userCAstChecks) (cs path)
  case result of
    Left err -> return $ ResultError err
    Right fp -> return $ ResultOk $ "Saved to: " <> cs fp
castPatternSaveAction _ _ = return $ ResultError "Usage: cast-pattern-save <filepath>"

castPatternLoadAction :: ShellState -> [Text] -> IO CommandResult
castPatternLoadAction st [path] = do
  result <- PS.loadUserChecks (st ^. #userCAstChecks) (cs path)
  case result of
    Left err -> return $ ResultError err
    Right names -> return $ ResultOk $ "Loaded " <> show (length names)
      <> " pattern(s): " <> T.intercalate ", " names
castPatternLoadAction _ _ = return $ ResultError "Usage: cast-pattern-load <filepath>"

castPatternShowAction :: ShellState -> [Text] -> IO CommandResult
castPatternShowAction st [name] = do
  all_ <- PS.getAllChecks (st ^. #userCAstChecks)
  case filter (\c -> c ^. #checkName == name) all_ of
    [matched] -> do
      let json = LBS.toStrict $ AesonPretty.encodePretty matched
      return $ ResultText $ TE.decodeUtf8 json
    [] -> return $ ResultError $ "No pattern named: " <> name
        <> ". Available: " <> T.intercalate ", " (fmap (^. #checkName) all_)
    _ -> return $ ResultError $ "Multiple patterns named: " <> name
castPatternShowAction _ _ = return $ ResultError "Usage: cast-pattern-show <name>"


-- ===================================================================
-- Shared helpers
-- ===================================================================

-- | Run checks on a function's statements, forcing evaluation for timing.
-- Returns tagged findings plus the set of check names whose results were
-- capped (see 'maxFindingsPerCheck') so callers can surface a truncation
-- warning.
runChecks
  :: [CAstCheck]
  -> FunctionRef
  -> [CStmt]
  -> IO ([(FunctionRef, CAstFinding)], HashSet Text)
runChecks checks fref stmts = do
  let perCheck = fmap (\c -> (c ^. #checkName, runCheckCapped c stmts)) checks
      fs = concatMap (findings . snd) perCheck
      tagged = fmap (fref,) fs
      truncNames = HashSet.fromList [ n | (n, r) <- perCheck, truncated r ]
  _ <- evaluate (length tagged)
  return (tagged, truncNames)


-- | Resolve check name(s) from user input against a full check list.
resolveChecksFrom :: [CAstCheck] -> Text -> Maybe [CAstCheck]
resolveChecksFrom all_ "all" = Just all_
resolveChecksFrom all_ name = case filter (\c -> c ^. #checkName == name) all_ of
  []      -> Nothing
  matched -> Just matched

-- | Build a lookup map from check name to check for formatting.
mkCheckMap :: [CAstCheck] -> HashMap Text CAstCheck
mkCheckMap checks = HashMap.fromList [(c ^. #checkName, c) | c <- checks]

lookupExplanation :: HashMap Text CAstCheck -> Text -> Text
lookupExplanation cm name = maybe "See description." (^. #explanation) (HashMap.lookup name cm)

lookupRemediation :: HashMap Text CAstCheck -> Text -> Text
lookupRemediation cm name = maybe "Review the flagged code." (^. #remediation) (HashMap.lookup name cm)

lookupCwe :: HashMap Text CAstCheck -> Text -> Text
lookupCwe cm name = case HashMap.lookup name cm >>= (^. #cwe) of
  Just n  -> " (CWE-" <> show n <> ")"
  Nothing -> ""


-- ---------------------------------------------------------------------------
-- Formatting: check-cast (single function)
-- ---------------------------------------------------------------------------

-- | Format findings for a single function, grouped by check name. Check
-- names present in 'truncatedChecks' get a "capped at N" marker so users
-- know the listing is incomplete.
formatFindings
  :: HashMap Text CAstCheck
  -> HashSet Text
  -> Text
  -> [CAstFinding]
  -> Text
formatFindings _ _ funcName [] = "No findings for " <> funcName
formatFindings checkMap truncatedChecks funcName fs =
  let byCheck = groupByKey (^. #findingName) fs
      sevOf (_, gs) = maybe Info (^. #severity) (listToMaybe gs)
      sorted = List.sortOn (Down . sevOf) byCheck
  in "C AST findings for " <> funcName <> " (" <> show (length fs) <> "):\n\n"
    <> T.intercalate "\n" (fmap (formatCheckGroup checkMap truncatedChecks) sorted)

formatCheckGroup
  :: HashMap Text CAstCheck
  -> HashSet Text
  -> (Text, [CAstFinding])
  -> Text
formatCheckGroup _ _ (_, []) = ""
formatCheckGroup checkMap truncatedChecks (checkName, fs@(representative:_)) =
  let n = length fs
      countStr = if n > 1 then " (" <> show n <> " matches)" else ""
      truncMark = if HashSet.member checkName truncatedChecks
        then " [capped at " <> show maxFindingsPerCheck <> ", more results truncated]"
        else ""
  in "  [" <> showSeverity (representative ^. #severity) <> "] "
    <> checkName <> lookupCwe checkMap checkName <> countStr <> truncMark <> "\n"
    <> "    " <> representative ^. #description <> "\n"
    <> "    Why: " <> lookupExplanation checkMap checkName <> "\n"
    <> "    Fix: " <> lookupRemediation checkMap checkName <> "\n"
    <> T.concat (imap (\i f -> formatFindingLocation (i + 1) Nothing f) fs)


-- ---------------------------------------------------------------------------
-- Formatting: cast-scan (all functions, grouped)
-- ---------------------------------------------------------------------------

-- | Format scan results: group by severity, then by check name, listing
-- locations. Any check in 'truncatedChecks' gets a "capped at N" marker.
formatScanResults
  :: HashMap Text CAstCheck
  -> HashSet Text
  -> [(FunctionRef, CAstFinding)]
  -> Text
formatScanResults _ _ [] = "No findings."
formatScanResults checkMap truncatedChecks taggedFindings =
  let total = length taggedFindings
      bySeverity = groupByKey (\(_, f) -> f ^. #severity) taggedFindings
      sorted = List.sortOn (Down . fst) bySeverity
      truncNote =
        if HashSet.null truncatedChecks then ""
        else "\n\nNote: findings for "
          <> T.intercalate ", " (List.sort (HashSet.toList truncatedChecks))
          <> " were capped at " <> show maxFindingsPerCheck
          <> " per function — refine the pattern to see the rest."
  in "C AST scan: " <> show total <> " finding(s)\n\n"
    <> T.intercalate "\n" (fmap (formatSeverityGroup checkMap truncatedChecks) sorted)
    <> truncNote
    <> "\n\n(Use 'check-cast <pattern> <func>' for per-function details)"

formatSeverityGroup
  :: HashMap Text CAstCheck
  -> HashSet Text
  -> (Severity, [(FunctionRef, CAstFinding)])
  -> Text
formatSeverityGroup checkMap truncatedChecks (sev, tagged) =
  let byCheck = groupByKey (\(_, f) -> f ^. #findingName) tagged
      sorted = List.sortOn fst byCheck
  in showSeverity sev <> " (" <> show (length tagged) <> "):\n"
    <> T.intercalate "\n" (fmap (formatScanCheckGroup checkMap truncatedChecks) sorted)

formatScanCheckGroup
  :: HashMap Text CAstCheck
  -> HashSet Text
  -> (Text, [(FunctionRef, CAstFinding)])
  -> Text
formatScanCheckGroup _ _ (_, []) = ""
formatScanCheckGroup checkMap truncatedChecks (checkName, tagged@((_, representative):_)) =
  let n = length tagged
      countStr = if n > 1 then " (" <> show n <> ")" else ""
      truncMark = if HashSet.member checkName truncatedChecks
        then " [capped]"
        else ""
  in "  " <> checkName <> lookupCwe checkMap checkName <> countStr <> truncMark <> "\n"
    <> "    " <> representative ^. #description <> "\n"
    <> "    Why: " <> lookupExplanation checkMap checkName <> "\n"
    <> "    Fix: " <> lookupRemediation checkMap checkName <> "\n"
    <> T.concat (imap (\i (fref, f) -> formatFindingLocation (i + 1) (Just fref) f) tagged)


-- ---------------------------------------------------------------------------
-- Shared formatting helpers
-- ---------------------------------------------------------------------------

formatFindingLocation :: Int -> Maybe FunctionRef -> CAstFinding -> Text
formatFindingLocation idx mFref f =
  let funcPart = case mFref of
        Just fref -> fref ^. #name <> " "
        Nothing   -> ""
      addrPart = formatAddrRanges (f ^. #location)
      bindPart = case HashMap.toList (f ^. #boundNames) of
        []    -> ""
        binds -> "  (" <> T.intercalate ", " (fmap (\(k, v) -> k <> "=" <> v) binds) <> ")"
  in "    " <> show idx <> ". " <> funcPart <> addrPart <> bindPart <> "\n"

formatAddrRanges :: [AddrRange] -> Text
formatAddrRanges [] = ""
formatAddrRanges ranges =
  let unique = List.nub ranges
      fmt r = "@ " <> showAddr (r ^. #minAddr) <> ".." <> showAddr (r ^. #maxAddr)
      shown = fmap fmt (take 3 unique)
      rest = if length unique > 3 then " (+" <> show (length unique - 3) <> " more)" else ""
  in T.intercalate ", " shown <> rest

showAddr :: Address -> Text
showAddr addr =
  let off = addr ^. #offset
      unitSize = addr ^. #space . #addressableUnitSize
      byteOff = off * fromIntegral unitSize
  in "0x" <> T.pack (Numeric.showHex (fromIntegral byteOff :: Word64) "")

showSeverity :: Severity -> Text
showSeverity = \case
  Critical -> "CRITICAL"
  High     -> "HIGH"
  Medium   -> "MEDIUM"
  Low      -> "LOW"
  Info     -> "INFO"

groupByKey :: Hashable k => (a -> k) -> [a] -> [(k, [a])]
groupByKey f xs =
  let grouped = foldl' (\m x -> HashMap.insertWith (<>) (f x) [x] m) HashMap.empty xs
      keys = List.nub (fmap f xs)
  in fmap (\k -> (k, fromMaybe [] (HashMap.lookup k grouped))) keys

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0..]


-- ---------------------------------------------------------------------------
-- Timing report
-- ---------------------------------------------------------------------------

formatTimingReport
  :: [FuncTiming] -> NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> Text -> Text
formatTimingReport timings totalTime totalDecomp totalMatch scopeLabel =
  let nFuncs = length timings
      nScanned = length (filter (\t -> ftMatchMs t > 0 || not (null (ftFindings t))) timings)
      slowest = take 5
        . List.sortOn (Down . ftMatchMs)
        . filter (\t -> ftMatchMs t > 0)
        $ timings
  in T.unlines
    [ "--- Timing ---"
    , "Total: " <> fmtTime totalTime
        <> " (" <> show nFuncs <> " " <> scopeLabel <> " functions, "
        <> show nScanned <> " scanned)"
    , "  Decompile: " <> fmtTime totalDecomp
        <> "  Match: " <> fmtTime totalMatch
    , if null slowest then "" else
        "Slowest (match time):\n" <> T.concat (fmap fmtSlowFunc slowest)
    ]

fmtSlowFunc :: FuncTiming -> Text
fmtSlowFunc t =
  "  " <> ftRef t ^. #name <> ": " <> fmtTime (ftMatchMs t) <> "\n"

fmtTime :: NominalDiffTime -> Text
fmtTime dt
  | secs < 0.001 = T.pack (printf "%.1fus" (secs * 1e6 :: Double))
  | secs < 1     = T.pack (printf "%.1fms" (secs * 1e3 :: Double))
  | otherwise     = T.pack (printf "%.2fs" secs)
  where
    secs = realToFrac dt :: Double


-- | Get function refs for scanning.
-- Default scope is analyzed functions only; --all opts into a whole-binary scan.
-- An empty analyzed cache is reported to the caller as-is (no silent fallback
-- to every function in the binary, which would turn a cheap cached scan into
-- a full decompile on large targets).
getFunctionRefsForScan :: ShellState -> Bool -> IO [FunctionRef]
getFunctionRefsForScan st scanAll
  | scanAll   = Store.getInternalFuncs (st ^. #cfgStore)
  | otherwise = do
      analyzed <- Store.getAnalyzedFunctionRefs (st ^. #cfgStore)
      unless (null analyzed) $ do
        allFuncs <- Store.getInternalFuncs (st ^. #cfgStore)
        putText $ "  (Scanning " <> show (length analyzed) <> " analyzed functions"
          <> " out of " <> show (length allFuncs) <> " total."
          <> " Use --all to scan everything.)"
      return analyzed
