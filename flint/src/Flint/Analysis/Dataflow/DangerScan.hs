-- | Scan dataflow summaries for dangerous patterns.
--
-- Uses the composed interprocedural summaries to detect data flows
-- from input sources to dangerous sinks (strcpy, sprintf, free, etc.)
-- without requiring full path sampling.
--
-- This complements the C AST pattern matcher (which works on decompiled
-- C within a single function) by checking cross-function flows at the
-- summary level.
module Flint.Analysis.Dataflow.DangerScan
  ( -- * Scanning
    dangerScan
  , dangerScanStore
  , dangerScanStoreWith
  , DangerScanOptions(..)
  , defaultDangerScanOptions
    -- * Results
  , DangerFinding(..)
  , DangerKind(..)
  , Severity(..)
    -- * Sink/source definitions
  , dangerousSinks
  , inputSources
  , libcWrapperPairs
  ) where

import Flint.Prelude

import Flint.Types.Analysis.Dataflow

import Blaze.Types.Function (FuncRef(..), funcRefName)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


-- | Severity level for a finding.
data Severity = Info | Medium | High | Critical
  deriving (Eq, Ord, Show)

-- | What kind of dangerous pattern was detected.
data DangerKind
  = UnboundedCopy
    -- ^ Data flows to an unbounded copy function (strcpy, strcat)
  | FormatString
    -- ^ Data flows to a format string parameter
  | HeapCorruption
    -- ^ Data flows to free/realloc in a suspicious way
  | CommandInjection
    -- ^ Data flows to system/popen/exec
  | BufferOverflow
    -- ^ Data flows to a copy function's size or destination
  | TaintedArithmetic
    -- ^ Tainted data used in size computation
  deriving (Eq, Ord, Show)

-- | A single finding from the danger scan.
data DangerFinding = DangerFinding
  { callerRef     :: FuncRef
    -- ^ Function containing the dangerous call site
  , calleeRef     :: FuncRef
    -- ^ The dangerous sink function
  , kind          :: DangerKind
  , severity      :: Severity
  , description   :: Text
  , callerParam   :: Int
    -- ^ Which caller parameter provides the tainted data
  , sinkParam     :: Int
    -- ^ Which sink parameter receives it
  } deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Sink and source definitions
-- ---------------------------------------------------------------------------

-- | Dangerous sink functions and which parameter is the "dangerous" one.
-- (funcName, paramIndex, dangerKind, severity, description)
--
-- Severity heuristic:
--   * Critical: unrecoverable if reached (gets, system, popen, exec*)
--   * High:     memory corruption / code exec likely (strcpy, memcpy size,
--               sprintf/fprintf format, printf format)
--   * Medium:   bounded or constrained variants (strncpy/snprintf take a
--               size, so exploitation requires additional mistakes;
--               free/realloc are only exploitable under UAF/double-free)
dangerousSinks :: [(Text, Int, DangerKind, Severity, Text)]
dangerousSinks =
  -- Unbounded copies (no size parameter)
  [ ("strcpy",   1, UnboundedCopy, High,     "unbounded string copy from tainted source")
  , ("strcat",   1, UnboundedCopy, High,     "unbounded string concatenation from tainted source")
  , ("gets",     0, UnboundedCopy, Critical, "gets() reads unbounded input")
  , ("wcscpy",   1, UnboundedCopy, High,     "unbounded wide string copy")
  , ("wcscat",   1, UnboundedCopy, High,     "unbounded wide string concatenation")
  -- Format string (format parameter = param 0 for printf, param 1 for fprintf/sprintf)
  , ("printf",   0, FormatString,  High,     "tainted data as printf format string")
  , ("sprintf",  1, FormatString,  High,     "tainted data as sprintf format string")
  , ("fprintf",  1, FormatString,  High,     "tainted data as fprintf format string")
  , ("vprintf",  0, FormatString,  High,     "tainted data as vprintf format string")
  , ("vsprintf", 1, FormatString,  High,     "tainted data as vsprintf format string")
  , ("snprintf", 2, FormatString,  Medium,   "tainted data as snprintf format string")
  -- Command injection
  , ("system",   0, CommandInjection, Critical, "tainted data passed to system()")
  , ("popen",    0, CommandInjection, Critical, "tainted data passed to popen()")
  , ("execve",   0, CommandInjection, Critical, "tainted data passed to execve()")
  , ("execl",    0, CommandInjection, Critical, "tainted data passed to execl()")
  -- Bounded copies where size parameter is tainted
  , ("memcpy",   2, BufferOverflow, High,    "tainted data controls memcpy size")
  , ("memmove",  2, BufferOverflow, High,    "tainted data controls memmove size")
  , ("strncpy",  2, BufferOverflow, Medium,  "tainted data controls strncpy size")
  , ("strncat",  2, BufferOverflow, Medium,  "tainted data controls strncat size")
  , ("read",     2, BufferOverflow, Medium,  "tainted data controls read size")
  , ("recv",     2, BufferOverflow, Medium,  "tainted data controls recv size")
  -- Heap functions
  , ("free",     0, HeapCorruption, Medium,  "tainted data flows to free()")
  , ("realloc",  1, HeapCorruption, Medium,  "tainted data controls realloc size")
  , ("malloc",   0, TaintedArithmetic, Medium, "tainted data controls malloc size")
  , ("calloc",   0, TaintedArithmetic, Medium, "tainted data controls calloc count")
  , ("calloc",   1, TaintedArithmetic, Medium, "tainted data controls calloc size")
  ]

-- | Known input source functions whose return or output params produce
-- attacker-controlled data. (funcName, outputParamOrRet)
inputSources :: HashSet Text
inputSources = HashSet.fromList
  [ "read", "recv", "recvfrom", "recvmsg"
  , "fread", "fgets", "scanf", "fscanf", "sscanf"
  , "getenv", "getc", "fgetc", "gets"
  -- CGC-specific
  , "cgc_receive", "cgc_receive_until", "cgc_receive_until_flush"
  ]

-- ---------------------------------------------------------------------------
-- Scan options
-- ---------------------------------------------------------------------------

-- | Options controlling 'dangerScanStoreWith'.
newtype DangerScanOptions = DangerScanOptions
  { excludeLibcPlumbing :: Bool
    -- ^ If True, suppress findings where the caller is a known libc
    -- wrapper whose implementation legitimately calls the sink (e.g.
    -- @printf@ calling @vprintf@). Default: True — these are almost
    -- always noise at the application-audit level.
  }
  deriving (Eq, Ord, Show, Generic)

defaultDangerScanOptions :: DangerScanOptions
defaultDangerScanOptions = DangerScanOptions
  { excludeLibcPlumbing = True
  }

-- | Known caller-callee pairs where the caller is the standard-library
-- wrapper whose implementation /is/ a call to the callee. These are
-- not vulnerabilities, just the plumbing of the library. Names are
-- compared after the usual prefix stripping (so e.g. @cgc_printf@
-- matches the @printf@ entry).
libcWrapperPairs :: HashSet (Text, Text)
libcWrapperPairs = HashSet.fromList
  -- printf family wraps the v* variants
  [ ("printf",   "vprintf")
  , ("sprintf",  "vsprintf")
  , ("fprintf",  "vfprintf")
  , ("snprintf", "vsnprintf")
  -- strdup = malloc + strcpy + strlen
  , ("strdup",   "malloc")
  , ("strdup",   "strcpy")
  , ("strdup",   "strlen")
  -- calloc = malloc + memset
  , ("calloc",   "malloc")
  , ("calloc",   "memset")
  -- strtok does its own internal copy via strcpy
  , ("strtok",   "strcpy")
  ]

-- ---------------------------------------------------------------------------
-- Scanning logic
-- ---------------------------------------------------------------------------

-- | Scan a DataflowStore with default options.
dangerScanStore :: DataflowStore -> [DangerFinding]
dangerScanStore = dangerScanStoreWith defaultDangerScanOptions

-- | Scan a DataflowStore with explicit options. For each function,
-- checks if any of its call sites target a known dangerous sink and
-- if the caller's parameters flow to the dangerous sink parameter via
-- the composed dataflow summaries.
dangerScanStoreWith :: DangerScanOptions -> DataflowStore -> [DangerFinding]
dangerScanStoreWith opts store =
  let summaries = store ^. #summaries
      raw = concatMap (dangerScan summaries) (HashMap.elems summaries)
  in if opts ^. #excludeLibcPlumbing
     then filter (not . isLibcWrapperFinding) raw
     else raw

-- | True if a finding pairs a known libc wrapper caller with its
-- implementation callee (e.g. @printf@ calling @vprintf@).
isLibcWrapperFinding :: DangerFinding -> Bool
isLibcWrapperFinding f =
  let callerNames = normalizedNames (funcRefName (f ^. #callerRef))
      calleeNames = normalizedNames (funcRefName (f ^. #calleeRef))
  in any (\c1 -> any (\c2 -> HashSet.member (c1, c2) libcWrapperPairs) calleeNames)
         callerNames

-- | Common prefixes used to wrap standard library names. Matching
-- strips any of these before comparing to the known sink list. Add
-- new prefixes here if you encounter a binary that uses one.
sinkNamePrefixes :: [Text]
sinkNamePrefixes = ["cgc_", "__wrap_", "__real_", "_"]

-- | Normalise a function name into all candidate names (original plus
-- any prefix-stripped variants) so that e.g. @cgc_strcpy@ matches a
-- sink list entry for @strcpy@.
normalizedNames :: Text -> [Text]
normalizedNames name =
  name : [ stripped
         | p <- sinkNamePrefixes
         , Just stripped <- [Text.stripPrefix p name]
         ]

-- | Scan a single function's summary for dangerous call-site flows.
dangerScan :: HashMap FuncRef FuncSummary -> FuncSummary -> [DangerFinding]
dangerScan _allSummaries summary
  | not (summary ^. #computed) = []
  | otherwise =
      let callerRef' = summary ^. #funcRef
          sites = summary ^. #callSites
      in concatMap (checkSite callerRef') sites
  where
    checkSite callerRef' site =
      let calleeNames = normalizedNames (funcRefName (site ^. #calleeRef))
      in concatMap (matchSink callerRef' site calleeNames) dangerousSinks

    matchSink callerRef' site calleeNames (sinkName, sinkParamIdx, dangerKind', severity', desc) =
      if sinkName `elem` calleeNames
      then
        [ DangerFinding
            { callerRef   = callerRef'
            , calleeRef   = site ^. #calleeRef
            , kind        = dangerKind'
            , severity    = severity'
            , description = desc
            , callerParam = callerIdx
            , sinkParam   = sinkParamIdx
            }
        | (calleeParamIdx, ParamEndpoint callerIdx) <- site ^. #argMapping
        , calleeParamIdx == sinkParamIdx
        ]
      else []
