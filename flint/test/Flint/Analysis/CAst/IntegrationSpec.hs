{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Evaluate" -}

module Flint.Analysis.CAst.IntegrationSpec
  ( module Flint.Analysis.CAst.IntegrationSpec
  ) where

import Flint.Prelude hiding (check)

import Test.Hspec

import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Decomp (DecompImporter(decompileFunctionAst))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Import.CallGraph as CG
import Blaze.Types.Function (FuncRef(..), FunctionRef)

import Flint.Analysis.CAst.Matcher (runCheck)
import Flint.Analysis.CAst.Patterns (allChecks)
import Flint.Types.Analysis.CAst.Finding (CAstFinding(..), Severity(..))

import qualified Data.HashMap.Strict as HashMap


-- ---------------------------------------------------------------------------
-- Test binary paths (relative to flint/ working directory)
-- ---------------------------------------------------------------------------

diveLoggerBin :: FilePath
diveLoggerBin = "../res/test_bins/Dive_Logger/Dive_Logger.gzf"


-- ---------------------------------------------------------------------------
-- Test context
-- ---------------------------------------------------------------------------

-- | Functions we directly assert about. Decompiling all 82 functions per test
-- is an order of magnitude slower than necessary — these tests only need the
-- specific functions whose findings we care about.
targetFunctionNames :: [Text]
targetFunctionNames =
  [ "cgc_PrintDiveEntry"   -- format-string FP regression (must produce 0 findings)
  , "cgc_SetParam"         -- deep-scan regression (strcpy nested in `if`)
  , "cgc_calloc"           -- unchecked-alloc target
  , "cgc_strncpy"          -- loop-missing-bounds-check target
  , "cgc_strdup"           -- dangerous-function (cgc_strcpy at top level)
  ]

newtype TestCtx = TestCtx
  { -- | Cached findings for every targeted function, keyed by name.
    -- Built once in 'beforeAll' so individual tests are pure assertions.
    findingsByFunc :: HashMap Text [CAstFinding]
  } deriving (Generic)

getTestCtx :: IO TestCtx
getTestCtx = do
  (imp' :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
  allRefs <- CG.getFunctions imp'
  let internals = internalRefs allRefs
      wanted = HashMap.fromList
        [ (name, fref)
        | fref <- internals
        , let name = fref ^. #name
        , name `elem` targetFunctionNames
        ]
  scanned <- forM (HashMap.toList wanted) $ \(name, fref) -> do
    fs <- runChecksOn imp' fref
    return (name, fs)
  return TestCtx { findingsByFunc = HashMap.fromList scanned }


-- ---------------------------------------------------------------------------
-- Helpers: decompile and run all checks against one function
-- ---------------------------------------------------------------------------

-- | Extract internal function refs only.
internalRefs :: [FuncRef] -> [FunctionRef]
internalRefs = mapMaybe (\case { Blaze.Types.Function.InternalRef f -> Just f; _ -> Nothing })

-- | Decompile a single function and run every check against its statements.
runChecksOn :: GhidraImporter -> FunctionRef -> IO [CAstFinding]
runChecksOn imp' fref = do
  mStmts <- decompileFunctionAst imp' (fref ^. #address)
  case mStmts of
    Nothing    -> return []
    Just stmts -> return $ concatMap (`runCheck` stmts) allChecks

-- | Look up the cached findings for a target function, filtered by check name.
findingsFor :: TestCtx -> Text -> Text -> [CAstFinding]
findingsFor ctx funcName checkN =
  let all_ = HashMap.lookupDefault [] funcName (ctx ^. #findingsByFunc)
  in filter (\f -> f ^. #findingName == checkN) all_


-- ---------------------------------------------------------------------------
-- Test spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = beforeAll getTestCtx . describe "C AST Integration (Dive_Logger binary)" $ do

  -- === format-string FP regression ===
  -- cgc_PrintDiveEntry calls cgc_printf many times, always with a literal
  -- format. Before the CLitString fix, the matcher fired on every call
  -- because Ghidra was emitting string literals as CIdent nodes. This test
  -- locks in that the matcher no longer flags any of them.
  it "cgc_PrintDiveEntry has zero format-string findings" $ \ctx ->
    findingsFor ctx "cgc_PrintDiveEntry" "format-string" `shouldBe` []

  -- === deep-scan regression ===
  -- cgc_SetParam wraps its cgc_strcpy call inside an `if`. Before the
  -- nested-body deep-scan fix, dangerous-function only walked top-level
  -- stmts and missed the call entirely.
  it "cgc_SetParam reports dangerous-function for the nested cgc_strcpy" $ \ctx -> do
    let fs = findingsFor ctx "cgc_SetParam" "dangerous-function"
    length fs `shouldSatisfy` (>= 1)
    forM_ fs $ \f -> do
      f ^. #findingName `shouldBe` "dangerous-function"
      f ^. #severity `shouldBe` Medium

  -- === unchecked-alloc on cgc_calloc ===
  -- cgc_calloc does `s = cgc_malloc(...); cgc_memset(s, 0, ...)` with no
  -- NULL check, which the pattern catches. Also exercises dedup — before
  -- that fix, CStar backtracking produced two identical findings here.
  it "cgc_calloc reports exactly one unchecked-alloc finding" $ \ctx -> do
    let fs = findingsFor ctx "cgc_calloc" "unchecked-alloc"
    case fs of
      [f] -> do
        f ^. #severity `shouldBe` Low
        HashMap.lookup "ptr" (f ^. #boundNames) `shouldBe` Just "s"
      other -> expectationFailure $
        "Expected exactly one unchecked-alloc finding, got " <> show (length other)

  -- === loop-missing-bounds-check on cgc_strncpy ===
  it "cgc_strncpy reports loop-missing-bounds-check" $ \ctx -> do
    let fs = findingsFor ctx "cgc_strncpy" "loop-missing-bounds-check"
    length fs `shouldSatisfy` (>= 1)
    forM_ fs $ \f -> f ^. #severity `shouldBe` Medium

  -- === dangerous-function on a top-level cgc_strcpy caller ===
  -- cgc_strdup calls cgc_strcpy at the top level (not nested). Catches a
  -- regression where the matcher fails to fire even on the trivial path.
  it "cgc_strdup reports dangerous-function" $ \ctx -> do
    let fs = findingsFor ctx "cgc_strdup" "dangerous-function"
    length fs `shouldSatisfy` (>= 1)

  -- === No CRITICAL on any of the targeted functions ===
  -- Dive_Logger has no UAF / double-free in the audited functions; if any
  -- of them ever escalates to Critical it's almost certainly a regression.
  it "no targeted function reports a CRITICAL severity" $ \ctx -> do
    let allFs = concat (HashMap.elems (ctx ^. #findingsByFunc))
        criticals = filter (\f -> f ^. #severity == Critical) allFs
    criticals `shouldBe` []
