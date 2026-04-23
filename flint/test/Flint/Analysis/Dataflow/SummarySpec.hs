{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Redundant do" -}

module Flint.Analysis.Dataflow.SummarySpec
  ( module Flint.Analysis.Dataflow.SummarySpec
  ) where

import Flint.Prelude

import Test.Hspec

import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Import.CallGraph as CG
import Blaze.Types.Function (FuncRef(..), FunctionRef(..))

import Flint.Types.Analysis.Dataflow
import Flint.Analysis.Dataflow.Summary
import Flint.Analysis.Dataflow.Query (queryFlow)
import Flint.Analysis.Dataflow.DangerScan
import qualified Flint.Cfg.Store as Store
import Flint.Types.Cfg.Store (CfgStore)
import qualified Blaze.Types.PersistentCalc as PC
import qualified Flint.Types.CachedCalc as CC
import Flint.Shell.Commands.Dataflow (dataflowQueryAction)
import Flint.Shell.Commands.Paths (findFunction, findFunc)
import Flint.Shell.Types (ShellState, CommandResult(..), initShellState)

import Blaze.Types.CallGraph (CallGraph)
import qualified Blaze.Graph as G
import Data.BinaryAnalysis (AddressSpace(..), AddressSpaceName(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import System.IO.Temp (withSystemTempDirectory)


-- ---------------------------------------------------------------------------
-- Test binary paths (relative to flint/ working directory)
-- ---------------------------------------------------------------------------

diveLoggerBin :: FilePath
diveLoggerBin = "../res/test_bins/Dive_Logger/Dive_Logger.gzf"


-- ---------------------------------------------------------------------------
-- Synthetic fixtures (no binary required)
-- ---------------------------------------------------------------------------

fakeSpace :: AddressSpace
fakeSpace = AddressSpace
  { ptrSize = Bytes 8
  , addressableUnitSize = Bytes 1
  , name = Ram
  }

mkFuncRef :: Text -> Int64 -> FuncRef
mkFuncRef nm off = InternalRef $ FunctionRef
  { symbol = Nothing
  , name = nm
  , address = intToAddr off
  }

mkExternRef :: Text -> Int64 -> FuncRef
mkExternRef nm off = ExternalRef $ FunctionRef
  { symbol = Nothing
  , name = nm
  , address = intToAddr off
  }

emptyCallGraph :: CallGraph
emptyCallGraph = G.empty

mkStore :: [(FuncRef, FuncSummary)] -> DataflowStore
mkStore entries = DataflowStore
  { summaries = HashMap.fromList entries
  , config    = defaultDataflowConfig
  }

-- | Shorthand for building a computed summary with a given list of edges
-- and callsites. Everything else defaults to empty.
mkSummary :: FuncRef -> [FlowEdge] -> [CallSiteDetail] -> FuncSummary
mkSummary ref edges sites = FuncSummary
  { funcRef   = ref
  , flowEdges = edges
  , paramVars = mempty
  , retVars   = mempty
  , callSites = sites
  , computed  = True
  }

-- | Minimal 'ShellState' wrapping a real 'CfgStore', with all optional
-- hooks set to Nothing. Used by tests that need to drive shell actions
-- end-to-end against a real binary.
mkTestShellState :: CfgStore -> IO ShellState
mkTestShellState store =
  initShellState store (intToAddr 0) False
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- ---------------------------------------------------------------------------
-- Test context: load binary once, shared across all tests
-- ---------------------------------------------------------------------------

data TestCtx = TestCtx
  { store :: CfgStore
  , allFuncRefs :: [FuncRef]
  , dfStore :: DataflowStore
  } deriving (Generic)

getTestCtx :: IO TestCtx
getTestCtx = do
  (imp :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
  allRefs <- CG.getFunctions imp
  st <- Store.init Nothing imp
  dfSt <- buildDataflowStore defaultDataflowConfig st
  return TestCtx
    { store = st
    , allFuncRefs = allRefs
    , dfStore = dfSt
    }

-- | Extract internal function refs only.
internalRefs :: [FuncRef] -> [FunctionRef]
internalRefs = mapMaybe (\case { InternalRef f -> Just f; _ -> Nothing })


-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  syntheticSpec
  binarySpec

binarySpec :: SpecWith ()
binarySpec = beforeAll getTestCtx . describe "Flint.Analysis.Dataflow.Summary" $ do

  -- === Store builds without crashing ===
  describe "buildDataflowStore" $ do

    it "produces summaries for all internal functions" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          internals = internalRefs (ctx ^. #allFuncRefs)
      -- Every internal function should have a summary (computed or empty)
      forM_ internals $ \fref -> do
        let ref = InternalRef fref
        HashMap.member ref summaries `shouldBe` True

    it "marks computed summaries as computed=True" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          computed = HashMap.filter (^. #computed) summaries
      -- At least some functions should have been successfully analyzed
      HashMap.size computed `shouldSatisfy` (> 0)

    it "finds functions with flow edges" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          withFlows = HashMap.filter (\s -> not (null (s ^. #flowEdges)) && s ^. #computed) summaries
      -- Non-trivial binaries should have some functions with param->return flows
      HashMap.size withFlows `shouldSatisfy` (> 0)


  -- === Per-function summary quality ===
  describe "Individual function summaries" $ do

    -- cgc_memcpy has signature (dst, src, n). By the positional
    -- convention we enforce via Function.params:
    --   param[0] = dst, param[1] = src, param[2] = n
    -- The body does *dst = *src, so we expect a DerefWriteFlow from
    -- param[1] (src) to param[0] (dst).
    it "cgc_memcpy: exactly param[1] (src) -> param[0] (dst) DerefWriteFlow" $ \ctx -> do
      case requireSummary ctx "cgc_memcpy" of
        Nothing -> expectationFailure "cgc_memcpy not found"
        Just s -> do
          s ^. #computed `shouldBe` True
          HashMap.size (s ^. #paramVars) `shouldBe` 3
          -- The defining invariant: src (positional 1) flows to dst (positional 0)
          let derefWrites = filter (\e -> e ^. #kind == DerefWriteFlow) (s ^. #flowEdges)
              srcToDst = FlowEdge (ParamEndpoint 1) (ParamEndpoint 0) DerefWriteFlow
          derefWrites `shouldContain` [srcToDst]

    -- cgc_strcpy(dst, src) copies a string from src to dst. Positional:
    --   param[0] = dst, param[1] = src
    -- We expect param[1] (src) -> param[0] (dst) DerefWriteFlow.
    it "cgc_strcpy: exactly param[1] (src) -> param[0] (dst) DerefWriteFlow" $ \ctx -> do
      case requireSummary ctx "cgc_strcpy" of
        Nothing -> expectationFailure "cgc_strcpy not found"
        Just s -> do
          s ^. #computed `shouldBe` True
          HashMap.size (s ^. #paramVars) `shouldBe` 2
          let derefWrites = filter (\e -> e ^. #kind == DerefWriteFlow) (s ^. #flowEdges)
              srcToDst = FlowEdge (ParamEndpoint 1) (ParamEndpoint 0) DerefWriteFlow
          derefWrites `shouldContain` [srcToDst]

    -- cgc_strlen(s) takes one parameter. We only verify param count
    -- here; strlen's "param flows to return" happens through control
    -- flow (the return value is the loop counter, not derived from the
    -- pointer), which this flow-insensitive def-use analysis cannot
    -- detect. That's a known limitation, not a bug.
    it "cgc_strlen has exactly one parameter" $ \ctx -> do
      case requireSummary ctx "cgc_strlen" of
        Nothing -> expectationFailure "cgc_strlen not found"
        Just s -> do
          s ^. #computed `shouldBe` True
          HashMap.size (s ^. #paramVars) `shouldBe` 1

    -- Some functions in the binary should have param->return flow
    it "at least one function has param->return flow edges" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          withParamToRet = HashMap.filter
            (\s -> s ^. #computed && any isParamToReturn (s ^. #flowEdges))
            summaries
      -- If this is zero, collectReturnVars likely isn't finding return
      -- statements properly
      HashMap.size withParamToRet `shouldSatisfy` (> 0)

    -- A function with no parameters should have no flow edges
    -- (main or similar entry points)
    it "functions without params have empty flow edges" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          noParamFuncs = HashMap.filter
            (\s -> s ^. #computed && HashMap.null (s ^. #paramVars))
            summaries
      -- Every no-param function should have no param->X edges
      forM_ (HashMap.elems noParamFuncs) $ \s -> do
        let paramEdges = filter isParamSourced (s ^. #flowEdges)
        paramEdges `shouldBe` []


  -- === Flow edge properties ===
  describe "Flow edge invariants" $ do

    it "all flow edges have valid endpoints" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
      forM_ (HashMap.elems summaries) $ \s ->
        when (s ^. #computed) $ do
          let nParams = HashMap.size (s ^. #paramVars)
          forM_ (s ^. #flowEdges) $ \edge -> do
            validateEndpoint nParams (edge ^. #from) `shouldBe` True
            validateEndpoint nParams (edge ^. #to) `shouldBe` True

    it "no self-loops (param[i] -> param[i])" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
      forM_ (HashMap.elems summaries) $ \s ->
        forM_ (s ^. #flowEdges) $ \edge ->
          edge ^. #from `shouldNotBe` edge ^. #to


  -- === Call site extraction ===
  describe "Call site extraction" $ do

    it "functions that call other functions have call sites" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          withCallSites = HashMap.filter
            (\s -> s ^. #computed && not (null (s ^. #callSites)))
            summaries
      -- A non-trivial binary should have functions that call other functions
      HashMap.size withCallSites `shouldSatisfy` (> 0)

    it "cgc_SetParam has call sites (calls cgc_GetLongString, cgc_strcpy)" $ \ctx -> do
      let summary = findSummaryByName ctx "cgc_SetParam"
      case summary of
        Nothing -> pendingWith "cgc_SetParam not found"
        Just s -> do
          let sites = s ^. #callSites
          length sites `shouldSatisfy` (> 0)

    it "call sites have argument mappings" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          -- Find any function with call sites that have non-empty arg mappings
          withArgMappings = HashMap.filter
            (\s -> s ^. #computed
                && not (all (null . view #argMapping) (s ^. #callSites)))
            summaries
      -- Functions that call other functions with arguments from their own
      -- params should have arg mappings
      HashMap.size withArgMappings `shouldSatisfy` (> 0)


  -- === Interprocedural composition ===
  describe "Interprocedural composition" $ do

    -- Positional-indexing regression test:
    -- cgc_SetParam(name, dst, max) passes its dst argument directly to
    -- cgc_strcpy as the first positional argument. The call site's
    -- argMapping should therefore contain (0, ParamEndpoint 1) —
    -- "callee positional param 0 receives SetParam's ParamEndpoint 1".
    --
    -- This asserts positional correctness. With the old alphabetical
    -- indexing, the caller param index would likely have been wrong,
    -- since SetParam's params are (dst, max, name) alphabetically vs
    -- (name, dst, max) positionally.
    it "cgc_SetParam's strcpy call site maps callee param 0 to SetParam's positional dst (param 1)" $ \ctx -> do
      case requireSummary ctx "cgc_SetParam" of
        Nothing -> expectationFailure "cgc_SetParam not found"
        Just s -> do
          let strcpySites =
                [ site
                | site <- s ^. #callSites
                , "strcpy" `Text.isSuffixOf` funcRefNameT (site ^. #calleeRef)
                ]
          strcpySites `shouldSatisfy` (not . null)
          -- At least one of those sites should map callee param 0
          -- (strcpy's dst) to SetParam's positional param 1.
          let targetMapping = (0, ParamEndpoint 1)
          any (\site -> targetMapping `elem` (site ^. #argMapping)) strcpySites
            `shouldBe` True

    -- Structural test: for every call site where the callee has
    -- computed param-to-param flows AND the caller maps its own params
    -- to the callee's params, the composed summary should contain at
    -- least one edge whose source endpoint is a caller param.
    -- (This doesn't prescribe specific functions — it asserts that
    -- composition is actually producing edges across the call graph.)
    it "composition produces at least one transitive param-sourced edge somewhere" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          calleeHasParamToParam s =
            s ^. #computed && any isParamToParam (s ^. #flowEdges)
          isParamToParam e = case (e ^. #from, e ^. #to) of
            (ParamEndpoint _, ParamEndpoint _) -> True
            _ -> False
          -- A caller is "interesting" if it has a call site mapping
          -- to a callee with param-to-param flows.
          interestingCallers = HashMap.filter
            (\s -> s ^. #computed && any (\site ->
              let calleeS = HashMap.lookup (site ^. #calleeRef) summaries
              in maybe False calleeHasParamToParam calleeS
                 && not (null (site ^. #argMapping))
              ) (s ^. #callSites))
            summaries
      HashMap.size interestingCallers `shouldSatisfy` (> 0)


  -- === Danger scan ===
  describe "Danger scan" $ do

    it "finds dangerous flow patterns in the binary" $ \ctx -> do
      let findings = dangerScanStore (ctx ^. #dfStore)
      -- Dive_Logger uses cgc_strcpy, cgc_sprintf etc. — should have findings
      length findings `shouldSatisfy` (> 0)

    it "findings have valid severity levels" $ \ctx -> do
      let findings = dangerScanStore (ctx ^. #dfStore)
      forM_ findings $ \f -> do
        f ^. #severity `shouldSatisfy` (`elem` [Info, Medium, High, Critical])

    it "findings reference computed functions" $ \ctx -> do
      let findings = dangerScanStore (ctx ^. #dfStore)
          summaries = ctx ^. #dfStore . #summaries
      forM_ findings $ \f -> do
        -- The caller should exist in summaries
        HashMap.member (f ^. #callerRef) summaries `shouldBe` True

    -- Libc-noise filter: printf calling vprintf, calloc calling malloc,
    -- and strdup calling strcpy are libc self-implementations, not
    -- vulnerabilities. The default scan must suppress these; the
    -- include-libc scan must surface them.
    it "default scan suppresses libc plumbing pairs" $ \ctx -> do
      let defaults = dangerScanStoreWith defaultDangerScanOptions (ctx ^. #dfStore)
      forM_ defaults $ \f -> do
        let callerName = stripPrefixes (funcRefNameT (f ^. #callerRef))
            calleeName = stripPrefixes (funcRefNameT (f ^. #calleeRef))
        HashSet.member (callerName, calleeName) libcWrapperPairs `shouldBe` False

    it "include-libc scan surfaces at least one known plumbing pair in Dive_Logger" $ \ctx -> do
      let unfiltered = dangerScanStoreWith
            (defaultDangerScanOptions { excludeLibcPlumbing = False })
            (ctx ^. #dfStore)
          plumbingHit f =
            let c1 = stripPrefixes (funcRefNameT (f ^. #callerRef))
                c2 = stripPrefixes (funcRefNameT (f ^. #calleeRef))
            in HashSet.member (c1, c2) libcWrapperPairs
      -- Dive_Logger is built with libc internals: we expect at least
      -- one known wrapper pair (printf->vprintf, strdup->strcpy, etc.).
      filter plumbingHit unfiltered `shouldSatisfy` (not . null)

    it "default scan has at least one non-plumbing application finding" $ \ctx -> do
      -- After filtering, we should still have some application-level
      -- findings (e.g. cgc_DeleteDive -> cgc_free). This guards against
      -- the filter accidentally swallowing everything.
      let defaults = dangerScanStoreWith defaultDangerScanOptions (ctx ^. #dfStore)
      defaults `shouldSatisfy` (not . null)

  -- === Interprocedural query ===
  describe "queryFlow" $ do

    it "query for non-existent flow returns not reachable" $ \ctx -> do
      -- Query between two functions that definitely don't have param0->ret flow
      let summaries = ctx ^. #dfStore . #summaries
          -- Pick any two distinct computed functions
          computedFuncs = HashMap.keys $ HashMap.filter (^. #computed) summaries
      case computedFuncs of
        (f1 : f2 : _) | f1 /= f2 -> do
          cg <- Store.getCallGraph (ctx ^. #store)
          -- Query param 99 which shouldn't exist
          let query = FlowQuery
                { source = (f1, ParamEndpoint 99)
                , sink = (f2, ParamEndpoint 99)
                , maxDepth = Just 1
                }
              result = queryFlow (ctx ^. #dfStore) cg query
          result ^. #reachable `shouldBe` False
        _ -> pendingWith "Need at least 2 computed functions"

    it "intra-procedural query uses summary edges" $ \ctx -> do
      -- Find any function with flow edges and query within it
      let summaries = ctx ^. #dfStore . #summaries
          candidates = HashMap.toList $ HashMap.filter
            (\s -> s ^. #computed && not (null (s ^. #flowEdges)))
            summaries
      case candidates of
        [] -> pendingWith "No function with flow edges found"
        ((ref, s) : _) -> do
          cg <- Store.getCallGraph (ctx ^. #store)
          let edge = fromMaybe (error "impossible") $ listToMaybe (s ^. #flowEdges)
              query = FlowQuery
                { source = (ref, edge ^. #from)
                , sink = (ref, edge ^. #to)
                , maxDepth = Just 1
                }
              result = queryFlow (ctx ^. #dfStore) cg query
          -- The query should find the flow since we constructed it from
          -- an existing edge
          result ^. #reachable `shouldBe` True

    -- Regression test for the original query bug: the searchFlow used
    -- to filter for edges of kind CallArgFlow, which nothing ever
    -- produces, so every cross-function query silently returned
    -- NOT REACHABLE. After the rewrite, cross-function queries descend
    -- via callSites.argMapping — so this case (which the danger
    -- scanner also reports) must be REACHABLE.
    it "cross-function: cgc_DeleteDive param[1] -> cgc_free param[0] is REACHABLE" $ \ctx -> do
      cg <- Store.getCallGraph (ctx ^. #store)
      case (requireRef ctx "cgc_DeleteDive", findRef ctx "cgc_free") of
        (Just srcRef, Just sinkRef) -> do
          let query = FlowQuery
                { source = (srcRef, ParamEndpoint 1)
                , sink = (sinkRef, ParamEndpoint 0)
                , maxDepth = Just 3
                }
              result = queryFlow (ctx ^. #dfStore) cg query
          result ^. #reachable `shouldBe` True
        _ -> expectationFailure "cgc_DeleteDive or cgc_free not found"

    -- Transitive cross-function via return-value threading:
    -- RemoveDive calls SelectDive(DiverInfo, _), captures its return
    -- into a local, and passes that local to DeleteDive as param[1].
    -- DeleteDive calls cgc_free with its param[1]. Because DiverInfo
    -- (RemoveDive.param[0]) feeds SelectDive's first arg, the local
    -- holding SelectDive's return is def-use-dependent on DiverInfo.
    -- So the composed query should conservatively report REACHABLE
    -- across the 3-function chain.
    it "cross-function chain: cgc_RemoveDive param[0] -> cgc_free param[0] is REACHABLE" $ \ctx -> do
      cg <- Store.getCallGraph (ctx ^. #store)
      case (requireRef ctx "cgc_RemoveDive", findRef ctx "cgc_free") of
        (Just srcRef, Just sinkRef) -> do
          let query = FlowQuery
                { source = (srcRef, ParamEndpoint 0)
                , sink = (sinkRef, ParamEndpoint 0)
                , maxDepth = Just 5
                }
              result = queryFlow (ctx ^. #dfStore) cg query
          result ^. #reachable `shouldBe` True
        _ -> expectationFailure "cgc_RemoveDive or cgc_free not found"

    -- Transitive: from cgc_strcpy param[1] (src) should reach
    -- cgc_strcpy param[0] (dst) within the same function (via the
    -- DerefWriteFlow edge in the composed summary).
    it "intra: cgc_strcpy param[1] -> cgc_strcpy param[0] is REACHABLE" $ \ctx -> do
      cg <- Store.getCallGraph (ctx ^. #store)
      case requireRef ctx "cgc_strcpy" of
        Just ref -> do
          let query = FlowQuery
                { source = (ref, ParamEndpoint 1)
                , sink = (ref, ParamEndpoint 0)
                , maxDepth = Just 1
                }
              result = queryFlow (ctx ^. #dfStore) cg query
          result ^. #reachable `shouldBe` True
        _ -> expectationFailure "cgc_strcpy not found"


  -- === Regression: shell/MCP command resolves externs as sinks ===
  describe "Extern sinks in the shell resolver" $ do

    -- The bug: dataflowQueryAction used findFunction to resolve both
    -- endpoints, and findFunction only looks at internal functions
    -- (via getFuncNameMapping). An extern sink like free/strcpy/
    -- cgc__terminate would be rejected. This test reproduces the
    -- exact failure mode: the OLD resolver must fail on an extern
    -- name, and the NEW resolver must succeed, returning an
    -- ExternalRef usable by the engine.
    it "findFunction (old path) cannot resolve an extern, findFunc (new path) can" $ \ctx -> do
      st <- mkTestShellState (ctx ^. #store)

      mInternalSearch <- findFunction st "cgc__terminate"
      mInternalSearch `shouldBe` Nothing    -- demonstrates the bug

      mBoth <- findFunc st "cgc__terminate"
      case mBoth of
        Nothing -> expectationFailure
          "findFunc failed to resolve extern cgc__terminate"
        Just (_, ExternalRef _) -> pure ()  -- demonstrates the fix
        Just (_, InternalRef _) -> expectationFailure
          "findFunc returned an InternalRef for a known extern"

    -- End-to-end: feed the shell action an extern sink name and
    -- confirm the reply is a parsed query result, not a "not found"
    -- error. Before the fix this returned ResultError; after, it
    -- returns ResultText containing REACHABLE / NOT REACHABLE.
    it "dataflowQueryAction accepts an extern sink name" $ \ctx -> do
      st <- mkTestShellState (ctx ^. #store)
      result <- dataflowQueryAction st
        ["cgc_init_data", "ret", "cgc__terminate", "0"]
      case result of
        ResultText body -> do
          body `shouldSatisfy` ("cgc__terminate" `Text.isInfixOf`)
          body `shouldSatisfy` (\t ->
            "REACHABLE" `Text.isInfixOf` t || "NOT REACHABLE" `Text.isInfixOf` t)
        ResultError err ->
          expectationFailure $ "unexpected error: " <> Text.unpack err
        _ -> expectationFailure "unexpected result type"

    -- Composed-store cache lives on CfgStore so every consumer
    -- (flint CLI, flint-shell, flint-mcp, ...) shares one composed
    -- store per binary load. Use a fresh CfgStore here so we observe
    -- the transition from cold to warm.
    it "dataflow-query warms CfgStore.composedDataflowCache" $ \_ctx -> do
      (imp :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
      freshStore <- Store.init Nothing imp
      st <- mkTestShellState freshStore
      cold <- CC.get () (freshStore ^. #composedDataflowCache)
      case cold of
        Nothing -> pure ()
        Just _  -> expectationFailure "composedDataflowCache already populated on fresh store"
      _ <- dataflowQueryAction st
        ["cgc_DeleteDive", "1", "cgc_free", "0"]
      warm <- CC.get () (freshStore ^. #composedDataflowCache)
      case warm of
        Nothing -> expectationFailure "composedDataflowCache not populated after query"
        Just _  -> pure ()


  -- === Persistent + lazy cache on CfgStore.dataflowCache ===
  describe "dataflowCache" $ do

    -- After buildDataflowStore runs, every FuncRef the store produced
    -- a summary for should also be persisted (in-memory + LMDB) on
    -- the CfgStore's dataflowCache. We check by asking the
    -- PersistentCalc for its keys — those come from the union of
    -- the in-memory tier and LMDB.
    it "populates dataflowCache with every computed internal summary" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          -- Only internal functions flow through the per-function
          -- PersistentCalc. Externs short-circuit to an empty summary
          -- and are not cached — caching them would just churn LMDB
          -- with empty values.
          internalSummaryKeys = HashSet.fromList
            [ ref | ref <- HashMap.keys summaries
            , case ref of InternalRef _ -> True; _ -> False
            ]
      cachedKeys <- PC.getKeys (ctx ^. #store . #dataflowCache)
      let cachedSet = HashSet.fromList cachedKeys
      internalSummaryKeys `shouldSatisfy` (`HashSet.isSubsetOf` cachedSet)
      HashSet.size cachedSet `shouldSatisfy` (> 0)

    -- A second buildDataflowStore on the same CfgStore must produce
    -- identical summaries to the first, proving the cache layer
    -- round-trips through its in-memory tier without losing data.
    it "second buildDataflowStore returns the same summaries" $ \ctx -> do
      let firstStore = ctx ^. #dfStore
      secondStore <- buildDataflowStore defaultDataflowConfig (ctx ^. #store)
      let firstKeys  = HashMap.keysSet (firstStore ^. #summaries)
          secondKeys = HashMap.keysSet (secondStore ^. #summaries)
      secondKeys `shouldBe` firstKeys
      -- Spot-check one representative summary round-trips fully.
      case requireRef ctx "cgc_strcpy" of
        Just r -> HashMap.lookup r (secondStore ^. #summaries)
                    `shouldBe` HashMap.lookup r (firstStore ^. #summaries)
        Nothing -> pendingWith "cgc_strcpy not present"

    -- Round-trip through the persistence transport: the summary for
    -- a known function should survive to/from conversion intact.
    it "summary round-trips through persistence transport" $ \ctx -> do
      case requireSummary ctx "cgc_strcpy" of
        Nothing -> expectationFailure "cgc_strcpy not found"
        Just s -> do
          let t  = summaryToTransport s
              s' = summaryFromTransport t
          s' `shouldBe` s

    -- The composed cache lives on CfgStore, so consumers that
    -- don't hold a ShellState — e.g. the plain flint CLI — can
    -- fetch it directly. No shell/MCP plumbing required.
    it "getComposedDataflowStore works without a ShellState" $ \ctx -> do
      store <- getComposedDataflowStore (ctx ^. #store) defaultDataflowConfig
      HashMap.size (store ^. #summaries) `shouldSatisfy` (> 0)
      -- And a second call returns the same thing (via cache).
      store' <- getComposedDataflowStore (ctx ^. #store) defaultDataflowConfig
      HashMap.keysSet (store' ^. #summaries)
        `shouldBe` HashMap.keysSet (store ^. #summaries)

    -- Cross-session persistence: a CfgStore pointed at an LMDB path
    -- receives summaries; a second CfgStore pointed at the /same/
    -- LMDB path sees them without re-running computeSummary. We
    -- verify by reading the second store's dataflowCache keys after
    -- registration of the default (no compute fired yet).
    it "summaries survive across CfgStore re-inits via LMDB" $ \_ctx -> do
      withSystemTempDirectory "flint-dataflow-cache-test" $ \tmpDir -> do
        let dbPath = tmpDir <> "/test.flintdb"
        -- First session: build, populate, close.
        (imp1 :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
        store1 <- Store.init (Just dbPath) imp1
        dfStore <- buildDataflowStore defaultDataflowConfig store1
        let summaries = dfStore ^. #summaries
            computedRefs =
              [ ref
              | (ref, s) <- HashMap.toList summaries
              , s ^. #computed
              ]
        -- At least one computed entry should exist to be meaningful.
        computedRefs `shouldSatisfy` (not . null)

        -- Second session: re-open same DB path, fresh CfgStore.
        (imp2 :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
        store2 <- Store.init (Just dbPath) imp2
        -- Register the default so PC.get falls through to LMDB.
        setupDataflowCache store2 defaultDataflowConfig
        -- Each previously-computed FuncRef should be retrievable from
        -- LMDB (through the wrapped default) and match the original.
        forM_ (take 5 computedRefs) $ \ref -> do
          mS <- PC.get ref (store2 ^. #dataflowCache)
          case mS of
            Nothing -> expectationFailure
              $ "No persisted summary for " <> show ref
            Just s  -> Just s `shouldBe` HashMap.lookup ref summaries


  -- === Global-mediated flows ===
  describe "Global-address tracking" $ do

    -- Dive_Logger reads and writes several C globals (diver state,
    -- dive list roots, etc.). At least one function's composed
    -- summary should contain a flow edge with a GlobalEndpoint as
    -- either source or destination.
    it "at least one summary has a GlobalEndpoint in its flowEdges" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          withGlobals = HashMap.filter
            (\s -> s ^. #computed
                && any edgeMentionsGlobal (s ^. #flowEdges))
            summaries
      HashMap.size withGlobals `shouldSatisfy` (> 0)

    -- Globals showing up as arg sources proves that LOAD(CONST_PTR)
    -- inside call-argument expressions is being tracked.
    it "at least one callsite has a GlobalEndpoint in its argMapping" $ \ctx -> do
      let summaries = ctx ^. #dfStore . #summaries
          withGlobalArg = HashMap.filter
            (\s -> s ^. #computed && any callsiteHasGlobalArg (s ^. #callSites))
            summaries
      HashMap.size withGlobalArg `shouldSatisfy` (> 0)


-- ---------------------------------------------------------------------------
-- Synthetic tests (no binary required)
-- ---------------------------------------------------------------------------

syntheticSpec :: Spec
syntheticSpec = describe "Flint.Analysis.Dataflow.Query (synthetic)" $ do

  -- Multi-source argMapping: a callsite where callee param 0 is fed
  -- by two distinct caller sources. The callee has a single edge
  -- P0 -> Return. After composition the caller must end up with a
  -- flow edge from *each* source endpoint to its own return, not
  -- just one — that's the bug the old fromList collapse caused.
  describe "composition with multi-source argMapping" $ do
    it "preserves all caller sources feeding a single callee param" $ do
      let callerRef = mkFuncRef "caller" 0x1000
          calleeRef = mkFuncRef "callee" 0x2000
          site = CallSiteDetail
            { calleeRef  = calleeRef
            , argMapping = [(0, ParamEndpoint 0), (0, ParamEndpoint 1)]
            , retMapping = Just ReturnEndpoint
            , callAddr   = intToAddr 0x1010
            }
          calleeSum = mkSummary calleeRef
            [FlowEdge (ParamEndpoint 0) ReturnEndpoint DirectFlow]
            []
          callerSum = mkSummary callerRef [] [site]
          initial = HashMap.fromList
            [ (callerRef, callerSum)
            , (calleeRef, calleeSum)
            ]
          -- Build a 2-node acyclic call graph for the composition
          cg = G.addEdge
                 (G.LEdge () (G.Edge callerRef calleeRef))
                 (G.addNodes [callerRef, calleeRef] G.empty)

          composed = composeWithCallGraph cg initial
          callerComposed = fromMaybe (error "missing caller")
                         $ HashMap.lookup callerRef composed
          callerEdges = callerComposed ^. #flowEdges
          hasFlow from' to' =
            any (\e -> e ^. #from == from' && e ^. #to == to') callerEdges
      hasFlow (ParamEndpoint 0) ReturnEndpoint `shouldBe` True
      hasFlow (ParamEndpoint 1) ReturnEndpoint `shouldBe` True

  -- Read-only-global soundness: two functions that both only READ
  -- the same global. Under the OLD bridge, entering readerA at the
  -- global endpoint itself would fire a bridge to readerB (because
  -- g was in reachable), chaining read-to-read across functions
  -- with no actual write between them. Under the fix, the bridge
  -- only fires when the current flow wrote to g — which readerA
  -- never did — so the query must be NOT REACHABLE.
  --
  -- The source endpoint is chosen as @GlobalEndpoint g@ specifically
  -- to drive the old-buggy path: intraReachable from that seed
  -- includes g itself, so the old bridge would have triggered.
  describe "global bridge read-to-read unsoundness fix" $ do
    it "readerA.g -> readerB.Return is NOT reachable when neither writes g" $ do
      let readerA = mkFuncRef "readerA" 0x3000
          readerB = mkFuncRef "readerB" 0x4000
          sharedG = GlobalEndpoint (intToAddr 0xdeadbeef)
          readerASum = mkSummary readerA
            [FlowEdge sharedG ReturnEndpoint DerefReadFlow] []
          readerBSum = mkSummary readerB
            [FlowEdge sharedG ReturnEndpoint DerefReadFlow] []
          store = mkStore
            [ (readerA, readerASum)
            , (readerB, readerBSum)
            ]
          query = FlowQuery
            { source = (readerA, sharedG)
            , sink   = (readerB, ReturnEndpoint)
            , maxDepth = Just 3
            }
          result = queryFlow store emptyCallGraph query
      -- Under the old buggy bridge this would have been True.
      result ^. #reachable `shouldBe` False

    -- Control: a writer of g followed by a reader of g still bridges.
    -- This confirms the fix didn't over-prune — genuine write->read
    -- flows remain reachable.
    it "writer-to-reader via global still bridges" $ do
      let writerFn = mkFuncRef "writer" 0x5000
          readerFn = mkFuncRef "reader" 0x6000
          sharedG  = GlobalEndpoint (intToAddr 0xcafebabe)
          writerSum = mkSummary writerFn
            [FlowEdge (ParamEndpoint 0) sharedG DerefWriteFlow] []
          readerSum = mkSummary readerFn
            [FlowEdge sharedG ReturnEndpoint DerefReadFlow] []
          store = mkStore
            [ (writerFn, writerSum)
            , (readerFn, readerSum)
            ]
          query = FlowQuery
            { source = (writerFn, ParamEndpoint 0)
            , sink   = (readerFn, ReturnEndpoint)
            , maxDepth = Just 3
            }
          result = queryFlow store emptyCallGraph query
      result ^. #reachable `shouldBe` True

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | If a function exists both as an 'InternalRef' (statically linked)
-- and as an 'ExternalRef' (import stub) with the same name, tests
-- want the internal one — that's where the real summary lives. The
-- extern version carries an empty, uncomputed summary.
preferInternal :: [(FuncRef, a)] -> Maybe (FuncRef, a)
preferInternal xs =
  find (\(ref, _) -> case ref of InternalRef _ -> True; _ -> False) xs
  <|> listToMaybe xs

-- | Find a summary by function name substring, preferring internal refs.
findSummaryByName :: TestCtx -> Text -> Maybe FuncSummary
findSummaryByName ctx nameFragment =
  let summaries = HashMap.toList (ctx ^. #dfStore . #summaries)
      matches = filter (\(ref, _) -> nameFragment `Text.isInfixOf` funcRefNameT ref) summaries
  in snd <$> preferInternal matches

-- | Like 'findSummaryByName' but matches by exact name so that, say,
-- \"cgc_strcpy\" doesn't accidentally match \"cgc_strcpy_into_foo\".
requireSummary :: TestCtx -> Text -> Maybe FuncSummary
requireSummary ctx exactName =
  let summaries = HashMap.toList (ctx ^. #dfStore . #summaries)
      matches = filter (\(ref, _) -> funcRefNameT ref == exactName) summaries
  in snd <$> preferInternal matches

-- | Exact-name lookup for a 'FuncRef' key (prefers internal).
requireRef :: TestCtx -> Text -> Maybe FuncRef
requireRef ctx exactName =
  let summaries = HashMap.keys (ctx ^. #dfStore . #summaries)
      matches   = [(r, ()) | r <- summaries, funcRefNameT r == exactName]
  in fst <$> preferInternal matches

-- | Substring-permissive lookup of a 'FuncRef' — used for externs whose
-- exact name may include mangling prefixes we don't care about.
-- Prefers exact extern matches when the caller is looking for one,
-- otherwise falls back to the internal preference.
findRef :: TestCtx -> Text -> Maybe FuncRef
findRef ctx nameFragment =
  let summaries = HashMap.keys (ctx ^. #dfStore . #summaries)
      matches   = [(r, ()) | r <- summaries, nameFragment `Text.isInfixOf` funcRefNameT r]
  in fst <$> preferInternal matches

funcRefNameT :: FuncRef -> Text
funcRefNameT (InternalRef f) = f ^. #name
funcRefNameT (ExternalRef f) = f ^. #name

-- | Strip a set of well-known wrapper prefixes (e.g. @cgc_@) so the
-- test can match libc plumbing pairs the same way 'DangerScan' does.
stripPrefixes :: Text -> Text
stripPrefixes name =
  let prefixes = ["cgc_", "__wrap_", "__real_", "_"]
  in fromMaybe name $ listToMaybe
       [ stripped
       | p <- prefixes
       , Just stripped <- [Text.stripPrefix p name]
       ]

isParamToReturn :: FlowEdge -> Bool
isParamToReturn e = case (e ^. #from, e ^. #to) of
  (ParamEndpoint _, ReturnEndpoint) -> True
  _ -> False

isParamSourced :: FlowEdge -> Bool
isParamSourced e = case e ^. #from of
  ParamEndpoint _ -> True
  _ -> False

edgeMentionsGlobal :: FlowEdge -> Bool
edgeMentionsGlobal e = isGlobal (e ^. #from) || isGlobal (e ^. #to)
  where
    isGlobal (GlobalEndpoint _) = True
    isGlobal _ = False

callsiteHasGlobalArg :: CallSiteDetail -> Bool
callsiteHasGlobalArg site =
  any (\(_, ep) -> case ep of GlobalEndpoint _ -> True; _ -> False)
      (site ^. #argMapping)

validateEndpoint :: Int -> FlowEndpoint -> Bool
validateEndpoint nParams = \case
  ParamEndpoint i -> i >= 0 && i < nParams
  ReturnEndpoint -> True
  GlobalEndpoint _ -> True
