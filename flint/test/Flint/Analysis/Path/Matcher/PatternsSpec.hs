module Flint.Analysis.Path.Matcher.PatternsSpec where

import Flint.Prelude

import qualified Flint.Analysis.Path.Matcher as M
import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import qualified Flint.Cfg.Path as CfgPath
import Flint.Types.Cfg.Store (CfgStore)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (BugMatch)
import qualified Flint.Types.Query as Q

import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Pil.Solver (solveStmtsWithZ3)
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Pil.Solver as Solver

import qualified Data.HashMap.Strict as HashMap

import Test.Hspec


dirtyBenchmark :: FilePath
dirtyBenchmark = "res/test_bins/dirty_benchmark/dirty_benchmark"

data TestCtx = TestCtx
  { bv :: GhidraImporter
  , store :: CfgStore
  , allFuncs :: [Function]
  , getFunc :: Text -> Function
  , funcHasPattern :: Bool -> Function -> BugMatch -> IO Bool
  } deriving (Generic)

getTestCtx :: IO TestCtx
getTestCtx = do
  (bv :: GhidraImporter) <- unsafeFromRight <$> openBinary dirtyBenchmark
  store <- CfgStore.init Nothing bv
  allFuncs <- CG.getFunctions bv
  let funcMapping = HashMap.fromList . fmap (\func -> (func ^. #name, func)) $ allFuncs
      getFunc = fromJust . flip HashMap.lookup funcMapping

      funcHasPattern :: Bool -> Function -> BugMatch -> IO Bool
      funcHasPattern actuallySolve func bms = do
        paths <- CfgPath.samplesFromQuery store func Q.QueryAllPaths
        let pathPreps = M.mkPathPrep [] <$> paths
        -- mapM_ (prettyStmts' . view #stmts) pathPreps
        matcherResults <- traverse (M.match solver (bms ^. #pathPattern)) pathPreps
        let onlyMatches = filter (is #_Match . snd) matcherResults
        return . isJust . headMay $ onlyMatches
          where
            solver = if actuallySolve
              then const . return $ Solver.Sat HashMap.empty
              else solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
  

  return $ TestCtx
    { bv = bv
    , store = store
    , allFuncs = allFuncs
    , getFunc = getFunc
    , funcHasPattern = funcHasPattern
    }

spec :: Spec
spec = beforeAll getTestCtx $ describe "Flint.Analysis.Path.Matcher.Patterns" $ do
  context "Dirty Benchmark 1" $ do

    -- it "should find a strcpy buffer overflow" $ \tctx -> do
    --   let func = (tctx ^. #getFunc) "buffer_overflow"
    --       pat = Pat.bufferOverflow
    --       action = (tctx ^. #funcHasPattern) False func pat
    --       expected = True     
    --   action `shouldReturn` expected

    -- it "should find a format string vulnerability" $ \tctx -> do
    --   let func = (tctx ^. #getFunc) "format_string_vulnerability"
    --       pat = Pat.formatStringVulnerability
    --       action = (tctx ^. #funcHasPattern) False func pat
    --       expected = True

    --   action `shouldReturn` expected

    it "should find a use-after-free" $ \tctx -> do
      let func = (tctx ^. #getFunc) "use_after_free"
          pat = Pat.useAfterFree
          action = (tctx ^. #funcHasPattern) False func pat
          expected = True

      action `shouldReturn` expected

    -- it "should find a null_pointer_dereference" $ do
    --   let func = getFunc "null_pointer_dereference"
    --       pat = Pat.nullPointerDereference
    --       action = funcHasPattern False func pat
    --       expected = True

    --   action `shouldReturn` expected

    -- it "should find a stack_based_buffer_overflow" $ \tctx -> do
    --   let func = (tctx ^. #getFunc) "stack_based_buffer_overflow"
    --       pat = Pat.stackBasedBufferOverflow
    --       action = (tctx ^. #funcHasPattern) False func pat
    --       expected = True

    --   action `shouldReturn` expected

