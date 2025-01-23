module Flint.Analysis.Path.Matcher.PatternsSpec where

import Flint.Prelude

import qualified Flint.Analysis.Path.Matcher as M
import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import qualified Flint.Cfg.Path as CfgPath
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (BugMatch)
import qualified Flint.Types.Query as Q

import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Binary (BinaryImporter(openBinary))
import qualified Blaze.Import.Pil as PilImporter
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Pil.Solver (solveStmtsWithZ3)
import Blaze.Pretty (prettyStmts')
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Pil.Solver as Solver

import qualified Data.HashMap.Strict as HashMap

import Test.Hspec


dirtyBenchmark :: FilePath
dirtyBenchmark = "res/test_bins/dirty_benchmark/dirty_benchmark"

spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Patterns" $ do
  context "Dirty Benchmark 1" $ do
    (bv :: GhidraImporter) <- unsafeFromRight <$> runIO (openBinary dirtyBenchmark)
    store <- runIO $ CfgStore.init Nothing bv

    allFuncs <- runIO $ CG.getFunctions bv
    let funcMapping = HashMap.fromList . fmap (\func -> (func ^. #name, func)) $ allFuncs
        getFunc = fromJust . flip HashMap.lookup funcMapping

        funcHasPattern :: Bool -> Function -> BugMatch -> IO Bool
        funcHasPattern actuallySolve func bms = do
          paths <- CfgPath.samplesFromQuery store func Q.QueryAllPaths
          matcherResults <- traverse (M.match solver (bms ^. #pathPattern) . M.mkPathPrep []) paths
          let onlyMatches = filter (is #_Match . snd) matcherResults
          return . isJust . headMay $ onlyMatches
          where
            solver = if actuallySolve
              then const . return $ Solver.Sat HashMap.empty
              else solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors

    runIO $ PilImporter.getFuncStatements bv (getFunc "buffer_overflow") 0
      >>= prettyStmts'

    runIO . pprint $ getFunc "buffer_overflow" ^. #params

    it "should find a strcpy buffer overflow" $ do
      let func = getFunc "buffer_overflow"
          pat = Pat.bufferOverflow
          action = funcHasPattern False func pat
          expected = True

      
      action `shouldReturn` expected

    it "should find a format string vulnerability" $ do
      let func = getFunc "format_string_vulnerability"
          pat = Pat.formatStringVulnerability
          action = funcHasPattern False func pat
          expected = True

      action `shouldReturn` expected

    it "should find a use-after-free" $ do
      let func = getFunc "use_after_free"
          pat = Pat.useAfterFree
          action = funcHasPattern False func pat
          expected = True

      action `shouldReturn` expected

    -- it "should find a null_pointer_dereference" $ do
    --   let func = getFunc "null_pointer_dereference"
    --       pat = Pat.nullPointerDereference
    --       action = funcHasPattern False func pat
    --       expected = True

    --   action `shouldReturn` expected

    it "should find a stack_based_buffer_overflow" $ do
      let func = getFunc "stack_based_buffer_overflow"
          pat = Pat.stackBasedBufferOverflow
          action = funcHasPattern False func pat
          expected = True

      action `shouldReturn` expected

