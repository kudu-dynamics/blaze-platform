{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.Matcher.Primitives.LibrarySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec as PrimSpec
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.StdLib as StdLibPrims
import qualified Flint.Cfg.Store as Store
import Flint.Query
import qualified Flint.Types.CachedMap as CM

import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Types.Function (_name)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Primitives.Library" $ do
  context "use-after-free" $ do
    it "should detect function that returns freed pointer" $ do
      (imp :: GhidraImporter) <- unsafeFromRight <$> openBinary "res/test_bins/juliet/CWE416/CWE416_Use_After_Free__return_freed_ptr_01-bad"
      store <- Store.init Nothing imp
      let stdLibPrims = StdLibPrims.allStdLibPrims
          prims = [ PrimLib.returnsFreedPointerPrim
                  , PrimLib.returnsFreedPointerPrim
                  ]
          action = do
            onionFlow 20 False 3 1.0 store stdLibPrims prims HashSet.empty HashMap.empty True
            m <- fmap asOldCallableWMIsMap . CM.getSnapshot $ store ^. #callablePrims
            -- info . cs . pshow $ m
            -- pprint m
            -- info "Billy Jane"
            -- putText "-------- DOUGLAS VAN COOPER ------------"
            putText "alive"
            
            return $ do
              s <- HashMap.lookup PrimSpec.returnsFreedPointerSpec m
              return $ HashSet.map (view $ #func . _name) s
          expected = Just $ HashSet.fromList
            [ "helperBad" ]

      action `shouldReturn` expected
