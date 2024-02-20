module Binja.TypeLibrarySpec ( spec ) where

import Binja.Prelude

import qualified Binja.Core as BN
import Binja.Core (BNBinaryView)
import Test.Hspec
import qualified Binja.TypeLibrary as BT
import Prelude (error)
import Data.Text (unpack)

typeLibTestBin :: FilePath
typeLibTestBin = "res/test_bins/TypeLibTest/TypeLibTest"

getBv :: FilePath -> IO BNBinaryView
getBv fp = do
  bv <- either (error . unpack) id <$> BN.getBinaryView fp
  liftIO $ BN.updateAnalysisAndWait bv
  return bv

getTypeLibTest :: IO BNBinaryView
getTypeLibTest = getBv typeLibTestBin

spec :: Spec
spec = describe "Binja.TypeLibrary" $ do

  context "Type Lib Test" $ do
    bv <- runIO getTypeLibTest

    tlibs <- runIO $ BN.getBinaryViewTypeLibraries bv

    it "should find a type library for TypeLibTest bin" $ do
      length tlibs `shouldBe` 1

    xs <- runIO $ BT.getFunctionTypes (head tlibs)

    it "should load some type lib FunctionTypes" $ do
      null xs `shouldBe` False
    

