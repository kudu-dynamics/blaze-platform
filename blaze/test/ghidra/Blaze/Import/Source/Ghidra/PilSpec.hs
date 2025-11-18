module Blaze.Import.Source.Ghidra.PilSpec where

import Blaze.Prelude hiding (const)

import Blaze.Import.CallGraph (CallGraphImporter (getFunction, getFunctions))
import Blaze.Import.Pil (PilImporter (getFuncStatements))
import qualified Blaze.Import.Source.Ghidra as G

import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

onionTestBin :: FilePath
onionTestBin = "../flint/res/test_bins/onion_test/onion_test"


spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Pil" $ do
  -- return ()
  context "Address widths" $ do
    -- This test is to address a problem with Address widths found in onion_test
    -- where the Store width of a CONST_PTR was 4 bytes and a LOAD of the same ptr
    -- was 8 bytes.
    let action = do
          imp <- G.getImporter onionTestBin
          funcs <- mapMaybe (^? #_Internal) <$> getFunctions imp
          let intOverflowFunc = fromJust . headMay . filter ((== "int_overflow_func") . view #name) $ funcs
          (stmt0:stmt1:_) <- getFuncStatements imp intOverflowFunc 0
          let addr0 = stmt0 ^?! #statement . #_Def . #value . #op . #_ADD . #left . #op . #_LOAD . #src
              addr1 = stmt1 ^?! #statement . #_Store . #addr
          return (addr0 ^. #size, addr1 ^. #size)
    it "should have 8 byte width for address in LOAD and 8 byte width in dest of STORE" $ do
      action `shouldReturn` (8, 8)

  context "Import Function" $ do
    importer <- runIO $ G.getImporter diveBin
    mFunc <- runIO $ getFunction importer (intToAddr 0x804d670) -- cgc_SetParam function
    it "should import a function by address" $ do
      mFunc ^? _Just . #_Internal . #name `shouldBe` Just "cgc_SetParam"

    let func = mFunc ^?! _Just . #_Internal
    stmts <- runIO $ getFuncStatements importer func 0

    -- stmt0: zf_1#1@10 = eax_4#1@10 == 0x0
    -- stmt1: var_14#4@10 = var_14#2@10
    -- stmt2: unique_1d00#1@10 = esp_4#1@10 + offset 0xffffffac
    -- stmt3: unique_1d00#2@10 = esp_4#1@10 + offset 0xffffffac
    let stmt00 = unsafeHead stmts
    let stmt0 = stmts !! 5
    let stmt1 = stmts !! 11
    let stmt2 = stmts !! 14
    let stmt3 = stmts !! 20

    
    let _pv0 = stmt0 ^? #statement . #_Def . #value . #op . #_CMP_E . #left . #op . #_VAR . #src
    let pv1_l = stmt1 ^? #statement . #_Def . #var
    let pv1_r = stmt1 ^? #statement . #_Def . #value . #op . #_VAR . #src
    let pv2 = stmt2 ^? #statement . #_Def . #var
    -- let pv2_val_l = stmt2 ^? #statement . #_Def . #value . #op . #_FIELD_ADDR . #baseAddr . #op . #_VAR . #src
    let pv3 = stmt3 ^? #statement . #_Def . #var

    let pv_param = stmt00 ^? #statement . #_Def . #value . #op . #_VAR . #src

-- TODO: make a test binary so these tests can be used
    -- it "should use register names for symbols" $ do
      -- pv2_val_l ^? _Just . #symbol `shouldBe` Just "esp_4"
      -- pv2_val_l ^? _Just . #version `shouldBe` Just (Just 1)

    it "should have separate number labels for assignments to the same stack var" $ do
      pv1_l ^? _Just . #symbol `shouldBe` Just "temp"
      pv1_l ^? _Just . #version `shouldBe` Just (Just 4)
      pv1_r ^? _Just . #symbol `shouldBe` Just "temp"
      pv1_r ^? _Just . #version `shouldBe` Just (Just 2)

    it "should have separate number labels for assignments to the same unique var" $ do
      pv2 ^? _Just . #symbol `shouldBe` Just "unique_3280"
      pv2 ^? _Just . #version `shouldBe` Just (Just 1)
      pv3 ^? _Just . #symbol `shouldBe` Just "unique_3280"
      pv3 ^? _Just . #version `shouldBe` Just (Just 2)
    
    it "should not include version #1 on param names" $ do
      pv_param ^? _Just . #symbol `shouldBe` Just "len"

    it "param should be labelled as a param" $ do
      pv_param ^? _Just . #isParam `shouldBe` Just True
