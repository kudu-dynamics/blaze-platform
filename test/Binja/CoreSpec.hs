{- HLINT ignore "Redundant do" -}

module Binja.CoreSpec (spec) where

import Binja.Prelude hiding (hash)

import qualified Binja.Analysis as An
import qualified Binja.BasicBlock as BB
import Binja.C.Enums (
  BNAnalysisSkipReason (NoSkipReason),
  BNFunctionAnalysisSkipOverride (AlwaysSkipFunctionAnalysis, DefaultFunctionAnalysisSkip, NeverSkipFunctionAnalysis),
 )
import qualified Binja.Core as BN
import qualified Binja.Function as Func
import qualified Data.Map as Map
import Crypto.Hash (MD5, hash, Digest)
import Test.Hspec
import Prelude (error)
import Data.Text (unpack)

diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

diveFuncSize :: Map Text Int
diveFuncSize = Map.fromList [("__do_global_dtors_aux", 31), ("__gmon_start__", 6), ("__libc_csu_fini", 2), ("__libc_csu_init", 93), ("__libc_start_main", 6), ("__x86.get_pc_thunk.bx", 4), ("_fini", 20), ("_init", 35), ("_start", 33), ("cgc_AddDive", 269), ("cgc_ChangeDive", 350), ("cgc_ChangeDiverInfo", 433), ("cgc_DeleteDive", 270), ("cgc_DiverStatistics", 282), ("cgc_DownloadDiveData", 574), ("cgc_EditDive", 85), ("cgc_EpochToDate", 501), ("cgc_GetChar", 94), ("cgc_GetInt", 141), ("cgc_GetLongString", 120), ("cgc_GetPositiveInt", 70), ("cgc_GetShortString", 120), ("cgc_GetUInt32", 127), ("cgc_LogNewDive", 75), ("cgc_MainMenu", 376), ("cgc_PrintDiveEntry", 363), ("cgc_PrintDiveLogs", 71), ("cgc_PrintDiverInfo", 286), ("cgc_RemoveDive", 72), ("cgc_SanitizeDate", 640), ("cgc_SanitizeDiveEntry", 45), ("cgc_SanitizeTime", 431), ("cgc_SelectDive", 442), ("cgc_SetInt", 133), ("cgc_SetParam", 174), ("cgc__terminate", 6), ("cgc_add_free_list", 186), ("cgc_allocate", 6), ("cgc_atof", 543), ("cgc_atoi", 476), ("cgc_bzero", 114), ("cgc_calloc", 86), ("cgc_deallocate", 6), ("cgc_destroy_data", 28), ("cgc_float_to_str", 817), ("cgc_floor", 184), ("cgc_free", 436), ("cgc_init_data", 74), ("cgc_int_to_hex", 201), ("cgc_int_to_str", 222), ("cgc_isalnum", 49), ("cgc_isalpha", 49), ("cgc_isdigit", 59), ("cgc_isinf", 62), ("cgc_islower", 59), ("cgc_isnan", 38), ("cgc_isspace", 99), ("cgc_isupper", 59), ("cgc_itoa", 227), ("cgc_log10", 6), ("cgc_malloc", 656), ("cgc_memcpy", 83), ("cgc_memset", 89), ("cgc_pow", 6), ("cgc_printf", 53), ("cgc_putc", 93), ("cgc_puts", 125), ("cgc_receive", 6), ("cgc_receive_until", 198), ("cgc_receive_until_flush", 285), ("cgc_rint", 6), ("cgc_round", 177), ("cgc_round_away_from_zero", 160), ("cgc_sprintf", 61), ("cgc_strcat", 145), ("cgc_strchr", 114), ("cgc_strcmp", 118), ("cgc_strcpy", 99), ("cgc_strdup", 167), ("cgc_strlen", 82), ("cgc_strncat", 192), ("cgc_strncpy", 166), ("cgc_strtok", 675), ("cgc_transmit", 6), ("cgc_vprintf", 2361), ("cgc_vsprintf", 2046), ("cgc_write", 102), ("deregister_tm_clones", 41), ("frame_dummy", 6), ("main", 65), ("memcpy", 6), ("memset", 6), ("register_tm_clones", 54), ("sub_8049496", 22)]

diveBlockCount :: Map Text Int
diveBlockCount = Map.fromList [("__do_global_dtors_aux", 3), ("__gmon_start__", 1), ("__libc_csu_fini", 1), ("__libc_csu_init", 4), ("__libc_start_main", 1), ("__x86.get_pc_thunk.bx", 1), ("_fini", 1), ("_init", 3), ("_start", 1), ("cgc_AddDive", 10), ("cgc_ChangeDive", 1), ("cgc_ChangeDiverInfo", 1), ("cgc_DeleteDive", 16), ("cgc_DiverStatistics", 7), ("cgc_DownloadDiveData", 17), ("cgc_EditDive", 3), ("cgc_EpochToDate", 15), ("cgc_GetChar", 3), ("cgc_GetInt", 3), ("cgc_GetLongString", 3), ("cgc_GetPositiveInt", 6), ("cgc_GetShortString", 3), ("cgc_GetUInt32", 4), ("cgc_LogNewDive", 3), ("cgc_MainMenu", 15), ("cgc_PrintDiveEntry", 1), ("cgc_PrintDiveLogs", 3), ("cgc_PrintDiverInfo", 1), ("cgc_RemoveDive", 3), ("cgc_SanitizeDate", 41), ("cgc_SanitizeDiveEntry", 1), ("cgc_SanitizeTime", 16), ("cgc_SelectDive", 14), ("cgc_SetInt", 6), ("cgc_SetParam", 6), ("cgc__terminate", 1), ("cgc_add_free_list", 5), ("cgc_allocate", 1), ("cgc_atof", 37), ("cgc_atoi", 32), ("cgc_bzero", 9), ("cgc_calloc", 1), ("cgc_deallocate", 1), ("cgc_destroy_data", 1), ("cgc_float_to_str", 40), ("cgc_floor", 6), ("cgc_free", 21), ("cgc_init_data", 3), ("cgc_int_to_hex", 11), ("cgc_int_to_str", 11), ("cgc_isalnum", 1), ("cgc_isalpha", 1), ("cgc_isdigit", 5), ("cgc_isinf", 1), ("cgc_islower", 5), ("cgc_isnan", 1), ("cgc_isspace", 9), ("cgc_isupper", 5), ("cgc_itoa", 10), ("cgc_log10", 1), ("cgc_malloc", 34), ("cgc_memcpy", 4), ("cgc_memset", 4), ("cgc_pow", 1), ("cgc_printf", 1), ("cgc_putc", 3), ("cgc_puts", 1), ("cgc_receive", 1), ("cgc_receive_until", 9), ("cgc_receive_until_flush", 14), ("cgc_rint", 1), ("cgc_round", 1), ("cgc_round_away_from_zero", 6), ("cgc_sprintf", 1), ("cgc_strcat", 8), ("cgc_strchr", 9), ("cgc_strcmp", 7), ("cgc_strcpy", 5), ("cgc_strdup", 6), ("cgc_strlen", 7), ("cgc_strncat", 11), ("cgc_strncpy", 10), ("cgc_strtok", 36), ("cgc_transmit", 1), ("cgc_vprintf", 117), ("cgc_vsprintf", 117), ("cgc_write", 4), ("deregister_tm_clones", 4), ("frame_dummy", 1), ("main", 1), ("memcpy", 1), ("memset", 1), ("register_tm_clones", 4), ("sub_8049496", 2)]

spec :: Spec
spec = describe "Binja.Core" $ do
  ebv <- runIO $ BN.getBinaryView diveBin
  context "Dive_Logger" $ do
    it "should load BV" $ do
      ebv `shouldSatisfy` isRight

    r <- runIO $ case ebv of
      Left e -> pure $ Left e
      Right bv -> do
        BN.updateAnalysisAndWait bv
        funcs <- Func.getFunctions bv
        funcBlocks <- mapM BB.getBasicBlocks funcs
        let funcBlocksMap = Map.fromList $ zip funcs funcBlocks
        return . Right $
          ( funcs
          , funcBlocksMap
          , sum . fmap blockSize <$> funcBlocksMap
          )
       where
        blockSize :: BB.BasicBlock Func.Function -> Int
        blockSize b = fromIntegral $ (b ^. BB.end) - (b ^. BB.start)

    case r of
      Left e -> it "should have loaded BV" $ do
        ("Didn't load BV, can't continue tests. Error: " <> e) `shouldBe` "Loaded BV"
      Right (funcs, blocks, funcSizes) -> do
        it "should load funcs" $ length funcs `shouldBe` 94

        it "should load basic blocks" $ do
          let lengths = fmap length . Map.mapKeys (view Func.name) $ blocks
          lengths `shouldBe` diveBlockCount

        it "should load instrs" $ do
          let counts = Map.mapKeys (view Func.name) funcSizes
          counts `shouldBe` diveFuncSize

    it "should check if function too large for analysis" $ do
      let bv = either (error . unpack) id ebv
      Just (func :: Func.Function) <- Func.getFunctionStartingAt bv Nothing 0x0804d210
      result <- Func.isFunctionTooLarge func
      result `shouldBe` False

    it "should check if function analysis was skipped" $ do
      let bv = either (error . unpack) id ebv
      Just (func :: Func.Function) <- Func.getFunctionStartingAt bv Nothing 0x0804d210
      result <- Func.isFunctionAnalysisSkipped func
      result `shouldBe` False

    it "should provide correct skip reason when not skipped" $ do
      let bv = either (error . unpack) id ebv
      Just (func :: Func.Function) <- Func.getFunctionStartingAt bv Nothing 0x0804d210
      result <- Func.getAnalysisSkipReason func
      result `shouldBe` NoSkipReason

    it "should provide get/set analysis skip override state" $ do
      let bv = either (error . unpack) id ebv
      Just (func :: Func.Function) <- Func.getFunctionStartingAt bv Nothing 0x0804d210
      result <- Func.getFunctionAnalysisSkipOverride func
      result `shouldBe` DefaultFunctionAnalysisSkip

      Func.setFunctionAnalysisSkipOverride func NeverSkipFunctionAnalysis
      result' <- Func.getFunctionAnalysisSkipOverride func
      result' `shouldBe` NeverSkipFunctionAnalysis

      Func.setFunctionAnalysisSkipOverride func AlwaysSkipFunctionAnalysis
      result'' <- Func.getFunctionAnalysisSkipOverride func
      result'' `shouldBe` AlwaysSkipFunctionAnalysis

    it "should get/set analysis parameters" $ do
      let bv = either (error . unpack) id ebv
      result <- An.getParametersForAnalysis bv
      result ^. An.maxFunctionSize `shouldBe` 65536

      An.setParametersForAnalysis bv (result & An.maxFunctionSize .~ 66560)

      result' <- An.getParametersForAnalysis bv
      result' ^. An.maxFunctionSize `shouldBe` 66560

  context "getOriginalBinary" $ do
    let bv = either (error . unpack) id ebv

    er <- runIO $ BN.getOriginalBinary bv

    it "should should not error when getting original binary" $ do
      isRight er `shouldBe` True

    let (Right ogb) = er
        r = show (hash ogb :: Digest MD5) :: Text
    
    it "should get exact original binary" $ do
      r `shouldBe` "58d7e707a6d51ec96e87c4c76747f24f"
