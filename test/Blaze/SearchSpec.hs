{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Blaze.SearchSpec where

import Blaze.Prelude

-- import qualified Binja.Core as BN
-- import Binja.Function (Function, MLILSSAFunction)
-- import qualified Binja.Function as Func
-- import qualified Blaze.CallGraph as Cg
-- import qualified Blaze.Import.CallGraph as Cgi
-- import Blaze.Import.Source.BinaryNinja (BNImporter (BNImporter))
-- import qualified Blaze.Import.Source.BinaryNinja as Bni
-- import Blaze.Path (AlgaPath)
-- import qualified Blaze.Path as Path
-- import Blaze.Import.Source.BinaryNinja.Pil.Path (convertPath)
-- import qualified Blaze.Search as Search
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Set as Set
import Test.Hspec

-- type F = MLILSSAFunction

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

-- dungeonBin :: FilePath
-- dungeonBin = "res/test_bins/Dungeon_Master/Dungeon_Master.bndb"

spec :: Spec
spec = describe "Blaze.Search" $ do
  it "placeholder" $
    True `shouldBe` True
--   context "searchBetween" $ do
--     it "should search between two instructions in same function" $ do
--       bv <- unsafeFromRight <$> BN.getBinaryView diveBin
--       let importer = Bni.BNImporter bv
--       bnFuncs <- Func.getFunctions bv
--       funcs <- Cgi.getFunctions importer
--       cg <- Cg.getCallGraph importer funcs
--       paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--       selectDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804e080
--       let result = Search.searchBetween_ 
--                     (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--                     (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--                     cg 
--                     paths
--                     selectDive 0
--                     selectDive 56
--                     :: [AlgaPath]
--       length result `shouldBe` 7

--     it "should search between three functions" $ do
--       bv <- unsafeFromRight <$> BN.getBinaryView diveBin
--       let importer = Bni.BNImporter bv
--       bnFuncs <- Func.getFunctions bv
--       funcs <- Cgi.getFunctions importer
--       cg <- Cg.getCallGraph importer funcs
--       paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--       editDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804d450
--       getPositiveInt <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804cc30
--       let result = Search.searchBetween_ 
--                 (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--                 (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--                 cg 
--                 paths 
--                 editDive 0
--                 getPositiveInt 12
--                 :: [AlgaPath]
--       length result `shouldBe` 6

--     it "should be able to find lots of paths" $ do
--       bv <- unsafeFromRight <$> BN.getBinaryView dungeonBin
--       let importer = BNImporter bv
--       bnFuncs <- Func.getFunctions bv
--       funcs <- Cgi.getFunctions importer
--       cg <- Cg.getCallGraph importer funcs
--       paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--       moveBat <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804d540
--       setObj <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804b8a0
--       let result = Search.searchBetween_ 
--               (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--               (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--               cg 
--               paths
--               moveBat 0
--               setObj 70
--               :: [AlgaPath]
--       length result `shouldBe` 416

--     it "should search between two instructions in same function" $ do
--       bv <- unsafeFromRight <$> BN.getBinaryView diveBin
--       let importer = BNImporter bv
--       bnFuncs <- Func.getFunctions bv
--       funcs <- Cgi.getFunctions importer
--       cg <- Cg.getCallGraph importer funcs
--       paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--       selectDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804e080
--       let result = Search.searchBetween_ 
--               (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--               (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--               cg 
--               paths
--               selectDive 5
--               selectDive 12
--               :: [AlgaPath]
--       length (Set.toList $ Set.fromList result) `shouldBe` 1

--     it "should only find 15 instructions" $ do
--       (Right bv) <- BN.getBinaryView dungeonBin
--       let importer = BNImporter bv
--       bnFuncs <- Func.getFunctions bv
--       funcs <- Cgi.getFunctions importer
--       cg <- Cg.getCallGraph importer funcs
--       paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--       moveBat <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804d540
--       let result = Search.searchBetween_ 
--               (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--               (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--               cg 
--               paths
--               moveBat 0
--               moveBat 15
--               :: [AlgaPath]
--           path = head result
--       stmts <- convertPath bv path
--       length stmts `shouldBe` 15

--     return ()


