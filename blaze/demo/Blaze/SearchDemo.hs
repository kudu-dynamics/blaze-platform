{- HLINT ignore "Reduce duplication" -}

module Blaze.SearchDemo where

-- import Blaze.Prelude

-- import qualified Binja.Core as BN
-- import Binja.Core (BNBinaryView)
-- import qualified Binja.Function as Func
-- import Binja.Function (Function)
-- import qualified Data.Map as Map
-- import qualified Blaze.Path as Path
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import Blaze.Path (AlgaPath)
-- import qualified Blaze.Search as Search  
-- import qualified Blaze.Import.Source.BinaryNinja as Bni
-- import qualified Blaze.Import.CallGraph as Cgi
-- import qualified Blaze.CallGraph as Cg
-- import qualified Data.HashMap.Strict as HM

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

-- dungeonBin :: FilePath
-- dungeonBin = "res/test_bins/Dungeon_Master/Dungeon_Master.bndb"

-- dungeonBv :: IO BNBinaryView
-- dungeonBv = do
--   (Right bv) <- BN.getBinaryView dungeonBin
--   return bv

-- diveBv :: IO BNBinaryView
-- diveBv = do
--   (Right bv) <- BN.getBinaryView diveBin
--   return bv

-- -- TODO: BasicBlock `end` instr index is +1 actual last instruction

-- pathsForAllFunctions :: BNBinaryView -> IO (Map Function [AlgaPath])
-- pathsForAllFunctions bv = do
--   fns <- Func.getFunctions bv
--   Map.fromList <$> traverse g fns
--   where
--     g :: Function -> IO (Function, [AlgaPath])
--     g fn = (fn,) <$> Path.allSimpleFunctionPaths bv fn

-- diveSearchDemo :: IO [AlgaPath]
-- diveSearchDemo = do
--   ebv <- BN.getBinaryView diveBin
--   let (Right bv) = ebv
--       importer = Bni.BNImporter bv
--   bnFuncs <- Func.getFunctions bv
--   funcs <- Cgi.getFunctions importer
--   cg <- Cg.getCallGraph importer funcs :: IO (AlgaGraph () Cg.Function)
--   paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--   _selectDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804e080 :: IO Function
--   editDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804d450
--   getPositiveInt <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804cc30
--   let r2 = Search.searchBetween_
--            (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--            (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--            cg 
--            paths
--            editDive 0
--            getPositiveInt 12
--            -- selectDive 56
--            :: [AlgaPath]
--   --prettyPrint . head $ r2
--   return r2

-- dungeonSearchDemo :: IO [AlgaPath]
-- dungeonSearchDemo = do
--   ebv <- BN.getBinaryView dungeonBin
--   let (Right bv) = ebv
--       importer = Bni.BNImporter bv
--   bnFuncs <- Func.getFunctions bv
--   funcs <- Cgi.getFunctions importer
--   cg <- Cg.getCallGraph importer funcs :: IO (AlgaGraph () Cg.Function)
--   paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--   moveBat <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804d540
--   _moveLeft <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804c600 :: IO Function
--   setObj <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804b8a0
--   let r2 = Search.searchBetween_ 
--             (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--             (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--             cg 
--             paths
--             moveBat 0
--             setObj 70
--             :: [AlgaPath]
--   return r2
