{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.PathDemo where

-- import Blaze.Prelude

-- import Binja.Core (BNBinaryView)
-- import qualified Binja.Core as BN
-- import Binja.Function (Function, MLILSSAFunction)
-- import qualified Binja.Function as Func
-- import qualified Blaze.CallGraph as Cg
-- import qualified Blaze.Import.CallGraph as Cgi
-- import qualified Blaze.Import.Source.BinaryNinja as Bni
-- import Blaze.Path (AlgaPath, allSimpleFunctionPaths)
-- import qualified Blaze.Path as Path
-- import Blaze.Pil.Analysis (simplify)
-- import Blaze.Pil.Display (pdisp)
-- import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as PP
-- import qualified Blaze.Search as Search
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import Blaze.Types.Pil (Stmt)
-- import qualified Data.HashMap.Strict as HM

-- type F = MLILSSAFunction

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

-- diveSearchDemo :: IO [AlgaPath]
-- diveSearchDemo = do
--   bv <- unsafeFromRight <$> BN.getBinaryView diveBin
--   let importer = Bni.BNImporter bv
--   bnFuncs <- Func.getFunctions bv
--   funcs <- Cgi.getFunctions importer
--   cg <- Cg.getCallGraph importer funcs :: IO (AlgaGraph () Cg.Function)
--   paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--   _selectDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804e080 :: IO Function
--   editDive <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804d450
--   getPositiveInt <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x804cc30
--   let r2 = Search.searchBetween_ 
--             (HM.fromList $ zip ((^. #address) <$> funcs) funcs)
--             (HM.fromList $ zip ((^. Func.start) <$> bnFuncs) bnFuncs)
--             cg 
--             paths
--             editDive 0
--             getPositiveInt 12
--             -- selectDive 56
--             :: [AlgaPath]
--   --prettyPrint . head $ r2
--   return r2

-- dungeonSearchDemo :: IO [AlgaPath]
-- dungeonSearchDemo = do
--   (Right bv) <- BN.getBinaryView dungeonBin
--   let importer = Bni.BNImporter bv
--   bnFuncs <- Func.getFunctions bv
--   funcs <- Cgi.getFunctions importer
--   cg <- Cg.getCallGraph importer funcs :: IO (AlgaGraph () Cg.Function)
--   paths <- Path.pathsForAllFunctions bv :: IO (Map Function [AlgaPath])
--   moveBat <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804d540
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

-- printPathDemo :: IO ()
-- printPathDemo = do
--   (Right bv) <- BN.getBinaryView dungeonBin
--   moveLeft <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804c600
--   moveLeftPaths :: [AlgaPath] <- allSimpleFunctionPaths bv moveLeft 
--   let firstPath = head moveLeftPaths -- moveLeftPaths !! 2
--   firstPathStmts <- PP.convertPath bv firstPath
--   pdisp firstPathStmts
--   putText "------\n"
--   pdisp $ simplify firstPathStmts
--   return ()

-- exampleDungeonPil :: IO [Stmt]
-- exampleDungeonPil = do
--   (Right bv) <- BN.getBinaryView dungeonBin
--   -- moveBat <- fromJust <$> Func.getFunctionStartingAt bv Nothing 0x0804d540
--   path <- head <$> dungeonSearchDemo
--   PP.convertPath bv path

-- printExampleDungeonPil :: IO ()
-- printExampleDungeonPil = exampleDungeonPil >>= pdisp
