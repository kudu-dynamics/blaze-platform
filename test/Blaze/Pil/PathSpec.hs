{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.PathSpec where

-- import Binja.BasicBlock
--   ( BasicBlock,
--     BlockEdge,
--   )
-- import qualified Binja.BasicBlock as BB
-- import Binja.Core
--   ( BNBinaryView,
--     InstructionIndex,
--   )
-- import qualified Binja.Core as BN
-- import Binja.Function (MLILSSAFunction)
-- import qualified Binja.Function as BNFunc
-- import qualified Blaze.Graph as Graph
-- import Blaze.Types.Path.AlgaPath (AlgaPath)
-- import qualified Blaze.Path as Path
-- import qualified Blaze.Pil.Construct as PC
import Blaze.Prelude hiding (sym)
-- import Blaze.Pretty
-- import Blaze.Types.Graph (Graph)
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import qualified Blaze.Types.Pil as Pil
-- import qualified Data.Text as Text
import Test.Hspec
-- import qualified Blaze.Types.CallGraph as CG
-- import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as BNPath
-- import qualified Blaze.Import.Source.BinaryNinja.CallGraph as BNCG

-- type F = MLILSSAFunction

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

-- diveBv :: IO BNBinaryView
-- diveBv = do
--   (Right bv) <- BN.getBinaryView diveBin
--   BN.updateAnalysisAndWait bv
--   return bv

-- dungeonBin :: FilePath
-- dungeonBin = "res/test_bins/Dungeon_Master/Dungeon_Master.bndb"

-- dungeonBv :: IO BNBinaryView
-- dungeonBv = do
--   (Right bv) <- BN.getBinaryView dungeonBin
--   BN.updateAnalysisAndWait bv
--   return bv

-- getBlockPath ::
--   (Graph (BlockEdge F) (BasicBlock F) g) =>
--   BNBinaryView ->
--   F ->
--   g ->
--   [InstructionIndex F] ->
--   IO AlgaPath
-- getBlockPath bv mlilFn bbg xs = do
--   bbs <- catMaybes <$> mapM (BB.getBasicBlockForInstruction mlilFn) xs
--   Path.pathFromBasicBlockList bv bbg bbs

-- isEnterContext :: CG.Function -> Pil.Stmt -> Maybe (Pil.EnterContextOp Pil.Expression)
-- isEnterContext callee (Pil.EnterContext x)
--   | x ^. #ctx . #func == callee = Just x
--   | otherwise = Nothing
-- isEnterContext _ _ = Nothing

-- defPilVar :: Pil.PilVar -> Pil.Expression -> Pil.Stmt
-- defPilVar pilVar val =
--   Pil.Def (Pil.DefOp pilVar val)

-- defSymWithCtx :: Pil.Ctx -> Pil.Symbol -> Pil.Expression -> Pil.Stmt
-- defSymWithCtx ctx sym =
--   defPilVar pilVar
--     where
--       pilVar :: Pil.PilVar
--       pilVar = Pil.PilVar sym (Just ctx)

-- varFromPilVar :: Pil.PilVar -> Pil.OperationSize -> Pil.Expression
-- varFromPilVar pvar size =
--   PC.mkExpr size (Pil.VAR $ Pil.VarOp pvar)

-- -- TODO: Make this safe (use Maybe) if moved into codebase as convenience function
-- varFromCtx :: Pil.Ctx -> Pil.Symbol -> [Pil.Stmt] -> Pil.Expression
-- varFromCtx ctx sym stmts =
--   varFromPilVar pvar size
--   where
--     defStmt :: Pil.Stmt
--     defStmt =
--       fromJust . headMay $
--         [ s | s@(Pil.Def (Pil.DefOp var _)) <- stmts, var ^. #symbol == sym
--                                                         && var ^. #ctx == Just ctx
--         ]
--     size :: Pil.OperationSize
--     size = defStmt ^?! (#_Def . #value . #size)
--     pvar :: Pil.PilVar
--     pvar = defStmt ^?! (#_Def . #var)

-- -- DefInfo is being used to select only symbols and context from a Def statement since
-- -- PilVar's currently have a _mapsTo field which contains information
-- -- that may be inconsistent and requires too much contextual information
-- -- to easily use for defining expected test values.
-- -- TODO: Remove this once PilVars are simplified and/or values
-- --       are consistent and easier to generate.
-- data DefInfo
--   = DefInfo
--       { defSym :: Pil.Symbol,
--         defCtx :: Pil.Ctx,
--         srcSym :: Pil.Symbol,
--         srcCtx :: Pil.Ctx
--       } deriving (Eq, Ord, Show)

-- mayDefInfo :: Pil.Stmt -> Maybe DefInfo
-- mayDefInfo stmt =
--   case stmt of
--     Pil.Def (Pil.DefOp defVar (Pil.Expression _ (Pil.VAR (Pil.VarOp srcVar)))) ->
--       Just $ DefInfo (defVar ^. #symbol)
--                      (fromJust (defVar ^. #ctx))
--                      (srcVar ^. #symbol)
--                      (fromJust (srcVar ^. #ctx))
--     _ -> 
--       Nothing

spec :: Spec
spec = describe "Blaze.Pil.Path" $ do
  context "when avoiding" $ do
    it "placeholder" $
      True `shouldBe` True
--   bv <- runIO diveBv
--   selectDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x804e080
--   cgSelectDive <- runIO $ BNCG.convertFunction bv selectDive
--   mlilSelectDive <- runIO $ BNFunc.getMLILSSAFunction selectDive
--   editDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x804d450
--   mlilEditDive <- runIO $ BNFunc.getMLILSSAFunction editDive
--   bbgSelectDive <- runIO (Graph.constructBasicBlockGraph mlilSelectDive :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--   bbgEditDive <- runIO (Graph.constructBasicBlockGraph mlilEditDive :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))

--   context "Single BB" $ do
--     bb62 <- runIO $ fromJust <$> BB.getBasicBlockForInstruction mlilSelectDive 62
--     p62 <- runIO (Path.pathFromBasicBlockList bv bbgSelectDive [bb62] :: IO AlgaPath)
--     stmts62 <- runIO $ BNPath.convertPath bv p62
--     it "should convert a single block" $
--       length stmts62 == 2
--   context "Intraprocedural path" $ do
--     path <- runIO $ getBlockPath bv mlilEditDive bbgEditDive [0, 8]
--     stmts <- runIO $ BNPath.convertPath bv path
--     let ptext = pretty (PStmts stmts)
--     -- this is sloppy to use the pretty output,
--     -- but who really wants to do all that pattern matching for a one-off test?
--     it "should convert multiple blocks and have calls" $
--       Text.isInfixOf "cgc_SelectDive" ptext && Text.isInfixOf "cgc_ChangeDive" ptext
--   context "Interprocedural path" $ do
--     callerPath <- runIO $ getBlockPath bv mlilEditDive bbgEditDive [0, 8]
--     calleePath <- runIO $ getBlockPath bv mlilSelectDive bbgSelectDive [0, 14, 38]
--     let (Just acn) = Path.findAbstractCallNode editDive 4 selectDive callerPath
--         path =
--           Path.expandAbstractCall
--             acn
--             (fromJust $ Path.mkInsertablePath calleePath)
--             callerPath
--     stmts <- runIO $ BNPath.convertPath bv path
--     let enters = mapMaybe (isEnterContext cgSelectDive) stmts
--     it "should convert path with expanded call" $
--       length enters == 1
--   context "Linking between caller and callee at expanded function calls" $ do
--     changeDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804dcc0
--     sanitizeDiveEntry <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804dfd0
--     setParam <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804d670
    
--     changeDiveMlil <- runIO $ BNFunc.getMLILSSAFunction changeDive
--     sanitizeDiveEntryMlil <- runIO $ BNFunc.getMLILSSAFunction sanitizeDiveEntry
--     setParamMlil <- runIO $ BNFunc.getMLILSSAFunction setParam
    
--     changeDiveCfg <- runIO (Graph.constructBasicBlockGraph changeDiveMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--     sanitizeDiveEntryCfg <- runIO (Graph.constructBasicBlockGraph sanitizeDiveEntryMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--     setParamCfg <- runIO (Graph.constructBasicBlockGraph setParamMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
    
--     changeDivePath <- runIO $ getBlockPath bv changeDiveMlil changeDiveCfg [0]
--     sanitizeDiveEntryPath <- runIO $ getBlockPath bv sanitizeDiveEntryMlil sanitizeDiveEntryCfg [0]
--     setParamPath <- runIO $ getBlockPath bv setParamMlil setParamCfg [0, 10, 22, 34, 43]

--     cgChangeDive <- runIO $ BNCG.convertFunction bv changeDive
--     cgSetParam <- runIO $ BNCG.convertFunction bv setParam
--     cgSanitizeDiveEntry <- runIO $ BNCG.convertFunction bv sanitizeDiveEntry

--     let changeDiveCtx = Pil.Ctx cgChangeDive 0
--         setParamCtx = Pil.Ctx cgSetParam 1
--         sanitizeDiveEntryCtx = Pil.Ctx cgSanitizeDiveEntry 2
--         (Just setParamAcn) = Path.findAbstractCallNode changeDive 5 setParam changeDivePath
--         (Just sanitizeDiveEntryAcn) = Path.findAbstractCallNode changeDive 47 sanitizeDiveEntry changeDivePath
--         path =
--           Path.expandAbstractCall
--             setParamAcn
--             (fromJust $ Path.mkInsertablePath setParamPath)
--             . Path.expandAbstractCall
--               sanitizeDiveEntryAcn
--               (fromJust $ Path.mkInsertablePath sanitizeDiveEntryPath)
--             $ changeDivePath
--     stmts <- runIO $ BNPath.convertPath bv path
--     -- If tests need to be updated,
--     -- can print statements with index using:
--     -- runIO $ mapM_ print (zip [0 ..] (pretty <$> stmts))

--     it "should link call site arguments with function parameters" $ do
--       stmts
--         `shouldContain` [ defSymWithCtx setParamCtx "arg1#0" (PC.const 0x804e670 4),
--                           defSymWithCtx setParamCtx "arg2#0" $ varFromCtx changeDiveCtx "var_28#1" stmts,
--                           defSymWithCtx setParamCtx "arg3#0" (PC.const 25 4)
--                         ]
--       stmts
--         `shouldContain` [defSymWithCtx sanitizeDiveEntryCtx "arg1#0" $ varFromCtx changeDiveCtx "var_2c#1" stmts]
        
--     it "should link call site result variable with function return value" $ do
--       mayDefInfo <$> stmts
--         `shouldContain` [Just $ DefInfo "eax_3#4" changeDiveCtx "eax_5#6" sanitizeDiveEntryCtx]
--       -- Additional check for positional stability of PIL statements
--       let varExpr = varFromCtx changeDiveCtx "eax_3#4" stmts 
--       stmts !! 92 `shouldBe` defPilVar (varExpr ^?! #op . #_VAR . #src) (varFromCtx sanitizeDiveEntryCtx "eax_5#6" stmts)

--   context "Program context management" $ do
--     logNewDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804d1c0
--     addDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804c7d0
--     changeDive <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804dcc0
--     sanitizeDiveEntry <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804dfd0

--     logNewDiveMlil <- runIO $ BNFunc.getMLILSSAFunction logNewDive
--     addDiveMlil <- runIO $ BNFunc.getMLILSSAFunction addDive
--     changeDiveMlil <- runIO $ BNFunc.getMLILSSAFunction changeDive
--     sanitizeDiveEntryMlil <- runIO $ BNFunc.getMLILSSAFunction sanitizeDiveEntry

--     logNewDiveCfg <- runIO (Graph.constructBasicBlockGraph logNewDiveMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--     addDiveCfg <- runIO (Graph.constructBasicBlockGraph addDiveMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--     changeDiveCfg <- runIO (Graph.constructBasicBlockGraph changeDiveMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--     sanitizeDiveEntryCfg <- runIO (Graph.constructBasicBlockGraph sanitizeDiveEntryMlil :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))

--     logNewDivePath <- runIO $ getBlockPath bv logNewDiveMlil logNewDiveCfg [0, 6, 14]
--     addDivePath <- runIO $ getBlockPath bv addDiveMlil addDiveCfg [0, 11, 15, 17, 31, 43]
--     changeDivePath <- runIO $ getBlockPath bv changeDiveMlil changeDiveCfg [0]
--     sanitizeDiveEntryPath <- runIO $ getBlockPath bv sanitizeDiveEntryMlil sanitizeDiveEntryCfg [0]

--     it "should correctly manage consecutive calls" $ do
--       let (Just addDiveAcn) = Path.findAbstractCallNode logNewDive 8 addDive logNewDivePath
--           (Just changeDiveAcn) = Path.findAbstractCallNode logNewDive 12 changeDive logNewDivePath
--           path = Path.expandAbstractCall changeDiveAcn
--                 (fromJust $ Path.mkInsertablePath changeDivePath)
--                 . Path.expandAbstractCall addDiveAcn
--                 (fromJust $ Path.mkInsertablePath addDivePath)
--                 $ logNewDivePath
--       stmts <- BNPath.convertPath bv path

--       -- If tests need to be updated,
--       -- can print statements with index using:
--       -- prettyIndexedStmts (zip [0..] stmts)

--       let ctxIdxGetter = #_Def . #var . #ctx . _Just . #ctxId
--           firstStmt = fromJust . headMay $ stmts
--           lastStmt = fromJust . lastMay $ stmts
--           addDiveStmt = stmts !! 9
--           changeDiveStmt = stmts !! 49
--           betweenCallsStmt = stmts !! 44
--       (firstStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 0
--       (lastStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 0
--       (addDiveStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 1
--       (changeDiveStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 2
--       (betweenCallsStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 0

--     it "should correctly manage nested calls" $ do
--       let (Just changeDiveAcn) = Path.findAbstractCallNode logNewDive 12 changeDive logNewDivePath
--           (Just sanitizeDiveEntryAcn) = Path.findAbstractCallNode changeDive 47 sanitizeDiveEntry changeDivePath
--           path = Path.expandAbstractCall sanitizeDiveEntryAcn
--                 (fromJust $ Path.mkInsertablePath sanitizeDiveEntryPath)
--                 . Path.expandAbstractCall changeDiveAcn
--                 (fromJust $ Path.mkInsertablePath changeDivePath)
--                 $ logNewDivePath
--       stmts <- BNPath.convertPath bv path

--       -- If tests need to be updated,
--       -- can print statements with index using:
--       -- mapM_ print (zip [0..] (pretty <$> stmts))

--       let ctxIdxGetter = #_Def . #var . #ctx . _Just . #ctxId
--           firstStmt = fromJust . headMay $ stmts
--           lastStmt = fromJust . lastMay $ stmts
--           changeDiveStmt = stmts !! 12
--           sanitizeDiveEntryStmt = stmts !! 61
--       (firstStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 0
--       (lastStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 0
--       (changeDiveStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 1
--       (sanitizeDiveEntryStmt ^?! ctxIdxGetter) `shouldBe` Pil.CtxIndex 2
