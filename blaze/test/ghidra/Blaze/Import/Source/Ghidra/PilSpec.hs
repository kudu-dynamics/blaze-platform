module Blaze.Import.Source.Ghidra.PilSpec where

import Blaze.Prelude hiding (const, Constraint)

import Blaze.Import.Pil
import Blaze.Import.Source.Ghidra
import Blaze.Types.Import
import Blaze.Types.Pil --(Stmt, AddressableStatement(..), Statement(..), DefOp(..))
import Blaze.Types.Pil.PilType
--import Control.Monad (when)
--import Data.Text (unpack)
import Blaze.Import.CallGraph (CallGraphImporter (getFunction, getFunctions))
import Blaze.Pil.Checker (checkStmtsWithTypeHints)
import Blaze.Types.Pil.Checker (DeepSymType(..){-,TypeReport, Constraint, SymType(..)-})
import Blaze.Types.Function (Function)
import qualified Blaze.Import.Source.Ghidra as G
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
--import qualified Blaze.Pretty as Pretty

import Test.Hspec


annotatedTestBin :: FilePath
annotatedTestBin = "../blaze/res/test_bins/annotated_binary/annotated_binary.gzf"

diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

onionTestBin :: FilePath
onionTestBin = "../flint/res/test_bins/onion_test/onion_test"

data TestCtx = TestCtx
    { diveLoggerImp :: GhidraImporter
    , diveLoggerFuncs :: [Function]
    , diveLoggerFuncStmts :: [[Stmt]]
    , diveLoggerPilMaps :: [TypeHints]
    , annotatedImp :: GhidraImporter
    , annotatedFuncs :: [Function]
    , annotatedFuncStmts :: [[Stmt]]
    , annotatedPilMaps :: [TypeHints]
    } deriving (Generic)

-- | Resolve FuncRef list from getFunctions to internal Function list.
resolveInternalFuncs :: CallGraphImporter a => a -> IO [Function]
resolveInternalFuncs imp = do
  markers <- getFunctions imp
  let internalRefs = mapMaybe (^? #_InternalRef) markers
  mapMaybeM (\fm -> fmap (>>= (^? #_Internal)) . getFunction imp $ fm ^. #address) internalRefs

getTestCtx :: IO TestCtx
getTestCtx = do
    diveLoggerImp <- getImporter diveBin
    diveLoggerFuncs <- resolveInternalFuncs diveLoggerImp
    (diveLoggerFuncStmts, diveLoggerPilMaps) <- (fmap unzip . getFuncStmtsWithTypeHints diveLoggerImp) diveLoggerFuncs
    annotatedImp <- getImporter annotatedTestBin
    annotatedFuncs <- resolveInternalFuncs annotatedImp
    (annotatedFuncStmts, annotatedPilMaps) <- (fmap unzip . getFuncStmtsWithTypeHints annotatedImp) annotatedFuncs
    --annotatedFuncStmts <- getFuncStmts annotatedImp annotatedFuncs
    --annotatedPilMaps <- getPVMaps annotatedImp annotatedFuncs
    return $ TestCtx
        { diveLoggerImp = diveLoggerImp
        , diveLoggerFuncs = diveLoggerFuncs
        , diveLoggerFuncStmts = diveLoggerFuncStmts
        , diveLoggerPilMaps = diveLoggerPilMaps
        , annotatedImp = annotatedImp
        , annotatedFuncs = annotatedFuncs
        , annotatedFuncStmts = annotatedFuncStmts
        , annotatedPilMaps = annotatedPilMaps
        }
    where
        getFuncStmtsWithTypeHints imp = mapM (\func -> getFuncStatementsWithTypeHints imp func 0)


spec :: Spec
spec = beforeAll getTestCtx . describe "Blaze.Import.Source.Ghidra.Pil" $ do
  -- return ()
  context "Address widths" $ do
    -- This test is to address a problem with Address widths found in onion_test
    -- where the Store width of a CONST_PTR was 4 bytes and a LOAD of the same ptr
    -- was 8 bytes.
    let action = do
          imp <- G.getImporter onionTestBin
          funcs <- resolveInternalFuncs imp
          let intOverflowFunc = fromJust . headMay . filter ((== "int_overflow_func") . view #name) $ funcs
          (stmt0:stmt1:_) <- getFuncStatements imp intOverflowFunc 0
          let addr0 = stmt0 ^?! #statement . #_Def . #value . #op . #_ADD . #left . #op . #_LOAD . #src
              addr1 = stmt1 ^?! #statement . #_Store . #addr
          return (addr0 ^. #size, addr1 ^. #size)
    it "should have 8 byte width for address in LOAD and 8 byte width in dest of STORE" $ \_ -> do
      action `shouldReturn` (8, 8)

  context "Import Function" $ do
    importer <- runIO $ G.getImporter diveBin
    mFunc <- runIO $ getFunction importer (intToAddr 0x804d670) -- cgc_SetParam function
    it "should import a function by address" $ \_ -> do
      mFunc ^? _Just . #_Internal . #name `shouldBe` Just "cgc_SetParam"

    let func = mFunc ^?! _Just . #_Internal
    stmts <- runIO $ getFuncStatements importer func 0

    -- After STACK_ADDR unification, stack vars are Store/Load, not Def/Var.
    -- Stack phis are preserved by phiVarNode with SSA versions.
    -- stmt10: DefPhi var_14#1 [var_14#2, var_14#3]   (stack phi)
    -- stmt11: unique_6600#1 = STACK_ADDR(-84)        (PTRSUB)
    -- stmt16: unique_6600#2 = STACK_ADDR(-84)        (PTRSUB at different pc)
    let stmtPhi = stmts !! 10
    let stmt_u1 = stmts !! 11
    let stmt_u2 = stmts !! 16

    let phiDest = stmtPhi ^? #statement . #_DefPhi . #dest
    let phiSrcs = stmtPhi ^? #statement . #_DefPhi . #src
    let pv_u1 = stmt_u1 ^? #statement . #_Def . #var
    let pv_u2 = stmt_u2 ^? #statement . #_Def . #var

    it "should have versioned stack vars in DefPhi from phiVarNode" $ \_ -> do
      phiDest ^? _Just . #symbol `shouldBe` Just "var_14"
      phiDest ^? _Just . #version `shouldBe` Just (Just 1)
      phiDest ^? _Just . #location `shouldBe` Just (StackMemory (-20))
      -- Phi inputs should have different versions
      let srcVersions = fmap (view #version) <$> phiSrcs
      srcVersions `shouldBe` Just [Just 2, Just 3]

    it "should have separate number labels for assignments to the same unique var" $ \_ -> do
      pv_u1 ^? _Just . #symbol `shouldBe` Just "unique_6600"
      pv_u1 ^? _Just . #version `shouldBe` Just (Just 1)
      pv_u2 ^? _Just . #symbol `shouldBe` Just "unique_6600"
      pv_u2 ^? _Just . #version `shouldBe` Just (Just 2)

    it "stack params should appear as LOAD from positive-offset STACK_ADDR" $ \_ -> do
      -- stmt0 is: Store [STACK_ADDR(-20)] (Load [STACK_ADDR(+12)] 4)
      -- The Load source is a positive-offset stack addr (param area)
      let stmt0 = unsafeHead stmts
          paramLoad = stmt0 ^? #statement . #_Store . #value . #op . #_LOAD . #src . #op . #_STACK_ADDR . #offset
      paramLoad `shouldBe` Just 12
    
  context "importer datatype constraints" $ do
    it "seeing the variables of a highfunction" $ \tctx -> do
        let action = do
                let funcNum = entryFuncIndex
                let funcStmts = (tctx ^. #annotatedFuncStmts)!!funcNum
                let pvMap = (tctx ^. #annotatedPilMaps)!!funcNum
                let reportWithImp = checkStmtsWithTypeHints pvMap Nothing funcStmts
                return $ case reportWithImp of
                            Right typeReport -> do
                                foldl' (\res (pv, dst) ->
                                            case HM.lookup (pv ^. #symbol) entryMap of
                                                Just set ->
                                                    case dst of
                                                        DSType (TBottom _) -> res
                                                        _ -> res && HS.member dst set
                                                Nothing -> res
                                    ) True (HM.toList $ typeReport ^. #varSymTypeMap)
                            Left _ -> False
        action `shouldReturn` True
{-
    it "printing for debugging purposes" $ \tctx -> do
        let debugging= do
                let funcNum = 2--entryFuncIndex
                let func = (tctx ^. #annotatedFuncs)!!funcNum

                print "Printing Unification Errors"
                let funcStmts = (tctx ^. #annotatedFuncStmts)!!funcNum
                let pvMap = (tctx ^. #annotatedPilMaps)!!funcNum
                let reportWithImp = checkStmts Nothing pvMap funcStmts
                let reportWithoutImp = checkStmts Nothing HM.empty funcStmts
                case (reportWithImp, reportWithoutImp) of
                    (Right typeReportWithImp, Right typeReportWithoutImp) -> printUnificationErrors typeReportWithImp (Just typeReportWithoutImp) funcStmts
                    _ -> print "It didn't work >:C"
                print "Done with Errors"

                return True
        debugging `shouldReturn` True
-}



entryMap :: HashMap Text (HashSet DeepSymType)
entryMap = HM.fromList
    [ ( "person"
      , HS.fromList
            [ DSType (TInt {bitWidth = Just (Bits 32), signed = Just True})
            , DSType (TChar {bitWidth = Just (Bits 8)})
            , DSType (TFloat {bitWidth = Just (Bits 32)})
            , DSType TBool
            , DSType (TFloat {bitWidth = Just (Bits 64)})
            ]
      )
    ]

entryFuncIndex :: Int
entryFuncIndex = 2

{-
printStoredExpr :: (Pretty.Tokenizable a) => TypeReport -> Statement a -> IO ()
printStoredExpr tr (Def (DefOp val expr)) = do
    Pretty.prettyPrint' $ do
        sym <- HM.lookup val (tr ^. #varSymMap)
        deepType <- HM.lookup sym (tr ^. #solutions)
        return (sym, deepType)
    Pretty.prettyPrint' expr
printStoredExpr _ (Constraint (ConstraintOp expr)) = Pretty.prettyPrint' expr
printStoredExpr _ (Store op@(StoreOp expr1 expr2)) = do
    Pretty.prettyPrint' expr1
    Pretty.prettyPrint' expr2
printStoredExpr _ (Call (CallOp dest _ exprs)) = forM_ exprs Pretty.prettyPrint'
--printStoredExpr (Constraint (ConstraintOp expr)) = Pretty.prettyPrint' expr

printStoredExpr _ _ = print "Not DefOp"


printStmtFromTypeReport :: TypeReport -> Int -> IO ()
printStmtFromTypeReport tr idx = traverse_
    (\(idx', addrStmt) ->
        when (idx == idx') $ case addrStmt of
            (Stmt _ stmt) -> printStoredExpr tr stmt) (tr ^. #symTypedStmts)

{-
printStmtFromTypeReport tr idx = case (tr ^. #symTypedStmts)!!idx of
    (_, addrStmt) -> case addrStmt of
        (_, stmt) -> printStoredExpr stmt
-}   
    
    --traverse_ (\(idx', stmt) -> when (idx == idx') $ Pretty.prettyPrint' stmt) (tr ^. #symStmts)

--getConflictingStmts ::

printSymWithAllConstraints :: TypeReport -> [Stmt] -> Sym -> IO ()
printSymWithAllConstraints tr stmts sym = do
    print $ "All constraints for " ++ (unpack . Pretty.pretty') sym ++ ": "
    let symConstraints = getConstraintsForSym (tr ^. #originMap) (tr ^. #ogConstraints) sym
    forM_ symConstraints (\constraint -> do
                            Pretty.prettyPrint' constraint
                            Pretty.prettyPrint' $ stmts!!(constraint ^. #stmtOrigin))
    print "---------"


collectSymsFromConstraints :: [Constraint] -> HashSet Sym
collectSymsFromConstraints = foldl' (\set constraint -> HS.union (getSymsFromConstraint constraint) set ) HS.empty

getSymsFromConstraint :: Constraint -> HashSet Sym
getSymsFromConstraint constraint =
    let set = HS.insert (constraint ^. #sym) HS.empty
    in case constraint ^. #symType of
            SVar sym -> HS.insert sym set
            _ -> set



printConstraint :: TypeReport -> [Stmt] -> Constraint -> IO ()
printConstraint tr stmts constraint = do
    --print "-------------------"
    Pretty.prettyPrint' constraint
    Pretty.prettyPrint' $ stmts!!(constraint ^. #stmtOrigin)
    printStmtFromTypeReport tr (constraint ^. #stmtOrigin)
    print "---"
    --Pretty.prettyPrint' $ constraint ^. #sym
    --print "------------"
    --let symConstraints = getConstraintsForSym (tr ^. #originMap) (tr ^. #ogConstraints) (constraint ^. #sym)

    --print (tr ^. #)
    --Pretty.prettyPrint' $ stmts!!(constraint ^. #stmtOrigin)
    --Pretty.prettyPrint' (constraint ^. #sym)
    --Pretty.prettyPrint' $ HM.lookup (constraint ^. #sym) (tr ^. #solutions)
    --Pretty.prettyPrint' (constraint ^. #symType)

printUnificationErrors :: TypeReport -> Maybe TypeReport -> [Stmt] -> IO ()
printUnificationErrors report Nothing stmts = do
    forM_ (report ^. #errors) (\error -> do
                    Pretty.prettyPrint' error
                    print error
          )

printUnificationErrors reportWithImp (Just reportWithoutImp) stmts = do
    print "********************************************"
    print "Constraint Errors (With Importer):"
    forM_ (reportWithImp ^. #errors) (printErrWithStmt stmts reportWithImp)
    print "********************************************"
    print "Constraint Errors (Without Importer):"
    forM_ (reportWithoutImp ^. #errors) (printErrWithStmt stmts reportWithoutImp)
    where
        printConflictingStmts stmts' constraints =
            let idxSet = collectConstraintOrigins constraints
            in forM_ idxSet (\idx -> do
                            let stmt = stmts'!!idx
                            print idx
                            Pretty.prettyPrint' stmt
                            {-print stmt-})
        collectConstraintOrigins :: [Constraint] -> HashSet Int
        collectConstraintOrigins = foldl' (\set constraint -> HS.insert (constraint ^. #stmtOrigin) set) HS.empty
        getStmt stmts' constraintError =
            let idx = constraintError ^. #stmtOrigin
            in stmts'!!idx
        printErrWithStmt stmts' report err = do
            let constraints = fromJust (HM.lookup (err ^. #sym) (report ^. #errorConstraints))
            --let stmt = getStmt stmts' err

            --Pretty.prettyPrint' err
            --print "Constraints: "
            --forM_ constraints (printConstraint report stmts')

            --print "----------------------"
            --print "Pretty"
            --print "----------------------"
            --print "----------------------"
            print "----------------------"
            Pretty.prettyPrint' err
            print "----------------------"
            print "Statement Origin:"
            Pretty.prettyPrint' $ stmts'!!(err ^. #stmtOrigin) 
            print "----------------------"
            print "Conflicting Statements:"
            printConflictingStmts stmts' constraints
            print "----------------------"
            print "Constraints: "
            forM_ constraints (printConstraint report stmts')
            --print "Solutions: "
            --forM_ (HM.toList (report ^. #solutions)) print
            --print $ HM.toList (report ^. #solutions)
            --print (report ^. #originMap)
            --print stmt
            --Pretty.prettyStmts' [stmt]
            --print "----------------------"
            --print "Printing Constraints for Syms:"
            --print "----------------------"
            --let syms = collectSymsFromConstraints constraints
            --forM_ syms (printSymWithAllConstraints report stmts')
            {-
            print "----------------------"
            print "Conflicting Statements:"
            printConflictingStmts stmts' constraints
            print "----------------------"
            print "Not Pretty"
            print "----------------------"
            print err
            forM_ constraints print
            -}
-}





