{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Evaluate" -}

module Main (main) where

import Flint.Prelude hiding (const)

import Flint.Analysis.Path.Matcher
import Flint.Analysis.Path.Matcher.Stub (StubSpec(StubSpec))
import qualified Flint.Analysis.Path.Matcher.Stub as Stub
import qualified Flint.Cfg.Store as Store

import Flint.Types.Query
import Flint.Query

import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


userControlledStrnCpyLen :: BugMatch
userControlledStrnCpyLen = BugMatch
  { pathPattern =
      [ Stmt $ Call (Just Wild) (CallFunc (FuncName "cgc_strncpy"))
        [ Wild
        , Wild
        , Bind "n" $ Contains (Bind "arg" (Var "arg"))
        ]
      ]
  , bugName = "User Controlled strncpy length"
  , bugDescription =
    "The function arg `" <> TextExpr "arg" <> "` is passed into the length argument of `strncpy` as the expression `" <> TextExpr "n" <> "`."
  , mitigationAdvice = "Ensure the third arg to `strncpy` can't be controlled by a user."
  }

userControlledStrcpySecondArg :: BugMatch
userControlledStrcpySecondArg = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc (FuncName "cgc_strcpy"))
        [ Wild
        , Bind "src" $ Contains (Bind "arg" (Var "arg"))
        ]
      ]
  , bugName = "User Controlled src arg of strcpy"
  , bugDescription =
    "The function arg `" <> TextExpr "arg" <> "` is passed into the src arg of `strcpy` as the expression `" <> TextExpr "src" <> "`."
  , mitigationAdvice = "Ensure the second arg to `strcpy` can't be controlled by a user."
  }

doubleFree :: BugMatch
doubleFree = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc (FuncName "cgc_free")) [ Bind "DivePtr" Wild ]
      , Stmt $ Call Nothing (CallFunc (FuncName "cgc_free")) [ Bind "DivePtr" Wild ]
      ]
  , bugName = "Double free"
  , bugDescription =
    "Bad: `" <> TextExpr "DivePtr" <> "`"
  , mitigationAdvice = "Bad job."
  }

useAfterFree :: BugMatch
useAfterFree = BugMatch
  { pathPattern =
      [ Stmt $ Call (Just $ Bind "ptr" Wild) (CallFunc (FuncName "cgc_malloc")) []
      , Stmt $ Call Nothing (CallFunc (FuncName "cgc_free")) [ Bind "ptr" Wild ]
      -- TODO: add the "use" part, ie memory load or store to "ptr"
      ]
  , bugName = "Use After Free"
  , bugDescription =
    "Bad: `" <> TextExpr "ptr" <> "`"
  , mitigationAdvice = "Bad job."
  }

incrementWithoutCheck :: Text -> BugMatch
incrementWithoutCheck mallocFuncName = BugMatch
  { pathPattern =
      [ AvoidUntil $ AvoidSpec
        { avoid = Stmt . BranchCond
                  $   ((load (Bind "ptr" Wild) ()) .< Wild)
                  .|| ((load (Bind "ptr" Wild) ()) .<= Wild)
        , until = Ordered
          [ Stmt $ Call Nothing (CallFunc (FuncName mallocFuncName)) []
          , Stmt $ Store (Bind "ptr" Wild) (add (load (Bind "ptr" Wild) ()) (Bind "n" Wild) ())
          ]
        }
      ]
  , bugName = "Increment Without Check"
  , bugDescription =
    "This path shows an increment of " <> TextExpr "n" <> " to the memory location `" <> TextExpr "ptr" <> "` without a bounds check. This could lead to an integer overflow."
  , mitigationAdvice = "Add a bounds check."
  }



oobWrite :: BugMatch
oobWrite = BugMatch
  { pathPattern =
      [ -- Stmt $ Call Nothing (CallFunc (FuncName "cgc_receive_until_flush")) []
        Stmt $ Call (Just $ Bind "ptr" Wild) (CallFunc (FuncName "cgc_malloc")) [Bind "ptrSize" Immediate]
      , Stmt (Store (Bind "ptrAccess" (Contains (Bind "ptr" Wild))) (Bind "val" Wild))
        `Where`
        [ cmpE (Bound "ptr") (const 0x100000 $ SizeOf "ptr") (ConstSize 1)
        , cmpSgt
          (Bound "ptrAccess")
          (add (Bound "ptr") (Bound "ptrSize") (SizeOf "ptrSize"))
          (SizeOf "ptrSize")
        ]
      ]
  , bugName = "Experiment 1"
  , bugDescription = "The pointer " <> TextExpr "ptr" <> " allocated by malloc was " <> TextExpr "ptrSize" <> " wide, but there is a write access to " <> TextExpr "ptrAccess" <> ", which might exceed its size."
--    "Bad: `" <> TextExpr "DivePtr" <> "`"
  , mitigationAdvice = "Bad job."
  }

oobRead :: BugMatch
oobRead = BugMatch
  { pathPattern =
      [ Stmt $ Call (Just $ Bind "ptr" Wild) (CallFunc (FuncName "cgc_malloc")) [Bind "ptrSize" Immediate]
      , Stmt (Def Wild
              (Bind "fullExpr"
               (Contains (load (Bind "ptrAccess" (Contains (Bind "ptr" Wild))) ()))))
        `Where`
        [ cmpE (Bound "ptr") (const 0x100000 $ SizeOf "ptr") (ConstSize 1)
        , cmpSgt
          (Bound "ptrAccess")
          (add (Bound "ptr") (Bound "ptrSize") (SizeOf "ptrSize"))
          (SizeOf "ptrSize")
        ]
      ]
  , bugName = "OOB read"
  , bugDescription = "The pointer " <> TextExpr "ptr" <> " allocated by malloc was " <> TextExpr "ptrSize" <> " wide, but there is a read access to " <> TextExpr "ptrAccess" <> " in the expr " <> TextExpr "fullExpr" <> " which might exceed its size."
--    "Bad: `" <> TextExpr "DivePtr" <> "`"
  , mitigationAdvice = "Bad job."
  }


divelogger :: IO ()
divelogger = do
  putText "starting"
  (Right (imp :: BNImporter)) <- openBinary "res/test_bins/Dive_Logger/Dive_Logger.bndb"
  putText "Loaded Dive_Logger.bndb"
  store' <- Store.init imp
  let funcMapping = mkFuncMapping $ store' ^. #funcs
      isUserlandFunc = (== Just True)
        . fmap isUpper
        . headMay
        . drop 4
        . Text.unpack
        . view #name
      userlandFuncs = HashSet.fromList
        . filter isUserlandFunc
        $ store' ^. #funcs
      deleteDive = HashSet.fromList
        . filter (\func -> func ^. #name == "cgc_DeleteDive")
        $ store' ^. #funcs
      funcNames = FuncSym <$>
        [ "cgc_AddDive"
        , "cgc_ChangeDive"
        , "cgc_ChangeDiverInfo"
        , "cgc_DeleteDive"
        , "cgc_DiverStatistics"
        , "cgc_DownloadDiveData"
        , "cgc_EditDive"
        , "cgc_EpochToDate"
        , "cgc_GetChar"
        , "cgc_GetInt"
        -- , "cgc_GetLongString"
        , "cgc_GetPositiveInt"
        -- , "cgc_GetShortString"
        , "cgc_GetUInt32"
        , "cgc_LogNewDive"
        , "cgc_MainMenu"
        , "cgc_PrintDiveEntry"
        , "cgc_PrintDiveLogs"
        , "cgc_PrintDiverInfo"
        , "cgc_RemoveDive"
        , "cgc_SanitizeDate"
        , "cgc_SanitizeDiveEntry"
        , "cgc_SanitizeTime"
        , "cgc_SelectDive"
        , "cgc_SetInt"
        , "cgc_SetParam"
        ]
      funcNames2 = FuncSym <$>
        [ "cgc_LogNewDive"
        ]

  onlyNamedFuncs <- HashSet.fromList <$> traverse (getFunction imp) funcNames

  let startFuncs = onlyNamedFuncs
      useSolver = True
      stubs = []
      callDepth = 10
      maxSamples = 80
      bugPattern = incrementWithoutCheck "cgc_malloc"

  -- mapM_ (\func -> putText $ func ^. #name) . sort . HashSet.toList $ startFuncs
  queryForBugMatch_ useSolver callDepth maxSamples store' funcMapping (Just startFuncs) stubs bugPattern
  putText "finished"


allocStub :: Text -> StubSpec
allocStub allocName = StubSpec
  { Stub.stmtToStub = Call (Just $ Bind "ptr" Wild) (CallFunc $ FuncName allocName) [Bind "sz" Wild]
  , Stub.removeOriginalStmt = False
  , Stub.stubs = [ store (bound "ptr") (Stub.newVar "freeVar" (SizeOf "ptr"))
            , constraint $ cmpNE (bound "freeVar") (const 0 (SizeOf "freeVar"))
              (ConstSize 8)
            ]
  }

electronictrading :: IO ()
electronictrading = do
  putText "starting"
  (Right (imp :: BNImporter)) <- openBinary "res/demo/cb/electronictrading.bndb"
  putText "Loaded electronictrading.bndb"
  store' <- Store.init imp
  let funcMapping = mkFuncMapping $ store' ^. #funcs
      isUserlandFunc = (== Just True)
        . fmap isUpper
        . headMay
        . drop 4
        . Text.unpack
        . view #name
      userlandFuncs = HashSet.fromList
        . filter isUserlandFunc
        $ store' ^. #funcs
      deleteDive = HashSet.fromList
        . filter (\func -> func ^. #name == "cgc_DeleteDive")
        $ store' ^. #funcs

  let allFuncs = HashSet.fromList $ store' ^. #funcs

  let startFuncs = allFuncs
      useSolver = False
      stubs = [allocStub "cgc_allocate", allocStub "cgc_pool_alloc"]
      callDepth = 10
      maxSamples = 80
      bugPattern = incrementWithoutCheck "cgc_allocate"

  -- mapM_ (\func -> putText $ func ^. #name) . sort . HashSet.toList $ startFuncs
  queryForBugMatch_ useSolver callDepth maxSamples store' funcMapping (Just startFuncs) stubs bugPattern
  
  -- mapM_ (\func -> putText $ func ^. #name) . sort . HashSet.toList $ funcs
  -- queryForBugMatch_ 10 80 store' funcMapping (Just funcs) stubs (incrementWithoutCheck "cgc_allocate")
  putText "finished"

main :: IO ()
main = divelogger
