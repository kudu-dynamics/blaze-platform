module Ghidra.ClangDecompSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Clang (ClangAST, ClangNode, CStmt(..), CExpr(..), CForInit(..), CCase(..), convertFunction, renderStmts)
import Ghidra.Core (runGhidraOrError)
import Test.Hspec

import qualified Data.Text as Text
import qualified System.IO as SIO


loopTestBin :: FilePath
loopTestBin = "res/test_bins/decomp-test/loop-test.gzf"

-- | Decompile a function at a given address and return both the C AST and rendered text
decompAt :: FilePath -> Int64 -> IO ([CStmt], Text)
decompAt path jaddr = runGhidraOrError $ do
  gs <- State.openDatabase_ path >>! State.analyze
  jaddr' <- State.mkAddressBased (gs ^. #program) jaddr
  mFunc <- Function.fromAddr (gs ^. #program) jaddr'
  case mFunc of
    Nothing -> return ([], "ERROR: No function at 0x" <> show jaddr)
    Just jfunc -> do
      clangAST <- Function.getClangAST gs jfunc
      let cStmts = convertFunction clangAST
      return (cStmts, renderStmts 0 cStmts)

-- | Find all CFor nodes recursively in a statement list
findForLoops :: [CStmt] -> [CStmt]
findForLoops = concatMap go
  where
    go s@(CFor _ _ _ _ body)      = s : findForLoops body
    go (CWhile _ _ body)          = findForLoops body
    go (CDoWhile _ body _)        = findForLoops body
    go (CIf _ _ body)             = findForLoops body
    go (CIfElse _ _ thenB elseB)  = findForLoops thenB <> findForLoops elseB
    go (CSwitch _ _ cases)        = concatMap goCase cases
    go (CBlock _ stmts)           = findForLoops stmts
    go _                       = []
    goCase (CCase _ stmts)  = findForLoops stmts
    goCase (CDefault stmts) = findForLoops stmts

-- | Find all CWhile nodes recursively
findWhileLoops :: [CStmt] -> [CStmt]
findWhileLoops = concatMap go
  where
    go s@(CWhile _ _ body)       = s : findWhileLoops body
    go (CFor _ _ _ _ body)       = findWhileLoops body
    go (CDoWhile _ body _)       = findWhileLoops body
    go (CIf _ _ body)            = findWhileLoops body
    go (CIfElse _ _ thenB elseB) = findWhileLoops thenB <> findWhileLoops elseB
    go (CSwitch _ _ cases)       = concatMap goCase cases
    go (CBlock _ stmts)          = findWhileLoops stmts
    go _                       = []
    goCase (CCase _ stmts)  = findWhileLoops stmts
    goCase (CDefault stmts) = findWhileLoops stmts

-- | Find all CSwitch nodes recursively
findSwitches :: [CStmt] -> [CStmt]
findSwitches = concatMap go
  where
    go s@(CSwitch _ _ _)          = [s]
    go (CFor _ _ _ _ body)        = findSwitches body
    go (CWhile _ _ body)          = findSwitches body
    go (CDoWhile _ body _)        = findSwitches body
    go (CIf _ _ body)             = findSwitches body
    go (CIfElse _ _ thenB elseB)  = findSwitches thenB <> findSwitches elseB
    go (CBlock _ stmts)           = findSwitches stmts
    go _                        = []

-- | Find all CIf/CIfElse nodes recursively
findIfs :: [CStmt] -> [CStmt]
findIfs = concatMap go
  where
    go s@(CIf _ _ body)            = s : findIfs body
    go s@(CIfElse _ _ thenB elseB) = s : findIfs thenB <> findIfs elseB
    go (CFor _ _ _ _ body)         = findIfs body
    go (CWhile _ _ body)           = findIfs body
    go (CDoWhile _ body _)         = findIfs body
    go (CSwitch _ _ cases)         = concatMap goCase cases
    go (CBlock _ stmts)            = findIfs stmts
    go _                         = []
    goCase (CCase _ stmts)  = findIfs stmts
    goCase (CDefault stmts) = findIfs stmts

spec :: Spec
spec = describe "Ghidra.ClangDecomp" $ do
  let path = loopTestBin

  -- Addresses from: nm loop-test | grep " T _"
  -- Note: Ghidra uses image-base-relative addresses, so we subtract 0x100000000
  let addrNestedFor         = 0x3838
  let addrBubbleSort        = 0x38f4
  let addrFindFirstNegative = 0x39f0
  let addrClassify          = 0x3acc
  let addrTestSwitch        = 0x3b48
  let addrProcess           = 0x3be4

  (stmtsNestedFor, renderedNestedFor)   <- runIO $ decompAt path addrNestedFor
  (stmtsBubbleSort, renderedBubbleSort) <- runIO $ decompAt path addrBubbleSort
  (stmtsFindFirstNeg, renderedFindFirstNeg) <- runIO $ decompAt path addrFindFirstNegative
  (stmtsClassify, renderedClassify)     <- runIO $ decompAt path addrClassify
  (stmtsSwitch, renderedSwitch)         <- runIO $ decompAt path addrTestSwitch
  (stmtsProcess, renderedProcess)       <- runIO $ decompAt path addrProcess

  context "Loop decompilation" $ do
    it "nested_for has an outer CFor containing an inner CFor" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- nested_for ---\n" <> Text.unpack renderedNestedFor
      let forLoops = findForLoops stmtsNestedFor
      length forLoops `shouldSatisfy` (>= 2)
      -- The first for loop should contain a nested for loop in its body
      case forLoops of
        (CFor _ _ _ _ body : _) ->
          findForLoops body `shouldSatisfy` (not . null)
        other -> expectationFailure $ "Expected CFor, got: " <> show (take 1 other)

    it "bubble_sort has nested CFor loops with CIf in inner body" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- bubble_sort ---\n" <> Text.unpack renderedBubbleSort
      let forLoops = findForLoops stmtsBubbleSort
      length forLoops `shouldSatisfy` (>= 2)
      -- Inner for body should contain an if
      case forLoops of
        (CFor _ _ _ _ outerBody : _) ->
          case findForLoops outerBody of
            (CFor _ _ _ _ innerBody : _) ->
              findIfs innerBody `shouldSatisfy` (not . null)
            _ -> expectationFailure "Expected inner for loop"
        _ -> expectationFailure "Expected outer for loop"

    it "find_first_negative has a CWhile loop" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- find_first_negative ---\n" <> Text.unpack renderedFindFirstNeg
      -- Ghidra may emit this as while or do-while
      let whiles = findWhileLoops stmtsFindFirstNeg
      let fors = findForLoops stmtsFindFirstNeg
      (length whiles + length fors) `shouldSatisfy` (>= 1)

    it "classify has CIf nodes" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- classify ---\n" <> Text.unpack renderedClassify
      let ifs = findIfs stmtsClassify
      length ifs `shouldSatisfy` (>= 1)

    it "test_switch produces CSwitch or equivalent if/else chain" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- test_switch ---\n" <> Text.unpack renderedSwitch
      -- Ghidra may emit switch as CSwitch or as nested if/else chains
      -- (common for small switches on ARM). Accept either.
      let switches = findSwitches stmtsSwitch
          ifs = findIfs stmtsSwitch
      (length switches + length ifs) `shouldSatisfy` (>= 1)
      -- Rendered output should mention at least 2 of the case strings
      let rendered = renderedSwitch
          mentionCount = length $ filter (`Text.isInfixOf` rendered)
            ["\"one\\n\"", "\"two\\n\"", "\"three\\n\"", "\"other\\n\""]
      mentionCount `shouldSatisfy` (>= 2)

    it "process has a loop (CFor or CWhile, Ghidra may optimize for+switch to while)" $ do
      SIO.hPutStrLn SIO.stderr $ "\n--- process ---\n" <> Text.unpack renderedProcess
      let forLoops = findForLoops stmtsProcess
          whileLoops = findWhileLoops stmtsProcess
      (length forLoops + length whileLoops) `shouldSatisfy` (>= 1)
