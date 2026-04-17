{- HLINT ignore "Evaluate" -}

module Flint.Analysis.CAst.MatcherSpec
  ( module Flint.Analysis.CAst.MatcherSpec
  ) where

import Flint.Prelude hiding (check)

import Test.Hspec

import Blaze.Types.CAst (CStmt(..), CExpr(..), CForInit(..), CAnn)
import Flint.Analysis.CAst.Matcher (runCheck, runCheckOnStmts)
import Flint.Analysis.CAst.Patterns
import Flint.Types.Analysis.CAst.Matcher
import Flint.Types.Analysis.CAst.Finding (CAstFinding(..))


-- ---------------------------------------------------------------------------
-- Helpers: build C AST fragments without real addresses
-- ---------------------------------------------------------------------------

-- | Empty annotation (no address info needed for unit tests).
ann :: CAnn
ann = []

-- | Expression helpers
ident :: Text -> CExpr
ident = CIdent ann

litInt :: Integer -> CExpr
litInt = CLitInt ann

call :: Text -> [CExpr] -> CExpr
call = CFuncall ann

binOp :: Text -> CExpr -> CExpr -> CExpr
binOp = CBinaryOp ann

assign :: CExpr -> CExpr -> CExpr
assign = CAssign ann "="

index :: CExpr -> CExpr -> CExpr
index = CIndex ann

-- | Statement helpers
expr :: CExpr -> CStmt
expr = CExprStmt ann

varDecl :: Text -> Text -> Maybe CExpr -> CStmt
varDecl = CVarDecl ann

ret :: Maybe CExpr -> CStmt
ret = CReturn ann

ifStmt :: CExpr -> [CStmt] -> CStmt
ifStmt = CIf ann

-- | Loop helpers
forLoop :: CForInit -> Maybe CExpr -> Maybe CExpr -> [CStmt] -> CStmt
forLoop = CFor ann

whileLoop :: CExpr -> [CStmt] -> CStmt
whileLoop = CWhile ann

doWhileLoop :: [CStmt] -> CExpr -> CStmt
doWhileLoop = CDoWhile ann

-- | Common for-loop: for (i = 0; i < n; i++)
stdForLoop :: Text -> Text -> Text -> [CStmt] -> CStmt
stdForLoop var' bound op =
  forLoop
    (CForInitExpr (Just (assign (ident var') (litInt 0))))
    (Just (binOp op (ident var') (ident bound)))
    (Just (CPostfixOp ann "++" (ident var')))

-- | Count findings from a check
countFindings :: CAstCheck -> [CStmt] -> Int
countFindings chk stmts = length (runCheck chk stmts)

-- | Check if a check finds anything
finds :: CAstCheck -> [CStmt] -> Bool
finds chk stmts = not . null $ runCheck chk stmts

-- | Check that a check finds nothing
noFindings :: CAstCheck -> [CStmt] -> Bool
noFindings chk stmts = null (runCheck chk stmts)


-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Flint.Analysis.CAst" $ do

  -- ----- Matcher engine basics -----
  describe "Matcher engine" $ do

    it "CStar matches empty statement list" $ do
      let results = runCheckOnStmts CStar []
      results `shouldSatisfy` (not . null)

    it "CStar matches any number of statements" $ do
      let stmts = [expr (litInt 1), expr (litInt 2), expr (litInt 3)]
      runCheckOnStmts CStar stmts `shouldSatisfy` (not . null)

    it "CSequence [CStar, target, CStar] finds target in middle" $ do
      let stmts =
            [ expr (litInt 1)
            , expr (call "dangerous" [])
            , expr (litInt 3)
            ]
          pat = CSequence [CStar, CStmtExprPat (CCallPat "dangerous" []), CStar]
      runCheckOnStmts pat stmts `shouldSatisfy` (not . null)

    it "CBind captures and CBound re-matches" $ do
      let stmts =
            [ expr (assign (ident "x") (litInt 42))
            , expr (assign (ident "y") (litInt 42))
            ]
          pat = CSequence
            [ CStmtExprPat (CAssignPat CWild (CBind "val" CLitIntPat))
            , CStmtExprPat (CAssignPat CWild (CBound "val"))
            ]
      runCheckOnStmts pat stmts `shouldSatisfy` (not . null)

    it "CBound fails when expression differs" $ do
      let stmts =
            [ expr (assign (ident "x") (litInt 42))
            , expr (assign (ident "y") (litInt 99))
            ]
          pat = CSequence
            [ CStmtExprPat (CAssignPat CWild (CBind "val" CLitIntPat))
            , CStmtExprPat (CAssignPat CWild (CBound "val"))
            ]
      runCheckOnStmts pat stmts `shouldSatisfy` null

    it "CUses finds bound expression inside a larger expression" $ do
      let stmts =
            [ expr (assign (ident "p") (call "malloc" [litInt 10]))
            , expr (call "memset" [ident "p", litInt 0, litInt 10])
            ]
          pat = CSequence
            [ CStmtExprPat (CAssignPat (CBind "ptr" CWild) CWild)
            , CStmtExprPat (CContains (CUses "ptr"))
            ]
      runCheckOnStmts pat stmts `shouldSatisfy` (not . null)

    it "CBodyContains searches nested blocks" $ do
      let stmts =
            [ whileLoop (ident "cond")
                [ ifStmt (ident "x")
                    [ expr (call "free" [ident "p"]) ]
                ]
            ]
          pat = CSequence
            [ CStar
            , CAnyLoopPat
                [ CBodyContains (CStmtExprPat (CContains (CCallPat "free" [CWild])))
                ]
            ]
      runCheckOnStmts pat stmts `shouldSatisfy` (not . null)

    it "CNotPat succeeds when inner pattern fails" $ do
      let stmts = [expr (litInt 1)]
          pat = CSequence [CStmtNot (CStmtExprPat (CCallPat "free" [])), CStar]
      runCheckOnStmts pat stmts `shouldSatisfy` (not . null)

    it "CNotPat fails when inner pattern succeeds" $ do
      let stmts = [expr (call "free" [ident "p"])]
          pat = CStmtNot (CStmtExprPat (CContains (CCallPat "free" [CWild])))
      runCheckOnStmts pat stmts `shouldSatisfy` null


  -- ----- Individual vulnerability patterns -----
  describe "Patterns" $ do

    -- loop-array-oob
    describe "loop-array-oob" $ do
      it "matches for-loop with array index using loop variable" $ do
        -- for (i = 0; i < n; i++) { buf[i] = 0; }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (assign (index (ident "buf") (ident "i")) (litInt 0)) ]
              ]
        loopArrayOOB `finds` stmts `shouldBe` True

      it "does not match for-loop without array access" $ do
        -- for (i = 0; i < n; i++) { x = x + 1; }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (assign (ident "x") (binOp "+" (ident "x") (litInt 1))) ]
              ]
        loopArrayOOB `noFindings` stmts `shouldBe` True

      it "matches when array access is nested in an if inside the loop" $ do
        -- for (i = 0; i < n; i++) { if (cond) { buf[i] = 1; } }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ ifStmt (ident "cond")
                      [ expr (assign (index (ident "buf") (ident "i")) (litInt 1)) ]
                  ]
              ]
        loopArrayOOB `finds` stmts `shouldBe` True

    -- loop-off-by-one
    describe "loop-off-by-one" $ do
      it "matches for-loop with <= and array access" $ do
        -- for (i = 0; i <= n; i++) { buf[i] = 0; }
        let stmts =
              [ stdForLoop "i" "n" "<="
                  [ expr (assign (index (ident "buf") (ident "i")) (litInt 0)) ]
              ]
        loopOffByOne `finds` stmts `shouldBe` True

      it "does not match for-loop with < (correct bound)" $ do
        -- for (i = 0; i < n; i++) { buf[i] = 0; }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (assign (index (ident "buf") (ident "i")) (litInt 0)) ]
              ]
        loopOffByOne `noFindings` stmts `shouldBe` True

    -- loop-accumulating-write
    describe "loop-accumulating-write" $ do
      it "matches strcat inside a while loop" $ do
        -- while (cond) { strcat(buf, src); }
        let stmts =
              [ whileLoop (ident "cond")
                  [ expr (call "strcat" [ident "buf", ident "src"]) ]
              ]
        loopAccumulatingWrite `finds` stmts `shouldBe` True

      it "matches strncat inside a for loop" $ do
        -- for (i = 0; i < n; i++) { strncat(buf, items[i], 64); }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (call "strncat" [ident "buf", index (ident "items") (ident "i"), litInt 64]) ]
              ]
        loopAccumulatingWrite `finds` stmts `shouldBe` True

      it "does not match strcat outside a loop" $ do
        let stmts = [ expr (call "strcat" [ident "buf", ident "src"]) ]
        loopAccumulatingWrite `noFindings` stmts `shouldBe` True

    -- loop-use-after-free
    describe "loop-use-after-free" $ do
      it "matches free(ptr) followed by use without reassignment" $ do
        -- while (cond) { free(ptr); use(ptr); }
        let stmts =
              [ whileLoop (ident "cond")
                  [ expr (call "free" [ident "ptr"])
                  , expr (call "use" [ident "ptr"])
                  ]
              ]
        loopUseAfterFree `finds` stmts `shouldBe` True

      it "does not match when ptr is reassigned after free" $ do
        -- while (node != NULL) { free(node); node = node->next; }
        -- ptr is reassigned before next use -- safe
        let stmts =
              [ whileLoop (binOp "!=" (ident "node") (ident "NULL"))
                  [ expr (call "free" [ident "node"])
                  , expr (assign (ident "node") (CArrow ann (ident "node") "next"))
                  ]
              ]
        loopUseAfterFree `noFindings` stmts `shouldBe` True

      it "does not match free outside a loop" $ do
        let stmts =
              [ expr (call "process" [ident "node"])
              , expr (call "free" [ident "node"])
              ]
        loopUseAfterFree `noFindings` stmts `shouldBe` True

    -- loop-double-free
    describe "loop-double-free" $ do
      it "matches free in loop without ptr=NULL guard" $ do
        -- while (cond) { free(ptr); do_stuff(); }
        let stmts =
              [ whileLoop (ident "cond")
                  [ expr (call "free" [ident "ptr"])
                  , expr (call "do_stuff" [])
                  ]
              ]
        loopDoubleFree `finds` stmts `shouldBe` True

      it "does not match when ptr=NULL follows free" $ do
        -- while (cond) { free(ptr); ptr = NULL; }
        let stmts =
              [ whileLoop (ident "cond")
                  [ expr (call "free" [ident "ptr"])
                  , expr (assign (ident "ptr") (ident "NULL"))
                  ]
              ]
        loopDoubleFree `noFindings` stmts `shouldBe` True

    -- loop-memory-leak
    describe "loop-memory-leak" $ do
      it "matches malloc in loop without free" $ do
        -- for (i = 0; i < n; i++) { p = malloc(64); use(p); }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (assign (ident "p") (call "malloc" [litInt 64]))
                  , expr (call "use" [ident "p"])
                  ]
              ]
        loopMemoryLeak `finds` stmts `shouldBe` True

      it "does not match when free is in the same loop body" $ do
        -- for (i = 0; i < n; i++) { p = malloc(64); use(p); free(p); }
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (assign (ident "p") (call "malloc" [litInt 64]))
                  , expr (call "use" [ident "p"])
                  , expr (call "free" [ident "p"])
                  ]
              ]
        loopMemoryLeak `noFindings` stmts `shouldBe` True

      it "does not match when free follows malloc outside loop" $ do
        -- p = malloc(64); use(p); free(p);
        let stmts =
              [ expr (assign (ident "p") (call "malloc" [litInt 64]))
              , expr (call "use" [ident "p"])
              , expr (call "free" [ident "p"])
              ]
        loopMemoryLeak `noFindings` stmts `shouldBe` True

    -- loop-unbounded-input
    describe "loop-unbounded-input" $ do
      it "matches for-loop bounded by recv()" $ do
        -- for (i = 0; i < recv(fd, buf, sz, 0); i++) { ... }
        let stmts =
              [ forLoop
                  (CForInitExpr (Just (assign (ident "i") (litInt 0))))
                  (Just (binOp "<" (ident "i") (call "recv" [ident "fd", ident "buf", ident "sz", litInt 0])))
                  (Just (CPostfixOp ann "++" (ident "i")))
                  [ expr (litInt 0) ]
              ]
        loopUnboundedInput `finds` stmts `shouldBe` True

      it "does not match for-loop with constant bound" $ do
        let stmts =
              [ stdForLoop "i" "n" "<"
                  [ expr (litInt 0) ]
              ]
        loopUnboundedInput `noFindings` stmts `shouldBe` True

    -- loop-missing-bounds-check
    describe "loop-missing-bounds-check" $ do
      it "matches while-loop with array access and non-comparison condition" $ do
        -- while (ptr != NULL) { buf[i] = *ptr; }
        let stmts =
              [ whileLoop (ident "running")
                  [ expr (assign (index (ident "buf") (ident "i")) (ident "val")) ]
              ]
        loopMissingBoundsCheck `finds` stmts `shouldBe` True

      it "does not match while-loop with comparison condition (has bounds check)" $ do
        -- while (i < n) { buf[i] = val; } -- condition IS a comparison
        let stmts =
              [ whileLoop (binOp "<" (ident "i") (ident "n"))
                  [ expr (assign (index (ident "buf") (ident "i")) (ident "val")) ]
              ]
        loopMissingBoundsCheck `noFindings` stmts `shouldBe` True

      it "does not match while-loop without array access" $ do
        let stmts =
              [ whileLoop (ident "running")
                  [ expr (assign (ident "x") (binOp "+" (ident "x") (litInt 1))) ]
              ]
        loopMissingBoundsCheck `noFindings` stmts `shouldBe` True

    -- dangerous-function
    describe "dangerous-function" $ do
      it "matches gets()" $ do
        let stmts = [ expr (call "gets" [ident "buf"]) ]
        dangerousFunction `finds` stmts `shouldBe` True

      it "matches strcpy()" $ do
        let stmts = [ expr (call "strcpy" [ident "dst", ident "src"]) ]
        dangerousFunction `finds` stmts `shouldBe` True

      it "matches sprintf()" $ do
        let stmts = [ expr (call "sprintf" [ident "buf", ident "fmt", ident "arg"]) ]
        dangerousFunction `finds` stmts `shouldBe` True

      it "does not match safe alternatives" $ do
        let stmts = [ expr (call "snprintf" [ident "buf", litInt 64, ident "fmt"]) ]
        dangerousFunction `noFindings` stmts `shouldBe` True

      it "does not match unrelated functions" $ do
        let stmts = [ expr (call "printf" [ident "fmt"]) ]
        dangerousFunction `noFindings` stmts `shouldBe` True

    -- unchecked-alloc
    describe "unchecked-alloc" $ do
      it "matches malloc without NULL check" $ do
        -- p = malloc(64); memset(p, 0, 64);
        let stmts =
              [ expr (assign (ident "p") (call "malloc" [litInt 64]))
              , expr (call "memset" [ident "p", litInt 0, litInt 64])
              ]
        uncheckedAlloc `finds` stmts `shouldBe` True

      it "does not match malloc followed by NULL check" $ do
        -- p = malloc(64); if (p == NULL) { return -1; } use(p);
        let stmts =
              [ expr (assign (ident "p") (call "malloc" [litInt 64]))
              , ifStmt (binOp "==" (ident "p") (ident "NULL"))
                  [ ret (Just (litInt (-1))) ]
              , expr (call "use" [ident "p"])
              ]
        uncheckedAlloc `noFindings` stmts `shouldBe` True

      it "matches calloc without NULL check" $ do
        let stmts =
              [ expr (assign (ident "arr") (call "calloc" [litInt 10]))
              , expr (call "use" [ident "arr"])
              ]
        uncheckedAlloc `finds` stmts `shouldBe` True


    -- format-string
    describe "format-string" $ do
      it "matches printf with variable format argument" $ do
        let stmts = [ expr (call "printf" [ident "user_input"]) ]
        formatStringVuln `finds` stmts `shouldBe` True

      it "does not match printf with string literal" $ do
        let stmts = [ expr (call "printf" [CLitString ann "hello %s"]) ]
        formatStringVuln `noFindings` stmts `shouldBe` True

      it "does not match fprintf (2-arg, format is second)" $ do
        -- fprintf has 2+ args, first is FILE*. Pattern only matches 1-arg printf.
        let stmts = [ expr (call "fprintf" [ident "stderr", ident "msg"]) ]
        formatStringVuln `noFindings` stmts `shouldBe` True


  -- ----- Realistic multi-pattern scenarios -----
  describe "Realistic scenarios" $ do

    it "safe linked-list traversal does NOT trigger use-after-free" $ do
      -- void free_list(node *head) {
      --   while (head != NULL) {
      --     node *next = head->next;
      --     free(head);
      --     head = next;     <-- reassignment, not a use of freed ptr
      --   }
      -- }
      let stmts =
            [ whileLoop (binOp "!=" (ident "head") (ident "NULL"))
                [ varDecl "node *" "next" (Just (CArrow ann (ident "head") "next"))
                , expr (call "free" [ident "head"])
                , expr (assign (ident "head") (ident "next"))
                ]
            ]
      loopUseAfterFree `noFindings` stmts `shouldBe` True

    it "safe for-loop with bounds does not trigger array-oob" $ do
      -- for (i = 0; i < 10; i++) { x = x + 1; }
      let stmts =
            [ stdForLoop "i" "10" "<"
                [ expr (assign (ident "x") (binOp "+" (ident "x") (litInt 1))) ]
            ]
      loopArrayOOB `noFindings` stmts `shouldBe` True

    it "function with multiple vulnerabilities triggers multiple checks" $ do
      -- gets(buf);
      -- p = malloc(64);
      -- for (i = 0; i <= n; i++) { buf[i] = p[i]; }
      let stmts =
            [ expr (call "gets" [ident "buf"])
            , expr (assign (ident "p") (call "malloc" [litInt 64]))
            , stdForLoop "i" "n" "<="
                [ expr (assign (index (ident "buf") (ident "i")) (index (ident "p") (ident "i"))) ]
            ]
          allFindings = concatMap (`runCheck` stmts) allChecks
      -- Should find: dangerous-function (gets), unchecked-alloc (malloc),
      -- loop-off-by-one (<=), and loop-array-oob (buf[i])
      length allFindings `shouldSatisfy` (>= 3)
      let names = fmap findingName allFindings
      names `shouldSatisfy` ("dangerous-function" `elem`)
      names `shouldSatisfy` ("loop-off-by-one" `elem`)

    it "clean function triggers no findings" $ do
      -- n = get_count();
      -- if (n > MAX) { return -1; }
      -- p = malloc(n);
      -- if (p == NULL) { return -1; }
      -- for (i = 0; i < n; i++) { p[i] = 0; }
      -- free(p);
      -- return 0;
      let stmts =
            [ expr (assign (ident "n") (call "get_count" []))
            , ifStmt (binOp ">" (ident "n") (ident "MAX"))
                [ ret (Just (litInt (-1))) ]
            , expr (assign (ident "p") (call "malloc" [ident "n"]))
            , ifStmt (binOp "==" (ident "p") (ident "NULL"))
                [ ret (Just (litInt (-1))) ]
            , stdForLoop "i" "n" "<"
                [ expr (assign (index (ident "p") (ident "i")) (litInt 0)) ]
            , expr (call "free" [ident "p"])
            , ret (Just (litInt 0))
            ]
          allFindings = concatMap (`runCheck` stmts) allChecks
          names = fmap findingName allFindings
      -- loop-array-oob will still fire (it doesn't verify the bound matches allocation)
      -- but dangerous-function, unchecked-alloc, off-by-one should NOT fire
      names `shouldSatisfy` ("dangerous-function" `notElem`)
      names `shouldSatisfy` ("loop-off-by-one" `notElem`)
      names `shouldSatisfy` ("unchecked-alloc" `notElem`)
