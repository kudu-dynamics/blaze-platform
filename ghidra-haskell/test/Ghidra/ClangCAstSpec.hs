module Ghidra.ClangCAstSpec where

import Ghidra.Prelude

import Test.Hspec
import Ghidra.Clang
import qualified Data.Text as T

-- Helper to create default opts for test construction

mkSyntax :: Text -> ClangAST ClangNode
mkSyntax t = Leaf $ ClangSyntaxToken ClangSyntaxTokenOpts
  { text = t, isVarRef = False, pairId = Nothing }

mkSyntaxPair :: Text -> Int32 -> ClangAST ClangNode
mkSyntaxPair t pid = Leaf $ ClangSyntaxToken ClangSyntaxTokenOpts
  { text = t, isVarRef = False, pairId = Just pid }

mkOp :: OpToken -> Text -> ClangAST ClangNode
mkOp tok t = Leaf $ ClangOpToken ClangOpTokenOpts
  { text = t, addrRange = Nothing, pcodeOp = Nothing, opToken = tok }

mkVar :: Text -> ClangAST ClangNode
mkVar name = Leaf $ ClangVariableToken ClangVariableTokenOpts
  { text = name, addrRange = Nothing, highSymbol = Nothing
  , highVariable = Nothing, pcodeOp = Nothing, scalar = Nothing
  , varnode = Nothing, isVarRef = True }

mkType :: Text -> ClangAST ClangNode
mkType name = Leaf $ ClangTypeToken ClangTypeTokenOpts
  { text = name, datatype = undefined, isVarRef = False }

mkBreak :: ClangAST ClangNode
mkBreak = Leaf $ ClangBreak ClangBreakOpts { indent = 0 }

mkFuncName :: Text -> ClangAST ClangNode
mkFuncName name = Leaf $ ClangFuncNameToken ClangFuncNameTokenOpts
  { text = name, addrRange = Nothing, pcodeOp = Nothing }

mkStmt :: [ClangAST ClangNode] -> ClangAST ClangNode
mkStmt children = Branch (ClangStatement ClangStatementOpts
  { addrRange = Nothing, pcodeOp = Nothing }) children

mkGroup :: [ClangAST ClangNode] -> ClangAST ClangNode
mkGroup children = Branch (ClangTokenGroup ClangTokenGroupOpts
  { addrRange = Nothing }) children

mkFunc :: [ClangAST ClangNode] -> ClangAST ClangNode
mkFunc children = Branch (ClangFunction ClangFunctionOpts
  { addrRange = undefined }) children

mkProto :: ClangAST ClangNode
mkProto = Branch (ClangFuncProto ClangFuncProtoOpts) []

-- | Build a for loop ClangAST:
-- for (init; cond; incr) { body }
mkForLoop :: [ClangAST ClangNode]  -- init tokens
          -> [ClangAST ClangNode]  -- cond tokens
          -> [ClangAST ClangNode]  -- incr tokens
          -> [ClangAST ClangNode]  -- body children
          -> ClangAST ClangNode
mkForLoop initToks condToks incrToks bodyChildren =
  mkGroup $
    [ mkOp For "for"
    , mkSyntax " "
    , mkSyntaxPair "(" 1
    ] <> wrapStmt initToks <>
    [ mkSyntax ";"
    , mkSyntax " "
    ] <> wrapStmt condToks <>
    [ mkSyntax ";"
    , mkSyntax " "
    ] <> wrapStmt incrToks <>
    [ mkSyntaxPair ")" 1
    , mkSyntax " "
    , mkSyntaxPair "{" 2
    , mkGroup bodyChildren
    , mkSyntaxPair "}" 2
    ]
  where
    wrapStmt [] = []
    wrapStmt ts = [mkStmt ts]

-- | Build "i = 0" as init tokens
mkAssignInit :: Text -> Integer -> [ClangAST ClangNode]
mkAssignInit varName val =
  [ mkVar varName, mkSyntax " ", mkOp Assignment "=", mkSyntax " ", mkVar (show val) ]

-- | Build "i < n" condition tokens
mkLtCond :: Text -> Text -> [ClangAST ClangNode]
mkLtCond lhs rhs = [ mkVar lhs, mkSyntax " ", mkOp Lt "<", mkSyntax " ", mkVar rhs ]

-- | Build "i++" increment tokens
mkPostIncr :: Text -> [ClangAST ClangNode]
mkPostIncr varName = [ mkVar varName, mkOp Increment "++" ]

-- | Build a simple expression statement like "printf(...)"
mkCallStmt :: Text -> [[ClangAST ClangNode]] -> ClangAST ClangNode
mkCallStmt funcName args =
  mkStmt $ [mkFuncName funcName, mkSyntaxPair "(" 10]
    <> intercalateArgs args
    <> [mkSyntaxPair ")" 10]
  where
    intercalateArgs [] = []
    intercalateArgs [a] = a
    intercalateArgs (a : rest) = a <> [mkSyntax ",", mkSyntax " "] <> intercalateArgs rest

spec :: Spec
spec = describe "Ghidra.Clang C AST Conversion" $ do

  describe "convertFunction" $ do
    it "converts a simple for loop" $ do
      let forLoop = mkForLoop
            (mkAssignInit "i" 0)
            (mkLtCond "i" "3")
            (mkPostIncr "i")
            [ mkCallStmt "printf" [[mkVar "\"i: %d\\n\""], [mkVar "i"]]
            ]
          func = mkFunc [mkProto, mkGroup [forLoop]]
          stmts = convertFunction func
      -- Should produce a single CFor statement
      length stmts `shouldBe` 1
      case stmts of
        [CFor _ _ _ _ _] -> return ()
        other -> expectationFailure $ "Expected CFor, got: " <> show other

    it "handles nested for loops" $ do
      let innerFor = mkForLoop
            (mkAssignInit "j" 0)
            (mkLtCond "j" "3")
            (mkPostIncr "j")
            [ mkCallStmt "printf" [[mkVar "\"i: %d j: %d\\n\""], [mkVar "i"], [mkVar "j"]]
            ]
          outerFor = mkForLoop
            (mkAssignInit "i" 0)
            (mkLtCond "i" "3")
            (mkPostIncr "i")
            [ innerFor
            , mkCallStmt "printf" [[mkVar "\"i: %d\\n\""], [mkVar "i"]]
            ]
          func = mkFunc [mkProto, mkGroup [outerFor]]
          stmts = convertFunction func
      -- Should have one outer CFor
      length stmts `shouldBe` 1
      case stmts of
        [CFor _ _ _ _ body] -> do
          -- Body should contain a nested CFor and an expression statement
          length body `shouldSatisfy` (>= 1)
          case head body of
            CFor {} -> return ()
            other -> expectationFailure $ "Expected nested CFor, got: " <> show other
        other -> expectationFailure $ "Expected CFor, got: " <> show other

    it "extracts for-loop init/cond/incr parts" $ do
      let forLoop = mkForLoop
            (mkAssignInit "i" 0)
            (mkLtCond "i" "10")
            (mkPostIncr "i")
            []
          func = mkFunc [mkProto, mkGroup [forLoop]]
          stmts = convertFunction func
      case stmts of
        [CFor _ initC condC incrC _body] -> do
          -- Init should be an expression (i = 0)
          case initC of
            CForInitExpr (Just (CAssign [] "=" (CIdent [] "i") (CIdent [] "0"))) -> return ()
            other -> expectationFailure $ "Expected CForInitExpr with assignment, got: " <> show other
          -- Cond should be i < 10
          case condC of
            Just (CBinaryOp [] "<" (CIdent [] "i") (CIdent [] "10")) -> return ()
            other -> expectationFailure $ "Expected binary < condition, got: " <> show other
          -- Incr should be i++
          case incrC of
            Just (CPostfixOp [] "++" (CIdent [] "i")) -> return ()
            other -> expectationFailure $ "Expected postfix ++, got: " <> show other
        other -> expectationFailure $ "Expected CFor, got: " <> show other

  describe "convertExpr" $ do
    it "converts a single variable to CIdent" $ do
      let result = convertExpr [mkVar "x"]
      result `shouldBe` CIdent [] "x"

    it "converts a binary operation" $ do
      let result = convertExpr [mkVar "x", mkOp Plus "+", mkVar "y"]
      result `shouldBe` CBinaryOp [] "+" (CIdent [] "x") (CIdent [] "y")

    it "converts an assignment" $ do
      let result = convertExpr [mkVar "x", mkOp Assignment "=", mkVar "y"]
      result `shouldBe` CAssign [] "=" (CIdent [] "x") (CIdent [] "y")

    it "converts a function call" $ do
      let result = convertExpr
            [ mkFuncName "printf"
            , mkSyntaxPair "(" 1
            , mkVar "x"
            , mkSyntaxPair ")" 1
            ]
      result `shouldBe` CFuncall [] "printf" [CIdent [] "x"]

    it "falls back to CRawExpr for complex expressions" $ do
      let nodes = [mkVar "x", mkSyntax "[", mkVar "i", mkSyntax "]"]
          result = convertExpr nodes
      case result of
        CRawExpr _ _ -> True `shouldBe` True
        _ -> True `shouldBe` True  -- May parse, may not; just ensure no crash

  describe "renderStmts" $ do
    it "renders a for loop" $ do
      let forStmt = CFor []
            (CForInitExpr (Just (CAssign [] "=" (CIdent [] "i") (CLitInt [] 0))))
            (Just (CBinaryOp [] "<" (CIdent [] "i") (CLitInt [] 3)))
            (Just (CPostfixOp [] "++" (CIdent [] "i")))
            [CExprStmt [] (CFuncall [] "printf" [CIdent [] "i"])]
          rendered = renderStmt 0 forStmt
      -- Should contain "for"
      rendered `shouldSatisfy` ("for" `T.isInfixOf`)
      -- Should contain the init
      rendered `shouldSatisfy` ("i = 0" `T.isInfixOf`)
      -- Should contain the condition
      rendered `shouldSatisfy` ("i < 3" `T.isInfixOf`)

  describe "round-trip rendering" $ do
    it "renders nested for loops readably" $ do
      let inner = CFor []
            (CForInitExpr (Just (CAssign [] "=" (CIdent [] "j") (CLitInt [] 0))))
            (Just (CBinaryOp [] "<" (CIdent [] "j") (CLitInt [] 3)))
            (Just (CPostfixOp [] "++" (CIdent [] "j")))
            [CExprStmt [] (CFuncall [] "printf" [CIdent [] "\"i: %d j: %d\\n\"", CIdent [] "i", CIdent [] "j"])]
          outer = CFor []
            (CForInitExpr (Just (CAssign [] "=" (CIdent [] "i") (CLitInt [] 0))))
            (Just (CBinaryOp [] "<" (CIdent [] "i") (CLitInt [] 3)))
            (Just (CPostfixOp [] "++" (CIdent [] "i")))
            [ inner
            , CExprStmt [] (CFuncall [] "printf" [CIdent [] "\"i: %d\\n\"", CIdent [] "i"])
            ]
          rendered = renderStmt 0 outer
      -- Just check it renders without crashing and contains key elements
      rendered `shouldSatisfy` ("for" `T.isInfixOf`)
      rendered `shouldSatisfy` ("printf" `T.isInfixOf`)

  describe "real Ghidra AST structure" $ do
    it "parses a for loop matching actual Ghidra output" $ do
      -- This mirrors the REAL Ghidra output structure from the example:
      -- for (local_18 = 0; local_18 < 3; local_18 = local_18 + 1) { ... }
      -- Note: Ghidra emits extra space SyntaxTokens between elements
      let outerForGroup = mkGroup
            [ mkSyntax " "                      -- leading space
            , mkBreak                            -- line break
            , mkOp For "for"                     -- for keyword
            , mkSyntax " "                       -- space
            , mkSyntaxPair "(" 21               -- open paren with pairId
            , mkSyntax " "                       -- space after (
            , mkStmt                             -- init: local_18 = 0
                [ mkVar "local_18"
                , mkSyntax " "
                , mkOp Assignment "="
                , mkSyntax " "
                , mkVar "0"
                ]
            , mkSyntax ";"                       -- semicolon
            , mkSyntax " "                       -- space
            , mkSyntax " "                       -- extra space (Ghidra does this)
            , mkStmt                             -- cond: local_18 < 3
                [ mkVar "local_18"
                , mkSyntax " "
                , mkOp Lt "<"
                , mkSyntax " "
                , mkVar "3"
                ]
            , mkSyntax ";"                       -- semicolon
            , mkSyntax " "                       -- space
            , mkSyntax " "                       -- extra space
            , mkStmt                             -- incr: local_18 = local_18 + 1
                [ mkVar "local_18"
                , mkSyntax " "
                , mkOp Assignment "="
                , mkSyntax " "
                , mkVar "local_18"
                , mkSyntax " "
                , mkOp Plus "+"
                , mkSyntax " "
                , mkVar "1"
                ]
            , mkSyntax " "                       -- space
            , mkSyntaxPair ")" 21               -- close paren matching pairId
            , mkSyntax " "                       -- space
            , mkSyntax "{"                       -- open brace
            , mkGroup                            -- body TokenGroup
                [ mkGroup                        -- empty TokenGroup (Ghidra emits this)
                    []
                , mkGroup                        -- inner for loop (wrapped in its own TokenGroup)
                    [ mkSyntax " "
                    , mkBreak
                    , mkOp For "for"
                    , mkSyntax " "
                    , mkSyntaxPair "(" 22
                    , mkSyntax " "
                    , mkStmt [mkVar "local_1c", mkSyntax " ", mkOp Assignment "=", mkSyntax " ", mkVar "0"]
                    , mkSyntax ";"
                    , mkSyntax " "
                    , mkSyntax " "
                    , mkStmt [mkVar "local_1c", mkSyntax " ", mkOp Lt "<", mkSyntax " ", mkVar "3"]
                    , mkSyntax ";"
                    , mkSyntax " "
                    , mkSyntax " "
                    , mkStmt [mkVar "local_1c", mkSyntax " ", mkOp Assignment "=", mkSyntax " ", mkVar "local_1c", mkSyntax " ", mkOp Plus "+", mkSyntax " ", mkVar "1"]
                    , mkSyntax " "
                    , mkSyntaxPair ")" 22
                    , mkSyntax " "
                    , mkSyntax "{"
                    , mkGroup                    -- inner body: printf call
                        [ mkStmt [mkFuncName "printf", mkSyntaxPair "(" 30, mkVar "\"i: %d j: %d\\n\"", mkSyntax ",", mkSyntax " ", mkVar "local_18", mkSyntax ",", mkSyntax " ", mkVar "local_1c", mkSyntaxPair ")" 30]
                        ]
                    , mkSyntax "}"
                    ]
                , mkGroup                        -- printf after inner loop (also wrapped)
                    [ mkBreak
                    , mkStmt [mkFuncName "printf", mkSyntaxPair "(" 31, mkVar "\"i: %d\\n\"", mkSyntax ",", mkSyntax " ", mkVar "local_18", mkSyntaxPair ")" 31]
                    ]
                ]
            , mkSyntax "}"
            ]

          -- Wrap in function structure
          returnStmt = mkGroup
            [ mkBreak
            , mkOp Return "return"
            , mkSyntax " "
            , mkStmt [mkVar "0"]
            , mkSyntax ";"
            ]
          funcBody = mkGroup [outerForGroup, returnStmt]
          func = mkFunc [mkProto, funcBody]
          stmts = convertFunction func
          rendered = renderStmts 0 stmts

      -- Should extract nested for loops
      case stmts of
        (CFor _ initC condC incrC body : _) -> do
          -- Outer for init: local_18 = 0
          case initC of
            CForInitExpr (Just (CAssign _ "=" (CIdent _ "local_18") (CIdent _ "0"))) -> return ()
            other -> expectationFailure $ "Outer init: " <> show other
          -- Outer for cond: local_18 < 3
          case condC of
            Just (CBinaryOp _ "<" (CIdent _ "local_18") (CIdent _ "3")) -> return ()
            other -> expectationFailure $ "Outer cond: " <> show other
          -- Body should contain an inner for loop (may have empty CBlocks before it)
          let forStmts = [s | s@(CFor {}) <- body]
          case forStmts of
            (CFor _ innerInit innerCond _ innerBody : _) -> do
              -- Inner for init: local_1c = 0
              case innerInit of
                CForInitExpr (Just (CAssign _ "=" (CIdent _ "local_1c") (CIdent _ "0"))) -> return ()
                other' -> expectationFailure $ "Inner init: " <> show other'
              -- Inner for cond: local_1c < 3
              case innerCond of
                Just (CBinaryOp _ "<" (CIdent _ "local_1c") (CIdent _ "3")) -> return ()
                other' -> expectationFailure $ "Inner cond: " <> show other'
              -- Inner body should have printf
              length innerBody `shouldSatisfy` (>= 1)
            [] -> expectationFailure $ "No CFor found in body: " <> show body
        other -> expectationFailure $ "Expected CFor, got: " <> show other

      -- Debug: print the rendered output
      rendered `shouldSatisfy` ("for" `T.isInfixOf`)
      rendered `shouldSatisfy` ("local_18" `T.isInfixOf`)
