{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.Matcher.Primitives.LibrarySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep, mkPathPrep)
import Flint.Types.Analysis.Path.Matcher.Primitives (PrimSpec, CallableWMI, FuncVarExpr)
import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec as PrimSpec
import Flint.Analysis.Path.Matcher.Primitives.Library.StdLib (allStdLibPrims)
import Flint.Query (checkPathForPrim_)
import Flint.Types.Symbol (Symbol)

import qualified Blaze.Pil.Construct as C
import Blaze.Types.Function (ExternFunction, Function, Func)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil

import qualified Data.BinaryAnalysis as BA

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


dummyAddressSpace :: BA.AddressSpace
dummyAddressSpace = BA.AddressSpace 8 8 BA.Ram

mkFuncAddress :: Int64 -> Address
mkFuncAddress = BA.Address dummyAddressSpace

mkFunc :: Text -> Int -> Function
mkFunc name numArgs = Func.Function Nothing name (mkFuncAddress 0)
  . fmap mkParam
  $ [0..(numArgs - 1)]
  where
    mkParam n = Func.FuncParamInfo (Func.ParamInfo ("arg" <> show (n + 1)) (Just 8) Func.Unknown)

mkExternFunc :: Text -> Int -> ExternFunction
mkExternFunc name numArgs = Func.ExternFunction Nothing name Nothing (mkFuncAddress 0)
  . fmap mkParam
  $ [0..(numArgs - 1)]
  where
    mkParam n = Func.FuncParamInfo (Func.ParamInfo ("arg" <> show (n + 1)) (Just 8) Func.Unknown)

spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Primitives.Library" $ do
  let testMatch
        :: HashMap (PrimSpec, Func) (HashSet CallableWMI)
        -> Prim
        -> Function
        -> [Pil.Stmt]
        -> [CallableWMI]
      testMatch initialWMIs prim func stmts = runIdentity $ checkPathForPrim_ 10 mctx mstate' func (pathPrep ^. #codeSummary) prim
        where
          
          pathPrep :: PathPrep TypedStmt
          pathPrep = let pp = (mkPathPrep [] stmts :: PathPrep TypedStmt) in
            pp & #stmts .~ (pp ^. #untouchedStmts)
          mctx :: MatcherCtx TypedStmt Identity
          mstate :: MatcherState TypedExpr TypedStmt
          (mctx, mstate) = mkMatcherState dummySolver pathPrep
          mstate' = mstate & #callablePrimitives .~ initialWMIs
          
      testMatchVars
        :: HashMap (PrimSpec, Func) (HashSet CallableWMI)
        -> Prim
        -> Function
        -> [Pil.Stmt]
        -> [HashMap (Symbol Pil.Expression) FuncVarExpr]
      testMatchVars initialWMIs prim func = fmap (fmap fst . view #varMapping) . testMatch initialWMIs prim func

      paramVar :: Pil.Symbol -> Pil.Size Pil.Expression -> Pil.Expression
      paramVar varName sz = C.var' (C.pilVar__ (coerce sz) Nothing Nothing varName True Pil.UnknownLocation) sz

  context "freeHeap" $ do
    let prim = PrimLib.freeHeapPrim
        freeFunc = mkExternFunc "free" 1
        freeCallDest = Pil.CallExtern freeFunc
        initialWMIs = Prim.getInitialWMIs allStdLibPrims [Func.External freeFunc]
    it "should properly populate initial WMIs" $ do
      let k = (PrimSpec.freeHeapSpec, Func.External freeFunc)
      HashMap.keys initialWMIs `shouldBe` [k]
      HashSet.size (fromJust $ HashMap.lookup k initialWMIs) `shouldNotBe` 0

    it "should detect simple call to free" $ do
      let ptr = paramVar "arg1" 8
          func = mkFunc "foo" 1
          stmts = [ C.defCall "x" freeCallDest [ ptr ] 8 ] :: [Pil.Stmt]
          expected =
            [ HashMap.fromList
              [ ("ptr", Prim.FuncVar $ Prim.Arg 0) ]
            ]
      testMatchVars initialWMIs prim func stmts `shouldBe` expected

  context "allocHeap" $ do
    let prim = PrimLib.allocHeapPrim
        allocFunc = mkExternFunc "malloc" 2
        allocCallDest = Pil.CallExtern allocFunc
        initialWMIs = Prim.getInitialWMIs allStdLibPrims [Func.External allocFunc]
    it "should properly populate initial WMIs" $ do
      let k = (PrimSpec.allocHeapSpec, Func.External allocFunc)
      HashMap.keys initialWMIs `shouldBe` [k]
      HashSet.size (fromJust $ HashMap.lookup k initialWMIs) `shouldNotBe` 0

    it "should detect simple call to alloc that returns ptr" $ do
      let ptr = C.pilVar 8 "x"
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall' ptr allocCallDest [ sz ] 8
                  , C.ret $ C.var' ptr 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("ptr", Prim.FuncVar Prim.Ret)
              , ("size", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect alloc that copies pointer to arg" $ do
      let ptr = C.pilVar 8 "x"
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall' ptr allocCallDest [ sz ] 8
                  , C.store (paramVar "arg1" 8) (C.var' ptr 8)
                  , C.ret $ C.const 1 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("ptr", Prim.FuncVar (Prim.Arg 0))
              , ("size", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect alloc that copies pointer to field of arg" $ do
      let ptr = C.pilVar 8 "x"
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall' ptr allocCallDest [ sz ] 8
                  , C.store (C.add (paramVar "arg1" 8) (C.const 12 8) 8) (C.var' ptr 8)
                  , C.ret $ C.const 1 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("ptr", C.add (Prim.FuncVar (Prim.Arg 0)) (C.const 12 $ Prim.ConstSize 8) (Prim.ConstSize 8))
              , ("size", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect alloc that copies pointer to field of global" $ do
      let ptr = C.pilVar 8 "x"
          func = mkFunc "foo" 1
          sz = C.const 17 8
          biggs = C.globalPtr 0x1234 (Just "biggs") 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall' ptr allocCallDest [ sz ] 8
                  , C.store (C.add biggs (C.const 12 8) 8) (C.var' ptr 8)
                  , C.ret $ C.const 1 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("ptr", C.add (Prim.FuncVar (Prim.Global biggs)) (C.const 12 $ Prim.ConstSize 8) (Prim.ConstSize 8))
              , ("size", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should not match anything if ptr is not returned in some way" $ do
      let ptr = C.pilVar 8 "x"
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall' ptr allocCallDest [ sz ] 8
                  , C.ret $ C.const 23 8
                  ]
          expected = []
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

  context "copyMem" $ do
    let prim = PrimLib.copyMemPrim
        memcpyFunc = mkExternFunc "memcpy" 3
        memcpyCallDest = Pil.CallExtern memcpyFunc
        initialWMIs = Prim.getInitialWMIs allStdLibPrims [Func.External memcpyFunc]

    it "should properly populate initial WMIs" $ do
      let k = (PrimSpec.copyMemSpec, Func.External memcpyFunc)
      HashMap.keys initialWMIs `shouldBe` [k]
      HashSet.size (fromJust $ HashMap.lookup k initialWMIs) `shouldNotBe` 0

    it "should detect simple call to memcpy that returns ptr" $ do
      let dest_ptr = C.var "x" 8
          src_ptr = paramVar "arg1" 8
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall "r" memcpyCallDest [dest_ptr, src_ptr, sz] 8
                  , C.ret dest_ptr
                  ]
          expected =
            [ HashMap.fromList
              [ ("dest_ptr", Prim.FuncVar Prim.Ret)
              , ("src_ptr", Prim.FuncVar (Prim.Arg 0))
              , ("len", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect simple call to memcpy that copies ptr to arg/global" $ do
      let dest_ptr = C.var "x" 8
          src_ptr = paramVar "arg1" 8
          biggs = C.globalPtr 0x1234 (Just "biggs") 8
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall "r" memcpyCallDest [dest_ptr, src_ptr, sz] 8
                  , C.store (C.add biggs (C.const 12 8) 8) dest_ptr
                  , C.ret $ C.const 1 8
                  ]
          expected :: [HashMap (Symbol Pil.Expression) FuncVarExpr]
          expected =
            [ HashMap.fromList
            
              [ ("dest_ptr", C.add (Prim.FuncVar (Prim.Global biggs)) (C.const 12 $ Prim.ConstSize 8) (Prim.ConstSize 8))
              , ("src_ptr", Prim.FuncVar (Prim.Arg 0))
              , ("len", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect STORE from input ptr to ptr that is returned" $ do
      let dest_ptr = C.var "x" 8
          src_ptr = paramVar "arg1" 8
          func = mkFunc "foo" 1
          stmts :: [Pil.Stmt]
          stmts = [ C.store dest_ptr (C.load src_ptr 8)
                  , C.ret dest_ptr
                  ]
          expected =
            [ HashMap.fromList
              [ ("dest_ptr", Prim.FuncVar Prim.Ret)
              , ("src_ptr", Prim.FuncVar (Prim.Arg 0))
              , ("len", C.const 8 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

    it "should detect simple call to memcpy that writes to global or arg ptr" $ do
      let dest_ptr = C.globalPtr 0x1234 (Just "x") 8
          src_ptr = paramVar "arg1" 8
          func = mkFunc "foo" 1
          sz = C.const 17 8
          stmts :: [Pil.Stmt]
          stmts = [ C.defCall "r" memcpyCallDest [dest_ptr, src_ptr, sz] 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("dest_ptr", Prim.FuncVar (Prim.Global dest_ptr))
              , ("src_ptr", Prim.FuncVar (Prim.Arg 0))
              , ("len", C.const 17 (Prim.ConstSize 8))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

  context "copyPtr" $ do
    let prim = PrimLib.copyPtrPrim
        initialWMIs = HashMap.empty

    it "should detect a STORE that copies an input ptr to a global" $ do
      let dest_ptr = C.globalPtr 0x1234 (Just "x") 8
          src_ptr = paramVar "arg1" 8
          func = mkFunc "foo" 1
          stmts :: [Pil.Stmt]
          stmts = [ -- this line helps type checker infer src_ptr is a ptr
                    C.def "r" $ C.load src_ptr 8 
                  , C.store dest_ptr src_ptr
                  , C.ret $ C.const 1 8
                  ]
          expected =
            [ HashMap.fromList
              [ ("dest", Prim.FuncVar (Prim.Global dest_ptr))
              , ("copied_ptr", Prim.FuncVar (Prim.Arg 0))
              ]
            ]
      PShow (testMatchVars initialWMIs prim func stmts) `shouldBe` PShow expected

  context "danglingPtr" $ do
    let prim = PrimLib.danglingPtrPrim
        freeFunc = mkExternFunc "free" 1
        freeCallDest = Pil.CallExtern freeFunc
        initialWMIs = Prim.getInitialWMIs allStdLibPrims [Func.External freeFunc]
    it "should properly populate initial WMIs" $ do
      let k = (PrimSpec.freeHeapSpec, Func.External freeFunc)
      HashMap.keys initialWMIs `shouldBe` [k]
      HashSet.size (fromJust $ HashMap.lookup k initialWMIs) `shouldNotBe` 0

    it "should detect dangling ptr where freed ptr is a deref and is not nulled out before return" $ do
      let ptr = paramVar "arg1" 8
          fullPtr = C.add ptr (C.const 52 8) 8
          func = mkFunc "foo" 1
          stmts = [ C.defCall "x" freeCallDest [ C.load fullPtr 8 ] 8
                  , C.def "y" (C.const 88 8)
                  , C.ret (C.const 0 8)
                  ] :: [Pil.Stmt]
          expected =
            [ HashMap.fromList
              [ ("ptr", C.add (Prim.FuncVar (Prim.Arg 0)) (C.const 52 $ Prim.ConstSize 8) (Prim.ConstSize 8))
              ]
            ]
      testMatchVars initialWMIs prim func stmts `shouldBe` expected

    it "should not detect dangling ptr where freed ptr is a deref and is nulled out before return" $ do
      let ptr = paramVar "arg1" 8
          fullPtr = C.add ptr (C.const 52 8) 8
          func = mkFunc "foo" 1
          stmts = [ C.defCall "x" freeCallDest [ C.load fullPtr 8 ] 8
                  , C.store fullPtr $ C.const 0 8
                  , C.def "y" (C.const 88 8)
                  , C.ret (C.const 0 8)
                  ] :: [Pil.Stmt]
          expected = []
      testMatchVars initialWMIs prim func stmts `shouldBe` expected
