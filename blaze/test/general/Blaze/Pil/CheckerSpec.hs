{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Pil.CheckerSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint, const)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.BinaryAnalysis as BA
import Blaze.Pil.Checker
import Blaze.Types.Pil.Checker hiding (signed, len)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Ctx(Ctx))
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Function as Func
import Blaze.Pil.Construct hiding (not)

import Test.Hspec


emptyTypeReport :: TypeReport
emptyTypeReport =
  TypeReport mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

spec :: Spec
spec = describe "Blaze.Pil.Checker" $ do
  context "flatToDeepSyms" $ do
    it "convert empty map" $ do
      let xs = []
          rs = []
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert type with no symbols" $ do
      let xs = [(Sym 0, TChar Nothing)]
          rs = [(Sym 0, DSType $ TChar Nothing)]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert two types with no symbols" $ do
      let xs = [ (Sym 0, TChar Nothing)
               , (Sym 1, TBottom $ Sym 1)
               ]
          rs = [ (Sym 0, DSType $ TChar Nothing)
               , (Sym 1, DSType $ TBottom (Sym 1))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert type with a symbol that has no solution" $ do
      let xs = [(Sym 0, TPointer (Just 64) (Sym 1))]
          rs = [(Sym 0, DSType $ TPointer (Just 64) (DSVar $ Sym 1))]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert type with a symbol that has solution" $ do
      let xs = [ (Sym 0, TPointer (Just 64) (Sym 1))
               , (Sym 1, TBool)
               ]
          rs = [ (Sym 0, DSType $ TPointer (Just 64) (DSType TBool))
               , (Sym 1, DSType TBool)
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert record fields" $ do
      let xs = [ (Sym 0, TRecord $ HashMap.fromList
                         [ (0, Sym 1)
                         , (8, Sym 2)
                         ])
               , (Sym 1, TChar (Just 8))
               , (Sym 2, TBottom $ Sym 1)
               ]
          rs = [ (Sym 0, DSType . TRecord
                   $ HashMap.fromList
                   [ (0, DSType $ TChar (Just 8))
                   , (8, DSType . TBottom $ Sym 1)
                   ])
               , (Sym 1, DSType $ TChar (Just 8))
               , (Sym 2, DSType $ TBottom (Sym 1))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert record with pointer in field" $ do
      let xs = [ (Sym 0, TRecord $ HashMap.fromList
                         [ (0, Sym 1)
                         , (8, Sym 2)
                         ])
               , (Sym 1, TPointer (Just 64) (Sym 3))
               , (Sym 2, TBottom $ Sym 1)
               , (Sym 3, TChar (Just 8))
               ]
          rs = [ (Sym 0, DSType . TRecord
                   $ HashMap.fromList
                   [ (0, DSType $ TPointer (Just 64) (DSType $ TChar (Just 8)))
                   , (8, DSType $ TBottom $ Sym 1)
                   ])
               , (Sym 1, DSType $ TPointer (Just 64) (DSType $ TChar (Just 8)))
               , (Sym 2, DSType $ TBottom (Sym 1))
               , (Sym 3, DSType $ TChar (Just 8))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert type with 2 layers of nesting" $ do
      let xs = [ (Sym 0, TBitVector (Just 32))
               , (Sym 2, TPointer (Just 64) (Sym 0))
               ]
          rs = [ (Sym 0, DSType $ TBitVector (Just 32))
               , (Sym 2, DSType $ TPointer (Just 64) (DSType $ TBitVector (Just 32)))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "nested type that has another nested type" $ do
      let xs = [ (Sym 0, TBitVector (Just 32))
               , (Sym 2, TPointer (Just 64) (Sym 0))
               , (Sym 4, TPointer (Just 64) (Sym 2))
               ]
          rs = [ (Sym 0, DSType $ TBitVector (Just 32))
               , (Sym 2, DSType $ TPointer (Just 64) (DSType $ TBitVector (Just 32)))
               , (Sym 4, DSType $
                   TPointer (Just 64) (DSType $
                                       TPointer (Just 64)
                                       (DSType $ TBitVector (Just 32))))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert self-referencing type" $ do
      let xs = [ (Sym 0, TPointer (Just 64) (Sym 0)) ]
          rs = [ (Sym 0, DSRecursive (Sym 0) (TPointer (Just 64) (DSVar $ Sym 0)))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert nested self-referencing type 0" $ do
      let xs = [ (Sym 0, TPointer (Just 64) (Sym 0))
               , (Sym 3, TPointer (Just 64) (Sym 0))
               ]
          rs = [ (Sym 0, DSRecursive (Sym 0) (TPointer (Just 64) (DSVar $ Sym 0)))
               , (Sym 3, DSType $
                   TPointer
                   (Just 64)
                   (DSRecursive (Sym 0) (TPointer (Just 64) (DSVar $ Sym 0))))
               ]

      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert nested self-referencing type inside another recursive type" $ do
      let xs = [ (Sym 0, TPointer (Just 64) (Sym 0))
               , (Sym 2, TRecord $ HashMap.fromList
                         [ (0, Sym 2)
                         , (32, Sym 0)
                         ])
               ]

          rs = [ (Sym 0, DSRecursive (Sym 0)
                   $ TPointer (Just 64) (DSVar $ Sym 0))
               , (Sym 2, DSRecursive (Sym 2) . TRecord
                   $ HashMap.fromList
                   [ (0, DSVar $ Sym 2)
                   , (32, DSType
                       $ TPointer (Just 64) (DSVar $ Sym 0))
                   ])
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert nested self-referencing type 1" $ do
      let xs = [ (Sym 0, TPointer (Just 64) (Sym 2))
               , (Sym 2, TPointer (Just 64) (Sym 0))
               ]

          rs = [ (Sym 0, DSRecursive (Sym 0)
                   . TPointer (Just 64)
                   . DSType $ TPointer (Just 64) (DSVar $ Sym 0))
               , (Sym 2, DSRecursive (Sym 2)
                   . TPointer (Just 64)
                   . DSType $ TPointer (Just 64) (DSVar $ Sym 2))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


  context "checkStmts" $ do
    let pv = pilVar
        mkVarSymTypeMap = HashMap.fromList
        checkVars = fmap (view #varSymTypeMap) . checkStmts Nothing
        -- checkSolutions = fmap (view solutions) . checkStmts
        -- checkVarEqMap = fmap (view varEqMap) . checkStmts
        -- signed = DSType $ TVSign True
        -- unsigned = DSType $ TVSign False
        bw :: Word64 -> Maybe BitWidth
        bw = Just . Bits
        -- len = DSType . TVLength
        -- arr a b = DSType $ TArray a b
        -- ptr = DSType . TPointer (bw 64)
        -- char = DSType TChar
        -- cf = DSType . TContainsFirst
        record :: [(BitOffset, DeepSymType)] -> PilType DeepSymType
        record = TRecord . HashMap.fromList

    it "simple def const statement" $ do
      let stmts = [def "a" $ const 888 4]

          rvars = [(pv 4 "a", DSType $ TBitVector (bw 32))]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "def sub statement" $ do
      let stmts = [def "a" $ sub (const 888 4) (const 999 4) 4]

          rvars = [(pv 4 "a", DSType (TInt (bw 32) Nothing))]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "pointer" $ do
      let stmts = [ def "a" $ const 888 8
                  , def "b" $ load (var "a" 8) 4
                  ]

          btype = DSType $ TBitVector (bw 32)
          rvars = [ (pv 8 "a", DSType (TPointer (bw 64) btype))
                  , (pv 4 "b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "preserves type of pointee" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (var "a" 8) (var "pointee" 4)
                  , def "b" $ load (var "a" 8) 4
                  ]

          btype = DSType $ TInt (bw 32) Nothing
          rvars = [ (pv 8 "a", DSType (TPointer (bw 64) btype))
                  , (pv 4 "pointee", btype)
                  , (pv 4 "b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "preserves type of pointee for fieldAddr" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (fieldAddr (var "a" 8) 0 8) (var "pointee" 4)
                  , def "b" $ load (fieldAddr (var "a" 8) 0 8) 4
                  ]

          btype = DSType $ TInt (bw 32) Nothing
          rectype = DSType . TRecord
                    . HashMap.fromList
                    $ [( BitOffset 0, btype )]
          rvars = [ (pv 8 "a", DSType (TPointer (bw 64) rectype))
                  , (pv 4 "pointee", btype)
                  , (pv 4 "b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "preserves type of pointee for stackLocalAddr" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (fieldAddr (var "a" 8) 0 8) (var "pointee" 4)
                  , def "b" $ load (fieldAddr (var "a" 8) 0 8) 4
                  ]

          btype = DSType $ TInt (bw 32) Nothing
          rectype = DSType . TRecord
                    . HashMap.fromList
                    $ [( BitOffset 0, btype )]
          rvars = [ (pv 8 "a", DSType (TPointer (bw 64) rectype))
                  , (pv 4 "pointee", btype)
                  , (pv 4 "b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

--- Records

    it "record with single field at offset 24 bytes" $ do
      let stmts = [ def "b" $ load (fieldAddr (var "a" 8) 24 8) 4
                  ]

          btype = DSType $ TBitVector (bw 32)
          rvars = [ (pv 8 "a", DSType (TPointer (bw 64)
                                  (DSType $ record [(24 * 8, btype)])))
                  , (pv 4 "b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "two records with single fields at same offset" $ do
      let stmts = [ def "b" $ load (fieldAddr (var "rec_ptr" 8) 24 8) 4
                  , def "c" $ load (fieldAddr (var "rec_ptr" 8) 24 8) 4
                  , def "d" $ sub (var "b" 4) (const 888 4) 4
                  ]

          btype = DSType $ TInt (bw 32) Nothing
          rvars = [ (pv 8 "rec_ptr", DSType (TPointer (bw 64)
                                        (DSType $ record [(24 * 8, btype)])))
                  , (pv 4 "b", btype)
                  , (pv 4 "c", btype)
                  , (pv 4 "d", btype)
                  ]

      PShow (checkVars stmts) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))

    it "two records with single fields at different offsets" $ do
      let stmts = [ def "f1" $ load (fieldAddr (var "rec_ptr" 8) 0 8) 4
                  , def "f2" $ load (fieldAddr (var "rec_ptr" 8) 8 8) 8
                  ]

          f1 = DSType $ TBitVector (bw 32)
          f2 = DSType $ TBitVector (bw 64)
          rvars = [ (pv 8 "rec_ptr", DSType (TPointer (bw 64)
                                        (DSType $ record [ (0 * 8, f1)
                                                         , (8 * 8, f2)])))
                  , (pv 4 "f1", f1)
                  , (pv 8 "f2", f2)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)


    it "linked list style recursive record" $ do
      let stmts = [ def "elem" $ load (fieldAddr (var "rec_ptr" 8) 0 8) 4
                  , def "rec_ptr" $ load (fieldAddr (var "rec_ptr" 8) 8 8) 8
                  ]

          elemType' = DSType $ TBitVector (bw 32)
          rvars = [ ( pv 4 "elem", elemType' )
                  , ( pv 8 "rec_ptr"
                    , DSRecursive (Sym 11)
                      (TPointer (bw 64)
                       (DSType $ record [ (0 * 8, elemType')
                                        , (8 * 8, DSVar $ Sym 11)])))
                  ]

      PShow (checkVars stmts) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))

  context "getRootFunctionParamInfo" $ do
    let params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.Unknown ]
        ctxFunc = Function (Just $ BA.Symbol "foo" "foo") "foo" 0xf00 params
        ctx = Ctx ctxFunc 0
        paramInfo = getRootFunctionParamInfo ctx
        callTarget = Pil.CallTarget { dest = Pil.CallFunc ctxFunc }
        expected = RootFunctionParamInfo
          { rootCtx = ctx
          , rootParamMap = HashMap.fromList
                           [ ("arg1", Pil.FuncParam callTarget $ Pil.ParamPosition 1) ]
          }
          
    it "should generate the correct root param mapping" $ do
        paramInfo `shouldBe` expected
        
  context "addAllConstraints" $ do
    let params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.Unknown ]
        ctxFunc = Function (Just $ BA.Symbol "foo" "foo") "foo" 0xf00 params
        ctx = Ctx ctxFunc 0
        pvX = pilVar' 8 ctx "x"
        pvArg1 = pilVar' 8 ctx "arg1"
        rootFuncParamInfo = getRootFunctionParamInfo ctx
        mArg1FuncVar = HashMap.lookup "arg1" $ rootFuncParamInfo ^. #rootParamMap

    it "Should get root function param info for single arg" $ do
      isJust mArg1FuncVar `shouldBe` True

    let arg1FuncVar = fromJust mArg1FuncVar
        cgCtx = emptyConstraintGenCtx
                & #rootFunctionParamInfo ?~ rootFuncParamInfo

    context "Linking root param to PilVar" $ do
      let stmts = [ (0, def' pvX (var' pvArg1 4)) ]
          (_stmts, csState) = unsafeFromRight $ addAllConstraints_ cgCtx stmts
          funcSymMap = csState ^. #funcSymMap
        
      it "should contain single FuncVar in the funcSymMap" $ do
        HashMap.keys funcSymMap `shouldBe` [arg1FuncVar]

      let arg1FuncVarSym = fromJust . HashMap.lookup arg1FuncVar $ csState ^. #funcSymMap
          arg1PilVarSym = fromJust . HashMap.lookup pvArg1 $ csState ^. #varSymMap
          arg1Eq (Constraint 0 s1 (SVar s2)) =
            s1 == arg1FuncVarSym && s2 == arg1PilVarSym
            ||
            s2 == arg1FuncVarSym && s1 == arg1PilVarSym
          arg1Eq _ = False
          equalityConstraints = filter arg1Eq $ csState ^. #constraints

      it "should have a constraint equality between arg1 pilvar and funcvar" $ do
        length equalityConstraints `shouldBe` 1

    context "Linking root param to recursive CALL expr params" $ do
      let pvY = pilVar' 4 ctx "y"
          stmts = [ (0, def' pvX (add (var' pvArg1 4) (const 1 4) 4))
                  , (1, defCall' pvY (Pil.CallFunc ctxFunc) [var' pvX 4] 4)
                  ]
          (_stmts, csState) = unsafeFromRight $ addAllConstraints_ cgCtx stmts
          funcSymMap = csState ^. #funcSymMap
          isFuncParam (Pil.FuncParam _ _) = True
          isFuncParam _ = False

      it "should contain single FuncParam in the funcSymMap" $ do
        filter isFuncParam (HashMap.keys funcSymMap) `shouldBe` [arg1FuncVar]

      let arg1FuncVarSym = fromJust . HashMap.lookup arg1FuncVar $ csState ^. #funcSymMap
          arg1PilVarSym = fromJust . HashMap.lookup pvArg1 $ csState ^. #varSymMap
          xPilVarSym = fromJust . HashMap.lookup pvX $ csState ^. #varSymMap
          onlySymToSym (Constraint _ a (SVar b)) = Just (a, b)
          onlySymToSym _ = Nothing
          symToSymConstraints = mapMaybe onlySymToSym $ csState ^. #constraints
          xPilVarSymLinksToArg1FuncVarSym = not . null $ do
            (a, b) <- symToSymConstraints
            (c, d) <- symToSymConstraints
            -- TODO: maybe add more permutations to make this guard more robust
            guard $ b == c && d == xPilVarSym && a == arg1FuncVarSym
            return (xPilVarSym, arg1FuncVarSym)
      
      it "should have a constraint equality between funcvar and callsite arg x" $ do
        xPilVarSymLinksToArg1FuncVarSym `shouldBe` True

      let arg1Eq (Constraint 0 s1 (SVar s2)) =
            s1 == arg1FuncVarSym && s2 == arg1PilVarSym
            ||
            s2 == arg1FuncVarSym && s1 == arg1PilVarSym
          arg1Eq _ = False
          arg1EqualityConstraints = filter arg1Eq $ csState ^. #constraints

      it "should have a constraint equality between arg1 pilvar and funcvar" $ do
        length arg1EqualityConstraints `shouldBe` 1

    context "Linking root param to recursive Call statement params" $ do
      let stmts = [ (0, def' pvX (add (var' pvArg1 4) (const 1 4) 4))
                  , (1, callStmt (Pil.CallFunc ctxFunc) [var' pvX 4])
                  ]
          (_stmts, csState) = unsafeFromRight $ addAllConstraints_ cgCtx stmts
          funcSymMap = csState ^. #funcSymMap
          isFuncParam (Pil.FuncParam _ _) = True
          isFuncParam _ = False

      it "should contain single FuncParam in the funcSymMap" $ do
        filter isFuncParam (HashMap.keys funcSymMap) `shouldBe` [arg1FuncVar]

      let arg1FuncVarSym = fromJust . HashMap.lookup arg1FuncVar $ csState ^. #funcSymMap
          arg1PilVarSym = fromJust . HashMap.lookup pvArg1 $ csState ^. #varSymMap
          xPilVarSym = fromJust . HashMap.lookup pvX $ csState ^. #varSymMap
          onlySymToSym (Constraint _ a (SVar b)) = Just (a, b)
          onlySymToSym _ = Nothing
          symToSymConstraints = mapMaybe onlySymToSym $ csState ^. #constraints
          xPilVarSymLinksToArg1FuncVarSym = not . null $ do
            (a, b) <- symToSymConstraints
            (c, d) <- symToSymConstraints
            -- TODO: maybe add more permutations to make this guard more robust
            guard $ b == c && d == xPilVarSym && a == arg1FuncVarSym
            return (xPilVarSym, arg1FuncVarSym)
      
      it "should have a constraint equality between funcvar and callsite arg x" $ do
        xPilVarSymLinksToArg1FuncVarSym `shouldBe` True

      let arg1Eq (Constraint 0 s1 (SVar s2)) =
            s1 == arg1FuncVarSym && s2 == arg1PilVarSym
            ||
            s2 == arg1FuncVarSym && s1 == arg1PilVarSym
          arg1Eq _ = False
          arg1EqualityConstraints = filter arg1Eq $ csState ^. #constraints

      it "should have a constraint equality between arg1 pilvar and funcvar" $ do
        length arg1EqualityConstraints `shouldBe` 1
