{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Pil.CheckerSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint, const)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Pil.Checker
import Blaze.Types.Pil.Checker hiding (signed, len)
import Blaze.Pil.Construct

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
      let xs = [(Sym 0, TChar)]
          rs = [(Sym 0, DSType TChar)]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert two types with no symbols" $ do
      let xs = [ (Sym 0, TChar)
               , (Sym 1, TBottom $ Sym 1)
               ]
          rs = [ (Sym 0, DSType TChar)
               , (Sym 1, DSType . TBottom $ Sym 1)
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert type with a symbol that has no solution" $ do
      let xs = [(Sym 0, TBitVector (Sym 1))]
          rs = [(Sym 0, DSType $ TBitVector (DSVar $ Sym 1))]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert type with a symbol that has solution" $ do
      let xs = [ (Sym 0, TBitVector (Sym 1))
               , (Sym 1, TVBitWidth 64)
               ]
          rs = [ (Sym 0, DSType $ TBitVector (DSType $ TVBitWidth 64))
               , (Sym 1, DSType $ TVBitWidth 64)
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert record fields" $ do
      let xs = [ (Sym 0, TRecord $ HashMap.fromList
                         [ (0, Sym 1)
                         , (8, Sym 2)
                         ])
               , (Sym 1, TChar)
               , (Sym 2, TBottom $ Sym 1)
               ]
          rs = [ (Sym 0, DSType . TRecord
                   $ HashMap.fromList
                   [ (0, DSType TChar)
                   , (8, DSType . TBottom $ Sym 1)
                   ])
               , (Sym 1, DSType TChar)
               , (Sym 2, DSType . TBottom $ Sym 1)
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert record with pointer in field" $ do
      let xs = [ (Sym 0, TRecord $ HashMap.fromList
                         [ (0, Sym 1)
                         , (8, Sym 2)
                         ])
               , (Sym 1, TPointer (Sym 4) (Sym 3))
               , (Sym 2, TBottom $ Sym 1)
               , (Sym 3, TChar)
               ]
          rs = [ (Sym 0, DSType . TRecord
                   $ HashMap.fromList
                   [ (0, DSType . TPointer (DSVar $ Sym 4) . DSType $ TChar)
                   , (8, DSType . TBottom $ Sym 1)
                   ])
               , (Sym 1, DSType . TPointer (DSVar $ Sym 4) . DSType $ TChar)
               , (Sym 2, DSType . TBottom $ Sym 1)
               , (Sym 3, DSType TChar)
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert type with 2 layers of nesting" $ do
      let xs = [ (Sym 0, TBitVector (Sym 1))
               , (Sym 1, TVBitWidth 64)
               , (Sym 2, TPointer (Sym 3) (Sym 0))
               ]
          rs = [ (Sym 0, DSType $ TBitVector (DSType $ TVBitWidth 64))
               , (Sym 1, DSType $ TVBitWidth 64)
               , (Sym 2, DSType . TPointer (DSVar $ Sym 3)
                         . DSType $ TBitVector (DSType $ TVBitWidth 64))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "nested type that has another nested type" $ do
      let xs = [ (Sym 0, TBitVector (Sym 1))
               , (Sym 1, TVBitWidth 64)
               , (Sym 2, TPointer (Sym 3) (Sym 0))
               , (Sym 4, TPointer (Sym 5) (Sym 2))
               ]
          rs = [ (Sym 0, DSType $ TBitVector (DSType $ TVBitWidth 64))
               , (Sym 1, DSType $ TVBitWidth 64)
               , (Sym 2, DSType . TPointer (DSVar $ Sym 3)
                         . DSType $ TBitVector (DSType $ TVBitWidth 64))
               , (Sym 4, DSType . TPointer (DSVar $ Sym 5)
                         . DSType . TPointer (DSVar $ Sym 3)
                         . DSType $ TBitVector (DSType $ TVBitWidth 64))
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert self-referencing type" $ do
      let xs = [ (Sym 0, TPointer (Sym 1) (Sym 0)) ]
          rs = [ (Sym 0, DSRecursive (Sym 0)
                   $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 0))
               ]
          -- rs = [ (Sym 0, DSType
          --          $ TPointer (DSVar $ Sym 1) (DSRecursive $ Sym 0))
          --      ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert nested self-referencing type 0" $ do
      let xs = [ (Sym 0, TPointer (Sym 1) (Sym 0))
               , (Sym 3, TPointer (Sym 2) (Sym 0))
               ]
          rs = [ (Sym 0, DSRecursive (Sym 0)
                   $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 0))
               , (Sym 3, DSType . TPointer (DSVar $ Sym 2)
                   . DSRecursive (Sym 0)
                   $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 0))
               ]

      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs

    it "convert nested self-referencing type inside another recursive type" $ do
      let xs = [ (Sym 0, TPointer (Sym 1) (Sym 0))
               , (Sym 2, TRecord $ HashMap.fromList
                         [ (0, Sym 2)
                         , (32, Sym 0)
                         ])
               ]

          rs = [ (Sym 0, DSRecursive (Sym 0)
                   $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 0))
               , (Sym 2, DSRecursive (Sym 2) . TRecord
                   $ HashMap.fromList
                   [ (0, DSVar $ Sym 2)
                   , (32, DSType
                       $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 0))
                   ])
               ]
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


    it "convert nested self-referencing type 1" $ do
      let xs = [ (Sym 0, TPointer (Sym 1) (Sym 2))
               , (Sym 2, TPointer (Sym 3) (Sym 0))
               ]

          rs = [ (Sym 0, DSRecursive (Sym 0)
                   . TPointer (DSVar $ Sym 1)
                   . DSType $ TPointer (DSVar $ Sym 3) (DSVar $ Sym 0))
               , (Sym 2, DSRecursive (Sym 2)
                   . TPointer (DSVar $ Sym 3)
                   . DSType $ TPointer (DSVar $ Sym 1) (DSVar $ Sym 2))
               ] 
      flatToDeepSyms (HashMap.fromList xs) `shouldBe` HashMap.fromList rs


  context "checkStmts" $ do
    let pv = pilVar
        mkVarSymTypeMap = HashMap.fromList . fmap (over _1 pv)
        checkVars = fmap (view #varSymTypeMap) . checkStmts
        -- checkSolutions = fmap (view solutions) . checkStmts
        -- checkVarEqMap = fmap (view varEqMap) . checkStmts
        signed = DSType $ TVSign True
        -- unsigned = DSType $ TVSign False
        bw = DSType . TVBitWidth
        -- len = DSType . TVLength
        -- arr a b = DSType $ TArray a b
        -- ptr = DSType . TPointer (bw 64)
        -- char = DSType TChar
        -- cf = DSType . TContainsFirst
        record :: [(BitOffset, DeepSymType)] -> PilType DeepSymType
        record = TRecord . HashMap.fromList

    it "simple def const statement" $ do
      let stmts = [def "a" $ const 888 4]

          rvars = [("a", DSType $ TBitVector (bw 32))]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "def sub statement" $ do
      let stmts = [def "a" $ sub (const 888 4) (const 999 4) 4]

          rvars = [("a", DSType (TInt (bw 32) signed))]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "pointer" $ do
      let stmts = [ def "a" $ const 888 8
                  , def "b" $ load (var "a" 8) 4
                  ]

          -- ztype = DSType $ TZeroField btype
          btype = DSType $ TBitVector (bw 32)
          rvars = [ ("a", DSType (TPointer (bw 64) btype))
                  , ("b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "preserves type of pointee" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (var "a" 8) (var "pointee" 4)
                  , def "b" $ load (var "a" 8) 4
                  ]

          -- ztype = DSType $ TZeroField btype
          btype = DSType $ TInt (bw 32) signed
          rvars = [ ("a", DSType (TPointer (bw 64) btype))
                  , ("pointee", btype)
                  , ("b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "preserves type of pointee for fieldAddr" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (fieldAddr (var "a" 8) 0 8) (var "pointee" 4)
                  , def "b" $ load (fieldAddr (var "a" 8) 0 8) 4
                  ]

          -- ztype = DSType $ TZeroField btype
          btype = DSType $ TInt (bw 32) signed
          rectype = DSType . TRecord
                    . HashMap.fromList
                    $ [( BitOffset 0, btype )]
          rvars = [ ("a", DSType (TPointer (bw 64) rectype))
                  , ("pointee", btype)
                  , ("b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)
      
    it "preserves type of pointee for stackLocalAddr" $ do
      let stmts = [ def "pointee" $ sub (const 888 4) (const 333 4) 4
                  , store (fieldAddr (var "a" 8) 0 8) (var "pointee" 4)
                  , def "b" $ load (fieldAddr (var "a" 8) 0 8) 4
                  ]

          -- ztype = DSType $ TZeroField btype
          btype = DSType $ TInt (bw 32) signed
          rectype = DSType . TRecord
                    . HashMap.fromList
                    $ [( BitOffset 0, btype )]
          rvars = [ ("a", DSType (TPointer (bw 64) rectype))
                  , ("pointee", btype)
                  , ("b", btype)
                  ]
      
      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

----------- records

    it "record with single field at offset 24 bytes" $ do
      let stmts = [ def "b" $ load (fieldAddr (var "a" 8) 24 8) 4
                  ]

          btype = DSType $ TBitVector (bw 32)
          rvars = [ ("a", DSType (TPointer (bw 64)
                                  (DSType $ record [(24 * 8, btype)])))
                  , ("b", btype)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)

    it "two records with single fields at same offset" $ do
      let stmts = [ def "b" $ load (fieldAddr (var "rec_ptr" 8) 24 8) 4
                  , def "c" $ load (fieldAddr (var "rec_ptr" 8) 24 8) 4
                  , def "d" $ sub (var "b" 4) (const 888 4) 4
                  ]

          btype = DSType $ TInt (bw 32) signed
          rvars = [ ("rec_ptr", DSType (TPointer (bw 64)
                                        (DSType $ record [(24 * 8, btype)])))
                  , ("b", btype)
                  , ("c", btype)
                  , ("d", btype)
                  ]

      PShow (checkVars stmts) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))

    it "two records with single fields at different offsets" $ do
      let stmts = [ def "f1" $ load (fieldAddr (var "rec_ptr" 8) 0 8) 4
                  , def "f2" $ load (fieldAddr (var "rec_ptr" 8) 8 8) 8
                  ]

          f1 = DSType $ TBitVector (bw 32)
          f2 = DSType $ TBitVector (bw 64)
          rvars = [ ("rec_ptr", DSType (TPointer (bw 64)
                                        (DSType $ record [ (0 * 8, f1)
                                                         , (8 * 8, f2)])))
                  , ("f1", f1)
                  , ("f2", f2)
                  ]

      checkVars stmts `shouldBe` Right (mkVarSymTypeMap rvars)


    it "linked list style recursive record" $ do
      let stmts = [ def "elem" $ load (fieldAddr (var "rec_ptr" 8) 0 8) 4
                  , def "rec_ptr" $ load (fieldAddr (var "rec_ptr" 8) 8 8) 8
                  ]

          elemType' = DSType $ TBitVector (bw 32)
          rvars = [ ( "elem", elemType' )
                  , ( "rec_ptr"
                    , DSRecursive (Sym 17)
                      (TPointer (bw 64)
                       (DSType $ record [ (0 * 8, elemType')
                                        , (8 * 8, DSVar $ Sym 17)])))
                  ]

      PShow (checkVars stmts) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))    

--       PShow (checkVars stmts) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))

--     it "cleanDeepSymType nested ptrs" $ do
--       let dst = ptr . cf . ptr . cf $ char

--           dst' = ptr . ptr $ char

--       PShow (cleanDeepSymType dst) `shouldBe` PShow dst'

--     it "cleanDeepSymType recursive container" $ do
--       let dst = DSRecursive (Sym 0)
--                 . TPointer (bw 64)
--                 . cf . ptr . cf . DSVar $ Sym 0

--           dst' = DSRecursive (Sym 0)
--                  . TPointer (bw 64)
--                  . ptr . DSVar $ Sym 0

--       PShow (cleanDeepSymType dst) `shouldBe` PShow dst'
