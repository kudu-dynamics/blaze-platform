{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.ConstraintsSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Pil.Checker.Constraints
import Blaze.Types.Pil.Checker
import Blaze.Pil.Construct (defCall, var)
import Blaze.Types.Pil (PilVar)
import qualified Blaze.Types.Pil as Pil

import Test.Hspec

testConstraintGenState ::
  Sym ->
  [(PilVar, Sym)] ->
  [(Sym, SymType)] ->
  ConstraintGenState
testConstraintGenState nextSym varsAndSyms constraints' =
  ConstraintGenState
    nextSym
    HashMap.empty
    (HashMap.fromList varsAndSyms)
    HashMap.empty
    (fmap (uncurry $ Constraint 0) constraints')
    0
    HashMap.empty

cxsTup :: Constraint -> (Sym, SymType)
cxsTup (Constraint _ v t) = (v, t)

constrainConvert ::
  Int ->
  [(Sym, SymType)] ->
  [(PilVar, Sym)] ->
  SymConstraint ->
  ([(Sym, SymType)], [(PilVar, Sym)])
constrainConvert nextSymNum cxTuples pilvarTuples tup =
  let (_, ConstraintGenState _ _ vars _ cxs _ _) =
        runConstraintGen
          (addConstraint tup)
          ( emptyConstraintGenCtx
          , testConstraintGenState
              (Sym nextSymNum)
              pilvarTuples
              cxTuples
          )
   in ( sort $ fmap cxsTup cxs
      , sort $ HashMap.toList vars
      )

constrainExpr ::
  Int ->
  [(Sym, SymType)] ->
  [(PilVar, Sym)] ->
  InfoExpression SymInfo ->
  ([(Sym, SymType)], [(PilVar, Sym)])
constrainExpr nextSymNum cxTuples pilvarTuples expr =
  let (_, ConstraintGenState _ _ vars _ cxs _ _) =
        runConstraintGen
          (addExprTypeConstraints expr)
          ( emptyConstraintGenCtx
          , testConstraintGenState
              (Sym nextSymNum)
              pilvarTuples
              cxTuples
          )
   in ( sort $ fmap (\(Constraint _ v t) -> (v, t)) cxs
      , sort $ HashMap.toList vars
      )

pv :: Text -> PilVar
pv name = Pil.PilVar name Nothing

spec :: Spec
spec = describe "Blaze.Pil.Checker.Constraints" $ do
  context "addConstraint conversion" $ do

    it "converts a sym" $ do
      let cxs = []
          vars = []
          tup = (Sym 0, CSVar $ Sym 1)
          nextSym = 2

          cxs' = [ (Sym 0, SVar $ Sym 1)
                 ]
          vars' = []
      constrainConvert nextSym cxs vars tup `shouldBe` (sort cxs', sort vars')

    it "converts a simple type with sub var" $ do
      let cxs = []
          vars = []
          tup = (Sym 0, CSType $ TBitVector Nothing)
          nextSym = 2

          cxs' = [ (Sym 0, SType $ TBitVector Nothing)
                 ]
          vars' = []
      constrainConvert nextSym cxs vars tup `shouldBe` (sort cxs', sort vars')

    it "converts a pointer type with two sub vars" $ do
      let cxs = []
          vars = []
          tup = (Sym 0, CSType $ TPointer Nothing (CSVar $ Sym 2))
          nextSym = 2

          cxs' = [ (Sym 0, SType $ TPointer Nothing (Sym 2))
                 ]
          vars' = []
      constrainConvert nextSym cxs vars tup `shouldBe` (sort cxs', sort vars')

    it "converts a pointer type with nested type" $ do
      let cxs = []
          vars = []
          tup = (Sym 0, CSType $ TPointer Nothing
                  (CSType $ TBitVector Nothing))
          nextSym = 3

          cxs' = [ (Sym 0, SType $ TPointer Nothing (Sym 3))
                 , (Sym 3, SType $ TBitVector Nothing)
                 ]
          vars' = []
      constrainConvert nextSym cxs vars tup `shouldBe` (sort cxs', sort vars')

    it "converts a pointer to type with nested type" $ do
      let cxs = []
          vars = []
          tup = (Sym 0, CSType $ TPointer Nothing
                  (CSType $ TBitVector $ Just 64))
          nextSym = 2

          cxs' = [ (Sym 0, SType $ TPointer Nothing (Sym 2))
                 , (Sym 2, SType $ TBitVector (Just 64))
                 ]
          vars' = []
      constrainConvert nextSym cxs vars tup `shouldBe` (sort cxs', sort vars')

--------------------------------------------------

  context "addExprTypeConstraints" $ do
    -- NOTE: This does NOT get constraints of nested types
    let constExpr s w n = InfoExpression
            { info = SymInfo w (Sym s)
            , op = Pil.CONST $ Pil.ConstOp n
            }

    it "generates constraints for a simple CONST" $ do
      let cxs = []
          vars = []
          expr = constExpr 0 64 8888
          nextSym = 1

          cxs' = [ (Sym 0, SType $ TBitVector (Just 64))
                 ]
          vars' = []
      constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')

    it "generates constraints for an add with consts" $ do
      let cxs = []
          vars = []
          expr = InfoExpression
            { info = SymInfo 64 (Sym 0)
            , op = Pil.ADD $ Pil.AddOp (constExpr 1 64 888) (constExpr 2 64 999)
            }

          nextSym = 3

          cxs' =
            [ (Sym 0, SVar (Sym 1))
            , (Sym 0, SType (TInt{bitWidth = Just 64, signed = Nothing}))
            , (Sym 1, SType (TInt{bitWidth = Just 64, signed = Nothing}))
            , (Sym 1, SType (TBitVector{bitWidth = Just 64}))
            , (Sym 1, SVar (Sym 2))
            , (Sym 2, SType (TInt{bitWidth = Just 64, signed = Nothing}))
            , (Sym 2, SType (TBitVector{bitWidth = Just 64}))
            ]

          vars' = []
      constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')

    it "generates constraints for a var" $ do
      let cxs = []
          vars = [(pv "a", Sym 0)]
          expr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a")
            }
          nextSym = 2

          cxs' = [ (Sym 0, SType (TBitVector {bitWidth = Just 64}))
                 , (Sym 1, SVar (Sym 0))
                 ]
          vars' = [ (pv "a", Sym 0) ]
      constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')


  context "addExprTypeConstraints" $ do
    -- addExprTypeConstraints decides whether to get constraints of nested types as well

    it "generates constraints for a field address" $ do
      let cxs = []
          vars = [(pv "a", Sym 0)]
          varExpr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a")
            }
          expr = InfoExpression
            { info = SymInfo 64 (Sym 2)
            , op = Pil.FIELD_ADDR $ Pil.FieldAddrOp varExpr 4
            }

          cxs' = [ (Sym 0, SType (TBitVector {bitWidth = Just 64}))
                 , (Sym 1, SVar (Sym 0))
                 , (Sym 1, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 101}))
                 , (Sym 2, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 100}))
                 , (Sym 101, SType (TRecord (HashMap.fromList [(32, Sym 100)])))
                 ]

          vars' = [ (pv "a", Sym 0) ]
      constrainExpr 100 cxs vars expr `shouldBe` (sort cxs', sort vars')


    it "generates constraints for the load of a field address" $ do
      let cxs = []
          vars = [(pv "a", Sym 0)]
          varExpr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a")
            }
          fieldAddrExpr = InfoExpression
            { info = SymInfo 64 (Sym 2)
            , op = Pil.FIELD_ADDR $ Pil.FieldAddrOp varExpr 4
            }
          expr = InfoExpression
            { info = SymInfo 128 (Sym 3)
            , op = Pil.LOAD $ Pil.LoadOp fieldAddrExpr
            }

          cxs' = [ (Sym 0, SType (TBitVector {bitWidth = Just 64}))
                 , (Sym 1, SVar (Sym 0))
                 , (Sym 1, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 102}))
                 , (Sym 2, SType (TPointer {bitWidth = Nothing, pointeeType = Sym 100}))
                 , (Sym 2, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 101}))
                 , (Sym 3, SVar (Sym 100))
                 , (Sym 3, SType (TBitVector {bitWidth = Just 128}))
                 , (Sym 102, SType (TRecord (HashMap.fromList [(32, Sym 101)])))
                 ]

          vars' = [ (pv "a", Sym 0) ]
      constrainExpr 100 cxs vars expr `shouldBe` (sort cxs', sort vars')

    it "generates constraints for a function call" $ do
      let cxs = []
          vars = [(pv "a", Sym 0)]
          varExpr =
            InfoExpression
              { info = SymInfo 32 (Sym 1),
                op = Pil.VAR $ Pil.VarOp (pv "a")
              }
          cp =
            InfoExpression
              { info = SymInfo 64 (Sym 2),
                op = Pil.CONST_PTR $ Pil.ConstPtrOp 0xDEADCAFE
              }
          dest :: Pil.CallDest (InfoExpression SymInfo)
          dest = Pil.mkCallDest cp
          args = [varExpr]
          expr =
            InfoExpression
              { info = SymInfo 64 (Sym 3),
                op =
                  Pil.CALL $
                    Pil.CallOp dest (Just "foo") args
              }
          resultCxs =
            [ (Sym 0, SType (TBitVector {bitWidth = Just 32}))
            , (Sym 1, SVar (Sym 0))
            , (Sym 3, SType (TBitVector {bitWidth = Just 64}))
              -- The constraint for: call arguments == function params
            , (Sym 100, SVar (Sym 1))
              -- The constraint for: call result == call expression
            , (Sym 101, SVar (Sym 3))
            ]
          resultVars = vars
      constrainExpr 100 cxs vars expr `shouldBe` (sort resultCxs, sort resultVars)

  context "addStmtTypeConstraints" $ do
    it "links call results and call arguments across call sites" $ do
      let callStmtA = defCall "a" (Pil.CallAddr $ Pil.ConstFuncPtrOp 0x00424242 Nothing) [var "x" 8] 4
          callStmtB = defCall "b" (Pil.CallAddr $ Pil.ConstFuncPtrOp 0x00424242 Nothing) [var "y" 8] 4
          constraintProg =
            addStmtTypeConstraints callStmtA
              >> addStmtTypeConstraints callStmtB
          (_, ConstraintGenState _ _ _ _ constraints' _ _) =
            runConstraintGen_ constraintProg
      -- s0, s3, s6, s8, and s12 are all related to the call args and are constrained
      -- to be equal.
      -- s1, s2, s4, s9, and s10 are all related to the call results and
      -- are constrained to be equal
      -- TODO: Make this expected result more obvious. How can we at a glance see
      --       the appropriate constraints are in place?
      (sort . fmap cxsTup $ constraints')
        `shouldBe` [(Sym 0, SVar (Sym 5)),
                    (Sym 1, SType (TBitVector {bitWidth = Just 32})),
                    (Sym 2, SVar (Sym 1)),
                    (Sym 3, SVar (Sym 0)),
                    (Sym 3, SVar (Sym 6)),
                    (Sym 4, SVar (Sym 1)),
                    (Sym 4, SVar (Sym 7)),
                    (Sym 5, SType (TBitVector {bitWidth = Just 64})),
                    (Sym 6, SVar (Sym 9)),
                    (Sym 7, SType (TBitVector {bitWidth = Just 32})),
                    (Sym 8, SVar (Sym 7)),
                    (Sym 9, SType (TBitVector {bitWidth = Just 64}))
                  ]
