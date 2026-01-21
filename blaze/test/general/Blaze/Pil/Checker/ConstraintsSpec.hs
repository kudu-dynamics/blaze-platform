{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.ConstraintsSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Pil.Checker.Constraints
import Blaze.Types.Pil.Checker
import Blaze.Pil.Construct (defCall, var)
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil (PilVar)
import Blaze.Types.Pil.PilType
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (ParamPosition(ParamPosition))
import Blaze.Util.Spec (defaultSize)

import qualified Data.HashSet as HashSet
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

pv :: Text -> Bool -> Pil.VarLocation -> PilVar
pv = Pil.PilVar defaultSize Nothing Nothing

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

    let helper op signedness =
         let cxs = []
             vars = []
             expr = InfoExpression
               { info = SymInfo 64 (Sym 0)
               , op = op (constExpr 1 64 888) (constExpr 2 64 999)
               }

             nextSym = 3

             cxs' =
               [ (Sym 0, SType TBool)
               , (Sym 1, SType TInt{bitWidth = Nothing, signed = signedness})
               , (Sym 1, SType TBitVector{bitWidth = Just 64})
               , (Sym 1, SVar (Sym 2))
               , (Sym 2, SType TInt{bitWidth = Nothing, signed = signedness})
               , (Sym 2, SType TBitVector{bitWidth = Just 64})
               ]

             vars' = []
         in constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')
      in do
        it "generates constraints for an ADD_WILL_CARRY with consts" $ do
          helper (\l r -> Pil.ADD_WILL_CARRY $ Pil.AddWillCarryOp l r) (Just False)

        it "generates constraints for an ADD_WILL_OVERFLOW with consts" $ do
          helper (\l r -> Pil.ADD_WILL_OVERFLOW $ Pil.AddWillOverflowOp l r) (Just True)

        it "generates constraints for a SUB_WILL_OVERFLOW with consts" $ do
          helper (\l r -> Pil.SUB_WILL_OVERFLOW $ Pil.SubWillOverflowOp l r) (Just True)

    it "generates constraints for an ARRAY_ADDR with consts" $ do
      let cxs = []
          vars = []
          expr = InfoExpression
            { info = SymInfo 64 (Sym 0)
            , op = Pil.ARRAY_ADDR $ Pil.ArrayAddrOp (constExpr 1 64 888) (constExpr 2 32 999) 999
            }

          nextSym = 4

          res = constrainExpr nextSym cxs vars expr
          pointeeCandidates =
            concatMap (\case (Sym 0, SType (TPointer _ pt)) -> [pt]; _ -> []) (fst res)

      length pointeeCandidates `shouldBe` 1
      let pointee = unsafeHead pointeeCandidates
          cxs' =
            [ (Sym 0, SVar (Sym 1))
            , (Sym 0, SType (TPointer (Just 64) pointee))
            , (Sym 1, SType (TBitVector (Just 64)))
            , (Sym 2, SType (TInt Nothing (Just False)))
            , (Sym 2, SType (TBitVector (Just 32)))
            ]
          vars' = []

      res `shouldBe` (sort cxs', sort vars')

    it "generates constraints for a POPCNT with consts" $ do
      let cxs = []
          vars = []
          expr = InfoExpression
            { info = SymInfo 64 (Sym 0)
            , op = Pil.POPCNT $ Pil.PopcntOp (constExpr 1 64 888)
            }

          nextSym = 2

          cxs' =
            [ (Sym 0, SType TInt{bitWidth = Just 64, signed = Nothing})
            , (Sym 1, SType (TBitVector Nothing))
            , (Sym 1, SType (TBitVector (Just 64)))
            ]

          vars' = []
      constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')

    it "generates constraints for a var" $ do
      let cxs = []
          vars = [(pv "a" False Pil.UnknownLocation, Sym 0)]
          expr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a" False Pil.UnknownLocation)
            }
          nextSym = 2

          cxs' = [ (Sym 0, SType (TBitVector {bitWidth = Just 64}))
                 , (Sym 1, SVar (Sym 0))
                 ]
          vars' = [ (pv "a" False Pil.UnknownLocation, Sym 0) ]
      constrainExpr nextSym cxs vars expr `shouldBe` (sort cxs', sort vars')


  context "addExprTypeConstraints" $ do
    -- addExprTypeConstraints decides whether to get constraints of nested types as well

    it "generates constraints for a field address" $ do
      let cxs = []
          vars = [(pv "a" False Pil.UnknownLocation, Sym 0)]
          varExpr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a" False Pil.UnknownLocation)
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

          vars' = [ (pv "a" False Pil.UnknownLocation, Sym 0) ]
      constrainExpr 100 cxs vars expr `shouldBe` (sort cxs', sort vars')


    it "generates constraints for the load of a field address" $ do
      let cxs = []
          vars = [(pv "a" False Pil.UnknownLocation, Sym 0)]
          varExpr = InfoExpression
            { info = SymInfo 64 (Sym 1)
            , op = Pil.VAR $ Pil.VarOp (pv "a" False Pil.UnknownLocation)
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

          vars' = [ (pv "a" False Pil.UnknownLocation, Sym 0) ]
      constrainExpr 100 cxs vars expr `shouldBe` (sort cxs', sort vars')

    it "generates constraints for a function call" $ do
      let cxs = []
          vars = [(pv "a" False Pil.UnknownLocation, Sym 0)]
          varExpr =
            InfoExpression
              { info = SymInfo 32 (Sym 1),
                op = Pil.VAR $ Pil.VarOp (pv "a" False Pil.UnknownLocation)
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
                    Pil.CallOp dest args
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
      let callTarget = Pil.CallAddr $ Pil.ConstFuncPtrOp (intToAddr 0x00424242) Nothing
          callStmtA = defCall "a" callTarget [var "x" 8] 4
          callStmtB = defCall "b" callTarget [var "y" 8] 4
          constraintProg = (,)
            <$> addStmtTypeConstraints callStmtA
            <*> addStmtTypeConstraints callStmtB

          ((callStmtA', callStmtB'), st) = first unsafeFromRight $ runConstraintGen_ constraintProg

          constraints' = st ^. #constraints
          getVarSym bw varName = case HashMap.lookup (C.pilVar bw varName) $ st ^. #varSymMap of
            Nothing -> error $ "getVarSym failed!!! " <> show varName
            Just x -> x
          retASym = getVarSym 4 "a"
          retBSym = getVarSym 4 "b"
          xSym = getVarSym 8 "x"
          ySym = getVarSym 8 "y"

          funcArg1Sym = fromJust . HashMap.lookup (Pil.FuncParam (Pil.CallTarget callTarget) $ ParamPosition 1) $ st ^. #funcSymMap
          funcRetSym = fromJust . HashMap.lookup (Pil.FuncResult $ Pil.CallTarget callTarget) $ st ^. #funcSymMap
          
          callArgSymA = callStmtA' ^?! #statement . #_Def . #value . #op . #_CALL . #args . ix 0 . #info . #sym
          callArgSymB = callStmtB' ^?! #statement . #_Def . #value . #op . #_CALL . #args . ix 0 . #info . #sym
          callExprA = callStmtA' ^?! #statement . #_Def . #value . #info . #sym
          callExprB = callStmtB' ^?! #statement . #_Def . #value . #info . #sym
          
          expectedConstraints = HashSet.fromList
            [ Constraint 0 callArgSymA (SVar xSym)
            , Constraint 0 callArgSymB (SVar ySym)
            , Constraint 0 funcArg1Sym (SVar callArgSymA)
            , Constraint 0 funcArg1Sym (SVar callArgSymB)
            , Constraint 0 retASym (SVar callExprA)
            , Constraint 0 retBSym (SVar callExprB)
            , Constraint 0 funcRetSym (SVar callExprA)
            ]

          _helpfulInfo =
            ( ("retASym", retASym)
            , ("retBSym", retBSym)
            , ("xSym", xSym)
            , ("ySym", ySym)
            , ("funcArg1Sym", funcArg1Sym)
            , ("funcRetSym", funcRetSym)
            , ("callArgSymA", callArgSymA)
            , ("callArgSymB", callArgSymB)
            , ("callExprA", callExprA)
            , ("callExprB", callExprB)
            )
      -- pprint _helpfulInfo
      PShow (HashSet.difference expectedConstraints $ HashSet.fromList constraints') `shouldBe` PShow HashSet.empty
      
    it "generates constraints for call args so vars can link outside of function args" $ do
      let callStmt = defCall "a" (Pil.CallAddr $ Pil.ConstFuncPtrOp (intToAddr 0x00424242) Nothing) [var "x" 8] 4
          retStmt = C.ret $ var "x" 8
          constraintProg = (,)
            <$> addStmtTypeConstraints callStmt
            <*> addStmtTypeConstraints retStmt
            
          ((callStmt', retStmt'), st) = first unsafeFromRight $ runConstraintGen_ constraintProg
          constraints' = st ^. #constraints
          xSym = fromJust . HashMap.lookup (C.pilVar 8 "x") $ st ^. #varSymMap
          callArgSym = callStmt' ^?! #statement . #_Def . #value . #op . #_CALL . #args . ix 0 . #info . #sym
          retSym = retStmt' ^?! #statement . #_Ret . #value . #info . #sym
          expectedConstraints = HashSet.fromList
            [ Constraint 0 callArgSym (SVar xSym)
            , Constraint 0 retSym (SVar xSym)
            ]

      (expectedConstraints `HashSet.isSubsetOf` HashSet.fromList constraints') `shouldBe` True
