{- HLINT ignore "Reduce duplication" -}

module Blaze.Pil.Checker.UnificationSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, const, Constraint)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Pil.Checker.Unification
import Blaze.Types.Pil.Checker

import Test.Hspec


mkSign :: Bool -> Maybe Bool
mkSign = Just

mkBitWidth :: BitWidth -> Maybe BitWidth
mkBitWidth = Just

mkSym :: Int -> SymType
mkSym = SVar . Sym

spec :: Spec
spec = describe "Blaze.Pil.Checker.Unification" $ do
  context "addVarEq" $ do
    let addVarEq' cxtuples solsList originsList sym1 sym2 =
          let (_, UnifyState cxs sols _ omap _) =
                runUnify (addVarEq sym1 sym2)
                $ UnifyState (fmap (uncurry $ Constraint 0) cxtuples)
                (HashMap.fromList solsList)
                []
                (HashMap.fromList originsList)
                0
          in
            ( sort $ fmap (\(Constraint _ a b) -> (a, b)) cxs
            , sort $ HashMap.toList sols
            , sort $ HashMap.toList omap
            )

    it "add var eq that combines two groups. send one solution to constraints" $ do
      let syms' = (Sym 0, Sym 1)

          cxs = []
          sols = [ (Sym 0, TChar Nothing)
                 , (Sym 1, TBottom (Sym 1))
                 ]
          vmap = [ (Sym 0, Sym 0)
                 , (Sym 1, Sym 1)
                 ]

          cxs' = [ (Sym 0, SType $ TChar Nothing) ]
          sols' = [ (Sym 1, TBottom (Sym 1)) ]
          vmap' = [ (Sym 0, Sym 1)
                  , (Sym 1, Sym 1)
                  ]

      uncurry (addVarEq' cxs sols vmap) syms'
        `shouldBe` (sort cxs', sort sols', sort vmap')

  context "unifyConstraint" $ do
    let unifyCx cxtuples solsList originsList cx =
          let (_, UnifyState cxs sols _ omap _) =
                runUnify (unifyConstraint (uncurry (Constraint 0) cx) >> substSolutions)
                $ UnifyState (fmap (uncurry $ Constraint 0) cxtuples)
                (HashMap.fromList solsList)
                []
                (HashMap.fromList originsList)
                0
          in
            ( sort $ fmap (\(Constraint _ a b) -> (a, b)) cxs
            , sort $ HashMap.toList sols
            , sort $ HashMap.toList omap
            )

    it "remove redudant var eq" $ do
      let cx = (Sym 0, SVar $ Sym 0)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq with zero state" $ do
      let cx = (Sym 0, SVar $ Sym 1)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq with prev var eq" $ do
      let cx = (Sym 0, SVar $ Sym 1)

          cxs = []
          sols = []
          vmap = [ (Sym 1, Sym 8)
                 , (Sym 8, Sym 8)
                 ]

          cxs' = []
          sols' = []
          vmap' = [ (Sym 0, Sym 8)
                  , (Sym 1, Sym 8)
                  , (Sym 8, Sym 8)
                  ]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq that has var already in sols" $ do
      let cx = (Sym 0, SVar $ Sym 1)

          cxs = []
          sols = [(Sym 1, TChar Nothing)]
          vmap = [(Sym 1, Sym 1)]

          cxs' = []
          sols' = [(Sym 1, TChar Nothing)]
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq that has var already in sols" $ do
      let cx = (Sym 1, SVar $ Sym 0)

          cxs = []
          sols = [(Sym 1, TChar Nothing)]
          vmap = [(Sym 1, Sym 1)]

          cxs' = []
          sols' = [(Sym 1, TChar Nothing)]
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq and leave constraints alone" $ do
      let cx = (Sym 0, SVar $ Sym 1)

          cxs = [ (Sym 0, SType $ TBitVector Nothing)
                , (Sym 3, SType $ TPointer Nothing (Sym 1) )
                , (Sym 1, SVar $ Sym 0)
                ]
          sols = []
          vmap = []

          cxs' = cxs
          sols' = []
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a var eq and perform var substitution on solutions" $ do
      let cx = (Sym 0, SVar $ Sym 1)

          cxs = []
          sols = [ (Sym 1, TBitVector Nothing)
                 , (Sym 3, TPointer Nothing (Sym 0) )
                 ]
          vmap = [ (Sym 1, Sym 1), (Sym 3, Sym 3) ]

          cxs' = []
          sols' = [ (Sym 1, TBitVector Nothing)
                  , (Sym 3, TPointer Nothing (Sym 1) )
                  ]
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1), (Sym 3, Sym 3)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a type for var without previous solution or var eq origin" $ do
      let cx = (Sym 0, SType $ TChar Nothing)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = [(Sym 0, TChar Nothing)]
          vmap' = [(Sym 0, Sym 0)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify a type for var without previous solution with var eq origin" $ do
      let cx = (Sym 0, SType $ TChar Nothing)

          cxs = []
          sols = []
          vmap = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

          cxs' = []
          sols' = [(Sym 1, TChar Nothing)]
          vmap' = [(Sym 0, Sym 1), (Sym 1, Sym 1)]

      unifyCx cxs sols vmap cx `shouldBe` (sort cxs', sort sols', sort vmap')

  context "unifyPilTypes" $ do
    let unifyPilTypes' cxtuples solsList originsList pt1 pt2 =
          let (er, UnifyState cxs sols _ omap _) =
                runUnify (do
                             r <- unifyPilTypes pt1 pt2
                             substSolutions
                             return r
                         )
                $ UnifyState (fmap (uncurry $ Constraint 0) cxtuples)
                (HashMap.fromList solsList)
                []
                (HashMap.fromList originsList)
                0
          in
            ( sort $ fmap (\(Constraint _ a b) -> (a, b)) cxs
            , sort $ HashMap.toList sols
            , sort $ HashMap.toList omap
            , er
            )

    it "unify Bottom" $ do
      let pt1 = TBottom $ Sym 0
          pt2 = TBottom $ Sym 0
          result = Right . TBottom $ Sym 0

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Array and Array" $ do
      let pt1 = TArray Nothing (Sym 0)
          pt2 = TArray Nothing (Sym 1)
          result = Right $ TArray Nothing (Sym 1)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = [ (Sym 0, Sym 1)
                  , (Sym 1, Sym 1)
                  ]

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Int and Int" $ do
      let pt1 = TInt Nothing (Just True)
          pt2 = TInt (Just 64) Nothing
          result = Right $ TInt (Just 64) (Just True)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Int and Pointer" $ do
      let pt1 = TInt (Just 64) Nothing
          pt2 = TPointer Nothing (Sym 0)
          result = Right $ TPointer (Just 64) (Sym 0)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Int and Char" $ do
      let pt1 = TInt Nothing Nothing
          pt2 = TChar Nothing
          result = Right $ TChar Nothing

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Pointer and Pointer" $ do
      let pt1 = TPointer (Just 64) (Sym 0)
          pt2 = TPointer (Just 64) (Sym 1)
          result = Right $ TPointer (Just 64) (Sym 1)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = [ (Sym 1, Sym 1)
                  , (Sym 0, Sym 1)
                  ]

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify BitVector and Array" $ do
      let pt1 = TBitVector Nothing
          pt2 = TArray Nothing (Sym 0)
          result = Right $ TArray Nothing (Sym 0)

          cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

    it "unify Array and CString" $ do
      let pt1 = TArray (Just 10) (Sym 0)
          pt2 = TCString (Just 10)
          result = Right $ TCString (Just 10)

          cxs = []
          sols = []
          vmap = []

          cxs' = [ (Sym 0, SType $ TChar (Just 8))
                 ]
          sols' = []
          vmap' = []

      unifyPilTypes' cxs sols vmap pt1 pt2
        `shouldBe` (sort cxs', sort sols', sort vmap', result)

  context "unifyRecords" $ do
    let unifyRecords' cxtuples solsList originsList fields1 fields2 =
          let (er, UnifyState cxs sols _ omap _) =
                runUnify (unifyRecords
                          (HashMap.fromList fields1)
                          (HashMap.fromList fields2))
                $ UnifyState (fmap (uncurry $ Constraint 0) cxtuples)
                (HashMap.fromList solsList)
                []
                (HashMap.fromList originsList)
                0
          in
            ( sort $ fmap (\(Constraint _ a b) -> (a, b)) cxs
            , sort $ HashMap.toList sols
            , sort $ HashMap.toList omap
            , sort . HashMap.toList <$> er
            )

    it "unify nothing" $ do
      let cxs = []
          sols = []
          vmap = []
          rec1 = []
          rec2 = []

          cxs' = []
          sols' = []
          vmap' = []
          rec3 = []

      unifyRecords' cxs sols vmap rec1 rec2 `shouldBe`
        (sort cxs', sort sols', sort vmap', Right . sort $ rec3)

    it "unify two fields at same offset" $ do
      let cxs = []
          sols = []
          vmap = []
          rec1 = [(88, Sym 0)]
          rec2 = [(88, Sym 1)]

          cxs' = [(Sym 0, SVar $ Sym 1)]
          sols' = []
          vmap' = []
          rec3 = [(88, Sym 0)]

      unifyRecords' cxs sols vmap rec1 rec2 `shouldBe`
        (sort cxs', sort sols', sort vmap', Right . sort $ rec3)

    it "unify multiple fields at different offsets" $ do
      let cxs = []
          sols = []
          vmap = []
          rec1 = [ (0, Sym 0)
                 , (64, Sym 2)
                 , (128, Sym 4)
                 ]
          rec2 = [ (32, Sym 1)
                 , (96, Sym 3)
                 ]


          cxs' = []
          sols' = []
          vmap' = []
          rec3 = [ (0, Sym 0)
                 , (32, Sym 1)
                 , (64, Sym 2)
                 , (96, Sym 3)
                 , (128, Sym 4)
                 ]

      unifyRecords' cxs sols vmap rec1 rec2 `shouldBe`
        (sort cxs', sort sols', sort vmap', Right . sort $ rec3)



  context "unify" $ do
    let unify' cxtuples solsList originsList =
          let (_, UnifyState cxs sols _ omap _) =
                runUnify unify
                $ UnifyState (fmap (uncurry $ Constraint 0) cxtuples)
                (HashMap.fromList solsList)
                []
                (HashMap.fromList originsList)
                0
          in
            ( sort $ fmap (\(Constraint _ a b) -> (a, b)) cxs
            , sort $ HashMap.toList sols
            , sort $ HashMap.toList omap
            )

    it "unify nothing" $ do
      let cxs = []
          sols = []
          vmap = []

          cxs' = []
          sols' = []
          vmap' = []

      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify one constraint" $ do
      let cxs = [(Sym 0, SType $ TChar (Just 8))]
          sols = []
          vmap = []

          cxs' = []
          sols' = [(Sym 0, TChar (Just 8))]
          vmap' = [(Sym 0, Sym 0)]

      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify two unrelated constraints" $ do
      let cxs = [ (Sym 0, SType $ TChar (Just 8))
                , (Sym 1, SType $ TBottom (Sym 2))
                ]
          sols = []
          vmap = []

          cxs' = []
          sols' = [ (Sym 0, TChar (Just 8))
                  , (Sym 1, TBottom $ Sym 2)
                  ]
          vmap' = [ (Sym 0, Sym 0)
                  , (Sym 1, Sym 1)
                  ]

      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify two related constraints" $ do
      let cxs = [ (Sym 0, SType $ TChar (Just 8))
                , (Sym 1, SType $ TBottom (Sym 2))
                , (Sym 0, SVar $ Sym 1)
                ]
          sols = []
          vmap = []

          cxs' = []
          sols' = [ (Sym 1, TBottom $ Sym 2)
                  ]
          vmap' = [ (Sym 0, Sym 1)
                  , (Sym 1, Sym 1)
                  ]

      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify two pointers with subtypes" $ do
      let cxs = [ (Sym 0, SType $ TPointer (Just 64) (Sym 1))
                , (Sym 1, SType $ TInt (Just 8) (Just False))
                , (Sym 2, SType $ TPointer (Just 64) (Sym 3))
                , (Sym 3, SType $ TChar (Just 8))
                , (Sym 0, SVar $ Sym 2)
                ]
          sols = []
          vmap = []

          cxs' = []
          sols' = [ (Sym 2, TPointer (Just 64) (Sym 3))
                  , (Sym 3, TChar (Just 8))
                  ]
          vmap' = [ (Sym 0, Sym 2)
                  , (Sym 2, Sym 2)
                  , (Sym 1, Sym 3)
                  , (Sym 3, Sym 3)
                  ]

      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')


    -- it "unify a record with a pointer" $ do
    --   let cxs = [ (Sym 0, SType $ TPointer (Sym 1) (Sym 2))
    --             , (Sym 2, SType $ TInt (Sym 3) (Sym 4))
    --             , (Sym 0, SType $ TPointer (Sym 5) (Sym 6))
    --             , (Sym 5, SVar $ Sym 7)
    --             , (Sym 6, SType $ TChar)
    --             , (Sym 7, SType $ TVBitWidth 64)
    --             ]
    --       sols = []
    --       vmap = []

    --       cxs' = []
    --       sols' = [ (Sym 0, TPointer (Sym 1) (Sym 2))
    --               , (Sym 2, TChar)
    --               , (Sym 3, TVBitWidth 8)
    --               , (Sym 4, TVSign False)
    --               , (Sym 1, TVBitWidth 64)
    --               ]
    --       vmap' = [ (Sym 0, Sym 0)
    --               , (Sym 1, Sym 1)
    --               , (Sym 5, Sym 1)
    --               , (Sym 7, Sym 1)
    --               , (Sym 2, Sym 2)
    --               , (Sym 6, Sym 2)
    --               , (Sym 3, Sym 3)
    --               , (Sym 4, Sym 4)
    --               ]

    --   unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')

    it "unify constraints from `def b = [field_addr a 24bytes]` " $ do
      -- this test comes from trying to fix unexpected result
      let cxs = [ (Sym 0, SType (TBitVector {bitWidth = Just 64}))
                , (Sym 100, SVar (Sym 0))
                , (Sym 100, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 109}))
                , (Sym 101, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 104}))
                , (Sym 101, SType (TPointer {bitWidth = Just 64, pointeeType = Sym 106}))
                , (Sym 102, SVar (Sym 104))
                , (Sym 102, SType (TBitVector {bitWidth = Just 32}))
                , (Sym 109, SType (TRecord (HashMap.fromList [(192, Sym 106)])))
                ]

          sols = []
          vmap = []

          cxs' = []
          sols' = [ (Sym 0, TPointer {bitWidth = Just 64, pointeeType = Sym 109})
                  , (Sym 101, TPointer {bitWidth = Just 64, pointeeType = Sym 104})
                  , (Sym 104, TBitVector {bitWidth = Just 32})
                  , (Sym 109, TRecord (HashMap.fromList [(192, Sym 104)]))]

          vmap' = [ (Sym 0, Sym 0)
                  , (Sym 100, Sym 0)
                  , (Sym 101, Sym 101)
                  , (Sym 102, Sym 104)
                  , (Sym 104, Sym 104)
                  , (Sym 106, Sym 104)
                  , (Sym 109, Sym 109)
                  ]


      unify' cxs sols vmap `shouldBe` (sort cxs', sort sols', sort vmap')
