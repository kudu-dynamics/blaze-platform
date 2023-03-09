{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Eta reduce" -}

module Blaze.Pil.SolverSpec where

import Blaze.Prelude hiding (const, numerator, denominator)

import Blaze.Pil.Solver hiding (pilVar)
import Blaze.Pil.Construct
import qualified Blaze.Types.Pil as Pil
import qualified Data.SBV.Trans as SBV
import qualified Data.HashMap.Strict as HashMap
import Data.SBV.Dynamic hiding (Solver)
import Blaze.Types.Pil.Checker as Ch hiding (signed)
import Blaze.Pil.Checker (checkStmts)
import Data.SBV.Trans ( (.>=)
                      , (.<)
                      , (.&&)
                      )
import Data.SBV.Internals (unSBV)
import Test.Hspec
import Numeric (showHex)

type SolverOutput = Either SolverError (SolverResult, [SolverError])

-- | Get a concrete variable from solver output.
getVar :: Text -> SolverOutput -> Maybe CV
getVar k r = r ^? _Right . _1 . #_Sat . at k . _Just

getInt :: CV -> Maybe Integer
getInt (CV _kind val) = case val of
  CInteger x -> Just x
  _ -> Nothing

getVal :: Text -> SolverOutput -> Maybe Integer
getVal k r = getVar k r >>= getInt

spec :: Spec
spec = describe "Blaze.Pil.SolverSpec" $ do
  let solveSolver m = checkSatWith_ SBV.z3 False AbortOnError m
      signed32 = Ch.DSType $ Ch.TInt (bw 32) (Just True)
      signed64 = Ch.DSType $ Ch.TInt (bw 64) (Just True)
      unsigned32 = Ch.DSType $ Ch.TInt (bw 32) (Just False)
      unsigned64 = Ch.DSType $ Ch.TInt (bw 64) (Just False)
      unsigned8 = Ch.DSType $ Ch.TInt (bw 8) (Just False)
      unsigned4 = Ch.DSType $ Ch.TInt (bw 4) (Just False)
      carry = Ch.DSType Ch.TBool
      bw = Just
      char = Ch.DSType $ Ch.TChar (Just 8)
      float = Ch.DSType $ Ch.TFloat (bw 80)
      tbool = Ch.DSType Ch.TBool
      -- pointer = Ch.DSType . Ch.TPointer (bw 64)
      bitVec = Ch.DSType . Ch.TBitVector
      floatEqual x y = unSBV $ (toSFloat' x .>= toSFloat' y)
        .&& (toSFloat' x .< SBV.fpAdd SBV.sRoundNearestTiesToAway (toSFloat' y) (toSFloat' . constFloat $ 0.0000001))
      -- x >= y && x < y + 0.0000.....

  context "aux ops" $ do

    context "signExtend" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 32)
            b <- newSymVar "b" (KBounded True 64)
            let sE = signExtend 64 a
                r = constInt 32 9
            constrain $ r `svEqual` a
            constrain $ sE `svEqual` b
          vars = [("a", CV (KBounded True 32) (CInteger 9)),
                  ("b", CV (KBounded True 64) (CInteger 9))]

      r <- runIO $ solveSolver cmd
      it "extends signed value so constraint on 32-bit number is also a constraint on 64-bit number" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "zeroExtend" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded False 32)
            b <- newSymVar "b" (KBounded False 64)
            let zE = zeroExtend 64 a
                r = constWord 32 9
            constrain $ r `svEqual` a
            constrain $ zE `svEqual` b
          vars = [("a", CV (KBounded False 32) (CInteger 9)),
                  ("b", CV (KBounded False 64) (CInteger 9))]

      r <- runIO $ solveSolver cmd
      it "extends unsigned value so constraint on 32-bit number is also a constraint on 64-bit number" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "matchBoundedWidth" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 32)
            b <- newSymVar "b" (KBounded True 32)
            constrain $ b `svEqual` (a `matchBoundedWidth` b)
          vars = [("a", CV (KBounded True 32) (CInteger 0)), ("b", CV (KBounded True 32) (CInteger 0))]

      r <- runIO $ solveSolver cmd
      it "should find a equals b after matching width" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "matchBoundedWidth" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 64)
            b <- newSymVar "b" (KBounded True 32)
            constrain $ a `svEqual` (a `matchBoundedWidth` b)
          vars = [("a", CV (KBounded True 64) (CInteger 0)), ("b", CV (KBounded True 32) (CInteger 0))]

      r <- runIO $ solveSolver cmd
      it "should find a equals b after matching width" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "matchSign" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 64)
            b <- newSymVar "b" (KBounded False 64)
            constrain $ a `svEqual` (a `matchSign` b)
          vars = [("a", CV (KBounded True 64) (CInteger 0)), ("b", CV (KBounded False 64) (CInteger 0))]

      r <- runIO $ solveSolver cmd
      it "should find a equals b after matching sign" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "Most Significant Bit" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 4)
            b <- newSymVar "b" KBool
            constrain $ a `svEqual` constInt 4 7
            constrain $ b `svEqual` msb a
          vars = [("a", CV (KBounded True 4) (CInteger 7)), ("b", CV KBool (CInteger 0))]

      r <- runIO $ solveSolver cmd
      it "should find the msb is zero" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

    context "Most Significant Bit" $ do
      let cmd = do
            a <- newSymVar "a" (KBounded True 4)
            b <- newSymVar "b" KBool
            constrain $ a `svEqual` constInt 4 8
            constrain $ b `svEqual` msb a
          vars = [("a", CV (KBounded True 4) (CInteger $ -8)), ("b", CV KBool (CInteger 1))]

      r <- runIO $ solveSolver cmd
      it "should find the msb is one" $ do
        r `shouldBe` Right (Sat (HashMap.fromList vars))

  context "declarePilVars" $ do
    let runDecl tenvTuples = do
          r <-
            runSolverWith
              SBV.z3
              declarePilVars
              (emptyState, SolverCtx (HashMap.fromList tenvTuples) mempty False AbortOnError)
          case r of
            Left _ -> return $ Left ()
            Right (_, ss) ->
              return . Right $
                ( kindOf <$> ss ^. #varMap
                , ss ^. #varNames
                )

    context "declarePilVars" $ do
      let tenv = [(pilVar "a", char)]
          vmap = [(pilVar "a", KBounded False 8)]
          vnames = [(pilVar "a", "a")]
      r <- runIO $ runDecl tenv
      it "put single pilvar in varMap and varNames in state" $ do
        r `shouldBe` Right (HashMap.fromList vmap, HashMap.fromList vnames)

    context "declarePilVars" $ do
      let tenv = [(pilVar "a", Ch.DSVar $ Sym 0)]
      r <- runIO $ runDecl tenv
      it "fail if a var is just a sym" $ do
        r `shouldBe` Left ()

    context "declarePilVars" $ do
      let tenv = [ (pilVar "a", char)
                 , (pilVar "b", signed32)
                 , (pilVar "c", Ch.DSType Ch.TBool)
                 ]
          vmap = [ (pilVar "a", KBounded False 8)
                 , (pilVar "b", KBounded True 32)
                 , (pilVar "c", KBool)
                 ]
          vnames = [ (pilVar "a", "a")
                   , (pilVar "b", "b")
                   , (pilVar "c", "c")
                   ]
      r <- runIO $ runDecl tenv
      it "put many pilvars in varMap and varNames in state" $ do
        r `shouldBe` Right (HashMap.fromList vmap, HashMap.fromList vnames)


  context "solve Expr/Stmt" $ do
    let runSolveCmd tenvTuples cmd = do
          r <- flip (checkSatWith SBV.z3)
               (emptyState, SolverCtx (HashMap.fromList tenvTuples) mempty False AbortOnError)
               $ declarePilVars >> cmd
          case r of
            Left e -> return $ Left e
            Right (x, ss) -> return . Right $ ( x
                                              , ss ^. #errors
                                              )

    context "solveExpr: ADD" $ do
      let tenv = []
          -- arg1 :: DSTExpression
          -- arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1)
          --        . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 88
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.ADD $ Pil.AddOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` constWord 8 99

          rvars = []
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                           , errs )

    let helper op (inw, l, r) (outw, outv) = do
          let tenv = []
              -- arg1 :: DSTExpression
              -- arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1)
              --        . Pil.VAR . Pil.VarOp $ pilVar "a"
              arg1 :: DSTExpression
              arg1 = Ch.InfoExpression (Ch.SymInfo inw $ Sym 2, Just (bitVec $ Just inw))
                     . Pil.CONST . Pil.ConstOp $ l
              arg2 :: DSTExpression
              arg2 = Ch.InfoExpression (Ch.SymInfo inw $ Sym 1, Just (bitVec $ Just inw))
                     . Pil.CONST . Pil.ConstOp $ r
              expr = Ch.InfoExpression (Ch.SymInfo outw $ Sym 0, Just (bitVec $ Just outw))
                     $ op arg1 arg2

              cmd = do
                res <- solveExpr expr
                constrain $ res `svEqual` constWord outw outv

              rvars = []
              errs = []

          res <- runIO $ runSolveCmd tenv cmd
          let hex n = "0x" <> showHex n ""
          it (hex l <> " " <> hex r <> " ~> " <> hex outv) $ do
            res `shouldBe` Right (Sat $ HashMap.fromList rvars, errs)
      in do
        context "ADD_WILL_CARRY" $ do
          context "two constants of the same size" $ do
            let helper' = helper (\l r -> Pil.ADD_WILL_CARRY $ Pil.AddWillCarryOp l r)
            helper' (32, 0x00000000, 0x00000000) (1, 0)
            helper' (32, 0x00000000, 0x00000001) (1, 0)
            helper' (32, 0x00000000, 0xffffffff) (1, 0)
            helper' (32, 0x7fffffff, 0x00000000) (1, 0)
            helper' (32, 0x7fffffff, 0x00000001) (1, 0)
            helper' (32, 0x7fffffff, 0xffffffff) (1, 1)
            helper' (32, 0x80000000, 0x00000000) (1, 0)
            helper' (32, 0x80000000, 0x00000001) (1, 0)
            helper' (32, 0x80000000, 0xffffffff) (1, 1)
            helper' (32, 0xffffffff, 0x00000000) (1, 0)
            helper' (32, 0xffffffff, 0x00000001) (1, 1)
            helper' (32, 0xffffffff, 0xffffffff) (1, 1)

            helper' (8, 0x00, 0x00) (1, 0)
            helper' (8, 0x00, 0x01) (1, 0)
            helper' (8, 0x00, 0xff) (1, 0)
            helper' (8, 0x7f, 0x00) (1, 0)
            helper' (8, 0x7f, 0x01) (1, 0)
            helper' (8, 0x7f, 0xff) (1, 1)
            helper' (8, 0x80, 0x00) (1, 0)
            helper' (8, 0x80, 0x01) (1, 0)
            helper' (8, 0x80, 0xff) (1, 1)
            helper' (8, 0xff, 0x00) (1, 0)
            helper' (8, 0xff, 0x01) (1, 1)
            helper' (8, 0xff, 0xff) (1, 1)

        context "ADD_WILL_OVERFLOW" $ do
          context "two constants of the same size" $ do
            let helper' = helper (\l r -> Pil.ADD_WILL_OVERFLOW $ Pil.AddWillOverflowOp l r)
            helper' (32, 0x00000000, 0x00000000) (1, 0)
            helper' (32, 0x00000000, 0x00000001) (1, 0)
            helper' (32, 0x00000000, 0xffffffff) (1, 0)
            helper' (32, 0x7fffffff, 0x00000000) (1, 0)
            helper' (32, 0x7fffffff, 0x00000001) (1, 1)
            helper' (32, 0x7fffffff, 0xffffffff) (1, 0)
            helper' (32, 0x80000000, 0x00000000) (1, 0)
            helper' (32, 0x80000000, 0x00000001) (1, 0)
            helper' (32, 0x80000000, 0xffffffff) (1, 1)
            helper' (32, 0xffffffff, 0x00000000) (1, 0)
            helper' (32, 0xffffffff, 0x00000001) (1, 0)
            helper' (32, 0xffffffff, 0xffffffff) (1, 0)

            helper' (8, 0x00, 0x00) (1, 0)
            helper' (8, 0x00, 0x01) (1, 0)
            helper' (8, 0x00, 0xff) (1, 0)
            helper' (8, 0x7f, 0x00) (1, 0)
            helper' (8, 0x7f, 0x01) (1, 1)
            helper' (8, 0x7f, 0xff) (1, 0)
            helper' (8, 0x80, 0x00) (1, 0)
            helper' (8, 0x80, 0x01) (1, 0)
            helper' (8, 0x80, 0xff) (1, 1)
            helper' (8, 0xff, 0x00) (1, 0)
            helper' (8, 0xff, 0x01) (1, 0)
            helper' (8, 0xff, 0xff) (1, 0)

        context "SUB_WILL_OVERFLOW" $ do
          context "two constants of the same size" $ do
            let helper' = helper (\l r -> Pil.SUB_WILL_OVERFLOW $ Pil.SubWillOverflowOp l r)
            helper' (32, 0x00000000, 0x00000000) (1, 0)
            helper' (32, 0x00000000, 0x00000001) (1, 0)
            helper' (32, 0x00000000, 0xffffffff) (1, 0)
            helper' (32, 0x7fffffff, 0x00000000) (1, 0)
            helper' (32, 0x7fffffff, 0x00000001) (1, 0)
            helper' (32, 0x7fffffff, 0xffffffff) (1, 1)
            helper' (32, 0x80000000, 0x00000000) (1, 0)
            helper' (32, 0x80000000, 0x00000001) (1, 1)
            helper' (32, 0x80000000, 0xffffffff) (1, 0)
            helper' (32, 0xffffffff, 0x00000000) (1, 0)
            helper' (32, 0xffffffff, 0x00000001) (1, 0)
            helper' (32, 0xffffffff, 0xffffffff) (1, 0)

            helper' (8, 0x00, 0x00) (1, 0)
            helper' (8, 0x00, 0x01) (1, 0)
            helper' (8, 0x00, 0xff) (1, 0)
            helper' (8, 0x7f, 0x00) (1, 0)
            helper' (8, 0x7f, 0x01) (1, 0)
            helper' (8, 0x7f, 0xff) (1, 1)
            helper' (8, 0x80, 0x00) (1, 0)
            helper' (8, 0x80, 0x01) (1, 1)
            helper' (8, 0x80, 0xff) (1, 0)
            helper' (8, 0xff, 0x00) (1, 0)
            helper' (8, 0xff, 0x01) (1, 0)
            helper' (8, 0xff, 0xff) (1, 0)

    let helper (baseW, base) (indexW, index) stride (outw, outv) = do
          let tenv = []
              base' = Ch.InfoExpression (Ch.SymInfo baseW $ Sym 0, Just (bitVec $ Just baseW))
                      . Pil.CONST . Pil.ConstOp $ base
              index' = Ch.InfoExpression (Ch.SymInfo indexW $ Sym 1, Just (bitVec $ Just indexW))
                       . Pil.CONST . Pil.ConstOp $ index
              expr = Ch.InfoExpression (Ch.SymInfo outw $ Sym 3, Just (bitVec $ Just outw))
                     . Pil.ARRAY_ADDR $ Pil.ArrayAddrOp base' index' stride

              cmd = do
                res <- solveExpr expr
                constrain $ res `svEqual` constWord outw outv

              rvars = []
              errs = []

          res <- runIO $ runSolveCmd tenv cmd
          let hex n = let n' = fromIntegral n :: Word64 in (if n' >= 0 then "0x" else "-0x") <> showHex (abs n') ""
          it (hex base <> "[" <> hex index <> " * " <> hex stride <> " bits] ~> " <> hex outv) $ do
            res `shouldBe` Right (Sat $ HashMap.fromList rvars, errs)
      in do
        context "ARRAY_ADDRESS" $ do
          context "all constants" $ do
            let base = fromIntegral (0xdeadbeefdeadbeef :: Word64) :: Int64
                res = 0xdeadbeefdeadbf3f
            helper (64, base) (32, 5) 16 (64, res)
            helper (64, base) ( 8, 5) 16 (64, res)
            helper (64, base) ( 3, 5) 16 (64, res)


    context "solveExpr: VAR" $ do
      let tenv = [(pilVar "a", char)]
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 88
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.ADD $ Pil.AddOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` constWord 8 99

          rvars = [("a", CV (KBounded False 8) (CInteger 11))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "add one constant and one var" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                           , errs )

    context "solveExpr: SUB" $ do
      let tenv = [(pilVar "a", char)]
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          -- arg1 :: DSTExpression
          -- arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
          --        . Pil.CONST . Pil.ConstOp $ 88
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.SUB $ Pil.SubOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` constWord 8 77

          rvars = [("a", CV (KBounded False 8) (CInteger 88))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: DIVS" $ do
      let tenv = [(pilVar "a", char)]
          numerator = 88
          expected = 8
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          -- arg1 :: DSTExpression
          -- arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
          --        . Pil.CONST . Pil.ConstOp $ 88
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ numerator
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.DIVS $ Pil.DivsOp arg2 arg1

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` constWord 8 expected

          rvsMatch [("a", CV (KBounded False 8) (CInteger denominator))] =
            denominator /= 0 && numerator `div` denominator == expected
          rvsMatch _ = False

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldSatisfy`
          \case
            Right (Sat rvs, []) -> rvsMatch $ HashMap.toList rvs
            _ -> False

    context "solveExpr: MODS" $ do
      let tenv = [(pilVar "a", signed32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just signed32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ 93
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just signed32)
                 . Pil.MODS $ Pil.ModsOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            rArg0 <- solveExpr arg0
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded True 32) (CInteger 5))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "positive numbers" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: MODS" $ do
      let tenv = [(pilVar "a", signed32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just signed32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ -93
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just signed32)
                 . Pil.MODS $ Pil.ModsOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            rArg0 <- solveExpr arg0
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded True 32) (CInteger $ -5))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "positive numbers" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: MODU" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 303
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 100
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.MODU $ Pil.ModuOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            rArg0 <- solveExpr arg0
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 3))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "positive numbers" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: MUL" $ do
      let tenv = [(pilVar "a", char)]
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          -- arg1 :: DSTExpression
          -- arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
          --        . Pil.CONST . Pil.ConstOp $ 88
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.MUL $ Pil.MulOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` constWord 8 77

          rvars = [("a", CV (KBounded False 8) (CInteger 7))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: INT_TO_FLOAT" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 42
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 80 $ Sym 1, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 80 $ Sym 0, Just float)
                 . Pil.INT_TO_FLOAT $ Pil.IntToFloatOp arg0

          cmd = do
            r <- solveExpr expr
            rArg1 <- solveExpr arg1
            constrain $ rArg1 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 42.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                           , errs )

    context "solveExpr: CMP_SGE" $ do
      let tenv = [(pilVar "x", char)]
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "x"
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.CMP_SGE $ Pil.CmpSgeOp arg1 arg2

          cmd = do
            r <- solveExpr expr
            constrain $ r `svEqual` svBool True

      r <- runIO $ runSolveCmd tenv cmd
      it "should provide a value x, where x >= 11" $ do
        r `shouldSatisfy` maybe False (>= 11) . getVal "x"

    context "solveExpr: CMP_SGT & CMP_SLT" $ do
      let tenv = [(pilVar "x", char)]
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.VAR . Pil.VarOp $ pilVar "x"
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          arg3 :: DSTExpression
          arg3 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 13
          expr0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.CMP_SGT $ Pil.CmpSgtOp arg1 arg2
          expr1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just char)
                 . Pil.CMP_SLT $ Pil.CmpSltOp arg1 arg3

          cmd = do
            r <- solveExpr expr0
            r2 <- solveExpr expr1
            constrain $ r `svEqual` svBool True
            constrain $ r2 `svEqual` svBool True

      r <- runIO $ runSolveCmd tenv cmd
      it "should provide a value x bound by 11 < x < 13" $ do
        getVal "x" r `shouldBe` Just 12

    context "solveExpr: FADD" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.3
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 11.0
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FADD $ Pil.FaddOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 14.3))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: CEIL" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.CEIL $ Pil.CeilOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 4.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FABS" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ -3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FABS $ Pil.FabsOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 3.3))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FLOOR" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FLOOR $ Pil.FloorOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 3.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FLOAT_TO_INT" $ do
      let tenv = [(pilVar "a", unsigned64)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 2, Just unsigned64)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 80 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.0
          expr = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 0, Just unsigned64)
                 . Pil.FLOAT_TO_INT $ Pil.FloatToIntOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 64) (CInteger 3))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                           , errs )

    context "solveExpr: FNEG" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FNEG $ Pil.FnegOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble $ -3.3))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FSQRT" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.0
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FSQRT $ Pil.FsqrtOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 2.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FTrunc" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FLOOR $ Pil.FloorOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 3.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "positive number" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FTrunc" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ -3.3
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FTRUNC $ Pil.FtruncOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble $ -3.0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "negative number" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FDIV" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 12.3
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FDIV $ Pil.FdivOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 3.0000000000000004))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FMUL" $ do
      let tenv = [(pilVar "a", float)
                 , (pilVar "b", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          argEq :: DSTExpression
          argEq = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "b"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.2
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.0
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FMUL $ Pil.FmulOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            rArgEq <- solveExpr argEq
            constrain $ rArg0 `svEqual` r
            constrain $ rArgEq `svEqual` (rArg0 `floatEqual` constFloat 12.6)

          rvars = [("a", CV KDouble (CDouble 12.600000000000001))
                  , ("b", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FSUB" $ do
      let tenv = [(pilVar "a", float)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 12.3
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just float)
                 . Pil.FSUB $ Pil.FsubOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KDouble (CDouble 8.200000000000001))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_E" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_E $ Pil.FcmpEOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_GE" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 3.2
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_GE $ Pil.FcmpGeOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_GT" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 5.2
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_GT $ Pil.FcmpGtOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_LE" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.5
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_LE $ Pil.FcmpLeOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_LT" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 2.0
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_LT $ Pil.FcmpLtOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_NE" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.0
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_NE $ Pil.FcmpNeOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_O" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ SBV.nan
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_O $ Pil.FcmpOOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: FCMP_UO" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ SBV.nan
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just float)
                 . Pil.CONST_FLOAT . Pil.ConstFloatOp $ 4.1
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.FCMP_UO $ Pil.FcmpUoOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: CMP_E" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 3, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 2, Just char)
                 . Pil.CONST . Pil.ConstOp $ 88
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 1, Just char)
                 . Pil.CONST . Pil.ConstOp $ 11
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just tbool)
                 . Pil.CMP_E $ Pil.CmpEOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: LOW_PART" $ do
      let tenv = [(pilVar "a", unsigned8)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 3, Just unsigned8)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned8)
                 . Pil.CONST . Pil.ConstOp $ 271
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 0, Just unsigned8)
                 . Pil.LOW_PART $ Pil.LowPartOp arg1
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 8) (CInteger 15))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: AND" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 92
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 15
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.AND $ Pil.AndOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 12))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: OR" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 92
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 15
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.OR $ Pil.OrOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 95))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: TEST_BIT" $ do
      let tenv = [(pilVar "a", tbool)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just tbool)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 15
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 1
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just tbool)
                 . Pil.TEST_BIT $ Pil.TestBitOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV KBool (CInteger 1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: XOR" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 92
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 15
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.XOR $ Pil.XorOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 83))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: ROL" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 1
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 3
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.ROL $ Pil.RolOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 8))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: ROR" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 1
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 3
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.ROR $ Pil.RorOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 536870912))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: LSR" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 1
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 3
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.LSR $ Pil.LsrOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 0))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: LSL" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 2
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 3
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.LSL $ Pil.LslOp arg1 arg2

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 16))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: NEG" $ do
      let tenv = [(pilVar "a", signed32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just signed32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ 2
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just signed32)
                 . Pil.NEG $ Pil.NegOp arg1

          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded True 32) (CInteger $ -2))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: NOT" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 2
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.NOT $ Pil.NotOp arg1
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 4294967293))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    let helper op (inw, x) (outw, outv) = do
          let tenv = []
              arg :: DSTExpression
              arg = Ch.InfoExpression (Ch.SymInfo inw $ Sym 1, Just (bitVec $ Just inw))
                     . Pil.CONST . Pil.ConstOp $ x
              expr = Ch.InfoExpression (Ch.SymInfo outw $ Sym 0, Just (bitVec $ Just outw))
                     $ op arg

              cmd = do
                res <- solveExpr expr
                constrain $ res `svEqual` constWord outw outv

              rvars = []
              errs = []

          res <- runIO $ runSolveCmd tenv cmd
          let hex n = let n' = fromIntegral n :: Word64 in (if n' >= 0 then "0x" else "-0x") <> showHex (abs n') ""
          it (hex x <> " ~> " <> hex outv) $ do
            res `shouldBe` Right (Sat $ HashMap.fromList rvars, errs)
      in do
        context "POPCNT" $ do
          context "one constant" $ do
            let helper' = helper (Pil.POPCNT . Pil.PopcntOp)
            helper' (64, -0x5335533553355336) (8, 32)
            helper' (64, -0x0000000000000001) (8, 64)
            helper' (64, -0x8000000000000000) (8, 1)
            helper' (32, 0x00000000) (8, 0)
            helper' (32, 0x00000001) (8, 1)
            helper' (32, 0x80000000) (8, 1)
            helper' (32, 0xf1f1f1f1) (8, 20)
            helper' (32, 0xffffffff) (8, 32)
            helper' (16, 0x0000) (8, 0)
            helper' (16, 0x0001) (8, 1)
            helper' (16, 0x8000) (8, 1)
            helper' (16, 0xf1f1) (8, 10)
            helper' (16, 0xffff) (8, 16)
            helper' (8, 0x0000) (8, 0)
            helper' (8, 0x01) (8, 1)
            helper' (8, 0x80) (8, 1)
            helper' (8, 0xf1) (8, 5)
            helper' (8, 0xff) (8, 8)

    context "solveExpr: SX" $ do
      let tenv = [(pilVar "a", signed64)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 3, Just signed64)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ -2
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 0, Just signed64)
                 . Pil.SX $ Pil.SxOp arg1
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded True 64) (CInteger $ -2))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "32 to 64 bit, negative value" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: SX" $ do
      let tenv = [(pilVar "a", signed64)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 3, Just signed64)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ 2
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 0, Just signed64)
                 . Pil.SX $ Pil.SxOp arg1
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded True 64) (CInteger 2))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "32 to 64 bit, positive value" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: ZX" $ do

      let tenv = [(pilVar "a", unsigned64)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 3, Just unsigned64)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 42
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 64 $ Sym 0, Just unsigned64)
                 . Pil.ZX $ Pil.ZxOp arg1
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 64) (CInteger 42))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "32 to 64 bit" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: ASR" $ do
      let tenv = [(pilVar "a", signed32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 3, Just signed32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just signed32)
                 . Pil.CONST . Pil.ConstOp $ -2
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 3
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just signed32)
                 . Pil.ASR $ Pil.AsrOp arg1 arg2
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          -- TODO: is this correct?
          rvars = [("a", CV (KBounded True 32) (CInteger $ -1))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "two constants of same size" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    -- TODO: All these tests could use a CONST_BOOL pil instruction for the carry:
    context "solveExpr: ADC" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 34
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 22
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ True
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.ADC $ Pil.AdcOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 57))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 1" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: SBB" $ do
      let tenv = [(pilVar "a", unsigned32)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 8 $ Sym 3, Just unsigned32)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 2, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 34
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 1, Just unsigned32)
                 . Pil.CONST . Pil.ConstOp $ 22
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ True
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 32 $ Sym 0, Just unsigned32)
                 . Pil.SBB $ Pil.SbbOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 32) (CInteger 11))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 1" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: RLC" $ do
      let tenv = [(pilVar "a", unsigned4)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 3, Just unsigned4)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 2, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 15
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 1, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 1
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ True
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 0, Just unsigned4)
                 . Pil.RLC $ Pil.RlcOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 4) (CInteger 15))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 1" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: RLC" $ do
      let tenv = [(pilVar "a", unsigned4)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 3, Just unsigned4)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 2, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 15
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 1, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 1
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ False
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 0, Just unsigned4)
                 . Pil.RLC $ Pil.RlcOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 4) (CInteger 14))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 0" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: RRC" $ do
      let tenv = [(pilVar "a", unsigned4)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 3, Just unsigned4)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 2, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 15
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 1, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 1
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ True
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 0, Just unsigned4)
                 . Pil.RRC $ Pil.RrcOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 4) (CInteger 15))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 1" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveExpr: RRC" $ do
      let tenv = [(pilVar "a", unsigned4)]
          arg0 :: DSTExpression
          arg0 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 3, Just unsigned4)
                 . Pil.VAR . Pil.VarOp $ pilVar "a"
          arg1 :: DSTExpression
          arg1 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 2, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 15
          arg2 :: DSTExpression
          arg2 = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 1, Just unsigned4)
                 . Pil.CONST . Pil.ConstOp $ 1
          c :: DSTExpression
          c = Ch.InfoExpression (Ch.SymInfo 1 $ Sym 4, Just carry)
                 . Pil.CONST_BOOL . Pil.ConstBoolOp $ False
          expr :: DSTExpression
          expr = Ch.InfoExpression (Ch.SymInfo 4 $ Sym 0, Just unsigned4)
                 . Pil.RRC $ Pil.RrcOp arg1 arg2 c
          cmd = do
            rArg0 <- solveExpr arg0
            r <- solveExpr expr
            constrain $ rArg0 `svEqual` r

          rvars = [("a", CV (KBounded False 4) (CInteger 7))]
          errs = []

      r <- runIO $ runSolveCmd tenv cmd
      it "with carry = 0" $ do
        r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                            , errs )

    context "solveStmt:" $ do

      context "Pil.Def" $ do
        let tenv = [(pilVar "a", bitVec (bw 32))]
            stmts' = [def "a" $ const 888 4]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, stmtInfoExpr) = (tReport ^. #symTypedStmts) !! 0
              solveStmt stmtInfoExpr


            rvars = [("a", CV (KBounded False 32) (CInteger 888))]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "def one var" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                              , errs )

      context "Pil.Def" $ do
        let tenv = [ (pilVar "a", bitVec (bw 32))
                   , (pilVar "b", bitVec (bw 32))
                   ]
            stmts' = [ def "a" $ const 888 4
                     , def "b" $ var "a" 4
                     ]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, defA) = (tReport ^. #symTypedStmts) !! 0
              let (_, defB) = (tReport ^. #symTypedStmts) !! 1
              solveStmt defA
              solveStmt defB


            rvars = [ ("a", CV (KBounded False 32) (CInteger 888))
                    , ("b", CV (KBounded False 32) (CInteger 888))
                    ]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "def one var equal to another" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                              , errs )
       -- eTReport `shouldBe` (Left $ Ch.UnhandledExpr)

      context "Pil.Def" $ do
        let tenv = [ (pilVar "a", bitVec (bw 32))
                   , (pilVar "b", bitVec (bw 32))
                   , (pilVar "c", bitVec (bw 32))
                   , (pilVar "d", bitVec (bw 32))
                   ]
            stmts' = [ def "a" $ const 888 4
                     , def "b" $ var "a" 4
                     , def "c" $ var "b" 4
                     , def "d" $ var "c" 4
                     ]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, defA) = (tReport ^. #symTypedStmts) !! 0
                  (_, defB) = (tReport ^. #symTypedStmts) !! 1
                  (_, defC) = (tReport ^. #symTypedStmts) !! 2
                  (_, defD) = (tReport ^. #symTypedStmts) !! 3
              solveStmt defA
              solveStmt defB
              solveStmt defC
              solveStmt defD

            rvars = [ ("a", CV (KBounded False 32) (CInteger 888))
                    , ("b", CV (KBounded False 32) (CInteger 888))
                    , ("c", CV (KBounded False 32) (CInteger 888))
                    , ("d", CV (KBounded False 32) (CInteger 888))
                    ]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "def one var equal to another" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                              , errs )

      context "Pil.Def" $ do
        let tenv = [(pilVar "a", tbool)]
            stmts' = [def "a" $ const 888 4]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, stmtInfoExpr) = (tReport ^. #symTypedStmts) !! 0
              solveStmt stmtInfoExpr

            errs = [ StmtError
                     { stmtIndex = 0
                     , stmtErr = GuardError "guardSameKind"
                       [KBool, KBounded False 32] "not same kind"
                     }]

        r <- runIO $ runSolveCmd tenv cmd
        it "mismatch variables" $ do
          fmap snd r `shouldBe` Right errs

      context "Pil.Constraint" $ do
        let tenv = [(pilVar "a", bitVec (bw 32))]
            stmts' = [constraint $ cmpE (var "a" 4) (const 42 4) 4]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, stmtInfoExpr) = (tReport ^. #symTypedStmts) !! 0
              solveStmt stmtInfoExpr

            rvars = [("a", CV (KBounded False 32) (CInteger 42))]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "def one var" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                              , errs )

      context "Pil.Store/Load" $ do
        let tenv = [(pilVar "a", unsigned32)]
            ptr = constPtr 0xdeadbeef 4
            stmts' = [ store ptr $ const 42 4
                    , def "a" $ load ptr 4
                    ]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, storeStmt) = (tReport ^. #symTypedStmts) !! 0
              let (_, loadStmt) = (tReport ^. #symTypedStmts) !! 1
              solveStmt storeStmt
              solveStmt loadStmt

            rvars = [("a", CV (KBounded False 32) (CInteger 42))]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "one store/load" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                              , errs )

      context "Pil.Load" $ do
        let tenv = [(pilVar "a", unsigned32)]
            ptr = constPtr 0xdeadbeef 4
            stmts' = [def "a" $ load ptr 4]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, loadStmt) = (tReport ^. #symTypedStmts) !! 0
              solveStmt loadStmt

            rvars = [ ("s1", CV (KBounded False 32) (CInteger 0))
                    , ("a", CV (KBounded False 32) (CInteger 0))]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "load non existing var; create free variable" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                             , errs )

      context "Pil.Load" $ do
        let tenv = [ (pilVar "a", unsigned32)
                   , (pilVar "b", unsigned32)
                   , (pilVar "c", tbool)
                   ]
            ptr = constPtr 0xdeadbeef 4
            stmts' = [ def "a" $ load ptr 4
                     , def "b" $ load ptr 4
                     , def "c" $ cmpE (var "a" 4) (var "b" 4) 4
                     ]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, arg0) = (tReport ^. #symTypedStmts) !! 0
              let (_, arg1) = (tReport ^. #symTypedStmts) !! 1
              let (_, arg2) = (tReport ^. #symTypedStmts) !! 2
              solveStmt arg0
              solveStmt arg1
              solveStmt arg2

            rvars = [ ("s3", CV (KBounded False 32) (CInteger 0))
                    , ("a", CV (KBounded False 32) (CInteger 0))
                    , ("b", CV (KBounded False 32) (CInteger 0))
                    , ("c", CV KBool (CInteger 1))
                    ]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "load non existing var; create free variable" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                             , errs)

      context "Pil.Load" $ do
        let tenv = [ (pilVar "a", unsigned64) ]
            ptr = constPtr 0xdeadbeef 4
            stmts' = [ def "a" $ zx ( sx (load ptr 1) 4) 8 ]
            eTReport = checkStmts Nothing stmts'
            (Right tReport) = eTReport

            cmd = do
              let (_, arg0) = (tReport ^. #symTypedStmts) !! 0
              solveStmt arg0

            rvars = [ ("s1", CV (KBounded False 8) (CInteger 0))
                    , ("a", CV (KBounded False 64) (CInteger 0))]
            errs = []

        r <- runIO $ runSolveCmd tenv cmd
        it "load non existing var; create free variable" $ do
          r `shouldBe` Right ( Sat $ HashMap.fromList rvars
                             , errs)
