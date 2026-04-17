{- HLINT ignore "Evaluate" -}

module Flint.Shell.Commands.PsumSpec where

import Flint.Prelude hiding (const, sym, not)

import Flint.Shell.Commands.Psum (includeInPsum)

import Blaze.Pil.Construct
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function(Function), ExternFunction(ExternFunction), FuncParamInfo(FuncParamInfo), ParamInfo(ParamInfo))
import qualified Blaze.Types.Function as Func

import Test.Hspec


sz :: Pil.Size Pil.Expression
sz = 8

-- | Helper: make a STACK_ADDR expression
stackLocal :: ByteOffset -> Pil.Expression
stackLocal off = Pil.Expression 8 . Pil.STACK_ADDR $
  Pil.StackOffset (Pil.Ctx dummyFunc 0 False) off

dummyFunc :: Function
dummyFunc = Function Nothing "test_func" (intToAddr 0x1000)
  [ FuncParamInfo $ ParamInfo "arg0" Nothing Func.In ]

-- | Helper: make a parameter variable expression
paramVar :: Pil.Symbol -> Pil.Expression
paramVar name = Pil.Expression 8 . Pil.VAR . Pil.VarOp $
  pilVar__ 8 Nothing Nothing name True Pil.UnknownLocation

-- | Helper: make a plain local variable expression
localVar :: Pil.Symbol -> Pil.Expression
localVar = flip var sz

-- | Helper: extern call dest
externDest :: Text -> Pil.CallDest Pil.Expression
externDest name = Pil.CallExtern $ ExternFunction Nothing name Nothing (intToAddr 0) []

-- | Helper: in_FS_OFFSET variable expression
fsOffsetVar :: Pil.Expression
fsOffsetVar = Pil.Expression 8 . Pil.VAR . Pil.VarOp $
  pilVar__ 8 Nothing Nothing "in_FS_OFFSET" False Pil.UnknownLocation


spec :: Spec
spec = describe "Flint.Shell.Commands.Psum" $ do
  describe "includeInPsum" $ do

    -- Rule 1: calls are included
    it "includes a call statement" $ do
      let stmt = callStmt (externDest "strncmp")
            [ localVar "lineptr", constStr "test" sz, const 4 sz ]
      includeInPsum stmt `shouldBe` True

    it "includes a def with CALL value (call result assignment)" $ do
      let stmt = defCall "iVar9" (externDest "strncmp")
            [ localVar "lineptr", constStr "test" sz, const 4 sz ] sz
      includeInPsum stmt `shouldBe` True

    -- Rule 1 exclusion: __stack_chk_fail calls
    it "excludes __stack_chk_fail call" $ do
      let stmt = callStmt (externDest "__stack_chk_fail") []
      includeInPsum stmt `shouldBe` False

    -- Rule 2: non-stack LOADs
    it "includes constraint with non-stack LOAD" $ do
      let loadExpr = load (add (paramVar "data") (const 0x178 sz) sz) sz
          condExpr = cmpUlt (const 0x31 sz) loadExpr sz
          stmt = constraint condExpr
      includeInPsum stmt `shouldBe` True

    it "includes def with non-stack LOAD" $ do
      let loadExpr = load (add (paramVar "conn") (const 0x48 sz) sz) sz
          stmt = def "val" loadExpr
      includeInPsum stmt `shouldBe` True

    -- Rule 2: non-stack STORE
    it "includes store to non-stack address" $ do
      let addr = add (paramVar "ci") (const 0x7ec sz) sz
          stmt = store addr (const 42 sz)
      includeInPsum stmt `shouldBe` True

    -- Excluded: stack-local operations
    it "excludes store to stack-local address" $ do
      let addr = add (stackLocal (-328)) (const 0 sz) sz
          stmt = store addr (const 0 sz)
      includeInPsum stmt `shouldBe` False

    it "excludes def with stack-local LOAD" $ do
      let loadExpr = load (stackLocal (-100)) sz
          stmt = def "localVal" loadExpr
      includeInPsum stmt `shouldBe` False

    -- Excluded: scalar-only constraints
    it "excludes scalar-only constraint" $ do
      let condExpr = cmpE (localVar "iVar9") (const 0 sz) sz
          stmt = constraint condExpr
      includeInPsum stmt `shouldBe` False

    it "excludes parameter-only constraint (no LOAD)" $ do
      let condExpr = Pil.Expression 1 . Pil.NOT . Pil.NotOp $ paramVar "httpheader"
          stmt = constraint condExpr
      includeInPsum stmt `shouldBe` False

    -- Excluded: Ret, Nop
    it "excludes Ret statement" $ do
      let stmt = ret (const 0 sz)
      includeInPsum stmt `shouldBe` False

    it "excludes Nop statement" $ do
      includeInPsum nop `shouldBe` False

    -- Excluded: in_FS_OFFSET (stack canary boilerplate)
    it "excludes in_FS_OFFSET load" $ do
      let loadExpr = load (add fsOffsetVar (const 0x28 sz) sz) sz
          stmt = def "canary" loadExpr
      includeInPsum stmt `shouldBe` False

    it "excludes constraint comparing in_FS_OFFSET" $ do
      let loadExpr = load (add fsOffsetVar (const 0x28 sz) sz) sz
          condExpr = cmpE (localVar "saved") loadExpr sz
          stmt = constraint condExpr
      includeInPsum stmt `shouldBe` False

    -- Edge case: stack-local store with interesting value
    it "includes stack-local store if value has a call" $ do
      let callExpr = mkExpr sz . Pil.CALL $ Pil.CallOp (externDest "malloc") [const 0x70 sz]
          stmt = store (stackLocal (-200)) callExpr
      includeInPsum stmt `shouldBe` True

    it "includes stack-local store if value has non-stack load" $ do
      let loadExpr = load (add (paramVar "src") (const 0 sz) sz) sz
          stmt = store (stackLocal (-200)) loadExpr
      includeInPsum stmt `shouldBe` True

    -- Pure local computation
    it "excludes def with pure arithmetic on locals" $ do
      let stmt = def "counter" (add (localVar "i") (const 1 sz) sz)
      includeInPsum stmt `shouldBe` False
