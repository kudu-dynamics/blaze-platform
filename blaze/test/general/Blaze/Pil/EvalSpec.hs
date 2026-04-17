module Blaze.Pil.EvalSpec where

import Blaze.Prelude hiding
  ( const,
    group,
    sym,
  )
-- import Blaze.Types.Pil
import Blaze.Pil.Eval
import qualified Blaze.Pil.Construct as C

import Test.Hspec


spec :: Spec
spec = describe "Blaze.Pil.Analysis.Eval" $ do
  describe "evalPilArithmeticExpr" $ do
    it "should return a const unchanged" $ do
      let expr = C.const 888 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected
      simplifyPilExpr expr `shouldBe` expr

    it "should return a float const unchanged" $ do
      let expr = C.fconst 888.0 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected
      simplifyPilExpr expr `shouldBe` expr

    it "should return a pointer const unchanged" $ do
      let expr = C.constPtr 888 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected
      simplifyPilExpr expr `shouldBe` expr

    it "should add two 64 bit consts" $ do
      let expr = C.add (C.const 100 8) (C.const 200 8) 8
          expected = Just $ C.const 300 8
          expected' = C.const 300 8
      evalPilArithmeticExpr expr `shouldBe` expected
      simplifyPilExpr expr `shouldBe` expected'

    it "should add const ptr and const" $ do
      let expr = C.add (C.constPtr 100 8) (C.const 200 8) 8
          expected = Just $ C.constPtr 300 8
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should add const and const ptr" $ do
      let expr = C.add (C.const 100 8) (C.constPtr 200 8) 8
          expected = Just $ C.constPtr 300 8
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should properly evalulate overflow of int op" $ do
      let expr = C.add (C.const 127 1) (C.const 5 1) 1
          expected = Just $ C.const (-124) 1
      evalPilArithmeticExpr expr `shouldBe` expected


    it "should return constants exactly as is" $ do
      let expr = C.const 127 1
          expected = C.const 127 1
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.constPtr 256 4
          expected' = C.constPtr 256 4
      simplifyPilExpr expr' `shouldBe` expected'      
      let expr'' = C.fconst 127.99 8
          expected'' = C.fconst 127.99 8
      simplifyPilExpr expr'' `shouldBe` expected''
      let expr''' = C.constBool True 8
          expected''' = C.constBool True 8
      simplifyPilExpr expr''' `shouldBe` expected'''
      let expr4 = C.constStr "test" 8
          expected4 = C.constStr "test" 8
      simplifyPilExpr expr4 `shouldBe` expected4

    it "should ADD" $ do
      let expr = C.add (C.const 127 8) (C.const 128 8) 8
          expected = C.const 255 8
      simplifyPilExpr expr `shouldBe` expected
 
    it "should AND" $ do
      let expr = C.and (C.constBool True 8) (C.constBool False 8) 8
          expected = C.constBool False 8
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.and (C.const 1 2) (C.const 7 2) 2
          expected' = C.const 1 2
      simplifyPilExpr expr' `shouldBe` expected'

    {-
    ASR
    BOOL_TO_INT
    CEIL
    -}

    it "should CMP_E" $ do
      let expr = C.cmpE (C.const 67 2) (C.const 65 2) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpE (C.sx (C.unit 2) 4) (C.sx (C.unit 2) 4) 4
          expected' = C.cmpE (C.sx (C.unit 2) 4) (C.sx (C.unit 2) 4) 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpE (C.const 55 2) (C.const 55 2) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should CMP_NE" $ do
      let expr = C.cmpNE (C.const 67 2) (C.const 65 2) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpNE (C.sx (C.unit 2) 4) (C.sx (C.unit 2) 4) 4
          expected' = C.cmpNE (C.sx (C.unit 2) 4) (C.sx (C.unit 2) 4) 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpNE (C.const 55 2) (C.const 55 2) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should CMP_SGE" $ do
      let expr = C.cmpSge (C.const (-55) 4) (C.const (-33) 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpSge (C.fconst (-44.3) 4) (C.fconst (-55.2) 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpSge (C.const 77 4) (C.const 77 4) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should CMP_SGT" $ do
      let expr = C.cmpSgt (C.const (-55) 4) (C.const (-33) 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpSgt (C.fconst (-44.3) 4) (C.fconst (-55.2) 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpSgt (C.const 77 4) (C.const 77 4) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should CMP_SLE" $ do
      let expr = C.cmpSle (C.fconst 23.74 4) (C.fconst 55 4) 8
          expected = C.constBool True 8
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpSle (C.const 101 4) (C.const (-1) 4) 8
          expected' = C.constBool False 8
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpSle (C.const 11 4) (C.const 11 4) 8
          expected'' = C.constBool True 8
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should CMP_SLT" $ do
      let expr = C.cmpSlt (C.fconst 23.74 4) (C.fconst 55 4) 8
          expected = C.constBool True 8
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpSlt (C.const 101 4) (C.const (-1) 4) 8
          expected' = C.constBool False 8
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpSlt (C.const 11 4) (C.const 11 4) 8
          expected'' = C.constBool False 8
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should CMP_UGE" $ do
      let expr = C.cmpUge (C.const (-5) 4) (C.const 4 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpUge (C.const 10 4) (C.const 10 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpUge (C.const 7 4) (C.const 10 4) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''
      let expr''' = C.cmpUge (C.const (-5) 4) (C.const (-4) 4) 4
          expected''' = C.constBool False 4
      simplifyPilExpr expr''' `shouldBe` expected'''
    
    it "should CMP_UGT" $ do
      let expr = C.cmpUgt (C.const (-5) 4) (C.const 4 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpUgt (C.const 10 4) (C.const 10 4) 4
          expected' = C.constBool False 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpUgt (C.const 7 4) (C.const 10 4) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''
      let expr''' = C.cmpUgt (C.const (-5) 4) (C.const (-4) 4) 4
          expected''' = C.constBool False 4
      simplifyPilExpr expr''' `shouldBe` expected'''
    
    it "should CMP_ULE" $ do
      let expr = C.cmpUle (C.const (-5) 4) (C.const 4 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpUle (C.const 10 4) (C.const 10 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpUle (C.const 7 4) (C.const 10 4) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''
      let expr''' = C.cmpUle (C.const (-5) 4) (C.const (-4) 4) 4
          expected''' = C.constBool True 4
      simplifyPilExpr expr''' `shouldBe` expected'''
    
    it "should CMP_ULT" $ do
      let expr = C.cmpUlt (C.const (-5) 4) (C.const 4 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.cmpUlt (C.const 10 4) (C.const 10 4) 4
          expected' = C.constBool False 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.cmpUlt (C.const 7 4) (C.const 10 4) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''
      let expr''' = C.cmpUlt (C.const (-5) 4) (C.const (-4) 4) 4
          expected''' = C.constBool True 4
      simplifyPilExpr expr''' `shouldBe` expected'''

      {-
      DIVS
      DIVS_DP
      DIVU
      DIVU_DP
      FABS
      FADD
      -}


    it "should FCMP_E" $ do
      let expr = C.fcmpE (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpE (C.fconst 4.3 4) (C.fconst 5.14 4) 4
          expected' = C.constBool False 4
      simplifyPilExpr expr' `shouldBe` expected'

    it "should FCMP_GE" $ do
      let expr = C.fcmpGe (C.fconst 4.3 4) (C.fconst 1.1 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpGe (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.fcmpGe (C.fconst (-4.3) 4) (C.fconst 1.1 4) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should FCMP_GT" $ do
      let expr = C.fcmpGt (C.fconst 4.3 4) (C.fconst 1.1 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpGt (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected' = C.constBool False 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.fcmpGt (C.fconst (-4.3) 4) (C.fconst 1.1 4) 4
          expected'' = C.constBool False 4
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should FCMP_LE" $ do
      let expr = C.fcmpLe (C.fconst 4.3 4) (C.fconst 1.1 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpLe (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.fcmpLe (C.fconst (-4.3) 4) (C.fconst 1.1 4) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should FCMP_LT" $ do
      let expr = C.fcmpLt (C.fconst 4.3 4) (C.fconst 1.1 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpLt (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected' = C.constBool False 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.fcmpLt (C.fconst (-4.3) 4) (C.fconst 1.1 4) 4
          expected'' = C.constBool True 4
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should FCMP_NE" $ do
      let expr = C.fcmpNE (C.fconst 4.3 4) (C.fconst 4.3 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.fcmpNE (C.fconst 4.3 4) (C.fconst 5.14 4) 4
          expected' = C.constBool True 4
      simplifyPilExpr expr' `shouldBe` expected'

    {-
    FCMP_O
    FCMP_UO
    FDIV
    FLOAT_CONV
    FLOAT_TO_INT
    FLOOR
    FMUL
    FNEG
    FSQRT
    FSUB
    FTRUNC
    INT_TO_FLOAT
    LOAD
    LOW_PART
    LSL
    LSR
    MODS
    MODS_DP
    MODU
    MODU_DP
    MUL
    MULS_DP
    MULU_DP
    NEG
    -}

    it "should NOT" $ do
      let expr = C.not (C.constBool True 4) 4
          expected = C.constBool False 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.not (C.const 5 4) 4
          expected' = C.const (-6) 4
      simplifyPilExpr expr' `shouldBe` expected'

    it "should OR" $ do
      let expr = C.or (C.constBool True 4) (C.constBool False 4) 4
          expected = C.constBool True 4
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.or (C.const 11 4) (C.const 4 4) 4
          expected' = C.const 15 4
      simplifyPilExpr expr' `shouldBe` expected'
    
    {-
    POPCNT
    RLC
    ROL
    ROR
    ROUND_TO_INT
    RRC
    SBB
    -}

    it "should SUB" $ do
      let expr = C.sub (C.const 5 4) (C.const 1 4) 8
          expected = C.const 4 8
      simplifyPilExpr expr `shouldBe` expected
      let expr' = C.sub (C.const 17 4) (C.const 19 4) 8
          expected' = C.const (-2) 8
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.sub (C.const 17 4) (C.const 17 4) 8
          expected'' = C.const 0 8
      simplifyPilExpr expr'' `shouldBe` expected''

    {-
    SUB_WILL_OVERFLOW
    -}

    it "should simplify SX" $ do
      let expr = C.sx (C.sx (C.unit 2) 4) 8
          expected = C.sx (C.unit 2) 8
      simplifyPilExpr expr `shouldBe` expected 
      let expr' = C.sx (C.sx (C.sx (C.sx (C.unit 2) 4) 8) 2) 4
          expected' = C.sx (C.sx (C.unit 2) 8) 4
          --expected' = C.sx (C.unit 2) 8
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.sx (C.sx (C.sx (C.zx (C.sx (C.sx (C.unit 4) 4) 1) 4) 4) 2) 2
          expected'' = C.sx (C.sx (C.zx (C.sx (C.sx (C.unit 4) 4) 1) 4) 4) 2
          --expected'' = C.sx (C.zx (C.sx (C.unit 4) 4) 4) 4
      simplifyPilExpr expr'' `shouldBe` expected''

    it "should evaluate SX" $ do
      let expr = C.sx (C.sx (C.const 127 4) 8) 2
          expected = C.const 127 2
      simplifyPilExpr expr `shouldBe` expected

    it "should sign extend" $ do
      let expr = C.sx (C.const (-1) 2) 4
          expected = C.const (-1) 4
      simplifyPilExpr expr `shouldBe` expected
    
    it "should truncate" $ do
      let expr = C.sx (C.const 30000 2) 1
          expected = C.const 48 1
      simplifyPilExpr expr `shouldBe` expected 

    {-
    TEST_BIT
    UNIMPL
    VAR_JOIN
    VAR
    VAR_FIELD
    XOR
    -}

    it "should simplify ZX" $ do
      let expr = C.zx (C.zx (C.unit 2) 4) 8
          expected = C.zx (C.unit 2) 8
      simplifyPilExpr expr `shouldBe` expected 
      let expr' = C.zx (C.zx (C.zx (C.zx (C.unit 2) 4) 8) 2) 4
          expected' = C.zx (C.zx (C.unit 2) 8) 4
      simplifyPilExpr expr' `shouldBe` expected'
      let expr'' = C.zx (C.zx (C.zx (C.sx (C.zx (C.zx (C.unit 4) 4) 1) 4) 4) 2) 2
          expected'' = C.zx (C.zx (C.sx (C.zx (C.zx (C.unit 4) 4) 1) 4) 4) 2
      simplifyPilExpr expr'' `shouldBe` expected''
    
    it "should evaluate ZX" $ do
      let expr = C.zx (C.zx (C.const 127 4) 8) 2
          expected = C.const 127 2
      simplifyPilExpr expr `shouldBe` expected

    it "should zero extend" $ do
      let expr = C.zx (C.const (-1) 2) 4
          expected = C.const 65535 4
      simplifyPilExpr expr `shouldBe` expected
    
    it "should truncate" $ do
      let expr = C.zx (C.const 30000 2) 1
          expected = C.const 48 1
      simplifyPilExpr expr `shouldBe` expected

    it "should be different after conversion" $ do
      let expr = C.zx (C.const (-5) 2) 4
          expected = C.const 65531 4
      simplifyPilExpr expr `shouldBe` expected

    {-
    CALL
    Extract
    StrCmp
    StrNCmp
    MemCmp
    ExternPtr
    STACK_ADDR
    UPDATE_VAR
    FIELD_ADDR
    UNIT
    ARRAY_ADDR
    -}