{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -w #-}

module Blaze.Cfg.PilLoopSpec (spec) where


import Blaze.Function (Function (..))
import qualified Blaze.Cfg as Cfg
import Blaze.Cfg (Cfg, CfEdge (..), BranchType (..))
import Blaze.Prelude
import Test.Hspec
import Blaze.Types.Pil (Ctx(Ctx))
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Construct as C
import Blaze.Util.Spec (bb, mkUuid2)
import Blaze.Types.Graph (Identifiable (getNodeId))
import Blaze.Types.HighCfg (HighCfg(..), HighEdge (..))
import Blaze.Types.HighCfNode
import qualified Blaze.CfgDot as CfgDot

import qualified Data.BinaryAnalysis as BA
import qualified Data.HashSet as HashSet

a :: Int64 -> Address
a = BA.intToAddr

byteSize :: Pil.Size Pil.Expression
byteSize = 8

{-
          [00] <- i = 0
     _____  |
    /     \ |
    |     [10] _ <- i < 100 conditional branching to [20] or [50]
    |  True |   \_ False [50]
    |     [20]
    |       |
    |     [30]
    |       |
    |     [40] <- this should have i = i + 1
    \_____/  
-}

pilCfgWithForLoop :: Cfg (HighCfNode [Pil.Stmt])
pilCfgWithForLoop =
  Cfg.mkCfg 0
    bb00
    [ bb00, bb10, bb20, bb30, bb40, bb50]
    [ CfEdge bb00 bb10 UnconditionalBranch -- Entry into loop
    , CfEdge bb10 bb20 TrueBranch -- To loop body if condition true
    , CfEdge bb10 bb50 FalseBranch -- To exit if condition false
    , CfEdge bb20 bb30 UnconditionalBranch
    , CfEdge bb30 bb40 UnconditionalBranch
    , CfEdge bb40 bb10 UnconditionalBranch -- Back to loop header
    ]
    

highCfgWithForLoop :: HighCfg [Pil.Stmt]
highCfgWithForLoop = HighCfg
  { highEdges = highEdges
  , focus = topCfg
  }
  where
    sequenceNode = Sequence $ SequenceNode
      { termNodeId = getNodeId bb50
      , uuid = getNodeUUID bb20
      , nodeData = [] -- TODO see note in HighCfNode
      , body = Cfg.mkCfg 0
          bb20
          [bb20, bb30]
          -- Note: The loop back to the header only lives as a High edge
          [ CfEdge bb20 bb30 UnconditionalBranch]
      }
    forLoopNode = ForLoop $ ForLoopNode
      { termNodeId = getNodeId bb50
      , uuid = getNodeUUID bb10
      , nodeData = [] -- TODO
      , header = bb10
      , initNode = bb00
      , initStmt = [initializeStmt]
      , condNode = bb10
      , condStmt = [conditionalStmt]
      , incNode = bb40
      , incStmt = [incrementStmt]
      , body = Cfg.mkCfg 0
          bb10
          [bb20, sequenceNode]
           -- Note: The FalseBranch goes to exit bb50 but that only lives as a High edge
          -- This points at the whole sequence
          [ CfEdge bb10 sequenceNode TrueBranch]
      }
    topCfg = Cfg.mkCfg 0 bb00
        [ bb00, forLoopNode, bb50 ]
        [ CfEdge bb00 forLoopNode UnconditionalBranch ]
    highEdges = HashSet.fromList
        [ HighEdge bb00.uuid bb10.uuid UnconditionalBranch
        , HighEdge bb10.uuid bb50.uuid FalseBranch
        , HighEdge bb40.uuid bb10.uuid UnconditionalBranch
        ]


-- Entry i = 0
bb00 = toHighCfNode $ bb ctx (a 0x00) (a 0x0F) [initializeStmt]

-- Conditional
bb10 :: HighCfNode [Pil.Stmt]
bb10 = toHighCfNode $ bb ctx (a 0x10) (a 0x1F) [conditionalStmt]

-- Loop Body in a sequence of blocks
bb20 = toHighCfNode $ bb ctx (a 0x20) (a 0x2F) [C.nop]
bb30 = toHighCfNode $ bb ctx (a 0x30) (a 0x3F) [C.nop]

-- Increment statement i = i + 1
bb40 :: HighCfNode [Pil.Stmt]
bb40 = toHighCfNode $ bb ctx (a 0x40) (a 0x4F) [incrementStmt]

-- Exit
bb50 = toHighCfNode $ bb ctx (a 0x50) (a 0x5F) [C.nop]

-- i = i + 1
incrementStmt :: Pil.Stmt
incrementStmt = C.store iPilVar' (C.add (C.load iPilVar' byteSize) (C.const 1 byteSize) byteSize)

-- TODO idk might be wrong but probably close enough
-- i < 100
conditionalStmt :: Pil.Stmt
conditionalStmt = C.branchCond $ C.cmpSle (C.load iPilVar' byteSize) (C.const 100 byteSize) byteSize


iPilVar :: Pil.PilVar
iPilVar = C.pilVar 8 "i"

iPilVar' :: Pil.Expression
iPilVar' = C.var' iPilVar 8

-- i = 0
initializeStmt :: Pil.Stmt
initializeStmt = C.store iPilVar' (C.const 0 byteSize)

ctx :: Ctx
ctx = Ctx func1 (Pil.CtxId 0)

func1 :: Function
func1 = Function
  { symbol = Nothing
  , name = "TODO idk"
  , address = a 0x1000
  , params = []
  }

spec :: Spec
spec = describe "PilLoopSpec" $ do
    it "placeholder" $ do
      True `shouldBe` True
