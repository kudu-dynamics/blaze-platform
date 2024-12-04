{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Blaze.Cfg.PathSpec where

import Blaze.Prelude hiding (pred)

import Blaze.Function (Function (Function))
import Blaze.Function qualified as Func
import Blaze.Cfg (Cfg, CfNode, BranchType(UnconditionalBranch, TrueBranch, FalseBranch), PilNode, mkCfg)
import qualified Blaze.Cfg as Cfg
import Test.Hspec
import qualified Blaze.Types.Path as P
import Blaze.Types.Pil (Ctx(Ctx), CtxId, PilVar, Stmt)
import qualified Blaze.Types.Graph as G
import Blaze.Cfg.Path (UnrollLoopState(UnrollLoopState, definedVars, visitedNodes), getAllSimplePaths, getSimplePathsContaining, getSimpleReturnPaths, initSequenceChooserState, samplePathContainingSequence_, updatePossiblyLoopingNode, updatePossiblyLoopingStmt, updatePilVarName_)
import qualified Blaze.Pil.Construct as C
import qualified Blaze.Types.Cfg.Path as CfgP
import qualified Blaze.Cfg.Path as CfgP
import Blaze.Types.Cfg.Path (Path)
import Blaze.Util.Spec (bb, mkUuid1, mkCallNode)
import qualified Data.HashSet as HashSet
import Data.String (fromString)
import Blaze.Types.Graph (Identifiable)
import Blaze.Types.Path ((-|), (|-), start)
import Blaze.Pretty (PrettyShow'(PrettyShow'), FullCfNode(FullCfNode))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID


pilVar' :: Ctx -> Text -> PilVar
pilVar' = C.pilVar' 8

-- TODO: Make Blaze.Types.Cfg/mkCfg like this so that it doesn't require the
-- redundant [n] nodes arg.
mkCfg'
  :: (Identifiable a UUID, Hashable a)
  => CtxId
  -> a
  -> [(BranchType, (a, a))]
  -> Cfg a
mkCfg' nextCtxIndex_ root edges = mkCfg nextCtxIndex_ root restNodes edges'
  where
    edges' = Cfg.fromTupleEdge <$> edges
    restNodes = HashSet.toList
                . HashSet.delete root
                $ foldr (\(_, (a, b)) s -> foldr HashSet.insert s [a, b]) HashSet.empty edges

mkTextCfg
  :: CtxId
  -> CfNode Text
  -> [(BranchType, (CfNode Text, CfNode Text))]
  -> Cfg (CfNode Text)
mkTextCfg = mkCfg'

instance IsString (CfNode Text) where
  fromString = bbn . cs

bbn :: Text -> CfNode Text
bbn name = bb ctx x x name
  where
    func = Function Nothing "foo" 0x00 []
    ctx = Ctx func 0
    x = fromIntegral $ hash name

cfgSingleNode :: Cfg (CfNode Text)
cfgSingleNode = mkTextCfg 0 "a" []

cfgSinglePath :: Cfg (CfNode Text)
cfgSinglePath = mkTextCfg 0 "a"
    [ (UnconditionalBranch, ("a", "b"))
    , (UnconditionalBranch, ("b", "c"))
    , (UnconditionalBranch, ("c", "d"))
    ]

cfgTwoPaths :: Cfg (CfNode Text)
cfgTwoPaths = mkTextCfg 0 "a"
  [ (FalseBranch, ("a", "b"))
  , (TrueBranch, ("a", "c"))
  , (UnconditionalBranch, ("b", "fin"))
  , (UnconditionalBranch, ("c", "fin"))
  ]

cfgTwoPathsButOneLoops :: Cfg (CfNode Text)
cfgTwoPathsButOneLoops = mkTextCfg 0 "a"
  [ (FalseBranch, ("a", "b"))
  , (TrueBranch, ("a", "c"))
  , (UnconditionalBranch, ("b", "a"))
  , (UnconditionalBranch, ("c", "fin"))
  ]


cfgWithGroupingNode :: Cfg (CfNode Text)
cfgWithGroupingNode = mkTextCfg 0 "aa"
  [ (FalseBranch, ("aa", gnode))
  , (TrueBranch, ("aa", "cc"))
  , (UnconditionalBranch, (gnode, "ffin"))
  , (UnconditionalBranch, ("cc", "ffin"))
  ]
  where
    gnode :: CfNode Text
    gnode = Cfg.Grouping $ Cfg.GroupingNode
      { termNodeId = G.getNodeId (bbn "fin")
      , Cfg.uuid = mkUuid1 (888 :: Int)
      , Cfg.grouping = cfgTwoPaths
      , Cfg.nodeData = "group"
      }

-- This is the outling for a similar PIL cfg used to mock out func4
cfgWithLoopAndInnerBranch :: Cfg (CfNode Text)
cfgWithLoopAndInnerBranch = mkTextCfg 0 "a"
  [ (FalseBranch, ("a", "b"))
  , (TrueBranch, ("a", "r"))
  
  , (UnconditionalBranch, ("b", "c"))

  , (FalseBranch, ("c", "d"))
  , (TrueBranch, ("c", "r"))

  , (FalseBranch, ("d", "e"))
  , (TrueBranch, ("d", "f"))

  , (UnconditionalBranch, ("e", "g"))
  , (UnconditionalBranch, ("f", "g"))

  , (UnconditionalBranch, ("g", "c"))
  ]


--- Mock PilPaths

bbp :: Ctx -> Text -> [Stmt] -> CfNode [Stmt]
bbp ctx_ name stmts = Cfg.BasicBlock $ Cfg.BasicBlockNode
  { ctx = ctx_
  , start = fromIntegral numname
  , end = fromIntegral numname
  , uuid = bbpUuid ctx_ name
  , nodeData = stmts
  }
  where
    numname = hash (ctx_ ^. #func, name)

bbp' :: Ctx -> Text -> [Stmt] -> CfNode [Stmt]
bbp' ctx_ name stmts = Cfg.BasicBlock $ Cfg.BasicBlockNode
  { ctx = ctx_
  , start = 0
  , end = 0
  , uuid = bbpUuid ctx_ name
  , nodeData = stmts
  }

bbpUuid :: Ctx -> Text -> UUID
bbpUuid ctx_ t = mkUuid1 $ hash (ctx_ ^. #func, t)

mkEnterFunc :: Text -> Ctx -> Ctx -> [Stmt] -> CfNode [Stmt]
mkEnterFunc name prevCtx_ nextCtx_ stmts = Cfg.EnterFunc $ Cfg.EnterFuncNode
  { prevCtx = prevCtx_
  , nextCtx = nextCtx_
  , callSiteAddress = 0
  , uuid = mkUuid1 $ hash (prevCtx_, nextCtx_, name)
  , nodeData = stmts
  }

mkLeaveFunc :: Text -> Ctx -> Ctx -> [Stmt] -> CfNode [Stmt]
mkLeaveFunc name prevCtx_ nextCtx_ stmts = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
  { prevCtx = prevCtx_
  , nextCtx = nextCtx_
  , callSiteAddress = 0
  , uuid = mkUuid1 $ hash (prevCtx_, nextCtx_, name)
  , nodeData = stmts
  }

func0 :: Function
func0 = Function
  { symbol = Nothing
  , name = "func0"
  , address = 0
  , params = []
  }

func1 :: Function
func1 = Function
  { symbol = Nothing
  , name = "func1"
  , address = 100
  , params = []
  }

func2 :: Function
func2 = Function
  { symbol = Nothing
  , name = "func2"
  , address = 200
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.In
             ]
  }

func3 :: Function
func3 = Function
  { symbol = Nothing
  , name = "func3"
  , address = 300
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In ]
  }

func4 :: Function
func4 = Function
  { symbol = Nothing
  , name = "func4"
  , address = 400
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.In
             ]
  }

func5 :: Function
func5 = Function
  { symbol = Nothing
  , name = "func5"
  , address = 500
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.In
             ]
  }

funcBar :: Function
funcBar = Function Nothing "bar" 0xb00 []

funcFoo :: Function
funcFoo = Function Nothing "foo" 0xf00 []

func0Path :: Path (CfNode [Stmt])
func0bb0 :: (CtxId -> CfNode [Stmt])
func0call0 :: (CtxId -> Cfg.CallNode [Stmt])
func0call1 :: (CtxId -> Cfg.CallNode [Stmt])
func0bb1 :: (CtxId -> CfNode [Stmt])
func0ctx :: (CtxId -> Ctx)
(func0Path, func0bb0, func0call0, func0call1, func0bb1, func0ctx) =
  ( CfgP.build 1
    $ start (bb0 0)
    -| UnconditionalBranch |- Cfg.Call (call0 0)
    -| UnconditionalBranch |- Cfg.Call (call1 0)
    -| UnconditionalBranch |- bb1 0
  , bb0
  , call0
  , call1
  , bb1
  , mkCtx
  )
  where
    mkCtx :: CtxId -> Ctx
    mkCtx = Ctx func0
    [x, y, r1, r2] = (\t ctxId -> pilVar' (mkCtx ctxId) t) <$> ["x", "y", "r1", "r2"]
    bb0 i = bbp (mkCtx i) "bb0"
      [ C.def' (x i) $ C.const 54 0x8
      , C.def' (y i) $ C.const 88 0x8
      ]
    call0 i = fst $ mkCallNode (mkCtx i) "callFunc3.0" (r1 i) func3
      [ C.var' (x i) 8
      ]
    call1 i = fst $ mkCallNode (mkCtx i) "callFunc3.1" (r2 i) func3
      [ C.var' (y i) 8
      ]
    bb1 i = bbp (mkCtx i) "bb2"
      [ C.ret $ C.add (C.var' (r1 i) 8) (C.var' (r2 i) 8) 8
      ]

func1Path :: Path (CfNode [Stmt])
func1bb0 :: (CtxId -> CfNode [Stmt])
func1call0 :: (CtxId -> Cfg.CallNode [Stmt])
func1bb1 :: (CtxId -> CfNode [Stmt])
func1ctx :: (CtxId -> Ctx)
(func1Path, func1bb0, func1call0, func1bb1, func1ctx) =
  ( CfgP.build 1
    $ start (bb0 0) -| UnconditionalBranch |- Cfg.Call (call0 0) -| UnconditionalBranch |- bb1 0
  , bb0
  , call0
  , bb1
  , mkCtx
  )
  where
    mkCtx :: CtxId -> Ctx
    mkCtx = Ctx func1
    [x, y, r] = (\t ctxId -> pilVar' (mkCtx ctxId) t) <$> ["x", "y", "r"]
    bb0 i = bbp (mkCtx i) "bb0"
      [ C.def' (x i) $ C.const 54 0x8
      , C.def' (y i) $ C.const 88 0x8
      ]
    call0 i = fst $ mkCallNode (mkCtx i) "callFunc2" (r i) func2
      [ C.var' (x i) 8
      , C.var' (y i) 8
      ]
    bb1 i = bbp (mkCtx i) "bb1"
      [ C.ret $ C.var' (r i) 8
      ]
    
func2Path :: Path (CfNode [Stmt])
func2bb0 :: (CtxId -> CfNode [Stmt])
func2bb1 :: (CtxId -> CfNode [Stmt])
func2call0 :: (CtxId -> Cfg.CallNode [Stmt])
func2bb2 :: (CtxId -> CfNode [Stmt])
func2ctx :: (CtxId -> Ctx)
(func2Path, func2bb0, func2bb1, func2call0, func2bb2, func2ctx) =
  ( CfgP.build 1 $
    start (bb0 0) -| TrueBranch |- bb1 0 -| UnconditionalBranch |- Cfg.Call (call0 0) -| UnconditionalBranch |- bb2 0
  , bb0
  , bb1
  , call0
  , bb2
  , mkCtx
  )
  where
    mkCtx :: CtxId -> Ctx
    mkCtx = Ctx func2
    [arg1, arg2, x, r, z] = (\t ctxId -> pilVar' (mkCtx ctxId) t) <$> ["arg1", "arg2", "x", "r", "z"]
    bb0 i = bbp (mkCtx i) "bb0"
      [ C.branchCond $ C.cmpSlt (C.var' (arg1 i) 8) (C.var' (arg2 i) 8) 8
      ]

    bb1 i = bbp (mkCtx i) "bb1"
      [ C.def' (x i) $ C.add (C.var' (x i) 8) (C.const 5 8) 8
      ]
    call0 i = fst $ mkCallNode (mkCtx i) "callFunc3" (r i) func3
      [ C.var' (x i) 8
      ]
    bb2 i = bbp (mkCtx i) "bb2"
      [ C.def' (z i) $ C.mul (C.var' (arg2 i) 8) (C.var' (r i) 8) 8
      , C.ret $ C.var' (r i) 8
      ]

func3Path :: Path (CfNode [Stmt])
func3bb0 :: (CtxId -> CfNode [Stmt])
func3ctx :: (CtxId -> Ctx)
(func3Path, func3bb0, func3ctx) =
  ( CfgP.build 1 $ start (bb0 0)
  , bb0
  , mkCtx
  )
  where
    mkCtx :: CtxId -> Ctx
    mkCtx = Ctx func3
    [arg1, x] = (\t ctxId -> pilVar' (mkCtx ctxId) t) <$> ["arg1", "x"]
    bb0 i = bbp (mkCtx i) "bb0"
      [ C.def' (x i) $ C.add (C.var' (arg1 i) 8) (C.var' (arg1 i) 8)  8
      , C.ret (C.var' (x i) 8)
      ]

func3PathNoRet :: Path (CfNode [Stmt])
func3bb0NoRet :: (CtxId -> CfNode [Stmt])
func3ctxNoRet :: (CtxId -> Ctx)
(func3PathNoRet, func3bb0NoRet, func3ctxNoRet) =
  ( CfgP.build 1 $ start (bb0 0)
  , bb0
  , mkCtx
  )
  where
    mkCtx :: CtxId -> Ctx
    mkCtx = Ctx func3
    [arg1, x] = (\t ctxId -> pilVar' (mkCtx ctxId) t) <$> ["arg1", "x"]
    bb0 i = bbp (mkCtx i) "bb0"
      [ C.def' (x i) $ C.add (C.var' (arg1 i) 8) (C.var' (arg1 i) 8)  8
      ]

func5path :: Path PilNode
func5cfg :: Cfg PilNode
func5bb1 :: PilNode
func5bb2 :: PilNode
func5bb3 :: PilNode
func5bb3_1 :: PilNode
func5bb4 :: PilNode
func5bb5 :: PilNode
func5bb6 :: PilNode
func5bb7 :: PilNode
func5bb7_1 :: PilNode
func5bb8 :: PilNode
func5ctx :: Ctx
(func5path, func5cfg, func5bb1, func5bb2, func5bb3, func5bb3_1, func5bb4, func5bb5, func5bb6, func5bb7, func5bb7_1, func5bb8, func5ctx) =
  ( CfgP.build 1 $
    start bb1
    -| FalseBranch |- bb2
    -| UnconditionalBranch |- bb3
    -| FalseBranch |- bb4
    -| FalseBranch |- bb5
    -| UnconditionalBranch |- bb7
    -| UnconditionalBranch |- bb3_1
    -| FalseBranch |- bb4_1
    -| UnconditionalBranch |- bb6
    -| UnconditionalBranch |- bb7_1
    -| UnconditionalBranch |- bb3_2
    -| TrueBranch |- bb8inPath
  , mkCfg 1 bb1 [ bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8inCfg ]
    . fmap Cfg.fromTupleEdge
    $ [ (FalseBranch, (bb1, bb2))
      , (TrueBranch, (bb1, bb8inCfg))

      , (UnconditionalBranch, (bb2, bb3))

      , (TrueBranch, (bb3, bb8inCfg))
      , (FalseBranch, (bb3, bb4))

      , (FalseBranch, (bb4, bb5))
      , (TrueBranch, (bb4, bb6))
      
      , (UnconditionalBranch, (bb5, bb7))
      , (UnconditionalBranch, (bb6, bb7))

      , (UnconditionalBranch, (bb7, bb3))
      ]
  , bb1
  , bb2
  , bb3
  , bb3_1
  , bb4
  , bb5
  , bb6
  , bb7
  , bb7_1
  , bb8inCfg
  , ctx
  )
  where
    ctx :: Ctx
    ctx = Ctx func5 0
    [arg1, arg2, x, r0, r1, r2, i0, i1, i2, rax] = (\t loopUseCount -> C.pilVar' 8 ctx $ updatePilVarName_ loopUseCount t) <$> ["arg1", "arg2", "x", "r0", "r1", "r2", "i0", "i1", "i2", "rax"]
    bb1 = bbp' ctx "bb1"
      [ C.def' (x 0) (C.var' (arg1 0) 8)
      , C.branchCond $ C.cmpE (C.var' (x 0) 8) (C.const 0 8) 8
      ]

    bb2 = bbp' ctx "bb2"
      [ C.def' (i0 0) (C.const 0 8)
      ]

    bb3 = bbp' ctx "bb3"
      [ C.defPhi' (i2 0) [i0 0, i1 0]
      , C.branchCond $ C.cmpE (C.var' (i2 0) 8) (C.const 100 8) 8
      ]

    bb3_1 = bbp' ctx "bb3_1"
      [ C.defPhi' (i2 1) [i0 0, i1 0]
      , C.branchCond $ C.cmpE (C.var' (i2 1) 8) (C.const 100 8) 8
      ]

    bb3_2 = bbp' ctx "bb3_2"
      [ C.defPhi' (i2 2) [i0 0, i1 1]
      , C.branchCond $ C.cmpE (C.var' (i2 2) 8) (C.const 100 8) 8
      ]

    bb4 = bbp' ctx "bb4"
      [ C.def' (rax 0) (C.load (C.var' (arg2 0) 8) 8)
      , C.branchCond $ C.cmpE (C.var' (rax 0) 8) (C.const 0 8) 8
      ]

    bb4_1 = bbp' ctx "bb4_1"
      [ C.def' (rax 1) (C.load (C.var' (arg2 0) 8) 8)
      , C.branchCond $ C.cmpE (C.var' (rax 1) 8) (C.const 0 8) 8
      ]

    bb5 = Cfg.Call . fst $ mkCallNode ctx "bar" (r0 0) funcBar []

    bb6 = Cfg.Call . fst $ mkCallNode ctx "foo" (r1 0) funcFoo []

    bb7 = bbp' ctx "bb7"
      [ C.def' (i1 0) (C.add (C.var' (i2 0) 8) (C.const 1 8) 8)
      ]

    bb7_1 = bbp' ctx "bb7_1"
      [ C.def' (i1 1) (C.add (C.var' (i2 1) 8) (C.const 1 8) 8)
      ]

    bb8inCfg = bbp' ctx "bb8"
      [ C.defPhi' (r2 1) [r0 0, r1 0, x 0]
      , C.ret $ C.var' (r2 1) 8
      ]

    bb8inPath = bbp' ctx "bb8"
      [ C.defPhi' (r2 1) [r0 0, r1 0, x 0]
      , C.ret $ C.var' (r2 1) 8
      ]

getUpdatedNodeId :: CfNode [Stmt] -> UUID
getUpdatedNodeId n = UUID.fromWords64 h h where h = fromIntegral $ hash (Cfg.getNodeUUID n)

-- | Changes UUID based on hash of old UUID. Used to prevent nodes with duplicate ids.
updateNodeId :: CfNode [Stmt] -> CfNode [Stmt]
updateNodeId n = Cfg.setNodeUUID (getUpdatedNodeId n) n

spec :: Spec
spec = describe "Blaze.Cfg.Path" $ do
  context "getAllSimplePaths" $ do
    it "should find singleton path for Cfg with single root node" $ do
      let cfg = cfgSingleNode
          result = getAllSimplePaths cfg
          expected = [ CfgP.build 0 $ start "a" ]
      result `shouldBe` expected

    it "should find single path for Cfg with single path" $ do
      let cfg = cfgSinglePath
          result = getAllSimplePaths cfg
          expected =
            [ CfgP.build 0 $ start "a"
              -| UnconditionalBranch |- "b"
              -| UnconditionalBranch |- "c"
              -| UnconditionalBranch |- "d"
            ]
      result `shouldBe` expected

    it "should find two paths" $ do
      let cfg = cfgTwoPaths
          result = sort $ getAllSimplePaths cfg
          expected = sort
            [ CfgP.build 0 $ start "a"
              -| FalseBranch |- "b"
              -| UnconditionalBranch |- "fin"
            , CfgP.build 0 $ start "a"
              -| TrueBranch |- "c"
              -| UnconditionalBranch |- "fin"
            ]
      PrettyShow' result `shouldBe` PrettyShow' expected

    it "should find paths within expanded grouping node" $ do
      let cfg = cfgWithGroupingNode
          result = sort $ getAllSimplePaths cfg
          expected = sort 
            [ CfgP.build 0 $ start "aa"
              -| FalseBranch |- "a"
              -| FalseBranch |- "b"
              -| UnconditionalBranch |- "fin"
              -| UnconditionalBranch |- "ffin"
            , CfgP.build 0 $ start "aa"
              -| FalseBranch |- "a"
              -| TrueBranch |- "c"
              -| UnconditionalBranch |- "fin"
              -| UnconditionalBranch |- "ffin"
            , CfgP.build 0 $ start "aa"
              -| TrueBranch |- "cc"
              -| UnconditionalBranch |- "ffin"
            ]
      PrettyShow' result `shouldBe` PrettyShow' expected

  context "getSimplePathsContaining" $ do
    it "should find paths within expanded grouping node that contain required node" $ do
      let cfg = cfgWithGroupingNode
          reqNodes = HashSet.fromList ["c"]
          result = sort $ getSimplePathsContaining reqNodes cfg
          expected = sort
            [ CfgP.build 0 $ start "aa"
              -| FalseBranch |- "a"
              -| TrueBranch |- "c"
              -| UnconditionalBranch |- "fin"
              -| UnconditionalBranch |- "ffin"
            ]
      PrettyShow' result `shouldBe` PrettyShow' expected    

  context "getSimpleReturnPaths" $ do
    it "should find singleton path for Cfg with single root node" $ do
      let cfg = cfgSingleNode
          result = getSimpleReturnPaths cfg
          expected = [ CfgP.build 0 $ start "a" ]
      result `shouldBe` expected

    it "should find single path for Cfg with single path" $ do
      let cfg = cfgSinglePath
          result = getSimpleReturnPaths cfg
          expected =
            [ CfgP.build 0 $ start "a"
              -| UnconditionalBranch |- "b"
              -| UnconditionalBranch |- "c"
              -| UnconditionalBranch |- "d"
            ]
      result `shouldBe` expected

    it "should find two paths" $ do
      let cfg = cfgTwoPaths
          result = sort $ getSimpleReturnPaths cfg
          expected = sort
            [ CfgP.build 0 $ start "a"
              -| FalseBranch |- "b"
              -| UnconditionalBranch |- "fin"
            , CfgP.build 0 $ start "a"
              -| TrueBranch |- "c"
              -| UnconditionalBranch |- "fin"
            ]
      PrettyShow' result `shouldBe` PrettyShow' expected

    it "should ignore looping path" $ do
      let cfg = cfgTwoPathsButOneLoops
          result = sort $ getSimpleReturnPaths cfg
          expected = sort
            [ CfgP.build 0 $ start "a"
              -| TrueBranch |- "c"
              -| UnconditionalBranch |- "fin"
            ]
      PrettyShow' result `shouldBe` PrettyShow' expected

  context "expandCall" $ do
    it "should expand call where target is single node path" $ do
      let outerPath = func2Path
          innerPath = func3Path
          callNode = func2call0 0
          leaveFuncUuid = mkUuid1 (1234 :: Int)
          result = CfgP.expandCall leaveFuncUuid outerPath callNode innerPath
          enterFuncNode = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func2ctx 0
            , nextCtx = func3ctx 1
            , callSiteAddress = 0
            , uuid = callNode ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func3ctx 1) "arg1")
                $ C.var' (pilVar' (func2ctx 0) "x") 8
              ]
            }
          leaveFuncNode = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func3ctx 1
            , nextCtx = func2ctx 0
            , callSiteAddress = 0
            , uuid = leaveFuncUuid
            , nodeData = [ C.def' (pilVar' (func2ctx 0) "r")
                                  (C.var' (pilVar' (func3ctx 1) "x") 8)
                         ]
            }
          expected = CfgP.build 2 $ start (func2bb0 0)
              -| TrueBranch |- func2bb1 0
              -| UnconditionalBranch |- enterFuncNode
              -| UnconditionalBranch |- func3bb0 1
              -| UnconditionalBranch |- leaveFuncNode
              -| UnconditionalBranch |- func2bb2 0
      PrettyShow' (fmap FullCfNode <$> result) `shouldBe` PrettyShow' (Just $ FullCfNode <$> expected)

    it "should snip off remainder of path if inner path has no Ret" $ do
      let outerPath = func2Path
          innerPath = func3PathNoRet
          callNode = func2call0 0
          leaveFuncUuid = mkUuid1 (1234 :: Int)
          result = CfgP.expandCall leaveFuncUuid outerPath callNode innerPath
          enterFuncNode = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func2ctx 0
            , nextCtx = func3ctxNoRet 1
            , callSiteAddress = 0
            , uuid = callNode ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func3ctxNoRet 1) "arg1")
                $ C.var' (pilVar' (func2ctx 0) "x") 8
              ]
            }
          expected = CfgP.build 2 $ start (func2bb0 0)
              -| TrueBranch |- func2bb1 0
              -| UnconditionalBranch |- enterFuncNode
              -| UnconditionalBranch |- func3bb0NoRet 1
      PrettyShow' (fmap FullCfNode <$> result) `shouldBe` PrettyShow' (Just $ FullCfNode <$> expected)

    it "should expand call where target has many nodes but a single context" $ do
      let outerPath = func1Path
          innerPath = func2Path
          callNode = func1call0 0
          leaveFuncUuid = mkUuid1 (1234 :: Int)
          result = CfgP.expandCall leaveFuncUuid outerPath callNode innerPath
          enterFuncNode = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func1ctx 0
            , nextCtx = func2ctx 1
            , callSiteAddress = 0
            , uuid = callNode ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func2ctx 1) "arg1")
                $ C.var' (pilVar' (func1ctx 0) "x") 8
              , C.def' (pilVar' (func2ctx 1) "arg2")
                $ C.var' (pilVar' (func1ctx 0) "y") 8
              ]
            }
          leaveFuncNode = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func2ctx 1
            , nextCtx = func1ctx 0
            , callSiteAddress = 0
            , uuid = leaveFuncUuid
            , nodeData = [ C.def' (pilVar' (func1ctx 0) "r")
                                  (C.var' (pilVar' (func2ctx 1) "r") 8)
                         ]
            }
          expected = CfgP.build 2 $ start (func1bb0 0)
              -| UnconditionalBranch |- enterFuncNode
              -| UnconditionalBranch |- func2bb0 1
              -| TrueBranch |- func2bb1 1
              -| UnconditionalBranch |- Cfg.Call (func2call0 1)
              -| UnconditionalBranch |- func2bb2 1
              -| UnconditionalBranch |- leaveFuncNode
              -| UnconditionalBranch |- func1bb1 0
      PrettyShow' (fmap FullCfNode <$> result) `shouldBe` PrettyShow' (Just $ FullCfNode <$> expected)

    it "should expand call where target has many nodes and multiple contexts" $ do
      let outerPath = func1Path
          innerPathLeaveFuncUuid = mkUuid1 (4567 :: Int)
          innerPathCallNode = func2call0 0
          innerPath = fromJust $ CfgP.expandCall innerPathLeaveFuncUuid func2Path innerPathCallNode func3Path
          callNode = func1call0 0
          leaveFuncUuid = mkUuid1 (1234 :: Int)
          result = CfgP.expandCall leaveFuncUuid outerPath callNode innerPath
          enterFuncNode = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func1ctx 0
            , nextCtx = func2ctx 1
            , callSiteAddress = 0
            , uuid = callNode ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func2ctx 1) "arg1")
                $ C.var' (pilVar' (func1ctx 0) "x") 8
              , C.def' (pilVar' (func2ctx 1) "arg2")
                $ C.var' (pilVar' (func1ctx 0) "y") 8
              ]
            }
          leaveFuncNode = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func2ctx 1
            , nextCtx = func1ctx 0
            , callSiteAddress = 0
            , uuid = leaveFuncUuid
            , nodeData = [ C.def' (pilVar' (func1ctx 0) "r")
                                  (C.var' (pilVar' (func2ctx 1) "r") 8)
                         ]
            }
          enterInnerFuncNode = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func2ctx 1
            , nextCtx = func3ctx 2
            , callSiteAddress = 0
            , uuid = innerPathCallNode ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func3ctx 2) "arg1")
                $ C.var' (pilVar' (func2ctx 1) "x") 8
              ]
            }
          leaveInnerFuncNode = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func3ctx 2
            , nextCtx = func2ctx 1
            , callSiteAddress = 0
            , uuid = innerPathLeaveFuncUuid
            , nodeData = [ C.def' (pilVar' (func2ctx 1) "r")
                                  (C.var' (pilVar' (func3ctx 2) "x") 8)
                         ]
            }

          expected = CfgP.build 3 $ start (func1bb0 0)
              -| UnconditionalBranch |- enterFuncNode
              -| UnconditionalBranch |- func2bb0 1
              -| TrueBranch |- func2bb1 1
              -| UnconditionalBranch |- enterInnerFuncNode
              -| UnconditionalBranch |- func3bb0 2
              -| UnconditionalBranch |- leaveInnerFuncNode
              -| UnconditionalBranch |- func2bb2 1            
              -| UnconditionalBranch |- leaveFuncNode
              -| UnconditionalBranch |- func1bb1 0
      PrettyShow' (fmap FullCfNode <$> result) `shouldBe` PrettyShow' (Just $ FullCfNode <$> expected)

    it "should expand two calls to the same function sequentially" $ do
      let outerPath = func0Path
          innerPath1 = func3Path
          innerPath2 = updateNodeId <$> func3Path
          callNode1 = func0call0 0
          callNode2 = func0call1 0
          leaveFuncUuid1 = mkUuid1 (1234 :: Int)
          leaveFuncUuid2 = mkUuid1 (2345 :: Int)
          p1 = fromJust $ CfgP.expandCall leaveFuncUuid1 outerPath callNode1 innerPath1
          result = CfgP.expandCall leaveFuncUuid2 p1 callNode2 innerPath2

          enterFuncNode1 = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func0ctx 0
            , nextCtx = func3ctx 1
            , callSiteAddress = 0
            , uuid = callNode1 ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func3ctx 1) "arg1")
                $ C.var' (pilVar' (func0ctx 0) "x") 8
              ]
            }
          leaveFuncNode1 = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func3ctx 1
            , nextCtx = func0ctx 0
            , callSiteAddress = 0
            , uuid = leaveFuncUuid1
            , nodeData = [ C.def' (pilVar' (func0ctx 0) "r1")
                                  (C.var' (pilVar' (func3ctx 1) "x") 8)
                         ]
            }
          enterFuncNode2 = Cfg.EnterFunc $ Cfg.EnterFuncNode
            { prevCtx = func0ctx 0
            , nextCtx = func3ctx 2
            , callSiteAddress = 0
            , uuid = callNode2 ^. #uuid
            , nodeData =
              [ C.def' (pilVar' (func3ctx 2) "arg1")
                $ C.var' (pilVar' (func0ctx 0) "y") 8
              ]
            }
          leaveFuncNode2 = Cfg.LeaveFunc $ Cfg.LeaveFuncNode
            { prevCtx = func3ctx 2
            , nextCtx = func0ctx 0
            , callSiteAddress = 0
            , uuid = leaveFuncUuid2
            , nodeData = [ C.def' (pilVar' (func0ctx 0) "r2")
                                  (C.var' (pilVar' (func3ctx 2) "x") 8)
                         ]
            }

          expected = CfgP.build 3 $ start (func0bb0 0)
              -| UnconditionalBranch |- enterFuncNode1
              -| UnconditionalBranch |- func3bb0 1
              -| UnconditionalBranch |- leaveFuncNode1
              -| UnconditionalBranch |- enterFuncNode2
              -| UnconditionalBranch |- updateNodeId (func3bb0 2)
              -| UnconditionalBranch |- leaveFuncNode2
              -| UnconditionalBranch |- func0bb1 0

      PrettyShow' (fmap FullCfNode <$> result) `shouldBe` PrettyShow' (Just $ FullCfNode <$> expected)

  context "updatePossiblyLoopingStmt" $ do
    it "should add unvisited defined var to state" $ do
      let stmt = C.def' (C.pilVar 0x8 "x") $ C.const 778 0x8
          st = HashMap.fromList []
          stmt' = C.def' (C.pilVar 0x8 "x") $ C.const 778 0x8
          st' = HashMap.fromList [(C.pilVar 0x8 "x", 0)]

      runState (updatePossiblyLoopingStmt stmt) st `shouldBe` (stmt', st')

    it "should increment previously visited defined var in state and rename" $ do
      let stmt = C.def' (C.pilVar 0x8 "x") $ C.const 778 0x8
          st = HashMap.fromList [(C.pilVar 0x8 "x", 0)]
          stmt' = C.def' (C.pilVar 0x8 "x_loop1") $ C.const 778 0x8
          st' = HashMap.fromList [(C.pilVar 0x8 "x", 1)]

      runState (updatePossiblyLoopingStmt stmt) st `shouldBe` (stmt', st')

    it "should not add unvisited stored var to state" $ do
      let stmt = C.store (C.var' (C.pilVar 0x8 "x") 0x8) $ C.const 778 0x8
          st = HashMap.fromList []
          stmt' = stmt
          st' = st

      runState (updatePossiblyLoopingStmt stmt) st `shouldBe` (stmt', st')

    it "should rename visited vars in statement" $ do
      let stmt = C.store (C.var' (C.pilVar 0x8 "x") 0x8) $ C.var' (C.pilVar 0x8 "y") 0x8
          st = HashMap.fromList
               [ (C.pilVar 0x8 "x", 3)
               , (C.pilVar 0x8 "y", 1)
               ]
          stmt' = C.store (C.var' (C.pilVar 0x8 "x_loop3") 0x8)
                  $ C.var' (C.pilVar 0x8 "y_loop1") 0x8
          st' = st

      runState (updatePossiblyLoopingStmt stmt) st `shouldBe` (stmt', st')

    -- Just in case we ever have non-SSA pil, might as well be able to handle it
    it "should handle x = x + 1 without SSA" $ do
      let stmt = C.def' (C.pilVar 0x8 "x")
            $ C.add (C.var' (C.pilVar 0x8 "x") 0x8) (C.const 4343 0x8) 0x8
          st = HashMap.fromList
               [ (C.pilVar 0x8 "x", 1)
               ]
          stmt' = C.def' (C.pilVar 0x8 "x_loop2")
            $ C.add (C.var' (C.pilVar 0x8 "x_loop1") 0x8) (C.const 4343 0x8) 0x8
          st' = HashMap.fromList
                [ (C.pilVar 0x8 "x", 2)
                ]

      runState (updatePossiblyLoopingStmt stmt) st `shouldBe` (stmt', st')

  context "updatePossiblyLoopingNode" $ do
    it "should add unvisited defined vars in unvisted node to state " $ do
      let newUuid = return $ mkUuid1 (88 :: Int)
          node = bbp (Ctx func0 0) "loopnode0"
                 [ C.def' (C.pilVar 0x8 "x") $ C.const 778 0x8
                 , C.def' (C.pilVar 0x8 "y") $ C.const 8888 0x8
                 ]
          st = UnrollLoopState
            { definedVars = HashMap.fromList []
            , visitedNodes = HashSet.fromList []
            }
          node' = node
          st' = UnrollLoopState
            { definedVars = HashMap.fromList
                            [ (C.pilVar 0x8 "x", 0)
                            , (C.pilVar 0x8 "y", 0)
                            ]
            , visitedNodes = HashSet.singleton node
            }
      runIdentity (updatePossiblyLoopingNode newUuid st node) `shouldBe` (node', st')

    it "should change names of visited vars in unvisited node" $ do
      let newUuid = return $ mkUuid1 (88 :: Int)
          node = bbp (Ctx func0 0) "loopnode0"
                 [ C.store (C.var' (C.pilVar 0x8 "x") 0x8) $ C.var' (C.pilVar 0x8 "y") 0x8
                 ]
          st = UnrollLoopState
            { definedVars = HashMap.fromList [(C.pilVar 0x8 "y", 1)]
            , visitedNodes = HashSet.fromList []
            }
          node' = Cfg.setNodeData
            [ C.store (C.var' (C.pilVar 0x8 "x") 0x8) $ C.var' (C.pilVar 0x8 "y_loop1") 0x8
            ]
            node
          st' = UnrollLoopState
            { definedVars = HashMap.fromList
                            [ (C.pilVar 0x8 "y", 1)
                            ]
            , visitedNodes = HashSet.singleton node
            }
      runIdentity (updatePossiblyLoopingNode newUuid st node) `shouldBe` (node', st')

  context "samplePathContainingSequence" $ do
    let samplePathContainingSequence__ rd ru dmap reqNodes n
          = fmap (fst <$>)
          . samplePathContainingSequence_ rd ru dmap (initSequenceChooserState reqNodes) n

    it "should find a path that must unroll loop to fulfill sequence" $ do
      let randDouble = randomIO -- do
          genUUID = randomIO -- do
          cfg = func5cfg
          dmap = G.calcStrictDescendantsMap cfg
          reqSeq = [func5bb5, func5bb6, func5bb8]
          startNode = func5bb1
          action = unsafeFromRight <$> samplePathContainingSequence__ randDouble genUUID dmap reqSeq startNode cfg

          pred p = HashSet.member (nullId func5bb7_1) nodes
            && HashSet.member (nullId func5bb5) nodes
            && HashSet.member (nullId func5bb6) nodes
            && HashSet.member (nullId func5bb3_1) nodes
            where
              nullId = Cfg.setNodeUUID $ mkUuid1 (0 :: Int)
              nodes = HashSet.map nullId $ P.nodes p
      pred <$> action `shouldReturn` True
