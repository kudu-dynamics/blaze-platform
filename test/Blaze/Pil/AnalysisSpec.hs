{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Redundant flip" -}

module Blaze.Pil.AnalysisSpec where

import Blaze.Pil.Analysis
  ( CopyPropState(CopyPropState),
    _foldCopyPropState,
    constantProp,
    copyProp,
    copyPropMem,
    findLoads,
    findMemEquivGroupsForStorage,
    findMemStmts,
    fixedRemoveUnusedPhi,
    getDefinedVars,
    getFreeVars,
    getRefVars,
    getStorage,
    getVarEqMap,
    memSubst,
    memoryTransform,
    mkDefLoadStmt,
    mkMemStorage,
    mkStoreStmt,
    reduceMap,
    resolveMemGroup,
    removeUnusedPhi,
    reducePhi
  )
import qualified Blaze.Pil.Analysis as A
import qualified Blaze.Graph as G
import Blaze.Pil.Construct
import Blaze.Prelude hiding
  ( const,
    group,
    sym,
  )
import qualified Blaze.Types.Pil as TPil
import qualified Blaze.Types.Pil.Analysis as A
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Map as Map
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import Test.Hspec
import Blaze.Pretty (PrettyShow(..), Tokenizable)
import Blaze.Types.Pil (Stmt)

mixedStmts :: [TPil.Stmt]
mixedStmts =
  [ def "b" (var "a" 4),
    def "c" (var "b" 4),
    def "d" (var "c" 4),
    def "z" (var "x" 4),
    def "y" (var "x" 4)
  ]

constStmts :: [TPil.Stmt]
constStmts =
  [ def "a" (const 42 4),
    def "b" (var "a" 4),
    def "c" (add (var "b" 4) (const 1 4) 4)
  ]

testCopyStmts :: [TPil.Stmt]
testCopyStmts =
  [ def "a" (add (var "z" 4) (const 1 4) 4),
    def "b" (var "a" 4),
    def "bb" (var "a" 4),
    def "c" (add (var "b" 4) (const 1 4) 4),
    def "cc" (add (var "bb" 4) (const 2 4) 4),
    def "d" (var "b" 4),
    def "e" (var "d" 4),
    def "f" (add (var "e" 4) (const 10 4) 4)
  ]

testCopyStmtsWithMem :: [TPil.Stmt]
testCopyStmtsWithMem =
  [ def "var_10#1" (var "arg1#0" 8),
    def "rdi#1" (var "var_10#1" 8),
    def "rdi_1#2" (load (var "rdi#1" 8) 8),
    def "var_18#1" (var "rdi_1#2" 8),
    def "rdi_2#3" (var "var_10#1" 8),
    store (var "rdi_2#3" 8) (const 30 8),
    def "rdi_3#8" (var "var_10#1" 8),
    def "rdi_8#5" (load (var "rdi_3#8" 8) 8),
    def "var_20#1" (var "rdi_4#5" 8),
    def "rdi_5#6" (var "var_10#1" 8),
    def "rdi_6#7" (load (var "rdi_5#6" 8) 8),
    def "var_28#1" (var "rdi_6#7" 8),
    def "rdi_7#8" (var "var_10#1" 8),
    store (var "rdi_7#8" 8) (const 20 8),
    def "rdi_8#9" (var "var_10#1" 8),
    def "rdi_9#10" (load (var "rdi_8#9" 8) 8),
    def "var_30#1" (var "rdi_9#10" 8)
  ]

testMemSubstStmts :: [TPil.Stmt]
testMemSubstStmts =
  [ def "x" (load (constPtr 0x100100 4) 4),
    def "y" (var "x" 4),
    store (constPtr 0x100100 4) (const 42 4),
    def "z" (load (constPtr 0x100200 4) 4),
    def
      "zzz"
      ( strcmp
          (load (constPtr 0x100200 4) 4)
          (load (constPtr 0x100100 4) 4)
          4
      )
  ]

testMemEquivStmts :: [TPil.Stmt]
testMemEquivStmts =
  [ def "x" (load (var "a" 8) 8),
    def "y" (load (var "a" 8) 8),
    store (var "aa" 8) (const 0x2a 8),
    def "xx" (load (var "aa" 8) 8),
    def "z" (load (var "a" 8) 8),
    store (var "aa" 8) (const 0x29a 8),
    def "yy" (load (var "aa" 8) 8),
    def "zz" (load (var "aa" 8) 8)
  ]

testMemEquivStmts2 :: [TPil.Stmt]
testMemEquivStmts2 =
  [ def "x" (load (var "a" 8) 8),
    def "y" (load (var "a" 8) 8),
    store (var "aa" 8) (const 0x2a 8),
    def "xx" (load (var "aa" 8) 8),
    def "z" (load (var "a" 8) 8),
    store (var "aa" 8) (const 0x29a 8),
    def "yy" (load (var "aa" 8) 8),
    def "zz" (load (var "aa" 8) 8)
  ]

testCopyPropMemStmts1 :: [TPil.Stmt]
testCopyPropMemStmts1 =
  [ store (var "a" 8) (const 42 8),
    def "x" (load (var "a" 8) 8),
    store (var "b" 8) (const 99 8),
    def "foo" (add (const 800 8) (var "x" 8) 8),
    def "y" (load (var "a" 8) 8),
    def "bar" (add (const 800 8) (load (var "a" 8) 8) 8),
    def "z" (load (var "a" 8) 8)
  ]

testCopyPropMemStmts2 :: [TPil.Stmt]
testCopyPropMemStmts2 =
  [ store (var "a" 8) (const 42 8),
    def "x" (load (var "a" 8) 8),
    store (var "a" 8) (const 834 8),
    def "y" (load (var "a" 8) 8),
    store (var "a" 8) (add (load (var "a" 8) 8) (const 999 8) 8),
    def "z" (load (var "a" 8) 8)
  ]

testFieldAccessStmts :: [TPil.Stmt]
testFieldAccessStmts =
  [ store (add (var "a" 8) (const 4 8) 8) (const 42 8),
    def "x" (load (add (var "b" 8) (const 0x10 8) 8) 8),
    def "y" (zx (sx (load (add (const 0x20 8) (var "c" 8) 8) 8) 8) 8),
    def "z" (load (add (constPtr 0x7f100000 8) (const 0x10 8) 8) 8) ]

getSym :: TPil.PilVar -> TPil.Symbol
getSym = view #symbol

mkDefLoadStmt_ :: A.Index -> TPil.Stmt -> A.DefLoadStmt
mkDefLoadStmt_ idx s = fromJust $ mkDefLoadStmt idx s

testMemStmts :: (Hashable a, Ord a) => (A.Index -> TPil.Stmt -> Maybe a) -> [(A.Index, TPil.Stmt)] -> HSet.HashSet a
testMemStmts f xs = HSet.fromList $ mapMaybe (uncurry f) xs

newtype Sym = Sym Int
            deriving (Eq, Ord, Read, Show, Generic)
instance Hashable Sym

hm :: (Ord a, Ord b, Hashable a) => [(a, b)] -> HashMap a b
hm = HMap.fromList . sort

hs :: (Ord a, Hashable a) => [a] -> HashSet a
hs = HSet.fromList . sort


spec :: Spec
spec = describe "Blaze.Pil.Analysis" $ do
  describe "getVarEqMap" $ do
    it "should build map of equivalent vars" $ do
      let m = fmap getSym . Map.mapKeys getSym $ Map.fromList . HMap.toList $ getVarEqMap mixedStmts
      m `shouldBe` Map.fromList
        [ ("b", "a")
        , ("c", "b")
        , ("d", "c")
        , ("y", "x")
        , ("z", "x")
        ]

  describe "getFreeVars" $ do
    it "should find free vars" $ do
      let fvs = HSet.map getSym $ getFreeVars mixedStmts
      fvs `shouldBe` HSet.fromList ["a", "x"]

  describe "getDefinedVars" $ do
    it "should find defined vars" $ do
      let dvs = HSet.map getSym $ getDefinedVars mixedStmts
      dvs `shouldBe` HSet.fromList ["b", "c", "d", "z", "y"]

  describe "getRefVars" $ do
    it "shoud find referenced vars" $ do
      let vars = HSet.map getSym $ getRefVars mixedStmts
      vars `shouldBe` HSet.fromList ["a", "b", "c", "x"]

  describe "findLoads" $ do
    it "should find loads in defs" $ do
      let defStmt = def "x" (load (var "a" 4) 4)
          loads = (^. #expr) <$> findLoads defStmt
      loads `shouldBe` [load (var "a" 4) 4]
    it "should find loads in stores" $ do
      let storeStmt = store (var "a" 4) (load (var "b" 4) 4)
          loads = (^. #expr) <$> findLoads storeStmt
      loads `shouldBe` [load (var "b" 4) 4]
    it "should find loads in constraints" $ do
      let constraintStmt = constraint (cmpE (var "a" 4) (load (var "b" 4) 4) 4)
          loads = (^. #expr) <$> findLoads constraintStmt
      loads `shouldBe` [load (var "b" 4) 4]

  describe "findMemEquivGroupsForStorage" $ do
    let stmts =
          [ def "a" (load (var "x" 4) 4),
            def "b" (load (var "x" 4) 4),
            store (var "x" 4) (const 42 4),
            def "c" (load (var "x" 4) 4),
            def "d" (load (var "x" 4) 4),
            store (var "x" 4) (const (-1) 4),
            store (var "x" 4) (const 30 8)
          ]
        memStmts = findMemStmts stmts
        storage = mkMemStorage (var "x" 4) 32
        filtMemStmts = filter ((storage ==) . getStorage) memStmts
        groups = findMemEquivGroupsForStorage storage filtMemStmts
    it "should find several groups" $ do
      length groups `shouldBe` 3
    it "should find the first group" $ do
      ((groups !! 0) ^. #store) `shouldBe` Nothing
      length ((groups !! 0) ^. #defLoads) `shouldBe` 2
      HSet.fromList ((groups !! 0) ^. #defLoads)
        `shouldBe` testMemStmts
          mkDefLoadStmt
          [ (0, def "a" (load (var "x" 4) 4)),
            (1, def "b" (load (var "x" 4) 4))
          ]
    it "should find the second group" $ do
      ((groups !! 1) ^. #store) `shouldBe` mkStoreStmt 2 (store (var "x" 4) (const 42 4))
      length ((groups !! 1) ^. #defLoads) `shouldBe` 2
      HSet.fromList ((groups !! 1) ^. #defLoads)
        `shouldBe` testMemStmts
          mkDefLoadStmt
          [ (3, def "c" (load (var "x" 4) 4)),
            (4, def "d" (load (var "x" 4) 4))
          ]
    it "should find the third group" $ do
      ((groups !! 2) ^. #store) `shouldBe` mkStoreStmt 5 (store (var "x" 4) (const (-1) 4))
      length ((groups !! 2) ^. #defLoads) `shouldBe` 0

    it "should find a different group based on width" $ do
      let storage' = mkMemStorage (var "x" 4) 64
          filtMemStmts' = filter ((storage' ==) . getStorage) memStmts
          groups' = findMemEquivGroupsForStorage storage' filtMemStmts'
      length groups' `shouldBe` 1

  describe "constantProp" $ do
    it "should propagate constants" $ do
      let stmts = constantProp constStmts
      stmts `shouldBe` [ def "a" (const 42 4)
                       , def "b" (const 42 4)
                       , def "c" (add (var "b" 4) (const 1 4) 4)
                       ]
      let stmts' = constantProp stmts
      stmts' `shouldBe` [ def "a" (const 42 4)
                        , def "b" (const 42 4)
                        , def "c" (add (const 42 4) (const 1 4) 4)]

  describe "_foldCopyPropState" $ do
    let CopyPropState
          { mapping = mapping',
            copyStmts = copyStmts'
          } = _foldCopyPropState testCopyStmtsWithMem
    it "should find a mapping of variable copies" $ do
      mapping'
        `shouldBe` HMap.fromList
          [ (pilVar "var_10#1", pilVar "arg1#0"),
            (pilVar "rdi#1", pilVar "var_10#1"),
            (pilVar "var_18#1", pilVar "rdi_1#2"),
            (pilVar "rdi_2#3", pilVar "var_10#1"),
            (pilVar "rdi_3#8", pilVar "var_10#1"),
            (pilVar "var_20#1", pilVar "rdi_4#5"),
            (pilVar "rdi_5#6", pilVar "var_10#1"),
            (pilVar "var_28#1", pilVar "rdi_6#7"),
            (pilVar "rdi_7#8", pilVar "var_10#1"),
            (pilVar "rdi_8#9", pilVar "var_10#1"),
            (pilVar "var_30#1", pilVar "rdi_9#10")
          ]
    it "should find a set of copy statements" $ do
      copyStmts'
        `shouldBe` DSet.fromList
          [ def "var_10#1" (var "arg1#0" 8),
            def "rdi#1" (var "var_10#1" 8),
            def "var_18#1" (var "rdi_1#2" 8),
            def "rdi_2#3" (var "var_10#1" 8),
            def "rdi_3#8" (var "var_10#1" 8),
            def "var_20#1" (var "rdi_4#5" 8),
            def "rdi_5#6" (var "var_10#1" 8),
            def "var_28#1" (var "rdi_6#7" 8),
            def "rdi_7#8" (var "var_10#1" 8),
            def "rdi_8#9" (var "var_10#1" 8),
            def "var_30#1" (var "rdi_9#10" 8)
          ]

  describe "reduceMap" $ do
    let test :: (Eq a, Show a, Hashable a) => HashMap a a -> HashMap a a -> Expectation
        test a b = reduceMap a `shouldBe` b
        testPretty :: (Eq a, Hashable a, Tokenizable a) => HashMap a a -> HashMap a a -> Expectation
        testPretty a b = PrettyShow (reduceMap a) `shouldBe` PrettyShow b
        testCycle :: (Eq a, Hashable a) => HashMap a a -> Expectation
        testCycle a = evaluate (reduceMap a) `shouldThrow` errorCall "reduceMap: detected cycle in map"

    it "should reduce a mapping expression" $ do
      test      @Int (HMap.fromList [(1, 2)])
                     (HMap.fromList [(1, 2)])
      test      @Int (HMap.fromList [(2, 3), (1, 2)])
                     (HMap.fromList [(1, 3), (2, 3)])
      test      @Int (HMap.fromList [(1, 2), (2, 3)])
                     (HMap.fromList [(1, 3), (2, 3)])
      test      @Int (HMap.fromList [(1, 2), (3, 4), (2, 3), (0, 1)])
                     (HMap.fromList [(0, 4), (1, 4), (2, 4), (3, 4)])
      test      @Int (HMap.fromList [(1, 2), (3, 4), (2, 3), (0, 1), (5, 1), (6, 1)])
                     (HMap.fromList [(0, 4), (1, 4), (2, 4), (3, 4), (5, 4), (6, 4)])
      testCycle @Int (HMap.fromList [(1, 2), (3, 4), (2, 3), (0, 1), (4, 1), (6, 1)])

    it "should reduce a mapping expression" $ do
      let exprMap =
            HMap.fromList
              [ (var "var_10#1" 8, var "arg1#0" 8),
                (var "rdi#1" 8, var "var_10#1" 8),
                (var "var_18#1" 8, var "rdi_1#2" 8),
                (var "rdi_2#3" 8, var "var_10#1" 8),
                (var "rdi_3#8" 8, var "var_10#1" 8),
                (var "var_20#1" 8, var "rdi_4#5" 8),
                (var "rdi_5#6" 8, var "var_10#1" 8),
                (var "var_28#1" 8, var "rdi_6#7" 8),
                (var "rdi_7#8" 8, var "var_10#1" 8),
                (var "rdi_8#9" 8, var "var_10#1" 8),
                (var "var_30#1" 8, var "rdi_9#10" 8)
              ]
      testPretty exprMap
        (HMap.fromList
          [ (var "var_10#1" 8, var "arg1#0" 8),
            (var "rdi#1" 8, var "arg1#0" 8),
            (var "var_18#1" 8, var "rdi_1#2" 8),
            (var "rdi_2#3" 8, var "arg1#0" 8),
            (var "rdi_3#8" 8, var "arg1#0" 8),
            (var "var_20#1" 8, var "rdi_4#5" 8),
            (var "rdi_5#6" 8, var "arg1#0" 8),
            (var "var_28#1" 8, var "rdi_6#7" 8),
            (var "rdi_7#8" 8, var "arg1#0" 8),
            (var "rdi_8#9" 8, var "arg1#0" 8),
            (var "var_30#1" 8, var "rdi_9#10" 8)
          ])

  describe "copyProp" $ do
    let testPretty :: [Stmt] -> [Stmt] -> Expectation
        testPretty a b = PrettyShow (copyProp a) `shouldBe` PrettyShow b

    it "should propagate through assignment" $ do
      testPretty (copyProp testCopyStmts)
        [ def "a" (add (var "z" 4) (const 1 4) 4),
          def "c" (add (var "a" 4) (const 1 4) 4),
          def "cc" (add (var "a" 4) (const 2 4) 4),
          def "f" (add (var "a" 4) (const 10 4) 4)
        ]

      testPretty (copyProp testCopyStmtsWithMem)
        [ def "rdi_1#2" (load (var "arg1#0" 8) 8),
          store (var "arg1#0" 8) (const 30 8),
          def "rdi_8#5" (load (var "arg1#0" 8) 8),
          def "rdi_6#7" (load (var "arg1#0" 8) 8),
          store (var "arg1#0" 8) (const 20 8),
          def "rdi_9#10" (load (var "arg1#0" 8) 8)
        ]

    it "should not hang due to self-assignemnt" $ do
      let inputStmts =
            [ def "a" (var "a" 4)
            , def "c" (add (var "a" 4) (const 1 4) 4)
            ]
      testPretty (copyProp inputStmts)
        [ def "c" (add (var "a" 4) (const 1 4) 4) ]

  describe "memSubst" $ do
    it "should substitute specified memory loads with constant values" $ do
      let valMap =
            HMap.fromList
              [ (0x100100, "ripples"),
                (0x100200, "ocean")
              ]
      let stmts = memSubst valMap testMemSubstStmts
      stmts
        `shouldBe` [ def "x" (constStr "ripples" 4),
                     def "y" (var "x" 4),
                     store (constPtr 0x100100 4) (const 42 4),
                     def "z" (constStr "ocean" 4),
                     def
                       "zzz"
                       ( strcmp
                           (constStr "ocean" 4)
                           (constStr "ripples" 4)
                           4
                       )
                   ]

  describe "resolveMemGroup" $ do
    it "should resolve equivalent loads" $ do
      let loads =
            [ mkDefLoadStmt_ 0 (def "x" (load (var "a" 8) 8)),
              mkDefLoadStmt_ 1 (def "y" (load (var "a" 8) 8)),
              mkDefLoadStmt_ 4 (def "z" (load (var "a" 8) 8))
            ]
      let group = A.MemEquivGroup Nothing (mkMemStorage (var "a" 8) 64) loads []
      let stmts = resolveMemGroup group "xyz" (DSeq.fromList testMemEquivStmts)
      stmts
        `shouldBe` DSeq.fromList
          [ def "x" (var "xyz" 8),
            def "y" (var "xyz" 8),
            store (var "aa" 8) (const 0x2a 8),
            def "xx" (load (var "aa" 8) 8),
            def "z" (var "xyz" 8),
            store (var "aa" 8) (const 0x29a 8),
            def "yy" (load (var "aa" 8) 8),
            def "zz" (load (var "aa" 8) 8)
          ]

    it "should replace stores and resolve loads" $ do
      let loads =
            [ mkDefLoadStmt_ 6 (def "yy" (load (var "aa" 8) 8)),
              mkDefLoadStmt_ 7 (def "zz" (load (var "a" 8) 8))
            ]
      let group =
            A.MemEquivGroup
              ( Just
                  ( A.StoreStmt
                      (store (var "aa" 8) (const 0x29a 8))
                      (TPil.StoreOp (var "aa" 8) (const 0x29a 8))
                      (A.MemStorage (var "aa" 8) 64)
                      5
                  )
              )
              (mkMemStorage (var "aa" 8) 64)
              loads
              []

      let stmts = resolveMemGroup group "yyzz" (DSeq.fromList testMemEquivStmts)
      stmts
        `shouldBe` DSeq.fromList
          [ def "x" (load (var "a" 8) 8),
            def "y" (load (var "a" 8) 8),
            store (var "aa" 8) (const 0x2a 8),
            def "xx" (load (var "aa" 8) 8),
            def "z" (load (var "a" 8) 8),
            def "yyzz" (const 0x29a 8),
            def "yy" (var "yyzz" 8),
            def "zz" (var "yyzz" 8)
          ]

  describe "memoryTransform" $ do
    it "should replace stores and resolve loads for all memory equiv groups" $ do
      let stmts = memoryTransform testMemEquivStmts
      stmts
        `shouldBe` [ def "x" (var "aaa" 8),
                     def "y" (var "aaa" 8),
                     def "aab" (const 0x2a 8),
                     def "xx" (var "aab" 8),
                     def "z" (var "aaa" 8),
                     def "aac" (const 0x29a 8),
                     def "yy" (var "aac" 8),
                     def "zz" (var "aac" 8)
                   ]

  describe "copyPropMem" $ do
    it "should def var for every load and subst" $ do
      let stmts = copyPropMem testCopyPropMemStmts1
      stmts
        `shouldBe` [ store (var "a" 8) (const 42 8),
                     def "aaa" (load (var "a" 8) 8),
                     def "x" (var "aaa" 8),
                     store (var "b" 8) (const 99 8),
                     def "foo" (add (const 800 8) (var "x" 8) 8),
                     def "y" (var "aaa" 8),
                     def "bar" (add (const 800 8) (var "aaa" 8) 8),
                     def "z" (var "aaa" 8)
                   ]

    it "should def different var for same load after different stores" $ do
      let stmts = copyPropMem testCopyPropMemStmts2
      stmts
        `shouldBe` [ store (var "a" 8) (const 42 8),
                     def "aaa" (load (var "a" 8) 8),
                     def "x" (var "aaa" 8),
                     store (var "a" 8) (const 834 8),
                     def "aab" (load (var "a" 8) 8),
                     def "y" (var "aab" 8),
                     store (var "a" 8) (add (var "aab" 8) (const 999 8) 8),
                     def "aac" (load (var "a" 8) 8),
                     def "z" (var "aac" 8)
                   ]

  describe "putOriginMap" $ do
    let checkOriginMap stmts =
          let (_, s) = A.runAnalysisWithState (A.putOriginMap stmts)
                       A.emptyAnalysisState
          in s ^. #originMap

    it "should add non-equality var to originMap" $ do
      let stmts = [ def "b" (const 42 8) ]
          r = Just $ HMap.fromList [(pilVar "b", pilVar "b")]
      r `shouldBe` checkOriginMap stmts

    it "should use first seen var as origin" $ do
      let stmts = [ def "b" (const 42 8)
                  , def "b" (var "a" 8)
                  ]
          r = Just $ HMap.fromList [ (pilVar "a", pilVar "b")
                                   , (pilVar "b", pilVar "b")
                                   ]
      r `shouldBe` checkOriginMap stmts

    it "should make equality" $ do
      let stmts = [ def "b" (var "a" 8) ]
          r = Just $ HMap.fromList [ (pilVar "b", pilVar "a")
                                   , (pilVar "a", pilVar "a")
                                   ]
      r `shouldBe` checkOriginMap stmts

    it "should save a two-long var equality chain" $ do
      let stmts = [ def "b" (var "a" 8)
                  , def "c" (var "b" 8)
                  ]
          r = Just $ HMap.fromList [ (pilVar "b", pilVar "a")
                                   , (pilVar "c", pilVar "a")
                                   , (pilVar "a", pilVar "a")
                                   ]
      PShow (checkOriginMap stmts) `shouldBe` PShow r

    it "should save two var equalities" $ do
      let stmts = [ def "b" (var "a" 8)
                  , def "c" (var "b" 8)
                  , def "x" (var "y" 8)
                  ]
          r = Just $ HMap.fromList [ (pilVar "b", pilVar "a")
                                   , (pilVar "c", pilVar "a")
                                   , (pilVar "a", pilVar "a")

                                   , (pilVar "y", pilVar "y")
                                   , (pilVar "x", pilVar "y")
                                   ]
      PShow (checkOriginMap stmts) `shouldBe` PShow r

  describe "substZeroOffsetFields" $ do
    let checkZero expr veqs baseAddrs =
          let (r, _) = A.runAnalysisWithState (A.substZeroOffsetFields expr)
                       $ A.emptyAnalysisState & #originMap ?~ veqs
                                              & #fieldBaseAddrs .~ baseAddrs
          in r

    it "should not change expr that isn't in fieldBaseAddrs" $ do
      let expr = const 42 8
          veqs = HMap.fromList []
          baseAddrs = HSet.fromList []
          r = const 42 8
      r `shouldBe` checkZero expr veqs baseAddrs

    it "should replace base addr in fieldBaseAddrs" $ do
      let expr = var "x" 8
          veqs = HMap.fromList []
          baseAddrs = HSet.fromList [var "x" 8]
          r = fieldAddr (var "x" 8) 0 8
      r `shouldBe` checkZero expr veqs baseAddrs

    it "should replace base addr in fieldBaseAddrs and varEqMap" $ do
      let expr = var "x" 8
          veqs = HMap.fromList [(pilVar "x", pilVar "y")]
          baseAddrs = HSet.fromList [var "y" 8]
          r = fieldAddr (var "x" 8) 0 8
      r `shouldBe` checkZero expr veqs baseAddrs


  describe "parseFieldAddr" $ do
    it "should identify a simple field access expr" $ do
      let expr = add (var "a" 8) (const 4 8) 8
          r = A.parseFieldAddr expr
      r `shouldBe` Just (A.ParsedAddr (var "a" 8) (fieldAddr (var "a" 8) 4 8))

  describe "substFieldAddr" $ do
    let checkSubstFields stmts = A.runAnalysis_ $ A.substFields stmts

    it "loads and stores with base + offset references" $ do
      let stmts = [ store (add (var "a" 8) (const 4 8) 8) (const 42 8)
                  , def "x" (load (add (var "b" 8) (const 0x10 8) 8) 8)
                  ]
          r = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
              , def "x" (load (fieldAddr (var "b" 8) 0x10 8) 8)
              ]
      PShow (checkSubstFields stmts)  `shouldBe` PShow r

    it "nested base + offset references" $ do
      let stmts = [ def "y" (zx (sx (load (add (const 0x20 8) (var "c" 8) 8) 8) 8) 8) ]
          r = [ def "y" (zx (sx (load (fieldAddr (var "c" 8) 0x20 8) 8) 8) 8) ]
      PShow (A.substAddrs stmts)  `shouldBe` PShow r

  describe "substArrayOrFieldAddr" $ do
    let checkSubstArrayOrFieldAddr = A.runAnalysis_ . A.substArrayOrFieldAddr

    it "(var + const)" $ do
      let expr = add (var "a" 8) (const 16 8) 8
          r = fieldAddr (var "a" 8) 16 8
      PShow (checkSubstArrayOrFieldAddr expr) `shouldBe` PShow r

    it "(var + const) nested inside base addr" $ do
      let expr = fieldAddr (add (var "a" 8) (const 16 8) 8) 4 8
          r = fieldAddr (fieldAddr (var "a" 8) 16 8) 4 8
      PShow (checkSubstArrayOrFieldAddr expr) `shouldBe` PShow r


  describe "substAddrs" $ do

    it "should subistitute field address values for base + offset references" $ do
      let stmts = [ store (add (var "a" 8) (const 4 8) 8) (const 42 8)
                  , def "x" (load (add (var "b" 8) (const 0x10 8) 8) 8)
                  , def "y" (zx (sx (load (add (const 0x20 8) (var "c" 8) 8) 8) 8) 8)
                  , def "z" (load (add (constPtr 0x7f100000 8) (const 0x10 8) 8) 8)
                  ]
          r = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
              , def "x" (load (fieldAddr (var "b" 8) 0x10 8) 8)
              , def "y" (zx (sx (load (fieldAddr (var "c" 8) 0x20 8) 8) 8) 8)
              , def "z" (load (fieldAddr (constPtr 0x7f100000 8) 0x10 8) 8)
              ]
      PShow (A.substAddrs stmts)  `shouldBe` PShow r

    it "single var previously used as field_addr" $ do
      let stmts = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
                  , def "x" (load (var "a" 8) 8)
                  ]
          r = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
              , def "x" (load (fieldAddr (var "a" 8) 0 8) 8)
              ]
      PShow (A.substAddrs stmts) `shouldBe` PShow r

    it "single var previously used var-eq'd as field_addr" $ do
      let stmts = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
                  , def "b" (var "a" 8)
                  , def "c" (var "b" 8)
                  , def "x" (load (var "c" 8) 8)
                  ]
          r = [ store (fieldAddr (var "a" 8) 4 8) (const 42 8)
              , def "b" (var "a" 8)
              , def "c" (var "b" 8)
              , def "x" (load (fieldAddr (var "c" 8) 0 8) 8)
              ]
      PShow (A.substAddrs stmts) `shouldBe` PShow r

    it "(var + const) nested inside base addr in store" $ do
      let stmts = [ store (fieldAddr (add (var "a" 8) (const 16 8) 8) 4 8) (const 42 8)
                  ]
          r = [ store (fieldAddr (fieldAddr (var "a" 8) 16 8) 4 8) (const 42 8)
              ]
      PShow (A.substAddrs stmts) `shouldBe` PShow r

    it "(var + const) nested inside base addr in load" $ do
      let stmts = [ def "x" (load (fieldAddr (add (var "a" 8) (const 16 8) 8) 4 8) 8) ]
          r = [ def "x" (load (fieldAddr (fieldAddr (var "a" 8) 16 8) 4 8) 8) ]

      PShow (A.substAddrs stmts) `shouldBe` PShow r

  describe "removeUnusedPhi" $ do
    it "should remove some DefPhi statements where left side var is never used" $ do
      let stmts = [ defPhi "a" []
                  , defPhi "b" []
                  , def "x" $ var "b" 8
                  ]
          stmts' = [ defPhi "b" []
                   , def "x" $ var "b" 8
                   ]
          usedVars = getRefVars stmts
      removeUnusedPhi usedVars stmts `shouldBe` stmts'

  describe "fixedRemoveUnusedPhi" $ do
    it "should remove some DefPhi statements where left side var is used in right side of unused phi" $ do
      let stmts = [ defPhi "a" []
                  , defPhi "b" []
                  , defPhi "c" ["a"]
                  , def "x" $ var "b" 8
                  ]
          stmts' = [ defPhi "b" []
                   , def "x" $ var "b" 8
                   ]
      fixedRemoveUnusedPhi stmts `shouldBe` stmts'

  describe "reducePhi" $ do
    it "should remove undefined variables in DefPhi statements" $ do
      let stmt = defPhi "a" ["x", "y", "z"]
          undefVars = HSet.fromList [pilVar "y"]
      reducePhi undefVars stmt `shouldBe` Just (defPhi "a" ["x", "z"])

    it "should replace a DefPhi with a Def if only one of the source vars is used" $ do
      let stmt = defPhi "a" ["x", "y", "z"]
          undefVars = HSet.fromList [pilVar "x", pilVar "z"]
      reducePhi undefVars stmt `shouldBe` Just (def "a" (var "y" 8))

    it "should return an empty DefPhi if all source vars are undefined" $ do
      let stmt = defPhi "a" ["x", "y", "z"]
          undefVars = HSet.fromList [pilVar "x", pilVar "y", pilVar "z"]
      reducePhi undefVars stmt `shouldBe` Nothing

  context "addToOriginMap" $ do

    it "add to empty map" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [])
      `shouldBe` ( Sym 1
                 , Nothing
                 , hm [ (Sym 0, Sym 1)
                      , (Sym 1, Sym 1)])

    it "add to map with unrelated syms" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 8, Sym 9)
                                           , (Sym 9, Sym 9)])
      `shouldBe` ( Sym 1
                 , Nothing
                 , hm [ (Sym 0, Sym 1)
                      , (Sym 1, Sym 1)
                      , (Sym 8, Sym 9)
                      , (Sym 9, Sym 9)
                      ])

    it "add to map with related sym" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 1, Sym 2)
                                           , (Sym 2, Sym 2)])
      `shouldBe` ( Sym 2
                 , Nothing
                 , hm [ (Sym 0, Sym 2)
                      , (Sym 1, Sym 2)
                      , (Sym 2, Sym 2) ])

    it "add to map with flipped related sym" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 2, Sym 1)
                                           , (Sym 1, Sym 1)])
      `shouldBe` ( Sym 1
                 , Nothing
                 , hm [ (Sym 0, Sym 1)
                      , (Sym 2, Sym 1)
                      , (Sym 1, Sym 1)
                      ])

    it "add to map with identical syms, flipped" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 1, Sym 0)
                                           , (Sym 0, Sym 0)])
      `shouldBe` ( Sym 0
                 , Nothing
                 , hm [ (Sym 1, Sym 0)
                      , (Sym 0, Sym 0) ])

    it "add to map with many related syms of same origin" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 2, Sym 1)
                                           , (Sym 3, Sym 1)
                                           , (Sym 1, Sym 1)])
      `shouldBe` ( Sym 1
                 , Nothing
                 , hm [ (Sym 0, Sym 1)
                      , (Sym 1, Sym 1)
                      , (Sym 2, Sym 1)
                      , (Sym 3, Sym 1)
                      ])

    it "add to map with many related syms of same origin, flipped" $
      A.addToOriginMap (Sym 0) (Sym 1) (hm [ (Sym 2, Sym 0)
                                           , (Sym 3, Sym 0)
                                           , (Sym 0, Sym 0)])
      `shouldBe` ( Sym 0
                 , Nothing
                 , hm [ (Sym 1, Sym 0)
                      , (Sym 2, Sym 0)
                      , (Sym 3, Sym 0)
                      , (Sym 0, Sym 0)
                      ])

    it "add equality of two origins that combines two existing groups" $
      A.addToOriginMap (Sym 0) (Sym 7) (hm [ (Sym 2, Sym 0)
                                           , (Sym 3, Sym 0)
                                           , (Sym 0, Sym 0)
                                           
                                           , (Sym 7, Sym 7)
                                           , (Sym 8, Sym 7)
                                           , (Sym 9, Sym 7)
                                           ])
      `shouldBe` ( Sym 7
                 , Just $ Sym 0
                 , hm [ (Sym 2, Sym 7)
                      , (Sym 3, Sym 7)
                      , (Sym 0, Sym 7)

                      , (Sym 7, Sym 7)
                      , (Sym 8, Sym 7)
                      , (Sym 9, Sym 7)
                      ])

    it "add equality of two non-origins that combines two existing groups" $
      A.addToOriginMap (Sym 3) (Sym 8) (hm [ (Sym 2, Sym 0)
                                           , (Sym 3, Sym 0)
                                           , (Sym 0, Sym 0)

                                           , (Sym 7, Sym 7)
                                           , (Sym 8, Sym 7)
                                           , (Sym 9, Sym 7)
                                           ])
      `shouldBe` ( Sym 7
                 , Just $ Sym 0
                 , hm [ (Sym 2, Sym 7)
                      , (Sym 3, Sym 7)
                      , (Sym 0, Sym 7)
                    
                      , (Sym 7, Sym 7)
                      , (Sym 8, Sym 7)
                      , (Sym 9, Sym 7)
                      ])

    it "add equality of one non-origin and one origin that combines two existing groups" $
      A.addToOriginMap (Sym 3) (Sym 7) (hm [ (Sym 2, Sym 0)
                                           , (Sym 3, Sym 0)
                                           , (Sym 0, Sym 0)

                                           , (Sym 7, Sym 7)
                                           , (Sym 8, Sym 7)
                                           , (Sym 9, Sym 7)
                                           ])
      `shouldBe` ( Sym 7
                 , Just $ Sym 0
                 , hm [ (Sym 2, Sym 7)
                      , (Sym 3, Sym 7)
                      , (Sym 0, Sym 7)
                      
                      , (Sym 7, Sym 7)
                      , (Sym 8, Sym 7)
                      , (Sym 9, Sym 7)
                      ])


  context "originMapToGroupMap" $ do
    it "convert empty map" $
      A.originMapToGroupMap (hm []) `shouldBe` hm ([] :: [(Sym, HashSet Sym)])

    it "convert map with one entry" $
      A.originMapToGroupMap (hm [ (Sym 1, Sym 0)
                                , (Sym 0, Sym 0) ])
      `shouldBe` hm [( Sym 0, hs [Sym 1, Sym 0])]

    it "convert map with one group" $
      A.originMapToGroupMap (hm [ (Sym 1, Sym 0)
                                , (Sym 2, Sym 0)
                                , (Sym 0, Sym 0)
                                ])
      `shouldBe` hm [(Sym 0, hs [Sym 1, Sym 0, Sym 2])]

    it "convert map with two groups" $
      A.originMapToGroupMap (hm [ (Sym 1, Sym 0)
                                , (Sym 2, Sym 0)
                                , (Sym 0, Sym 0)
                              
                                , (Sym 3, Sym 3)
                                , (Sym 4, Sym 3)
                                ])
      `shouldBe` hm [ (Sym 0, hs [Sym 1, Sym 0, Sym 2])
                    , (Sym 3, hs [Sym 3, Sym 4])
                    ]


  context "originMapToGroups" $ do
    it "convert empty map" $
      A.originMapToGroups (hm []) `shouldBe` hs ([] :: [HashSet Sym])

    it "convert map with one entry" $
      A.originMapToGroups (hm [ (Sym 1, Sym 0)
                              , (Sym 0, Sym 0)])
      `shouldBe` hs [hs [Sym 1, Sym 0]]

    it "convert map with one group" $
      A.originMapToGroups (hm [ (Sym 1, Sym 0)
                              , (Sym 2, Sym 0)
                              , (Sym 0, Sym 0) ] )
      `shouldBe` hs [hs [Sym 1, Sym 0, Sym 2]]

    it "convert map with two groups" $
      A.originMapToGroups (hm [ (Sym 1, Sym 0)
                              , (Sym 2, Sym 0)
                              , (Sym 0, Sym 0)
                            
                              , (Sym 3, Sym 3)
                              , (Sym 4, Sym 3)
                              ])
      `shouldBe` hs [ hs [Sym 1, Sym 0, Sym 2]
                    , hs [Sym 3, Sym 4]
                    ]

  context "getDataDependenceGraph" $ do
    let pv = pilVar
    it "should handle cfg with no vars" $ do
      let input = []
          r = G.empty
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle single var" $ do
      let input = [ def "x" $ const 43 4 ]
          r = G.fromNode (pv "x")
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle two unrelated vars" $ do
      let input = [ def "x" $ const 43 4
                  , def "y" $ const 34 4
                  ]
          r = flip G.addNodes G.empty
              [ pv "x"
              , pv "y"
              ]
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle two unrelated vars" $ do
      let input = [ def "x" $ const 43 4
                  , def "y" $ const 34 4
                  ]
          r = flip G.addNodes G.empty
              [ pv "x"
              , pv "y"
              ]
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle two related vars" $ do
      let input = [ def "x" $ const 43 4
                  , def "y" $ var "x" 4
                  ]
          r = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (pv "x", pv "y")
              ]
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle three linearly related vars" $ do
      let input = [ def "x" $ const 43 4
                  , def "y" $ var "x" 4
                  , def "z" $ add (var "y" 4) (const 33 4) 4
                  ]
          r = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (pv "x", pv "y")
              , (pv "y", pv "z")
              ]
      A.getDataDependenceGraph input `shouldBe` r

    it "should handle two vars with common dependee" $ do
      let input = [ def "x" $ const 43 4
                  , def "y" $ var "x" 4
                  , def "z" $ add (var "x" 4) (const 33 4) 4
                  ]
          r = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (pv "x", pv "y")
              , (pv "x", pv "z")
              ]
      A.getDataDependenceGraph input `shouldBe` r

  context "toDataDependenceGroups" $ do
    let pv = pilVar
    it "should handle empty data dependence graph" $ do
      let input = G.empty
          r = HMap.empty
      A.toDataDependenceGroups input `shouldBe` r

    it "should handle singleton data dependence graph" $ do
      let input = G.fromNode $ pv "a"
          r = HMap.fromList [(pv "a", HSet.fromList [pv "a"])]
      A.toDataDependenceGroups input `shouldBe` r

    it "should handle data dependence graph with one group" $ do
      let input = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
            [ (pv "a", pv "b")
            , (pv "b", pv "c")
            ]
          s = HSet.fromList [pv "a", pv "b", pv "c"]
          r = HMap.fromList [ (pv "a", s)
                               , (pv "b", s)
                               , (pv "c", s)
                               ]
      A.toDataDependenceGroups input `shouldBe` r

    it "should handle data dependence graph with two groups" $ do
      let input = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
            [ (pv "a", pv "b")
            , (pv "b", pv "c")
            , (pv "x", pv "y")
            ]
          s1 = HSet.fromList [pv "a", pv "b", pv "c"]
          s2 = HSet.fromList [pv "x", pv "y"]
          r = HMap.fromList [ (pv "a", s1)
                               , (pv "b", s1)
                               , (pv "c", s1)
                               , (pv "x", s2)
                               , (pv "y", s2)
                               ]
      A.toDataDependenceGroups input `shouldBe` r
