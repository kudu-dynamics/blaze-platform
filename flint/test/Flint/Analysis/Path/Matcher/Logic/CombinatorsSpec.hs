module Flint.Analysis.Path.Matcher.Logic.CombinatorsSpec where

import Flint.Prelude hiding (until)

import Flint.Analysis.Path.Matcher.Logic.Combinators
import Flint.Types.Analysis.Path.Matcher

import Test.Hspec

type Matcher a = MatcherT () Int Identity a

spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Logic.Combinators" $ do
  let defaultMatcherState = emptyMatcherState
      defaultMatcherCtx = MatcherCtx dummySolver
      maxResults = 20 -- should always be less for these tests
      observeAll :: Matcher a -> [a]
      observeAll = fmap fst
        . runIdentity
        . observeManyMatcherT defaultMatcherCtx defaultMatcherState maxResults

      isEven :: Int -> Bool
      isEven = even

      getEven :: Int -> Maybe Int
      getEven n = if even n then Just n else Nothing

  context "choose" $ do
    it "should choose one of each element in a list" $ do
      let xs :: [Int]
          xs = [1, 2, 3]
      sort (observeAll (choose xs)) `shouldBe` xs

  context "chooseAndReduce" $ do
    it "should choose one of each element in and return remaining list" $ do
      let xs :: [Int]
          xs = [1, 2, 3]
          expected = [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
      sort (over _2 sort <$> observeAll (chooseAndReduce xs)) `shouldBe` expected

  context "parseUntil" $ do
    it "should return no results if there is no match" $ do
      let stmts :: [Int]
          stmts = [1]
          pat = bool bad good . (== 0)
          expected = []
      observeAll (parseUntil pat stmts) `shouldBe` expected

    it "should return single result when single thing in list matches" $ do
      let stmts :: [Int]
          stmts = [1]
          pat = bool bad good . (== 1)
          expected = [((), [])]
      observeAll (parseUntil pat stmts) `shouldBe` expected 

    it "should return single matching result and remaining list" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4]
          pat n = bool bad (return n) $ n == 2
          expected = [(2, [3, 4])]
      observeAll (parseUntil pat stmts) `shouldBe` expected 

    it "should return multiple matching results with appropriate remaining lists" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          pat n = bool bad (return n) $ isEven n
          expected = [ (2, [3, 4, 5])
                     , (4, [5])
                     ]
      sort (observeAll $ parseUntil pat stmts) `shouldBe` sort expected 

  context "parseNext" $ do
    it "should return no results if there is no match" $ do
      let stmts :: [Int]
          stmts = [1]
          pat = bool bad good . (== 0)
          expected = []
      observeAll (parseNext pat stmts) `shouldBe` expected 

    it "should return single result when single thing in list matches" $ do
      let stmts :: [Int]
          stmts = [1]
          pat = bool bad good . (== 1)
          expected = [((), [])]
      observeAll (parseNext pat stmts) `shouldBe` expected 

    it "should not return non-next result, even if it matches" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4]
          pat n = bool bad (return n) $ n == 2
          expected = []
      observeAll (parseNext pat stmts) `shouldBe` expected 

    it "should not return multiple matching results" $ do
      let stmts :: [Int]
          stmts = [2, 3, 4, 5]
          pat n = bool bad (return n) $ isEven n
          expected = [ (2, [3, 4, 5])
                     ]
      sort (observeAll $ parseNext pat stmts) `shouldBe` sort expected 

  context "avoidUntil" $ do
    it "should return no results if `until` is not matched" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          avoid _ _ = bad
          until :: [Int] -> Matcher ((), [Int])
          until _ = bad
          expected = []
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should return no results if `until` is matched but `avoid` also matches" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          avoid _ _ = good
          until :: [Int] -> Matcher (Text, [Int])
          until xs = do
            parseUntil (bool bad (return "Got it") . (== 3)) xs
          expected = []
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should return no results if `until` is matched but `avoid` matches once" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          avoid _ xs = void $ parseUntil (bool bad good . (== 2)) xs
          until :: [Int] -> Matcher (Text, [Int])
          until xs = do
            parseUntil (bool bad (return "Got it") . (== 3)) xs
          expected = []
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should return result if `until` matches and `avoid` doesn't match" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          avoid _ _ = bad
          until :: [Int] -> Matcher (Text, [Int])
          until xs = do
            parseUntil (bool bad (return "Got it") . (== 3)) xs
          expected = [("Got it", [4, 5])]
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should return result if `until` matches and `avoid` doesn't match until after `until`" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          avoid _ xs = void $ parseUntil (bool bad good . (== 4)) xs
          until :: [Int] -> Matcher (Text, [Int])
          until xs = do
            parseUntil (bool bad (return "Got it") . (== 3)) xs
          expected = [("Got it", [4, 5])]
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should return results for two matching `until`s if there are no avoids" $ do
      let stmts :: [Int]
          stmts = [1, 3, 4, 5, 6, 7, 9]
          avoid _ xs = void $ parseUntil (bool bad good . (== 14)) xs
          until :: [Int] -> Matcher (Int, [Int])
          until xs = do
            parseUntil (maybe bad return . getEven) xs
          expected = [ (4, [5, 6, 7, 9])
                     , (6, [7, 9])
                     ]
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

    it "should prune off one matching `until` because of an `avoid` while letting other succeed because the `avoid` comes after" $ do
      let stmts :: [Int]
          stmts = [1, 3, 4, 5, 6, 7, 9]
          avoid _ xs = void $ parseUntil (bool bad good . (== 5)) xs
          until :: [Int] -> Matcher (Int, [Int])
          until xs = do
            parseUntil (maybe bad return . getEven) xs
          expected = [ (4, [5, 6, 7, 9])
                     ]
      sort (observeAll $ avoidUntil avoid until stmts) `shouldBe` sort expected 

  context "anyOne" $ do
    it "should return no results of there are no parsers" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          parsers :: [[Int] -> Matcher ((), [Int])]
          parsers = []
          expected = []
      sort (observeAll $ anyOne parsers stmts) `shouldBe` sort expected 

    it "should return matching results with just one parser" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          parsers :: [[Int] -> Matcher (Int, [Int])]
          parsers = [ parseUntil (maybe bad return . getEven) ]
          expected = [ (2, [3, 4, 5])
                     , (4, [5])
                     ]
      sort (observeAll $ anyOne parsers stmts) `shouldBe` sort expected 

    it "should return matching results with multiple parsers" $ do
      let stmts :: [Int]
          stmts = [1, 2, 3, 4, 5]
          parsers :: [[Int] -> Matcher (Int, [Int])]
          parsers = [ parseUntil (maybe bad return . getEven)
                    , parseUntil (bool bad (return 3) . (== 3))
                    ]
          expected = [ (2, [3, 4, 5])
                     , (4, [5])
                     , (3, [4, 5])
                     ]
      sort (observeAll $ anyOne parsers stmts) `shouldBe` sort expected 

  context "parseThroughList" $ do
    it "should return no results if pats don't match" $ do
      let stmts :: [Int]
          stmts = [1, 2]
          pats = [bool bad good . (== 0)]
          expected = []
      observeAll (parseThroughList pats stmts) `shouldBe` expected

    it "should return empty results and all stmts if parser list empty" $ do
      let stmts :: [Int]
          stmts = [1, 2]
          pats = [] :: [Int -> MatcherT () Int Identity Bool]
          expected = [([], stmts)]
      observeAll (parseThroughList pats stmts) `shouldBe` expected

    it "should return single result when single thing in list matches" $ do
      let stmts :: [Int]
          stmts = [0, 1, 2, 3, 4, 5]
          pats = [ bool bad (return ("hey" :: Text)) . (== 1)
                 , bool bad (return ("there" :: Text)) . (== 3)]
          expected = [(["hey", "there"], [4, 5])]
      observeAll (parseThroughList pats stmts) `shouldBe` expected 

    it "should return multiple results that match" $ do
      let stmts :: [Int]
          stmts = [0, 1, 2, 3, 4, 5]
          pats = [ bool bad (return ("hey" :: Text)) . (== 1)
                 , \n -> bool bad (return $ show n) $ isEven n
                 ]
          expected = [ (["hey", "2"], [3, 4, 5])
                     , (["hey", "4"], [5])
                     ]
      observeAll (parseThroughList pats stmts) `shouldBe` expected 
