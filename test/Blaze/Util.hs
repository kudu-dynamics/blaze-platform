-- | Utility functions for tests
module Blaze.Util where
import Test.Hspec (HasCallStack, Expectation, shouldBe)
import Prelude (Eq)
import Blaze.Pretty (Tokenizable, PrettyShow (PrettyShow))

prettyShouldBe :: (HasCallStack, Eq a, Tokenizable a) => a -> a -> Expectation
prettyShouldBe x y = PrettyShow x `shouldBe` PrettyShow y
