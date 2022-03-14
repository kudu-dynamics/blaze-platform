-- | Utility functions for tests
module Blaze.Util where
import Blaze.Prelude hiding (show)
import Test.Hspec (Expectation, shouldBe)
import Prelude (show)
import Blaze.Pretty (Tokenizable, pretty')
import Text.Pretty.Simple (pStringNoColor)

newtype PrettyPrettyShow a = PrettyPrettyShow a
  deriving (Eq, Ord)

instance Tokenizable a => Show (PrettyPrettyShow a) where
  show (PrettyPrettyShow x) = cs . pStringNoColor . cs $ pretty' x

prettyShouldBe :: (HasCallStack, Eq a, Tokenizable a) => a -> a -> Expectation
prettyShouldBe x y = PrettyPrettyShow x `shouldBe` PrettyPrettyShow y
