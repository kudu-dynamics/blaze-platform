module CheckerTest where

import Blaze.Prelude
import qualified Prelude as P

newtype Pointer a = Pointer a
  deriving (Eq, Ord, Show)

newtype Array a = Array a
  deriving (Eq, Ord, Show)

newtype Program a = Program { runProgram_ :: Identity a }
  deriving (Functor, Applicative, Monad)

runProgram :: Program a -> a
runProgram = runIdentity . runProgram_

intPlus :: (Integral a, Integral b, Integral c) => a -> b -> c
intPlus = undefined


intPlus' = undefined

newVar :: Program a
newVar = P.error "sorry"

load :: Pointer a -> Program a
load = undefined

store :: Pointer a -> a -> Program a
store = undefined

strNCmp :: Pointer Text -> Pointer Text -> Int -> Program Bool
strNCmp = undefined

demo :: Program Bool
demo = do
  -- s1 <- newVar :: Program (Pointer Text)
  -- s2 <- newVar  
  a <- newVar :: Program Int
  b <- newVar :: Program Word64
  c <- pure $ intPlus a b
--  d <- pure $ intPlus (5 :: Int32) b
  return True
