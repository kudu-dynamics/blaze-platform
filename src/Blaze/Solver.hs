module Blaze.Solver where

import Blaze.Prelude

import Data.SBV
import qualified Data.SBV.String as S


add5 :: SWord16 -> SWord16
add5 n = n + 5

bigtest :: Symbolic SBool
bigtest = do
  x <- exists "x"
  y <- exists "y"
  constrain $ add5 x .== (y :: SWord16)
  return $ add5 x .== y
