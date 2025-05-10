module Flint.Types.Symbol where

import Flint.Prelude

import qualified Blaze.Pretty as Pretty

import Data.String.Conversions (ConvertibleStrings(convertString))


newtype Symbol a = Symbol { unSymbol :: Text }
  deriving (Eq, Ord, Read, Show, Generic)
  deriving newtype (IsString, Hashable, Semigroup, StringConv String, FromJSON, ToJSON)

instance Pretty.Tokenizable (Symbol a) where
  tokenize (Symbol t) = return [Pretty.tt t]

instance (ConvertibleStrings Text b) => ConvertibleStrings (Symbol a) b where
  convertString (Symbol t) = convertString t
