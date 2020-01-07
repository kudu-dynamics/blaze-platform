module Blaze.Pretty
  ( Pretty( pretty )
  , prettyPrint
  ) where

import Protolude

import qualified Numeric
import qualified Data.Set as Set
import Binja.Core (InstructionIndex(InstructionIndex), Address(Address))
import Data.Text (Text)
import qualified Data.Text as Text
import Binja.Function (Function)
import qualified Binja.MLIL as MLIL
import qualified Binja.Function as Func
import Control.Lens

class Pretty x where
  pretty :: x -> Text

prettyPrint :: Pretty x => x -> IO ()
prettyPrint = putText . pretty

showHex :: (Integral a, Show a) => a -> Text
showHex x = Text.pack $ "0x" <> Numeric.showHex x ""

instance Pretty Function where
  pretty x = x ^. Func.name

instance Pretty (InstructionIndex a) where
  pretty (InstructionIndex x) = show x

instance Pretty Address where
  pretty (Address x) = showHex x

instance Pretty a => Pretty [a] where
  pretty ys = "[" <> f ys <> "]" where
    f [] = ""
    f [x] = pretty x
    f (x:xs) = pretty x <> ", " <> f xs

instance Pretty a => Pretty (Set a) where
  pretty ys = "#{" <> f (Set.toList ys) <> "}" where
    f [] = ""
    f [x] = pretty x
    f (x:xs) = pretty x <> ", " <> f xs

instance Pretty (MLIL.Expression a) where
  pretty _ = "(TODO: MLIL Expression)"
