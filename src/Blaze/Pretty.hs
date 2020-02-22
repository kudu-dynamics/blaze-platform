module Blaze.Pretty
  ( Pretty( pretty )
  , prettyPrint
  ) where

import Protolude

import qualified Numeric
import qualified Data.Set as Set
import qualified Data.Map as Map
import Binja.Core (InstructionIndex(InstructionIndex), Address(Address))
import Data.Text (Text)
import qualified Data.Text as Text
import Binja.Function (Function)
import qualified Binja.MLIL as MLIL
import qualified Binja.Function as Func
import Control.Lens

-- TODO: make pretty return a monad instead of text,
-- which can do things like `indent`

class Pretty' t x | x -> t where
  pretty' :: x -> Text

instance Pretty' Text Int where
  pretty' n = show n

-- instance Pretty' () Int where
--   pretty' n = show (n + 1)


class Pretty x where
  pretty :: x -> Text

prettyPrint :: Pretty x => x -> IO ()
prettyPrint = putText . pretty

showHex :: (Integral a, Show a) => a -> Text
showHex x = Text.pack $ "0x" <> Numeric.showHex x ""

instance Pretty Function where
  pretty x = x ^. Func.name

instance Pretty Int where
  pretty = show

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

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" <> pretty a <> ", " <> pretty b <> ")"

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = "Map: " <> pretty (Map.toList m)
