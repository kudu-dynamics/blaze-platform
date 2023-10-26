module Flint.Util
  ( module Flint.Util
  ) where

import Flint.Prelude

import qualified Data.UUID as UUID

incUUID :: UUID -> UUID
incUUID = uncurry UUID.fromWords64 . bimap (+1) (+1) . UUID.toWords64
