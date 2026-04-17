module Flint.Types.Analysis.CAst.Finding
  ( Severity(..)
  , CAstFinding(..)
  ) where

import Flint.Prelude

import Blaze.Types.CAst (AddrRange)


data Severity
  = Info
  | Low
  | Medium
  | High
  | Critical
  deriving (Eq, Ord, Show, Generic, Hashable)

data CAstFinding = CAstFinding
  { findingName :: Text
  , description :: Text
  , severity    :: Severity
  , location    :: [AddrRange]   -- binary addresses of the match
  , cSource     :: Text          -- rendered C code snippet
  , boundNames  :: HashMap Text Text  -- bound variable name -> matched text
  } deriving (Eq, Ord, Show, Generic, Hashable)
