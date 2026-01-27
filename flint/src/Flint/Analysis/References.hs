module Flint.Analysis.References where

import Flint.Prelude
import Flint.Analysis.Path.Matcher as M
import Data.Aeson (Value(Object), parseJSON, (.:))
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep(PathPrep))
import Blaze.Types.Pil (Stmt)

data RegionChunk = RegionChunk
  { offset :: ByteOffset
  , length :: Bytes
  }
  deriving (Show)

instance FromJSON RegionChunk where
  parseJSON (Object a) =
    RegionChunk
      <$> a .: "offset"
      <*> a .: "length"
  parseJSON _ = mzero

data ReferenceKind = ReferenceKind
  { shape :: [RegionChunk]
  , access :: [Stmt] -- change to StmtPattern
  }
  deriving (Show)

-- only works if type of access is an instance of Read
-- instance FromJSON ReferenceKind where
--   parseJSON (Object a) = ReferenceKind <$>
--                          a .: "shape" <*>
--                          (read <$> (a .: "access"))
--   parseJSON _ = mzero

instance FromJSON ReferenceKind where
  parseJSON (Object a) =
    ReferenceKind
      <$> a .: "shape"
      <*> a .: "access"
  parseJSON _ = mzero

-- creates PathPrep with untouchedStmts in place of stmts to match
-- on patterns of untouched Stmts
forgePathPrep :: PathPrep stmt -> PathPrep stmt
forgePathPrep pp = PathPrep (pp ^. #untouchedStmts) (pp ^. #untouchedStmts) (pp ^. #taintSet) $ pp ^. #codeSummary

matchInvPats :: (HasAddress stmt
                , IsStatement expr stmt
                , IsExpression expr
                ) => [M.StmtPattern] -> Int -> PathPrep stmt -> [(MatcherState expr stmt, [stmt])]
matchInvPats pats maxMatches = runIdentity . M.match maxMatches M.dummySolver pats . forgePathPrep

matchInvPats_ :: (HasAddress stmt
                 , IsStatement expr stmt
                 , IsExpression expr
                 ) => [M.StmtPattern] -> PathPrep stmt -> [[stmt]]
matchInvPats_ pats pp = view _2 <$> matchInvPats pats 20 pp
