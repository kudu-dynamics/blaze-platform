{-# LANGUAGE OverloadedLists     #-}

module Blaze.Pil.Solver.List where

import Blaze.Prelude hiding (length, take, drop, head, State)
import qualified Prelude as P
import Data.SBV
import Data.SBV.Dynamic (SVal, svBool, svInteger, svEqual)
import qualified Data.SBV.String as S
import qualified Data.SBV.Maybe as M
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Tuple as T
import qualified Data.SBV.Char as C
import qualified Data.SBV.List as L
import Data.SBV.Internals ( SBV(SBV), unSBV, SeqOp(SeqNth), SVal(SVal), Op(SeqOp)
                          , cache
                          , sbvToSV
                          , svToSV
                          , newExpr
                          , SBVExpr(SBVApp)
                          , SV
                          , State
                          , uncache
                          , NamedSymVar(NamedSymVar)
                          , CV(CV)
                          , CVal (CList, CInteger)
                          )
import Data.SBV.Control
import qualified Data.Map.Strict as Map

unlift1 :: (SBV () -> SBV ()) -> SVal -> SVal
unlift1 f = unSBV . f . SBV

asUnitList :: SVal -> SList ()
asUnitList = SBV


type SValList = SVal
type SValInteger = SVal

length :: SValList -> SInteger
length = L.length . asUnitList

take :: SInteger -> SValList -> SValList
take n = unSBV . L.take n . asUnitList

head :: SValList -> SVal
head = unSBV . L.head . asUnitList

constrain_ :: SVal -> Symbolic ()
constrain_ = constrain . SBV

bilbo :: Maybe Int -> Text
bilbo x
  | Just n <- x = "ok " <> show n
  | otherwise = "bad"


-- lift2 :: forall a b c. (SymVal a, SymVal b, SymVal c) => SeqOp -> Maybe (a -> b -> c) -> SBV a -> SBV b -> SBV c
-- lift2 w mbOp a b
--   | Just cv <- concEval2 mbOp a b
--   = cv
--   | True
--   = SBV $ SVal k $ Right $ cache r
--   where k = kindOf (Proxy @c)
--         r st = do sva <- sbvToSV st a
--                   svb <- sbvToSV st b
--                   newExpr st k (SBVApp (SeqOp w) [sva, svb])

-- unliteralSVal :: SVal -> Maybe a
-- unliteralSVal (SVal _ (Left c)) = Just $ fromCV c
-- unliteralSVal _ = Nothing

concEval2 :: (SymVal a, SymVal b, SymVal c) => Maybe (a -> b -> c) -> SBV a -> SBV b -> Maybe (SBV c)
concEval2 mbOp a b = literal <$> (mbOp <*> unliteral a <*> unliteral b)

lift2_ :: SeqOp -> SVal -> SVal -> SVal
lift2_ w a b
  -- | Just cv <- concEval2 mbOp a b
  -- = cv
  -- | True
  = SVal k $ Right $ cache r
  where k = kindOf a
        r st = do sva <- svToSV st a
                  svb <- svToSV st b
                  newExpr st k (SBVApp (SeqOp w) [sva, svb])


elemAt :: SValList -> SInteger -> SVal
elemAt xs@(SVal listK xsv) i = case listK of
  KList elemK -> case xsv of
    Right _ -> SVal elemK $ Right $ cache r
      where
        r st = do sva <- svToSV st xs
                  svb <- sbvToSV st i
                  newExpr st elemK (SBVApp (SeqOp SeqNth) [sva, svb])
    Left (CV _ (CList cvals)) -> case i of
      (SBV (SVal _ (Left (CV _ (CInteger i'))))) ->
        SVal elemK . Left . CV elemK $ cvals !! fromIntegral i'
      _ -> P.error "sad"
    _ -> P.error "oh well"
  _ -> P.error "oops"


-- elemAt :: SVal -> SInteger -> SVal
-- elemAt l i
--   -- | Just xs <- unliteral l, Just ci <- unliteral i, ci >= 0, ci < genericLength xs, let x = xs `genericIndex` ci
--   -- = literal x
--   -- | True
--   = lift2 SeqNth Nothing l i


test :: Symbolic ()
test = do
  let xs'' = [3, 10, 18, 8, 4] :: SList Integer
  xs' <- sList "xs" :: Symbolic (SList Integer)
  ys' <- sList "ys" :: Symbolic (SList Integer)
  i <- sInteger "i"
  let xs = unSBV xs'
      ys = unSBV ys'
  constrain $ length xs .== 5
  constrain $ xs' .== xs''
  -- constrain_ $ ys `svEqual` take 3 xs
  -- constrain_ $ svInteger KUnbounded 18 `svEqual` elemAt xs i
  constrain_ $ svInteger KUnbounded 18 `svEqual` elemAt xs 2
    -- `svEqual` svInteger KUnbounded 5
  return ()
