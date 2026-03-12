{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Flint.Analysis.Path.Matcher.Taint where

import Flint.Prelude hiding (sym, negate, Location)
import Flint.Types.Analysis (Parameter(..), Taint(..), TaintPropagator(..))

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Size(Size))
import Blaze.Pil.Construct (var')

import qualified Data.HashSet as HashSet

-- TODO: add more TODOs for things
-- TODO more bit-taint specific helper functions later?
newtype BitSet = BitSet { unBitSet :: Word64 }
  deriving (Eq, Ord, Show, Generic)

-- every bit tainted
allBits :: BitSet
allBits = BitSet maxBound

-- none are tainted
noBits :: BitSet
noBits = BitSet 0

union :: BitSet -> BitSet -> BitSet
union (BitSet a) (BitSet b) = BitSet (a .|. b)

intersection :: BitSet -> BitSet -> BitSet
intersection (BitSet a) (BitSet b) = BitSet (a .&. b)

-- | Transitive closure of a 'HashSet Taint'
taintTransClos :: HashSet Taint -> HashSet Taint
taintTransClos ts =
  HashSet.fromList $ do
    t1 <- HashSet.toList ts
    t2 <- HashSet.toList ts
    if t1 == t2
      then [t1]
      else case (t1, t2) of
        (Tainted src1 (Left dst1), Tainted src2 dst2)
          | Just dst1 == src2 ^? #op . #_VAR . #_VarOp -> [t1, Tainted src1 dst2]
        (Tainted src1 (Right dst1), Tainted src2 (Left dst2))
          | dst1 == src2 -> [t1, Tainted src1 (Right $ var' dst2 (coerce $ dst2 ^. #size :: Size Pil.Expression))]
        (Tainted src1 (Right dst1), Tainted src2 (Right dst2))
          | dst1 == src2 -> [t1, Tainted src1 (Right dst2)]
        _ -> [t1]

-- | Collect any taints from the expression if it matches one or more
-- 'TaintPropagator's.
mkTaintPropagatorTaintSet ::
  [TaintPropagator] ->
  Maybe Pil.PilVar ->
  Pil.ExprOp Pil.Expression ->
  HashSet Taint
mkTaintPropagatorTaintSet tps mRetVar =
  \case
    Pil.CALL (Pil.CallOp dest args) -> case Pil.destName dest of
                                         Nothing -> HashSet.empty
                                         Just name -> go name args
    _ -> HashSet.empty
  where
    go name args =
      HashSet.fromList $ do
        tps >>= \case
          FunctionCallPropagator propName (Parameter (atMay args -> Just fromExpr)) toParam
            | name == propName ->
                case toParam of
                  Parameter (atMay args -> Just toExpr) -> [Tainted fromExpr (Right toExpr)]
                  ReturnParameter ->
                    case mRetVar of
                      Just retVar -> [Tainted fromExpr (Left retVar)]
                      _ -> []
                  _ -> []
          FunctionCallPropagator {} -> []

mkStmtTaintSet :: [TaintPropagator] -> Pil.Stmt -> HashSet Taint
mkStmtTaintSet tps (Pil.Stmt _ statement) =
  case statement of
    Pil.Def (Pil.DefOp dst src) ->
      HashSet.fromList (interestingSubexpressions src <&> (`Tainted` Left dst))
        <> mkTaintPropagatorTaintSet tps (Just dst) (src ^. #op)
    Pil.Store (Pil.StoreOp dst src) ->
      HashSet.fromList (interestingSubexpressions src <&> (`Tainted` Right dst))
        <> mkTaintPropagatorTaintSet tps Nothing (src ^. #op)
    Pil.Call callOp -> mkTaintPropagatorTaintSet tps Nothing (Pil.CALL callOp)
    _ -> HashSet.empty
  where
    interestingSubexpressions :: Pil.Expression -> [Pil.Expression]
    interestingSubexpressions e =
      case e ^. #op of
        Pil.CONST _ -> []
        Pil.CONST_PTR _ -> []
        Pil.CONST_FLOAT _ -> []
        Pil.ConstStr _ -> []
        Pil.ConstFuncPtr _ -> []
        Pil.CALL _ ->
          -- Do not recurse into 'CALL' subexpressions, since 'tps' are supposed
          -- to handle these
          []
        op -> e : foldMap interestingSubexpressions (toList op)

mkTaintSet :: [TaintPropagator] -> [Pil.Stmt] -> HashSet Taint
mkTaintSet tps = taintTransClos . HashSet.unions . fmap (mkStmtTaintSet tps)


-- notes to self:
-- [x] make the primitive
--{x] figure out how pattern matching works
  -- [x] primnspec.toml
   -- [x} Library.hs
  -- [x] PrimSpec,hs
  -- [x] StdLib.hs
-- [x] match based on source pattern
-- [] bit track taint analysis
-- [] cleann code
-- [] copy prop memchr or it wont be useful
-- [] take list from aggressive expand take list
-- [} include stmt with context field to show what == to what

-- CLEANUP ME...
-- refactoring will be done later done the line.
containsGlobalPtr ::Pil.Expression -> Bool
containsGlobalPtr e = case  e^. #op of
  Pil.GLOBAL_PTR _ -> True
  op -> any containsGlobalPtr(toList op)

--need to see if expr is tainted by source matching the filter
isTaintedBySrc :: [TaintPropagator] -> (Pil.Expression -> Bool) -> HashSet Taint -> Pil.Expression -> Bool
isTaintedBySrc tps srcFalter ts dstExpr =
  any (\src -> doesTaint tps ts src dstExpr /= noBits) matchingSources
  where
    -- both isTainted and isTaintedBySrc do this deduplication stuff
    -- however, i'd keep it seperate for now... i dont want a helper
    -- isTainted needs the taintList and reuses it for for indirectTaint
    matchingSources = filter srcFalter
      . HashSet.toList
      . HashSet.fromList
      $ map (view #src)(HashSet.toList ts)

isTainted :: [TaintPropagator] -> HashSet Taint -> Pil.Expression -> Bool
isTainted tps ts dstExpr = directTaint ||  indrectTaint
  where
    taintList = HashSet.toList ts
    allSources = HashSet.toList . HashSet.fromList $ map (view #src) taintList
    directTaint = any (\src -> doesTaint tps ts src dstExpr /= noBits) allSources
    dstSymbol :: Either Pil.PilVar Pil.Expression -> Maybe Text
    dstSymbol (Left pv) = Just (pv ^. #symbol)
    dstSymbol (Right expr) = expr ^? #op . #_VAR . #_VarOp . #symbol

    flowTargets = mapMaybe (\(Tainted src dst) ->
      if src == dstExpr then dstSymbol dst else Nothing) taintList

    -- catches pointer too tainted data
    -- if &buf flows into the same variable that a propagator marks
    -- then they are the same thing but what if if pilvar is different then???
    -- sloppy FIX LATER
    indrectTaint = any (\sym -> any (\(Tainted src dst) ->
      src /= dstExpr && dstSymbol dst == Just sym) taintList) flowTargets

-- TODO: Imeplement the doesTaint that will query data flow
-- does this source taint this destination
doesTaint :: [TaintPropagator] -> HashSet Taint -> Pil.Expression -> Pil.Expression -> BitSet
doesTaint tps taintSet srcExpr dstExpr

  | srcExpr == dstExpr
  = allBits

  -- dst is a variable
  | Just dstVar <- dstExpr ^? #op . #_VAR . #_VarOp
  , Tainted srcExpr (Left dstVar) `HashSet.member` taintSet
  = allBits

  -- dst is memory location
  | Tainted srcExpr (Right dstExpr) `HashSet.member` taintSet
  = allBits

    --handle sub expressions for a compound
    -- TODO recurse deeper here maybe?
  -- so if the src taints any arg that propagates to the return then that means dst is tainted right?
  | Pil.CALL (Pil.CallOp dest args) <- dstExpr ^. #op
  , Just funcName <- Pil.destName dest
  , let matchProps = filter (\case
          FunctionCallPropagator pName (Parameter _) ReturnParameter -- ugh why is the
            | pName == funcName -> True
          _ -> False) tps
  , not (null matchProps)
  , let arhsTainted = any (\case
          FunctionCallPropagator _ (Parameter argIdx) ReturnParameter
            | Just argExpr <- atMay args argIdx
            -> doesTaint tps taintSet srcExpr argExpr /= noBits
          _ -> False) matchProps
  , arhsTainted
  = allBits

    | let suvExprs = toList (dstExpr ^. #op)
    , not (null suvExprs)
    , any ((/= noBits) . doesTaint tps taintSet srcExpr) suvExprs
    = allBits

  -- TODO: there are probably more cases we should handle proabbly maybe?

  -- TODo: Find out which bits in BitSet are actually tainted
  -- it may be overkill for now
  -- so fow now, return all or nothing...
  | otherwise = noBits
