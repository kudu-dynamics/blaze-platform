module Flint.Analysis.References where

import Flint.Prelude
import qualified Flint.Analysis.Path.Matcher as M
import qualified Flint.Types.Analysis.Path.Matcher.Func as M
import Data.Aeson (Value(Object), parseJSON, (.:))
import qualified Data.HashMap.Strict as HashMap
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Function (Func, _name)
import qualified Data.HashSet as HashSet
import Flint.Analysis.Path.Matcher.Primitives (locationFromFunc)
import Flint.Types.Analysis.Path.Matcher.Primitives (PrimSpec(PrimSpec), CallableWMI(..))
import qualified Data.Text as DT
import Flint.Types.Symbol (Symbol)

-- should switch to fromMaybe from Data.Maybe and !? from Data.List instead of head hacks

data RegionChunk = RegionChunk
  { offset :: ByteOffset
  , chunkLength :: Bytes
  }
  deriving (Show, Generic)

instance FromJSON RegionChunk where
  parseJSON (Object a) =
    RegionChunk
      <$> a .: "offset"
      <*> a .: "length"
  parseJSON _ = mzero

data ReferenceKind = ReferenceKind
  { name :: Text -- hoping these are unique when provided
  , shape :: [RegionChunk]
  , access :: [Stmt] -- change to StmtPattern
  }
  deriving (Show, Generic)

-- using temporarily for function-based references
data PseudoReferenceKind = PseudoReferenceKind
  { name :: Text
  , createFuncName :: Text
  , storeFuncName :: Text
  , storeDeleteFuncName :: Text
  , deleteFuncName :: Text
  }
  deriving (Show, Generic, Eq, Hashable, FromJSON)

-- instance FromJSON PseudoReferenceKind where
--   parseJSON (Object a) =
--     PseudoReferenceKind
--       <$> a .: "name"
--       <*> a .: "createFuncName"
--       <*> a .: "storeFuncName"
--       <*> a .: "deleteFuncName"
--   parseJSON _ = mzero

-- storereference wmi give id based on referencekind in output <- do this?
-- mini storereference, deletereference, etc. wmis
-- the subprimitives are only subprimitives because they should not be output
-- if they were output, it would be simpler to use the Primitive pattern in StmtPattern
-- in flint/app/Flint/Main.hs, allPrims from Library.hs is used. add new wmis to allPrims
-- create new specs here
-- maybe bubble up reference usage in addition to wmis

-- only works if type of access is an instance of Read
-- instance FromJSON ReferenceKind where
--   parseJSON (Object a) = ReferenceKind <$>
--                          a .: "shape" <*>
--                          (read <$> (a .: "access"))
--   parseJSON _ = mzero

instance FromJSON ReferenceKind where
  parseJSON (Object a) =
    ReferenceKind
      <$> a .: "name"
      <*> a .: "shape"
      <*> a .: "access"
  parseJSON _ = mzero

-- want to match on patterns of untouched Stmts
forgePathPrep :: PathPrep stmt -> PathPrep stmt
forgePathPrep pp = pp & #stmts .~ (pp ^. #untouchedStmts)

matchInvPats :: ( M.HasAddress stmt
                , M.IsStatement expr stmt
                , M.IsExpression expr
                ) => [M.StmtPattern] -> Int -> PathPrep stmt -> [(M.MatcherState expr stmt, [stmt])]
matchInvPats pats maxMatches = runIdentity . M.match maxMatches M.dummySolver pats . forgePathPrep

matchInvPats_ :: ( M.HasAddress stmt
                 , M.IsStatement expr stmt
                 , M.IsExpression expr
                 ) => [M.StmtPattern] -> PathPrep stmt -> [[stmt]]
matchInvPats_ pats pp = view _2 <$> matchInvPats pats 20 pp

-- doing this instead of parameterizing Prim (or adding Either, etc.)
-- actually, can just pass ReferenceKinds
-- data RefPrim = RefPrim
  -- { name :: Text
  -- ,

-- give each referencekind a uniqueid, and create RefPrims
-- genRefPrims :: [ReferenceKind] -> [M.Prim]
-- genRefPrims refKinds = []

fromPseudoRefSubPrim :: M.Prim -> Func -> CallableWMI
fromPseudoRefSubPrim x func = CallableWMI
  { prim = x ^. #primType
  , func = func
  , callDest = M.FuncName $ func ^. _name
  , varMapping = HashMap.empty -- suspect
  , constraints = [] -- suspect
  , locations = HashMap.fromList
                . fmap (toSnd . const . locationFromFunc $ func)
                . HashSet.toList
                $ x ^. #primType . #locations
  , linkedVars = HashSet.empty -- suspect
  }

getInitialPseudoRefSubPrimsForFunc :: Func -> [(Text, M.Prim)] -> [CallableWMI]
getInitialPseudoRefSubPrimsForFunc func = mapMaybe f
  where
    f sprimPair = if (func ^. _name) == fst sprimPair
      then Just $ fromPseudoRefSubPrim (snd sprimPair) func
      else Nothing

-- plagiarized from Flint.Analysis.Path.Matcher.Primitives
-- refactor into typeclass if keeping
-- does this bubble? or do I need additional Primitive StmtPatterns?
getInitialPseudoRefSubPrims :: [(Text, M.Prim)]
                            -> [Func]
                            -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
getInitialPseudoRefSubPrims sprimPairs
  = foldr M.addCallableWMI_ HashMap.empty
  . foldMap (`getInitialPseudoRefSubPrimsForFunc` sprimPairs)

-- probably need create, store, delete_store, delete
genPseudoRefSubPrims :: [PseudoReferenceKind] -> [[(Text, M.Prim)]]
genPseudoRefSubPrims = map genSubPrimsOnce
  where
    genPrim :: PseudoReferenceKind -> Text -> [M.StmtPattern] -> (Text, M.Prim)
    genPrim pRefKind op pats =
      ( case op of
          "store" -> pRefKind ^. #storeFuncName
          "create" -> pRefKind ^. #createFuncName
          "delete" -> pRefKind ^. #deleteFuncName
          "storedelete" -> pRefKind ^. #storeDeleteFuncName
          _ -> error "cosmic radiation detected"
      , M.Prim (PrimSpec (DT.concat [pRefKind ^. #name, "_", op]) HashSet.empty HashSet.empty) pats)
    callPat name = M.Stmt $ M.Call Nothing (M.CallFunc $ M.FuncName name) []
    genSubPrimsOnce :: PseudoReferenceKind -> [(Text, M.Prim)]
    genSubPrimsOnce pRefKind = zipWith ($) (map (genPrim pRefKind) ["create", "store", "storedelete", "delete"])
      [ [M.Star, callPat $ pRefKind ^. #createFuncName]
      , [M.Star, callPat $ pRefKind ^. #storeFuncName]
      , [M.Star, callPat $ pRefKind ^. #storeDeleteFuncName]
      , [M.Star, callPat $ pRefKind ^. #deleteFuncName]
      ]

-- one full pattern OR another full pattern, etc.
-- need to add "use" at end. can't really do that with call-level
-- primitive since there is no data tracking (since calls are
-- assumed to be arbitrary)
-- readSpec, maybe?
-- maybe append reference name to prim name
-- what if the alloc occurred before the function where the free occurs?
-- the terminal ordered subchains of subprimitives need to themselves bubble up
staleReferencePrims :: [[M.Prim]] -> [M.Prim]
staleReferencePrims =
  map genPrim
  where
    getSubPrimName :: M.Prim -> Text
    getSubPrimName subPrim = DT.take (DT.length longName - 7) longName
      where
        longName = subPrim ^. #primType . #name
    headHack = view _1 . fromMaybe (M.Prim (PrimSpec "fake" HashSet.empty HashSet.empty) [], []) . uncons
    genPrim :: [M.Prim] -> M.Prim
    -- maybe bind to ensure arg to funcs is same
    genPrim subPrims = M.Prim (PrimSpec (DT.concat ["stale_reference_", getSubPrimName $ headHack subPrims]) HashSet.empty $ HashSet.fromList ["create", "store", "delete"])
      [ M.Star
      , M.Location "create" $ M.CallsPrimitive (view #primType $ headHack subPrims) HashMap.empty
      , M.Star
      , M.Location "store" $ M.CallsPrimitive (view #primType $ subPrims !! 1) HashMap.empty
      , M.And M.Star . M.AvoidUntil . M.AvoidSpec (M.CallsPrimitive (view #primType $ subPrims !! 2) HashMap.empty) . M.Location "delete" $ M.CallsPrimitive (view #primType $ subPrims !! 3) HashMap.empty
      ]

-- storedelete nonexistence might also be implied
applyImpliedMatches :: [[(Text, M.Prim)]] -> [M.Prim] -> [M.Prim]
applyImpliedMatches = zipWith z
 where
   dropAndSet :: M.Prim -> Int -> (HashSet (Symbol Address) -> HashSet (Symbol Address)) -> M.Prim
   dropAndSet prim n hsFunc = (prim & #stmtPattern .~ drop n (prim ^. #stmtPattern)) & #primType .~ ((prim ^. #primType) & #locations .~ hsFunc (prim ^. #primType . #locations))
   safeHead = fromMaybe ("", M.Prim (PrimSpec "fake" HashSet.empty HashSet.empty) []) . listToMaybe
   z x y
     | (fst . safeHead $ x) == "#implied" && fst (x !! 1) == "#implied" = dropAndSet y 4 $ HashSet.delete "store" . HashSet.delete "create"
     | (fst . safeHead $ x) == "#implied" = dropAndSet y 2 $ HashSet.delete "create"
     | otherwise = y

refPrims :: [M.Prim]
refPrims = []

-- testing stuff below (should really be done in matcher)

patStarts :: ( M.HasAddress stmt
             , M.IsStatement expr stmt
             , M.IsExpression expr
             ) => [M.StmtPattern] -> PathPrep stmt -> [[stmt]]
patStarts pats pp = startHelper [] (pp ^. #untouchedStmts) pats pp

startHelper :: ( M.HasAddress stmt
               , M.IsStatement expr stmt
               , M.IsExpression expr
               ) => [stmt] -> [stmt] -> [M.StmtPattern] -> PathPrep stmt -> [[stmt]]
startHelper stmts origList pats pp
  | length stmts == length origList = if matched then [stmts] else []
  | matched = stmts : startHelper [] (take (length origList - length stmts) origList) pats pp -- split
  | otherwise = startHelper (drop (length origList - length stmts - 1) origList) origList pats pp
  where
    tested = matchInvPats_ pats (pp & #untouchedStmts .~ stmts)
    matched = not . null $ tested
    -- matched = and [not . null $ tested, not . null . (!! 0) $ tested]

patEnds :: ( M.HasAddress stmt
           , M.IsStatement expr stmt
           , M.IsExpression expr
           ) => [M.StmtPattern] -> PathPrep stmt -> [[stmt]]
patEnds pats pp = map (endHelper pats pp) $ patStarts pats pp

endHelper :: ( M.HasAddress stmt
             , M.IsStatement expr stmt
             , M.IsExpression expr
             ) => [M.StmtPattern] -> PathPrep stmt -> [stmt] -> [stmt]
endHelper pats pp stmts =
  if not matched
    then stmts
    else endHelper pats pp shortenedList
  where
    shortenedList = take (length stmts - 1) stmts
    tested = matchInvPats_ pats $ pp & #untouchedStmts .~ shortenedList
    matched = not . null $ tested
    -- matched = and [not . null $ tested, not . null . (!! 0) $ tested]
