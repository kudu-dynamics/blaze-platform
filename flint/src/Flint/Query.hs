{- HLINT ignore "Use if" -}

module Flint.Query
  ( module Flint.Query
  , module Flint.Types.Query
  ) where

import Flint.Prelude

import Flint.Analysis (showPathsWithMatches)
import Flint.Analysis.Path.Matcher (StmtPattern)
import Flint.Analysis.Path.Matcher.Stub (StubSpec, stubPath)
import qualified Flint.Analysis.Path.Matcher as M
import Flint.Cfg.Path (CallDepth, samplesFromQuery, pickFromList)
import qualified Flint.Types.CachedCalc as CC
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Types.Query

import Blaze.Cfg.Path (PilPath)
import Blaze.Pil.Solver (solveStmtsWithZ3)
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (LEdge(LEdge), Edge(Edge))
import qualified Blaze.Types.Graph as G
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Path as Path
import qualified Blaze.Types.Pil.Solver as Solver

-- import Blaze.Pretty (prettyStmts')

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE


getCallSequenceGraph_
  :: [StmtPattern]
  -> State Int ([FuncNode M.Func], CallSequenceGraph M.Func)
getCallSequenceGraph_ = foldM go ([], G.empty)
  where
    useId = do
      n <- get
      put $ n + 1
      return n
    connectToParents [] node g = G.addNodes [node] g
    connectToParents parents node g = flip G.addEdges g $ do
      parent <- parents
      return $ LEdge () (Edge parent node)
    go
      :: ([FuncNode M.Func], CallSequenceGraph M.Func)
      -> StmtPattern
      -> State Int ([FuncNode M.Func], CallSequenceGraph M.Func)
    go (parents, g) = \case
      M.Stmt (M.Call _ (M.CallFunc func) _) -> do
        node <- FuncNode func <$> useId
        let g' = connectToParents parents node g
        return ([node], g')
      M.Stmt _ -> return (parents, g)
      -- Even if there is a Call pattern in AvoidUntil, we can't just avoid
      -- paths that call that function because it might be part of a more complex
      -- pattern where it's only not ok to call the func under certain contexts.
      -- So this just has to be handled during pattern matching.
      M.AvoidUntil s -> go (parents, g) $ s ^. #until
      M.AnyOne pats -> foldM f ([], g) pats
        where
          f (allEndNodes, g') pat = do
            (endNodes, g'') <- go (parents, g') pat
            return (endNodes <> allEndNodes, g'')
      M.Unordered pats -> foldM go (parents, g) pats
        -- Below is the correct way, but could get expensive if len(pats) > 4.
        -- Currently, the path sampler that uses the CallSequenceGraph doesn't
        -- look at function call ordering, so for now we can just treat unordered
        -- like an ordered.
        -- go (parents, g) . M.AnyOne . fmap M.Ordered $ permutations pats

      M.Ordered pats -> foldM go (parents, g) pats
      M.Where pat _ -> go (parents, g) pat

getCallSequenceGraph :: [StmtPattern] -> CallSequenceGraph M.Func
getCallSequenceGraph pats = snd $ evalState (getCallSequenceGraph_ pats) 0

mkFuncAddrMapping :: [Function] -> HashMap Address Function
mkFuncAddrMapping = foldl' (\m func -> HashMap.insert (func ^. #address) func m) HashMap.empty

mkFuncNameMapping :: [Function] -> HashMap Text Function
mkFuncNameMapping = foldl' (\m func -> HashMap.insert (func ^. #name) func m) HashMap.empty

data FuncMapping = FuncMapping
  { addrMap :: HashMap Address Function
  , nameMap :: HashMap Text Function
  }
  deriving (Eq, Ord, Show, Generic)

mkFuncMapping :: [Function] -> FuncMapping
mkFuncMapping funcs = FuncMapping (mkFuncAddrMapping funcs) (mkFuncNameMapping funcs)

getFullFunction :: FuncMapping -> M.Func -> Maybe Function
getFullFunction m (M.FuncName x) = HashMap.lookup x $ m ^. #nameMap
getFullFunction m (M.FuncAddr x) = HashMap.lookup x $ m ^. #addrMap

toFullFunctionSequenceGraph
  :: FuncMapping
  -> CallSequenceGraph M.Func
  -> Maybe (CallSequenceGraph Function)
toFullFunctionSequenceGraph m = traverse . traverse $ getFullFunction m

-- | Gets all single paths through the CallSequenceGraph, as well as information
-- that will is useful for sampling Cfg paths along the call seq path.
-- The optional second argument is a set that restricts the possible start functions
-- TODO: break this out into some pure functions and add tests
getCallSeqPreps :: CfgStore -> Maybe (HashSet Function) -> CallSequenceGraph Function -> IO [CallSeqPrep]
getCallSeqPreps store restrictStartFuncs g = do
  let starts = HashSet.toList $ G.sources g
  fmap concat . forConcurrently starts $ \start -> do
    let (callSeqs :: [NonEmpty Function]) = (view #function <$>) <$> (Path.toNodeList <$> (Path.getAllPaths (const identity) 1 start g :: [AlgaPath () Int (FuncNode Function)]) :: [NonEmpty (FuncNode Function)])
    forConcurrently callSeqs $ \callSeq -> do
      let firstCall_ = NE.head callSeq
          lastCall_ = NE.last callSeq
          callSet_ = HashSet.fromList . NE.toList $ callSeq
      callersOfFirstCall <-
        CC.get firstCall_ (store ^. #ancestorsCache) >>= \case
          Nothing -> return ([] :: [Function])
          Just s -> return
            . maybe identity (\sfuncs -> filter (`HashSet.member` sfuncs)) restrictStartFuncs
            $ HashSet.toList s
      -- Trying to limit unnecessary STDOUT output. Having a debug flag to
      -- enable diagnostic output would be preferable. - Hazmat
      -- putText $ "Callers of first call: " <> show (length callersOfFirstCall)
      (reachList :: [Maybe Function]) <- forConcurrently callersOfFirstCall $ \func -> do
        CC.get (func :: Function) (store ^. #descendantsCache) >>= \case
          Nothing -> return Nothing
          Just descs -> case callSet_ `HashSet.isSubsetOf` descs of
            False -> return Nothing
            True -> return $ Just func
      return $ CallSeqPrep
        { canReach = HashSet.fromList . catMaybes $ reachList
        , callSet = callSet_
        , firstCall = firstCall_
        , lastCall = lastCall_
        , callSeq = callSeq
        }

-- | Returns a single sampled path for the CallSeqPrep
samplePathFromPrep
  :: CallDepth
  -> Word64
  -> CfgStore
  -- TODO: taints
  -- TODO: stubs
  -> CallSeqPrep
  -> IO (Maybe (Function, [PilPath]))
samplePathFromPrep callDepth numSamples store prep = do
  case HashSet.toList (prep ^. #canReach) of
    [] -> return Nothing -- no functions can reach all the calls
    (x:xs) -> do
      luckyStart <- pickFromList randomRIO (x :| xs)
      let opts = QueryCallSeqOpts
            { start = luckyStart
            , callSeqPrep = prep
            , numSamples = numSamples
            , callExpandDepthLimit = callDepth
            }
      paths <- samplesFromQuery store $ QueryCallSeq opts
      return $ Just (luckyStart, paths)

-- | Uses static calls in pattern to narrow down search space.
-- TODO: implement some sort of pattern sampling exhaustion.
-- Each call seq gets its own entry in storage that saves all the hashes of all
-- the paths it has previously sampled. Any new samples are checked against this
-- to determine how exhausted each CallSeq+startFunc is, which could help us
-- choose less exhausted CallSeqs.
queryForBugMatch_
  :: Bool -- use solver to actually check, or let everything be SAT?
  -> CallDepth
  -> Word64 -- max samples
  -> CfgStore
  -> FuncMapping
  -> Maybe (HashSet Function) -- restrict start funcs
  -- TODO: taints
  -> [StubSpec]
  -> BugMatch
  -> IO ()
queryForBugMatch_ actuallySolve maxCallDepthLimit maxSamples store funcMapping restrictToStartFuncs stubs bm = do
  case toFullFunctionSequenceGraph funcMapping . getCallSequenceGraph $ bm ^. #pathPattern of
    Nothing -> error "Couldn't create call sequence graph"
    Just callSequenceGraph -> do
      getCallSeqPreps store restrictToStartFuncs callSequenceGraph >>= \case
        [] -> do
          -- TODO: pick random functions for starts and use ExploreDeep strategy
          putText "No functions found in BugMatch. See TODO."
          return ()
        (x:xs) -> do
          tryPrepsUntilExhausted HashSet.empty 0 $ x :| xs
  where
    solver = if actuallySolve
      then const . return $ Solver.Sat HashMap.empty
      else solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
    getMatch path = (path',) <$> M.matchPath solver [] (bm ^. #pathPattern) path'
      where
        path' = stubPath stubs path
    tryPrepsUntilExhausted prevSamples numSamples preps
      | numSamples >= maxSamples = return ()
      | otherwise = do
          prep <- pickFromList randomRIO preps
          let samplesPerPrep = 1 -- maybe should make this an arg
          _newPaths <- samplePathFromPrep maxCallDepthLimit samplesPerPrep store prep >>= \case
            Nothing -> return []
            Just (func, paths) -> do
              -- TODO: Filter out previously checked paths. Also, stop when the % paths
              -- that are repeats pass a certain level.
              -- let newPaths = filter (not . (`HashSet.member` prevSamples)) paths
              let newPaths = paths
              matches <- traverse getMatch newPaths
              forM_ matches $ \(path, (ms, mr)) -> do
                case mr of
                  M.NoMatch -> return ()
                  M.Match _stmts -> do
                    -- putText "--- These Bad ---"
                    -- prettyStmts' _stmts
                    -- putText "--- mm hum --"
                    -- pprint $ ms ^. #solutions
                    showPathsWithMatches (func ^. #name) [(path, [((ms, mr), bm)])]
              return newPaths
              -- write them to file
          -- let prevSamples' = prevSamples <> HashSet.fromList newPaths
          let prevSamples' = prevSamples
          tryPrepsUntilExhausted prevSamples' (numSamples + samplesPerPrep) preps
