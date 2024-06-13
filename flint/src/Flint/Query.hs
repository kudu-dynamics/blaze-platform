module Flint.Query
  ( module Flint.Query
  , module Flint.Types.Query
  ) where

import Flint.Prelude

import Flint.Analysis (showPathsWithMatches)
import qualified Flint.Analysis as FA
import Flint.Analysis.Path.Matcher (StmtPattern)
import Flint.Analysis.Path.Matcher.Stub (StubSpec, stubPath)
import qualified Flint.Analysis.Path.Matcher as M
import Flint.Cfg.Path (CallDepth, samplesFromQuery, pickFromList, sampleFromRouteIO)
import Flint.Types.Analysis (TaintPropagator)
import qualified Flint.Types.CachedCalc as CC
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Types.Query

import Blaze.Cfg.Interprocedural (getCallTargetFunction)
import Blaze.Cfg.Path (PilPath)
import Blaze.Graph (OuterNodeDescendants, RouteMakerCtx, Route, RouteAction)
import qualified Blaze.Graph as G
import Blaze.Import.CallGraph (CallGraphImporter)
import Blaze.Pretty (Tokenizable(tokenize), Tokenizer, Token, (<++>), tt, pretty')
import qualified Blaze.Pretty as P
import Blaze.Pil.Solver (solveStmtsWithZ3)
import Blaze.Types.Cfg (PilNode, PilCfg)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Graph (LEdge(LEdge), Edge(Edge))
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Path as Path
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Pil.Solver as Solver

import Data.List.NonEmpty ((<|))
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
      M.Neighbors pats -> foldM go (parents, g) pats
      M.Where pat _ -> go (parents, g) pat
      M.Necessarily pat _ -> go (parents, g) pat
      M.EndOfPath -> return (parents, g)

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
-- TODO: could fill this out to work with regex... or...
--       we'll probably deprecate queryForBugMatch_ one day, so just procrastinate
getFullFunction _m (M.FuncNameRegex _x) = Nothing

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

makeRoutesStartingFromFunc
  :: RouteMakerCtx Function PilNode
  -> Function
  -> PilNode
  -> [PilNode]
  -> [Route Function PilNode]
makeRoutesStartingFromFunc = G.makeRoutes 

-- | Returns all possible PilNodes that match each pattern in sequence.
-- The patterns are meant to match statements an isolated basic block.
-- Thus, it could match a single statement or something like Ordered
-- could be used to match multiple statements in the block.
-- Bound pattern variables are not saved/matched between StmtPatterns, but
-- can be used in a singled StmtPattern to match stmts in a single basic block.
-- `Where` patterns are currently ignored, but could be supported if needed.
matchNodesFulfillingSeq
  :: [TaintPropagator]
  -> HashSet PilNode
  -> [StmtPattern]
  -> [HashSet PilNode]
matchNodesFulfillingSeq tps allNodes = fmap getAllMatches
  where
    -- TODO: pre-compute simplified and pass in (HashSet (PilNode, [Stmt]))
    simplified :: [(PilNode, [Pil.Stmt])]
    simplified
      = fmap (\x -> (x, FA.simplify $ Cfg.getNodeData x))
      . HashSet.toList
      $ allNodes

    getAllMatches :: StmtPattern -> HashSet PilNode
    getAllMatches pat
      = HashSet.fromList
      . mapMaybe (matchesPat pat)
      $ simplified
    -- getAllMatches pat = HashSet.filter (matchesPat pat) allNodes
    
    matchesPat :: StmtPattern -> (PilNode, [Pil.Stmt]) -> Maybe PilNode
    matchesPat pat (node, nodeData) =
      case M.pureMatchStmts' tps [pat] nodeData of
        M.Match _ -> Just node
        M.NoMatch -> Nothing

getAllSeqCombos :: [HashSet a] -> [[a]]
getAllSeqCombos [] = []
getAllSeqCombos [s] = pure <$> HashSet.toList s
getAllSeqCombos (s:ss) = do
  x <- HashSet.toList s
  (x:) <$> getAllSeqCombos ss

getFuncsThatReachNodes
  :: Maybe (HashSet Function)
  -> OuterNodeDescendants Function PilNode
  -> HashSet PilNode
  -> HashSet Function
getFuncsThatReachNodes mLimitFuncs outerReach reqNodes
  = HashMap.keysSet
  . maybe (HashMap.filter (reqNodes `HashSet.isSubsetOf`))
          (\limitFuncs -> HashMap.filterWithKey
           (\func reaches -> reqNodes `HashSet.isSubsetOf` reaches
                    && HashSet.member func limitFuncs))
          mLimitFuncs
  $ outerReach

-- | Gets all routes starting from various start functions
-- TODO: make arg that restricts starting funcs
getAllRoutesForSeq
  :: RouteMakerCtx Function PilNode
  -> Maybe (HashSet Function)
  -> [PilNode]
  -> HashMap Function [Route Function PilNode]
getAllRoutesForSeq ctx mLimitStartFuncs reqNodes = HashMap.fromList $ do
  startFunc <- startFuncs
  let startNode
        = fromMaybe
          (error $ "Cannot find start node of " <> show startFunc <> " in RouteMakerCtx")
        . HashMap.lookup startFunc
        $ ctx ^. #getStartNode
      routes = makeRoutesStartingFromFunc ctx startFunc startNode reqNodes
  return (startFunc, routes)
  where
    startFuncs :: [Function]
    startFuncs = HashSet.toList
      . getFuncsThatReachNodes mLimitStartFuncs (ctx ^. #outerContextNodeDescendants)
      . HashSet.fromList
      $ reqNodes

getAllRoutesForAllSeqCombos
  :: RouteMakerCtx Function PilNode
  -> Maybe (HashSet Function)
  -> [HashSet PilNode]
  -> HashMap [PilNode] (HashMap Function [Route Function PilNode])
getAllRoutesForAllSeqCombos ctx mLimitStartFuncs combos = HashMap.fromList $ do
  reqSeq <- getAllSeqCombos combos
  let routeMap = getAllRoutesForSeq ctx mLimitStartFuncs reqSeq
  return (reqSeq, routeMap)

-- | Transforms the results of getAllRoutesForAllSeqCombos to a flattened list
-- of actual routes.
flattenRoutes
  :: HashMap [PilNode] (HashMap Function [Route Function PilNode])
  -> [(Function, Route Function PilNode)]
flattenRoutes = concatMap getRoutes . HashMap.elems
  where
    getRoutes = concatMap (\(func, routes) -> (func,) <$> routes) . HashMap.toList


-- filterReachingSeqCombos
--   :: RouteMakerCtx Function PilNode
--   -> Maybe (HashSet Function)
--   -> [HashSet PilNode]
--   -> HashMap [PilNode] (HashMap Function [Route Function PilNode])


newtype AllRoutes = AllRoutes (HashMap [PilNode] (HashMap Function [Route Function PilNode]))

newtype FlattenedRoutes = FlattenedRoutes [(Function, Route Function PilNode)]

newtype FlattenedRoute = FlattenedRoute (Function, Route Function PilNode)

newtype AllRoutesFromStartFuncs = AllRoutesFromStartFuncs (HashMap Function [Route Function PilNode])

newtype RouteList = RouteList [Route Function PilNode]

newtype RouteNode = RouteNode PilNode

newtype RouteFunc = RouteFunc Function

instance Tokenizable FlattenedRoutes where
  tokenize (FlattenedRoutes xs)
    = fmap (P.delimitedList [P.tt ""] [P.tt "\n\n"] [P.tt ""])
    . traverse (tokenize . FlattenedRoute)
    $ xs

instance Tokenizable FlattenedRoute where
  tokenize (FlattenedRoute (func, route)) =
    tokenize (RouteFunc func)
    <++> P.tt "\n"
    <++> (P.tokenizeAsNewlinedList
          . fmap wrapRouteAction
          $ route)

instance Tokenizable RouteFunc where
  tokenize (RouteFunc func) = pure [tt $ func ^. #name]

instance Tokenizable RouteList where
  tokenize (RouteList routes)
    = fmap (P.delimitedList [P.tt ""] [P.tt "\n\n"] [P.tt ""])
    . traverse (P.tokenizeAsNewlinedList . (wrapRouteAction <$>))
    $ routes

instance Tokenizable AllRoutesFromStartFuncs where
  tokenize (AllRoutesFromStartFuncs m)
    = fmap (P.delimitedList [P.tt "--------------\n"] [P.tt "\n\n"] [P.tt "\n"])
    . traverse f
    . HashMap.toList
    $ m
    where
      f :: (Function, [Route Function PilNode]) -> Tokenizer [Token]
      f (func, routes) = P.tt ("Starting at " <> show (func ^. #name) <> ": \n")
        <++> tokenize (RouteList routes)

instance Tokenizable RouteNode where
  tokenize (RouteNode pilNode) = case pilNode of
    Cfg.BasicBlock x -> pure $
      [ tt "BasicBlock: "
      , tt "@"
      ]
      <> P.paren [P.addressToken Nothing $ x ^. #start]
    Cfg.Call x -> pure $
      [ tt $ "CallNode: " <> (maybe "?" (view #name)
                              . getCallTargetFunction
                              $ x ^. #callDest
                             )
      , tt " @"
      ]
      <> P.paren [P.addressToken Nothing $ x ^. #start]
    Cfg.EnterFunc x -> pure [tt $ "EnterFunc: " <> x ^. #nextCtx . #func . #name]
    Cfg.LeaveFunc x -> pure [tt $ "LeaveFunc: " <> x ^. #prevCtx . #func . #name]
    Cfg.Grouping _ -> pure [tt "GroupingNode"] -- We plan to remove GroupingNode soon

wrapRouteAction :: G.RouteAction Function PilNode -> G.RouteAction RouteFunc RouteNode
wrapRouteAction (G.InnerNode n) = G.InnerNode $ RouteNode n
wrapRouteAction (G.EnterContext n func) = G.EnterContext (RouteNode n) (RouteFunc func)
wrapRouteAction (G.ExitContext func) = G.ExitContext (RouteFunc func)
wrapRouteAction G.Finished = G.Finished

instance Tokenizable AllRoutes where
  tokenize (AllRoutes m)
    = fmap (P.delimitedList [P.tt "\n\n"] [P.tt "\n\n"] [P.tt "\n"])
    . traverse f
    . HashMap.toList
    $ m
    where
      f :: ([PilNode], HashMap Function [Route Function PilNode]) -> Tokenizer [Token]
      f (reqSeq, mp) = P.tt "Routes for seq: "
        <++> tokenize (RouteNode <$> reqSeq)
        <++> tt "\n"
        <++> tokenize (AllRoutesFromStartFuncs mp)

getAllCfgs :: CfgStore -> IO (HashMap Function PilCfg)
getAllCfgs store = do
  cfgInfos <- fmap catHashMapMaybes . CC.getSnapshot $ store ^. #cfgCache
  return $ view #cfg <$> cfgInfos

data RouteStart func node = RouteStart
 { startFunc :: func
 , startNode :: Maybe node -- Nothing starts at root of func
 , route :: Route func node
 } deriving (Eq, Ord, Show, Generic)

reifyRouteAction
  :: (CallGraphImporter imp, GetFunction func', GetFunction func, GetNode node)
  => imp
  -> CfgStore
  -> func'
  -> RouteAction func node
  -> IO (RouteAction Function PilNode)
reifyRouteAction imp store currentFunc = \case
  G.InnerNode n -> do
    currentFunc' <- getFunction imp currentFunc
    G.InnerNode <$> getNode store currentFunc' n
  G.EnterContext n outerFunc -> do
    currentFunc' <- getFunction imp currentFunc
    G.EnterContext <$> getNode store currentFunc' n <*> getFunction imp outerFunc
  G.ExitContext outerFunc -> G.ExitContext <$> getFunction imp outerFunc
  G.Finished -> pure G.Finished

reifyRoute'
  :: (CallGraphImporter imp, GetFunction func, GetNode node)
  => imp
  -> CfgStore
  -> Route func node
  -> StateT (NonEmpty Function) IO (Route Function PilNode)
reifyRoute' imp store = \case
  [] -> pure []
  (p:ps) -> do
    p' <- case p of
      G.InnerNode n -> do
        currentFunc <- NE.head <$> get
        G.InnerNode <$> lift (getNode store currentFunc n)
      G.EnterContext n outerFunc -> do
        outerFunc' <- lift (getFunction imp outerFunc)
        currentFunc <- NE.head <$> get
        p' <- G.EnterContext <$> lift (getNode store currentFunc n) <*> pure outerFunc'
        modify (outerFunc' <|)
        return p'
      G.ExitContext outerFunc -> do
        s <- get
        case NE.uncons s of
          (_, Nothing) -> error "Exited too many contexts"
          (_, Just xs) -> put xs
        G.ExitContext <$> lift (getFunction imp outerFunc)
      G.Finished -> pure G.Finished
    (p':) <$> reifyRoute' imp store ps

reifyRoute
  :: (CallGraphImporter imp, GetFunction func', GetFunction func, GetNode node)
  => imp
  -> CfgStore
  -> func'
  -> Route func node
  -> IO (Route Function PilNode)
reifyRoute imp store currentFunc route = do
  currentFunc' <- getFunction imp currentFunc
  flip evalStateT (NE.singleton currentFunc') $ reifyRoute' imp store route
    
data SampleRoutePrep = SampleRoutePrep
  { funcCfgs :: HashMap Function PilCfg
  , funcDmaps :: HashMap Function (G.StrictDescendantsMap PilNode)
  } deriving (Eq, Ord, Show, Generic, Hashable)

getSampleRoutePrep :: CfgStore -> IO SampleRoutePrep
getSampleRoutePrep store = do
  cfgInfos <- fmap catHashMapMaybes . CC.getSnapshot $ store ^. #cfgCache
  let dmaps = view #strictDescendantsMap <$> cfgInfos
      cfgs = view #cfg <$> cfgInfos
  return $ SampleRoutePrep cfgs dmaps
  
sampleRoute
  :: (CallGraphImporter imp, GetFunction func, GetNode node)
  => imp
  -> CfgStore
  -> SampleRoutePrep
  -> func
  -> Route func node
  -> IO (Maybe PilPath)
sampleRoute imp store pprep startFunc route = do
  startFunc' <- getFunction imp startFunc
  rroute <- reifyRoute imp store startFunc' route
  -- TODO: should we make startNode be innerNode if that is first is route?
  startNode <- case HashMap.lookup startFunc' $ pprep ^. #funcCfgs of
    Nothing -> error $ "Could not find func " <> show startFunc' <> " in CfgStore"
    Just cfg -> return $ Cfg.getRootNode cfg
  P.prettyPrint' rroute
  sampleFromRouteIO (pprep ^. #funcCfgs) (pprep ^. #funcDmaps) startFunc' startNode rroute >>= \case
    Nothing -> return Nothing
    Just (p, ([], _)) -> return $ Just p
    Just (_p, (leftoverRoute, _)) -> error $ "Failed to finish route:\n" <> show (pretty' leftoverRoute)

{-# ANN sampleRoutes ("HLint: ignore Eta reduce" :: String) #-}
-- TODO: make a version that just takes Function and PilNode instead of func and PilNode, then make a wrapper that does the conversion.
sampleRoutes
  :: (CallGraphImporter imp, GetFunction func, GetNode node)
  => imp
  -> CfgStore
  -> SampleRoutePrep
  -> Word64           -- maxSamplesPerRoute
  -> [(func, Route func node)]
  -> IO [((func, Route func node), [PilPath])]
sampleRoutes imp store pprep maxSamplesPerRoute routes = mapConcurrently f routes
  where
    f flatRoute
      = fmap ((flatRoute,) . catMaybes)
      . mapConcurrently (uncurry $ sampleRoute imp store pprep)
      . replicate (fromIntegral maxSamplesPerRoute)
      $ flatRoute

flattenSampleRoutesResult
  :: [((func, Route func node), [PilPath])]
  -> [((func, Route func node), PilPath)]
flattenSampleRoutesResult = concatMap f
  where
    f :: ((func, Route func node), [PilPath]) -> [((func, Route func node), PilPath)]
    f (r, paths) = (r,) <$> paths

queryForBugMatchUsingRoutes
  :: (CallGraphImporter imp)
  => Bool -- use solver to actually check, or let everything be SAT?
  -> imp
  -> CfgStore
  -> RouteMakerCtx Function PilNode
  -> SampleRoutePrep
  -> HashSet PilNode                -- allNodes
  -> Maybe (HashSet Function)       -- restrict start funcs
  -> Word64                         -- maxSamplesPerRoute
  -> [TaintPropagator]
  -> [StubSpec]
  -> [StmtPattern]                   -- node seq to help route planner
  -> BugMatch
  -> IO ()
queryForBugMatchUsingRoutes actuallySolve imp store routeMakerCtx sampleRoutePrep allNodes mLimitStartFuncs maxSamplesPerRoute taints stubs routeSeq bugMatch = do
  let combos = matchNodesFulfillingSeq taints allNodes routeSeq
  putText $ "Matching combos: " <> show (HashSet.size <$> combos)
  let allRoutes = flattenRoutes
        $ getAllRoutesForAllSeqCombos routeMakerCtx mLimitStartFuncs combos
  putText $ "Found " <> show (length allRoutes) <> " routes."
  putText $ "Sampling " <> show maxSamplesPerRoute <> " per route."
  routePaths <- flattenSampleRoutesResult
    <$> sampleRoutes imp store sampleRoutePrep maxSamplesPerRoute allRoutes
  matches <- traverse getMatch routePaths
  forM_ matches $ \(func, path, (ms, mr)) -> do
    case mr of
      M.NoMatch -> return ()
      M.Match _stmts -> do
        -- putText "--- These Bad ---"
        -- prettyStmts' _stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        showPathsWithMatches (func ^. #name) [(path, [((ms, mr), bugMatch)])]
  where
    solver = if actuallySolve
      then const . return $ Solver.Sat HashMap.empty
      else solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
    getMatch ((func, _route), path)
      = (func, path',) <$> M.matchPath solver [] (bugMatch ^. #pathPattern) path'
      where
        path' = stubPath stubs path

-- | Takes un-simplified paths and prints out successful bug matches.
-- You probably want to use Flint.Cfg.Path.samplesFromQuery to get the paths.
-- TODO: use taints
checkPathsForBugMatch
  :: Bool -- use solver to actually check, or let everything be SAT?
  -> [TaintPropagator]
  -> [StubSpec]
  -> BugMatch
  -> [(Function, PilPath)]
  -> IO ()
checkPathsForBugMatch actuallySolve _taints stubs bugMatch fpaths = do
  matches <- traverse getMatch fpaths
  forM_ matches $ \(func, path, (ms, mr)) -> do
    case mr of
      M.NoMatch -> do
        putText "No match"
        -- let stmts = PA.aggressiveExpand . CfgPath.toStmts $ path

        -- putText "\n--- These Bad ---\n"
        -- P.prettyStmts' stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        return ()
      M.Match _stmts -> do
        -- putText "--- These Bad ---"
        -- prettyStmts' _stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        showPathsWithMatches (func ^. #name) [(path, [((ms, mr), bugMatch)])]
  where
    solver = case actuallySolve of
      False -> const . return $ Solver.Sat HashMap.empty
      True -> solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
    getMatch (func, path)
      = (func, path',) <$> M.matchPath solver [] (bugMatch ^. #pathPattern) path'
      where
        path' = stubPath stubs path
