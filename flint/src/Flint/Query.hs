module Flint.Query
  ( module Flint.Query
  , module Flint.Types.Query
  ) where

import Flint.Prelude

import qualified Flint.Analysis as FA
import Flint.Analysis.Path.Matcher (StmtPattern, StmtSolver, IsStatement, IsExpression, HasAddress, TypedStmt)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep, mkPathPrep, mkPathPrepWithTypeHints)
import Flint.Analysis.Path.Matcher.Stub (StubSpec, stubPath)
import qualified Flint.Analysis.Path.Matcher as M
import qualified Flint.Types.Analysis.Path.Matcher.Func as M
import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import Flint.Cfg.Path (CallDepth, samplesFromQuery, pickFromList, sampleFromRouteIO, getCallNodeFunc, sampleSinglePathWithVisitCounts)
import Blaze.Path (VisitCounts(..), emptyVisitCounts)
import Flint.Types.Analysis (TaintPropagator)
import Flint.Types.Analysis.Path.Matcher (Prim)
import qualified Flint.Types.CachedMap as CM
import Flint.Analysis.Path.Matcher.Primitives (mkCallableWMI, getInitialWMIs, squashCallableWMIs)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, MkCallableWMIError, KnownFunc, PrimSpec)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Types.CachedCalc as CC
import Flint.Types.Cfg.Store (CfgStore)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query
import Flint.Analysis.References hiding (length)

import Blaze.Cfg.Interprocedural (getCallTargetFunction)
import Blaze.Cfg.Path (PilPath)
import Blaze.Graph (OuterNodeDescendants, RouteMakerCtx, Route, RouteAction)
import qualified Blaze.Graph as G
import Blaze.Import.CallGraph (CallGraphImporter)
import Blaze.Pil.Summary (CodeSummary)
import qualified Blaze.Pil.Summary as Summary
import Blaze.Pretty (Tokenizable(tokenize), Tokenizer, Token, (<++>), tt, pretty')
import qualified Blaze.Pretty as P
import qualified Blaze.Pil.Construct as C
import Blaze.Pil.Solver (solveStmtsWithZ3)
import Blaze.Types.Cfg (PilNode, PilCfg)
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Function (Function, Func)
import Blaze.Types.Graph (LEdge(LEdge), Edge(Edge))
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Path as Path
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Pil.Solver as Solver
import Blaze.Types.Import (TypeHints)

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (nub)
import qualified Data.Text as Text
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

      -- TODO: This used to be for `AnyOne pats`. I don't remember what this does,
      --       so to get it to compile, I just put pat1 and pat2 in a list
      M.Or pat1 pat2 -> foldM f ([], g) [pat1, pat2]
        where
          f (allEndNodes, g') pat = do
            (endNodes, g'') <- go (parents, g') pat
            return (endNodes <> allEndNodes, g'')

        -- Below is the correct way, but could get expensive if len(pats) > 4.
        -- Currently, the path sampler that uses the CallSequenceGraph doesn't
        -- look at function call ordering, so for now we can just treat unordered
        -- like an ordered.
        -- go (parents, g) . M.orr . fmap M.Ordered $ permutations pats
 
      M.And pat1 pat2 -> foldM go (parents, g) [pat1, pat2]
      M.Where pat _ -> go (parents, g) pat
      M.Necessarily pat _ -> go (parents, g) pat
      M.EndOfPath -> return (parents, g)
      M.Location _ pat -> go (parents, g) pat
      M.SubPrimitive _ _ -> return (parents, g)
      M.CallsPrimitive _ _ -> return (parents, g)
      M.Star -> return (parents, g)
      M.Good -> return (parents, g)
      M.Bad -> return (parents, g)

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
getFullFunction _m (M.FuncNames _x) = Nothing

toFullFunctionSequenceGraph
  :: FuncMapping
  -> CallSequenceGraph M.Func
  -> Maybe (CallSequenceGraph Function)
toFullFunctionSequenceGraph m = traverse . traverse $ getFullFunction m

-- | Gets all single paths through the CallSequenceGraph, as well as information
-- that will is useful for sampling Cfg paths along the call seq path.
-- The optional second argument is a set that restricts the possible start functions
-- TODO: break this out into some pure functions and add tests
getCallSeqPreps
  :: CfgStore
  -> Maybe (HashSet Function)
  -> CallSequenceGraph Function
  -> IO [CallSeqPrep]
getCallSeqPreps store restrictStartFuncs g = do
  let starts = HashSet.toList $ G.sources g
  fmap concat . forConcurrently starts $ \start -> do
    let (callSeqs :: [NonEmpty Function]) = (view #function <$>) <$> (Path.toNodeList <$> (Path.getAllPaths (const identity) 1 start g :: [AlgaPath () Int (FuncNode Function)]) :: [NonEmpty (FuncNode Function)])
    forConcurrently callSeqs $ \callSeq -> do
      let firstCall_ = NE.head callSeq
          lastCall_ = NE.last callSeq
          callSet_ = HashSet.fromList . NE.toList $ callSeq
      callersOfFirstCall <-
        CfgStore.getAncestors' store firstCall_ >>= \case
          Nothing -> return ([] :: [Function])
          Just s -> return
            . maybe identity (\sfuncs -> filter (`HashSet.member` sfuncs)) restrictStartFuncs
            $ HashSet.toList s
      -- Trying to limit unnecessary STDOUT output. Having a debug flag to
      -- enable diagnostic output would be preferable. - Hazmat
      -- putText $ "Callers of first call: " <> show (length callersOfFirstCall)
      (reachList :: [Maybe Function]) <- forConcurrently callersOfFirstCall $ \func -> do
        CfgStore.getDescendants' store (func :: Function) >>= \case
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
      let q = QueryCallSeq $ QueryCallSeqOpts
              { callSeqPrep = prep
              , numSamples = numSamples
              , callExpandDepthLimit = callDepth
              }
      paths <- samplesFromQuery store luckyStart q
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
  -> Maybe (HashSet Function)
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
          warn "No functions found in BugMatch. See TODO."
          return ()
        (x:xs) -> do
          tryPrepsUntilExhausted HashSet.empty 0 $ x :| xs
  where
    solver = if actuallySolve
      then solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
      else const . return $ Solver.Sat HashMap.empty
    -- TODO: make this use regular `match` that can return multiple results
    getMatch path = (path',) <$> M.singleMatch solver (bm ^. #pathPattern) (mkPathPrep [] path')
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
              forM_ matches $ \(path, mr) -> do
                case mr of
                  Nothing -> return ()
                  Just r -> do
                    -- putText "--- These Bad ---"
                    -- prettyStmts' _stmts
                    -- putText "--- mm hum --"
                    -- pprint $ ms ^. #solutions
                    FA.showPathsWithMatches (func ^. #name) [(path, [(r, bm)])]
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
      case M.singlePureMatch [pat] (mkPathPrep tps nodeData :: PathPrep Pil.Stmt) of
        Just _ -> Just node
        Nothing -> Nothing

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
    . filter (not . null . snd)
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
  debug $ "Matching combos: " <> show (HashSet.size <$> combos)
  let allRoutes = flattenRoutes
        $ getAllRoutesForAllSeqCombos routeMakerCtx mLimitStartFuncs combos
  info $ "Found " <> show (length allRoutes) <> " routes."
  info $ "Sampling " <> show maxSamplesPerRoute <> " per route."
  routePaths <- flattenSampleRoutesResult
    <$> sampleRoutes imp store sampleRoutePrep maxSamplesPerRoute allRoutes
  matches <- traverse getMatch routePaths
  forM_ matches $ \(func, path, mr) -> do
    case mr of
      Just r -> do
        -- putText "--- These Bad ---"
        -- prettyStmts' _stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        FA.showPathsWithMatches (func ^. #name) [(path, [(r, bugMatch)])]
      Nothing -> return ()
  where
    solver = if actuallySolve
      then solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
      else const . return $ Solver.Sat HashMap.empty
    getMatch ((func, _route), path)
      = (func, path',) <$> M.singleMatch solver (bugMatch ^. #pathPattern) (mkPathPrep taints path')
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
checkPathsForBugMatch actuallySolve tps stubs bugMatch fpaths = do
  matches <- traverse getMatch fpaths
  forM_ matches $ \(func, path, mr) -> do
    case mr of
      Nothing -> do
        -- let stmts = PA.aggressiveExpand . CfgPath.toStmts $ path

        -- putText "\n--- These Bad ---\n"
        -- P.prettyStmts' stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        return ()
      Just r -> do
        -- putText "--- These Bad ---"
        -- prettyStmts' _stmts
        -- putText "--- mm hum --"
        -- pprint $ ms ^. #solutions
        FA.showPathsWithMatches (func ^. #name) [(path, [(r, bugMatch)])]
  where
    solver = case actuallySolve of
      False -> const . return $ Solver.Sat HashMap.empty
      True -> solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
    getMatch (func, path)
      = (func, path',) <$> M.singleMatch solver (bugMatch ^. #pathPattern) (mkPathPrep tps path')
      where
        path' = stubPath stubs path

-- | Convenient function to query multiple functions and check for a bug match
checkFuncs
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [BugMatch]             -- checks all on each path
  -> (MatchingResult -> IO ()) -- stream out matches per function
  -> HashSet Function       -- start funcs
  -> IO ()
checkFuncs actuallySolve store q bugMatches streamResults funcs = do
  mapConcurrently_ (checkFunc actuallySolve store q bugMatches streamResults) . HashSet.toList $ funcs
  
checkFunc
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [BugMatch]             -- checks all on each path
  -> (MatchingResult -> IO ()) -- stream out matches per function
  -> Function               -- start func
  -> IO ()
checkFunc actuallySolve store q bugMatches streamResults startFunc = flip catch reportError $ do
  paths <- samplesFromQuery store startFunc q
  -- putText $ "Got Some paths: " <> show (length paths)
  forConcurrently_ paths $ \path -> do
    forConcurrently_ bugMatches $ \bugMatch -> do
      M.singleMatch solver (bugMatch ^. #pathPattern) (mkPathPrep [] path) >>= \case
        Nothing -> return ()
        Just (ms, stmtsWithAssertions) -> do
          streamResults $ MatchingResult
            { func = startFunc
            , path = path
            -- TODO: do we want to include a bug match's SMT assertions in result path?
            , pathAsStmts = stmtsWithAssertions
            , bugName = bugMatch ^. #bugName
            , bugDescription = resolveText $ bugMatch ^. #bugDescription
            , mitigationAdvice = resolveText $ bugMatch ^. #mitigationAdvice
            }
            where
              resolveText = M.resolveBoundText (ms ^. #boundSyms)
  where
    solver = if actuallySolve
      then solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
      else const . return $ Solver.Sat HashMap.empty
    reportError :: SomeException -> IO ()
    reportError e = do
      let msg = "\n---------------------------\n"
                <> "Error when checking function: "
                <> show (startFunc ^. #name)
                <> "\n"
                <> show e
      warn msg

checkKernelLifecycle
  :: Bool
  -> CfgStore
  -> Word64
  -> Word64
  -> (MatchingResult -> IO ())
  -> IO ()
checkKernelLifecycle actuallyUseSolver store maxSamplesPerFunc expandCallDepth streamResults = do
  funcs <- CfgStore.getInternalFuncs store
  case (findFuncByName funcs "init_module", findFuncByName funcs "cleanup_module") of
    (Just initFunc, Just cleanupFunc) -> do
      uuidInit <- randomIO
      uuidClean <- randomIO
      uuidBbEnd <- randomIO
      let lifecycleFunc = Func.Function Nothing "_kernel_module_lifecycle" (intToAddr 0) []
          lifeCtx = Pil.Ctx lifecycleFunc 0
          initDest = Pil.CallFunc initFunc
          cleanDest = Pil.CallFunc cleanupFunc
          callNodeInit
            = Cfg.Call $ Cfg.CallNode
              lifeCtx
              (intToAddr 0)
              initDest
              uuidInit
              [ C.defCall "r1" initDest [] 8 ]

          callNodeCleanup
            = Cfg.Call $ Cfg.CallNode
              lifeCtx
              (intToAddr 8)
              cleanDest
              uuidClean
              [ C.defCall "r1" cleanDest [] 8 ]

          bbEnd = Cfg.BasicBlock $ Cfg.BasicBlockNode
                  lifeCtx
                  (intToAddr 0x10)
                  (intToAddr 0x12)
                  uuidBbEnd
                  [ C.ret $ C.var "r1" 8 ]
                  
          lifeCfg = Cfg.mkCfg
            0
            callNodeInit
            [ callNodeInit, callNodeCleanup, bbEnd]
            [ Cfg.CfEdge callNodeInit callNodeCleanup Cfg.UnconditionalBranch
            , Cfg.CfEdge callNodeCleanup bbEnd Cfg.UnconditionalBranch
            ]
          lifeCfgInfo = CfgStore.calcCfgInfo lifeCfg
      CC.setCalc lifecycleFunc (store ^. #cfgCache) . return $ Just lifeCfgInfo
      
      let q :: Query Function
          q = QueryExpandAll $ QueryExpandAllOpts
              { callExpandDepthLimit = expandCallDepth + 1
              -- TODO: At some point, we should base the # samples on the size of func
              , numSamples = maxSamplesPerFunc
              }
          bms :: [BugMatch]
          bms = Pat.kernelModulePatterns

      checkFunc
        actuallyUseSolver
        store
        q
        bms
        streamResults
        lifecycleFunc

      return ()
      
    _ -> do
      putText "Failed to find both `init_module` and `cleanup_module` for kernel"
      return ()
  where
    findFuncByName funcs t = headMay . filter (\func -> func ^. #name == t) $ funcs  

checkKernelLifecycleForPrims'
  :: Bool
  -> CfgStore
  -> Word64
  -> Word64
  -> IO [MatchingPrim]
checkKernelLifecycleForPrims' actuallyUseSolver store maxSamplesPerFunc expandCallDepth = do
  funcs <- CfgStore.getInternalFuncs store
  case ( findFuncByName funcs $ HashSet.fromList ["_init_module", "init_module", "__pfx_init_module"]
       , findFuncByName funcs $ HashSet.fromList ["_cleanup_module", "cleanup_module", "__pfx_cleanup_module"]) of
    (Just initFunc, Just cleanupFunc) -> do
      uuidInit <- randomIO
      uuidClean <- randomIO
      uuidBbEnd <- randomIO
      let lifecycleFunc = Func.Function Nothing "_kernel_module_lifecycle" (intToAddr 0) []
          lifeCtx = Pil.Ctx lifecycleFunc 0
          initDest = Pil.CallFunc initFunc
          cleanDest = Pil.CallFunc cleanupFunc
          callNodeInit
            = Cfg.Call $ Cfg.CallNode
              lifeCtx
              (intToAddr 0)
              initDest
              uuidInit
              [ C.defCall "r1" initDest [] 8 ]

          callNodeCleanup
            = Cfg.Call $ Cfg.CallNode
              lifeCtx
              (intToAddr 8)
              cleanDest
              uuidClean
              [ C.defCall "r1" cleanDest [] 8 ]

          bbEnd = Cfg.BasicBlock $ Cfg.BasicBlockNode
                  lifeCtx
                  (intToAddr 0x10)
                  (intToAddr 0x12)
                  uuidBbEnd
                  [ C.ret $ C.var "r1" 8 ]
                  
          lifeCfg = Cfg.mkCfg
            0
            callNodeInit
            [ callNodeInit, callNodeCleanup, bbEnd]
            [ Cfg.CfEdge callNodeInit callNodeCleanup Cfg.UnconditionalBranch
            , Cfg.CfEdge callNodeCleanup bbEnd Cfg.UnconditionalBranch
            ]
          lifeCfgInfo = CfgStore.calcCfgInfo lifeCfg
      CC.setCalc lifecycleFunc (store ^. #cfgCache) . return $ Just lifeCfgInfo

      let q :: Query Function
          q = QueryExpandAll $ QueryExpandAllOpts
              { callExpandDepthLimit = expandCallDepth + 1
              -- TODO: At some point, we should base the # samples on the size of func
              , numSamples = maxSamplesPerFunc
              }
          prims :: [Prim]
          --- TODO: add these kernel prims back in
          prims = PrimLib.kernelModulePrims

      checkFuncForPrims'
        actuallyUseSolver
        store
        q
        prims
        lifecycleFunc
      
    _ ->
      -- TODO: WARN
      return []
  where
    findFuncByName :: [Function] -> HashSet Text -> Maybe Function
    findFuncByName funcs s = headMay . filter (\func -> HashSet.member (func ^. #name) s) $ funcs  

-- checkPathForPrim_
--   :: Monad m
--   => M.MatcherCtx Pil.Stmt m
--   -> M.MatcherState Pil.Expression Pil.Stmt
--   -> Function
--   -> CodeSummary
--   -> Prim
--   -> m (Maybe (Either MkCallableWMIError CallableWMI))
-- checkPathForPrim_ mctx mstate func codeSummary prim = do
--   M.singleMatch_  mctx mstate (prim ^. #stmtPattern) >>= \case
--     Nothing -> return Nothing
--     Just (ms, stmtsWithAssertions, _) -> do
--       return . Just $ mkCallableWMI
--         (Func.Internal func)
--         codeSummary
--         (prim ^. #primType)
--         (ms ^. #boundSyms)
--         (ms ^. #locations)
--         stmtsWithAssertions

checkPathForPrim_
  :: forall m expr stmt.
     ( Monad m
     , HasAddress stmt
     , IsStatement expr stmt
     , IsExpression expr
     )
  => Word64
  -> M.MatcherCtx stmt m
  -> M.MatcherState expr stmt
  -> Function
  -> CodeSummary
  -> Prim
  -> m [CallableWMI]
checkPathForPrim_ maxResultsPerPath mctx mstate func codeSummary prim = do
  mapMaybe f <$> M.match_ (fromIntegral maxResultsPerPath) mctx mstate (prim ^. #stmtPattern)
    where
      f :: (M.MatcherState expr stmt, [stmt])
        -> Maybe CallableWMI
      f (ms, stmtsWithAssertions) =
        either (error . cs . pshow) Just $ mkCallableWMI
          (Func.Internal func)
          codeSummary
          (prim ^. #primType)
          boundSyms'
          (ms ^. #locations)
          (M.asStmts stmtsWithAssertions)
        where
          boundSyms' = HashMap.fromList
            . fmap (bimap coerce M.asExpression)
            . HashMap.toList
            $ ms ^. #boundSyms


checkPathForPrim
  :: StmtSolver Pil.Stmt IO
  -> Function
  -> PathPrep Pil.Stmt
  -> CodeSummary
  -> Prim
  -> IO (Maybe (Either MkCallableWMIError CallableWMI))
checkPathForPrim solver func prep codeSummary prim = do
  when (func ^. #name == "int_inc_ioctl") $ do
      -- sequentialPutText . ("\n----\n" <>) . pretty' . P.PStmts . filter ((== 0x100130) . view #addr) $ prep ^. #stmts
      sequentialPutText . cs . pshow . filter ((== intToAddr 0x100263) . view #addr) $ prep ^. #untouchedStmts
      sequentialPutText . ("\n----\n" <>) . pretty' . P.PStmts $ prep ^. #untouchedStmts

  M.singleMatch solver (prim ^. #stmtPattern) prep >>= \case
    Nothing -> return Nothing
    Just (ms, stmtsWithAssertions) -> do
      -- putText "MAAAAATCHED!!!!"
      return . Just $ mkCallableWMI
        (Func.Internal func)
        codeSummary
        (prim ^. #primType)
        (ms ^. #boundSyms)
        (ms ^. #locations)
        stmtsWithAssertions

checkFuncForPrims
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [Prim]             -- checks all on each path
  -> (MatchingPrim -> IO ()) -- stream out matches per function
  -> Function               -- start func
  -> IO ()
checkFuncForPrims actuallySolve store q prims streamResults func = flip catch reportError $ do
  paths <- samplesFromQuery store func q
  debug $ "Got Some paths: " <> show (length paths)
  forConcurrently_ paths $ \path -> do
    let pathPrep = mkPathPrep [] path
        codeSum = Summary.fromStmts $ pathPrep ^. #stmts
    forConcurrently_ prims $ \prim -> do
      checkPathForPrim solver func pathPrep codeSum prim >>= \case
        Nothing -> return ()
        Just (Left cprimError) -> do
          -- TODO: make this log a warning properly
          warn $ "Error constructing callable Primitive:\n" <> show cprimError
          return ()
        Just (Right cprim) -> do
          let matchingPrim = MatchingPrim
                { func = func
                , callablePrim = cprim
                , path = pathPrep ^. #stmts
                }
          streamResults matchingPrim
  where
    solver = if actuallySolve
      then solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
      else const . return $ Solver.Sat HashMap.empty
    reportError :: SomeException -> IO ()
    reportError e = do
      let msg = "\n---------------------------\n"
                <> "Error when checking function: "
                <> show (func ^. #name)
                <> "\n"
                <> show e
      warn msg

checkFuncForPrims'
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [Prim]             -- checks all on each path
  -> Function               -- start func
  -> IO [MatchingPrim]
checkFuncForPrims' actuallySolve store q prims func = do
  paths <- nub <$> samplesFromQuery store func q
  -- putText $ "Got Some paths: " <> show (length paths)
  fmap concat . forConcurrently paths $ \path -> do
    let pathPrep = mkPathPrep [] path
        codeSum = Summary.fromStmts $ pathPrep ^. #stmts
    fmap catMaybes . forConcurrently prims $ \prim -> do
      checkPathForPrim solver func pathPrep codeSum prim >>= \case
        Nothing -> return Nothing
        Just (Left cprimError) -> do
          -- TODO: make this log a warning properly
          warn $ "Error constructing callable Primitive:\n" <> show cprimError
          return Nothing
        Just (Right cprim) -> do
          let matchingPrim = MatchingPrim
                { func = func
                , callablePrim = cprim
                , path = pathPrep ^. #stmts
                }
          return $ Just matchingPrim
  where
    solver = if actuallySolve
      then solveStmtsWithZ3 Solver.AbortOnError -- Solver.IgnoreErrors
      else const . return $ Solver.Sat HashMap.empty

-- | Convenient function to query multiple functions and check for callable primitives
checkFuncsForPrims
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [Prim]             -- checks all on each path
  -> (MatchingPrim -> IO ()) -- stream out matches per function
  -> HashSet Function
  -> IO ()
checkFuncsForPrims actuallySolve store q prims streamResults funcs = do
  mapConcurrently_ (checkFuncForPrims actuallySolve store q prims streamResults) . HashSet.toList $ funcs
  debug "Finished checkFuncsForPrims"

-- | Convenient function to query multiple functions and check for callable primitives
checkFuncsForPrims'
  :: Bool                   -- actually use SMT solver?
  -> CfgStore
  -> Query Function         -- method to get the paths from each func
  -> [Prim]             -- checks all on each path
  -> HashSet Function
  -> IO [MatchingPrim]
checkFuncsForPrims' actuallySolve store q prims funcs = do
  r <- mapConcurrently (checkFuncForPrims' actuallySolve store q prims)
    . HashSet.toList
    $ funcs
  debug "Finished checkFuncsForPrims'"
  return $ concat r

-- | This is a flat sample that doesn't expand calls.
-- The number of samples is based on the complexity of the func
-- For now, we use a simple approach:
-- num samples = num nodes ** exponator
-- TODO: base it on something smarter like # of conditional branches
onionSampleBasedOnFuncSize
  :: Double
  -> CfgStore
  -> Function
  -> IO (Maybe [PilPath])
onionSampleBasedOnFuncSize multiplier store func = CfgStore.getFuncCfgInfo store func >>= \case
  Nothing -> return Nothing
  Just cfgInfo -> do
    let numSamples = max 1 . floor $ fromIntegral (HashSet.size $ cfgInfo ^. #nodes) * multiplier
        q = QueryExpandAll $ QueryExpandAllOpts
            { callExpandDepthLimit = 0
            , numSamples = numSamples
            }
    paths <- nub <$> samplesFromQuery store func q
    return $ Just paths

-- | Looks for prims with the current set of CallablePrims
-- and if it matches one, returns it
matchAndReturnCallablePrim
  :: ( IsStatement expr stmt
     , IsExpression expr
     , HasAddress stmt
     )
  => Word64
  -> StmtSolver stmt IO
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> Function
  -> PathPrep stmt
  -> Prim
  -> IO [CallableWMI]
matchAndReturnCallablePrim maxResultsPerPath solver callablePrimSnapshot func pprep prim = do
  -- when (func ^. #name == "int_inc_ioctl") $ do
  --   let solver' = const . return $ Solver.Sat HashMap.empty
  --   r <- checkPathForPrim solver' func pprep (pprep ^. #codeSummary) prim
  --   case r of
  --     Just (Right _) -> putText "Match city"
  --     Just (Left _) -> putText "match error"
  --     Nothing -> putText "no match :("
  
  let (mctx, mstate) = M.mkMatcherState solver pprep
      mstate' = mstate & #callablePrimitives .~ callablePrimSnapshot
      
  checkPathForPrim_ maxResultsPerPath mctx mstate' func (pprep ^. #codeSummary) prim

-- | Checks path for prim and updates the callable primitives if the path
-- is an instance of that callable primitive
{-# SCC onionCheckPathForPrim #-}
onionCheckPathForPrim
  :: Word64
  -> StmtSolver TypedStmt IO
  -> CfgStore
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> Function
  -> PathPrep TypedStmt
  -> Prim
  -> IO ()
onionCheckPathForPrim maxResultsPerPath solver store callablePrimSnapshot func pprep prim = do
  -- putText $ func ^. #name <> ": " <> prim ^. #primType . #name
  -- | Because so many things can go wrong with patterns, this little section allows
  -- you to choose a func and primitive to print out debug info for.
  let debuggingOn = False
      debugFuncName = "menu"
      debugPrimName = "ControlledIndirectCall"
      debugMode = debuggingOn
        && func ^. #name == debugFuncName
        && prim ^. #primType . #name == debugPrimName
  -- let pats = [M.Stmt $ M.Def M.Wild M.Wild] -- [M.Stmt $ M.Constraint M.Wild]
  -- when ((func ^. #name) == "main") $ do
  --   info . show $ matchInvPats_ pats pprep
  when debugMode $ do
    -- m <- fmap M.asOldCallableWMIsMap . CM.getSnapshot $ store ^. #callablePrims
    -- info . cs . pshow . HashMap.lookup PrimSpec.freeHeapSpec $ m
    -- info . cs . pshow $ m
    -- putText $ prim ^. #primType . #name
    info $ Text.concat
      [ ("\n---\n" <>) . pretty' . P.PStmts . M.asStmts $ pprep ^. #untouchedStmts
       -- writeAsJSON "/tmp/untouched_path.json" $ pprep ^. #untouchedStmts
      , (<> "\n\n") . ("\n+++\n" <>) . pretty' . P.PStmts . M.asStmts $ pprep ^. #stmts
      -- , (<> "\n\n||\n") . cs . pshow $ take 2 (drop (length (pprep ^. #stmts) - 2) $ pprep ^. #stmts)
      ]
    -- info . cs . pshow $ take 2 (drop (length (pprep ^. #stmts) - 2) . M.asStmts $ pprep ^. #stmts)

  -- when debugMode $ do
  --   checkPathForPrim solver func pprep (pprep ^. #codeSummary) prim >>= \case
  --     Nothing -> debug "|| NO MATCH ||"
  --     Just (Left _) -> debug "|| err ||"
  --     Just (Right _) -> debug "|| MATCH!!! ||"
  matchAndReturnCallablePrim maxResultsPerPath solver callablePrimSnapshot func pprep prim >>= \case
    [] -> do
      when debugMode $ do
        debug "Found nothing"
    cprims -> do
      when debugMode $ do
        debug $ "MATCHED " <> show (length cprims) <> " prims"
      CM.modify_ (HashSet.union (HashSet.fromList cprims)) (prim ^. #primType, Func.Internal func) $ store ^. #callablePrims


-- onionCheckPathForPrim
--   :: StmtSolver Pil.Stmt IO
--   -> CfgStore
--   -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
--   -> Function
--   -> PathPrep Pil.Stmt
--   -> Prim
--   -> IO ()
-- onionCheckPathForPrim solver store callablePrimSnapshot func pprep prim = do
--   -- putText $ func ^. #name <> ": " <> prim ^. #primType . #name
--   -- | Because so many things can go wrong with patterns, this little section allows
--   -- you to choose a func and primitive to print out debug info for.
--   let debuggingOn = False
--       debugFuncName = "clearHistory"
--       debugPrimName = "freeHeap"
--       debugMode = debuggingOn
--         && func ^. #name == debugFuncName
--         && prim ^. #primType . #name == debugPrimName
--   when debugMode $ do
--     debug $ prim ^. #primType . #name
--     -- putText . ("\n---\n" <>) . pretty' . P.PStmts $ pprep ^. #untouchedStmts
--     -- writeAsJSON "/tmp/untouched_path.json" $ pprep ^. #untouchedStmts
--     debug . ("\n+++\n" <>) . pretty' . P.PStmts $ pprep ^. #stmts
--     -- pprint $ take 2 (pprep ^. #untouchedStmts)

--   when debugMode $ do
--     checkPathForPrim solver func pprep (pprep ^. #codeSummary) prim >>= \case
--       Nothing -> debug "|| NO MATCH ||"
--       Just (Left _) -> debug "|| err ||"
--       Just (Right _) -> debug "|| MATCH!!! ||"
--   matchAndReturnCallablePrim solver callablePrimSnapshot func pprep prim >>= \case
--     Nothing -> do
--       when debugMode $ do
--         debug "Found nothing"
--     Just (Left cprimError) -> do
--       -- TODO: make this log a warning properly
--       warn $ "WARNING: Error constructing callable Primitive:\n" <> show cprimError
--     Just (Right cprim) -> do
--       when debugMode $ do
--         debug "MATCH!"
--       CM.modify_ (HashSet.insert cprim) (prim ^. #primType, cprim ^. #func) $ store ^. #callablePrims


-- | Samples paths on-demand for function, and checks them for each prim.
-- Adds instances found of CallableWMIs back into CfgStore
onionCheckFunc
  :: Word64
  -> StmtSolver TypedStmt IO
  -> CfgStore
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> [Prim]
  -> Double
  -> HashMap Function TypeHints
  -> Function
  -> IO ()
onionCheckFunc maxResultsPerPath solver store callablePrimSnapshot prims pathSamplingFactor funcToTypeHintsMap func = do
  mPaths <- onionSampleBasedOnFuncSize pathSamplingFactor store func
  let paths = fromMaybe [] mPaths
      typeHints = fromMaybe HashMap.empty (HashMap.lookup func funcToTypeHintsMap)
      pathPreps = mkPathPrepWithTypeHints typeHints [] <$> paths
      pathPrimCombos = (,) <$> pathPreps <*> prims
  forConcurrently_ pathPrimCombos $
    uncurry (onionCheckPathForPrim maxResultsPerPath solver store callablePrimSnapshot func)


-- | Does a single pass over all the funcs.
-- Adds instances found of CallableWMIs back into CfgStore
onionSinglePass
  :: Word64
  -> StmtSolver TypedStmt IO
  -> CfgStore
  -> [Prim]
  -> [Function]
  -> Bool
  -> Double
  -> HashMap Function TypeHints
  -> IO ()
onionSinglePass maxResultsPerPath solver store prims funcs doSquash pathSamplingFactor funcToTypeHintsMap = do
  -- Get a fresh snapshot every time
  debug "Getting callable primitives snapshot"
  cprimsSnapshot <- CM.getSnapshot (store ^. #callablePrims)
  cprimsToUse <- if doSquash
    then do
      let squashedSnapshot = fmap squashCallableWMIs cprimsSnapshot
      debug "Squashing duplicate CallableWMIs"
      CM.putSnapshot squashedSnapshot (store ^. #callablePrims)
      return squashedSnapshot
    else return cprimsSnapshot
  forM_ (zip [1::Int ..] funcs) $ \(idx, func) -> do
    debug $ "Checking function " <> show idx <> "/" <> show (length funcs) <> ": " <> show (func ^. #name)
    onionCheckFunc maxResultsPerPath solver store cprimsToUse prims pathSamplingFactor funcToTypeHintsMap func

-- onionSinglePass
--   :: StmtSolver Pil.Stmt IO
--   -> CfgStore
--   -> [Prim]
--   -> [Function]
--   -> IO ()
-- onionSinglePass solver store prims funcs = do
--   -- Get a fresh snapshot every time
--   cprimsSnapshot <- CM.getSnapshot (store ^. #callablePrims)
--   mapM_ (onionCheckFunc solver store cprimsSnapshot prims) funcs


chooseSolver
  :: IsStatement expr stmt
  => Bool -- actually solve?
  -> StmtSolver stmt IO
chooseSolver True = solveStmtsWithZ3 Solver.AbortOnError . M.asStmts
chooseSolver False = const . return $ Solver.Sat HashMap.empty
-- Tried to make one that uses the types in the TypedStmt, but we're missing
-- a hashmap of PilVar -> DeepSymType.
-- chooseSolver True = either (const $ pure Solver.Unk) (return . view #result)
--   <=< solveTypedStmtsWith z3 Solver.IgnoreErrors . zip [0..]


onionFlow
  :: Word64
  -> Bool               -- actually use SMT solver?
  -> Word64             -- max times to iterate checking whole binary
  -> Double             -- path sampling factor
  -> CfgStore
  -> [KnownFunc]
  -> [Prim]             -- checks all on each path
  -> HashMap Function TypeHints
  -> Bool
  -> [ReferenceKind]
  -> HashSet Text       -- attack surface function names (empty = all funcs)
  -> Word64             -- attack surface BFS depth
  -> IO ()              -- it writes results into CfgStore and hopefully DB
onionFlow maxResultsPerPath actuallyUseSolver maxIterations pathSamplingFactor store stdLibPrims prims funcToTypeHintsMap doSquash refKinds attackSurface attackSurfaceDepth = do
  _ <- return refKinds -- suppressing warning
  allFuncs <- CfgStore.getFuncs store
  internalFuncs <- CfgStore.getInternalFuncs store
  funcs <- if HashSet.null attackSurface
    then return internalFuncs
    else computeAttackSurfaceWorkingSet store internalFuncs attackSurface attackSurfaceDepth
  debug $ "Onion working set: " <> show (length funcs) <> " functions"
  let initialPrims = getInitialWMIs stdLibPrims allFuncs
  CM.putSnapshot initialPrims $ store ^. #callablePrims
  replicateM_ (fromIntegral maxIterations) $ onionSinglePass maxResultsPerPath solver store prims funcs doSquash pathSamplingFactor funcToTypeHintsMap
  when doSquash $ do
    debug "Squashing the rest of the duplicate CallableWMIs"
    snapshot <- CM.getSnapshot (store ^. #callablePrims)
    squashed <- fmap HashMap.fromList . mapConcurrently (\(k, v) -> pure (k, squashCallableWMIs v)) $ HashMap.toList snapshot
    CM.putSnapshot squashed $ store ^. #callablePrims
  where
    solver = chooseSolver actuallyUseSolver

-- onionFlow
--   :: Bool               -- actually use SMT solver?
--   -> Word64             -- max times to iterate checking whole binary
--   -> CfgStore
--   -> [KnownFunc]
--   -> [Prim]             -- checks all on each path
--   -> IO ()              -- it writes results into CfgStore and hopefully DB
-- onionFlow actuallyUseSolver maxIterations store stdLibPrims prims = do
--   allFuncs <- CfgStore.getFuncs store
--   funcs <- CfgStore.getInternalFuncs store
--   forM_ funcs $ \func -> do
--     debug $ "Getting paths for: " <> show (func ^. #name)
--     paths <- fromMaybe [] <$> onionSampleBasedOnFuncSize 1.0 store func
--     debug $ "Got " <> show (length paths) <> " paths."
--     let pathPreps = mkPathPrep [] <$> paths

--     CM.set func pathPreps $ store ^. #pathSamples
--   debug "Finished sampling paths"
--   let initialPrims = getInitialWMIs stdLibPrims allFuncs
--   CM.putSnapshot initialPrims $ store ^. #callablePrims
--   replicateM_ (fromIntegral maxIterations) $ onionSinglePass solver store prims funcs
--   where
--     solver = chooseSolver actuallyUseSolver


-- | Compute the set of functions reachable from attack surface entry points
-- via BFS through CfgInfo calls, up to a given depth.
computeAttackSurfaceWorkingSet
  :: CfgStore -> [Function] -> HashSet Text -> Word64 -> IO [Function]
computeAttackSurfaceWorkingSet store internalFuncs entryNames maxDepth = do
  let nameToFunc = HashMap.fromList [(f ^. #name, f) | f <- internalFuncs]
      entryFuncs = mapMaybe (`HashMap.lookup` nameToFunc) . HashSet.toList $ entryNames
  visitedRef <- newIORef (HashSet.fromList entryFuncs :: HashSet Function)
  queueRef <- newIORef [(f, 0 :: Word64) | f <- entryFuncs]
  let go = readIORef queueRef >>= \case
        [] -> return ()
        ((func, depth):rest) -> do
          modifyIORef' queueRef (const rest)
          when (depth < maxDepth) $ do
            mCfgInfo <- CfgStore.getFuncCfgInfo store func
            case mCfgInfo of
              Nothing -> return ()
              Just cfgInfo -> do
                let callees = mapMaybe getCallNodeFunc (cfgInfo ^. #calls)
                visited <- readIORef visitedRef
                let newCallees = filter (\c -> not $ HashSet.member c visited) callees
                modifyIORef' visitedRef (HashSet.union (HashSet.fromList newCallees))
                modifyIORef' queueRef (++ [(c, depth + 1) | c <- newCallees])
          go
  go
  HashSet.toList <$> readIORef visitedRef


-- | Pick a function weighted by remaining samples needed.
-- Returns Nothing when all functions have met their sample targets.
pickWeightedFunc
  :: IORef (HashMap Function Word64)
  -> HashMap Function Word64
  -> [Function]
  -> IO (Maybe Function)
pickWeightedFunc countsRef targets funcs = do
  counts <- readIORef countsRef
  let weights = [(f, max 0 (fromMaybe 1 (HashMap.lookup f targets) - fromMaybe 0 (HashMap.lookup f counts))) | f <- funcs]
      totalWeight = sum (map snd weights)
  if totalWeight <= 0
    then return Nothing
    else do
      r <- randomRIO (0, totalWeight - 1)
      return . Just $ pickByWeight r weights
  where
    pickByWeight _ [(f, _)] = f
    pickByWeight _ [] = error "pickWeightedFunc: empty list"
    pickByWeight r ((f, w):rest)
      | r < w = f
      | otherwise = pickByWeight (r - w) rest


-- | Check a path for a prim and immediately squash into the callable prims store.
steadyStateCheckPathForPrim
  :: Word64
  -> StmtSolver TypedStmt IO
  -> CfgStore
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
  -> Function
  -> PathPrep TypedStmt
  -> Prim
  -> Bool
  -> IO ()
steadyStateCheckPathForPrim maxResultsPerPath solver store snapshot func pprep prim doSquash = do
  matchAndReturnCallablePrim maxResultsPerPath solver snapshot func pprep prim >>= \case
    [] -> return ()
    cprims -> do
      let newWMIs = HashSet.fromList cprims
          modifier existing =
            let merged = HashSet.union newWMIs existing
            in if doSquash then squashCallableWMIs merged else merged
      CM.modify_ modifier (prim ^. #primType, Func.Internal func) $ store ^. #callablePrims


-- | Steady-state onion analysis. Stochastically picks functions, samples paths
-- on-demand, checks for WMIs immediately, squashes incrementally, and periodically
-- outputs intermediate results.
steadyStateOnionFlow
  :: Word64            -- max results per path
  -> Bool              -- actually use SMT solver?
  -> Double            -- path sampling factor
  -> CfgStore
  -> [KnownFunc]
  -> [Prim]
  -> HashMap Function TypeHints
  -> Bool              -- doSquash
  -> [ReferenceKind]
  -> HashSet Text      -- attack surface function names
  -> Word64            -- attack surface BFS depth
  -> Word64            -- report interval
  -> IO ()             -- output callback
  -> IO ()
steadyStateOnionFlow maxResultsPerPath actuallyUseSolver pathSamplingFactor store stdLibPrims prims funcToTypeHintsMap doSquash refKinds attackSurface attackSurfaceDepth reportInterval outputCallback = do
  _ <- return refKinds -- suppressing warning
  allFuncs <- CfgStore.getFuncs store
  internalFuncs <- CfgStore.getInternalFuncs store

  -- Initialize WMIs from stdlib prims
  let initialPrims = getInitialWMIs stdLibPrims allFuncs
  CM.putSnapshot initialPrims $ store ^. #callablePrims

  -- Compute working set
  workingSet <- if HashSet.null attackSurface
    then return internalFuncs
    else computeAttackSurfaceWorkingSet store internalFuncs attackSurface attackSurfaceDepth
  debug $ "Steady-state working set: " <> show (length workingSet) <> " functions"

  -- Pre-compute function weights (target samples per func based on node count)
  targets <- fmap (HashMap.fromList . catMaybes) . forM workingSet $ \func -> do
    mCfgInfo <- CfgStore.getFuncCfgInfo store func
    return $ case mCfgInfo of
      Nothing -> Nothing
      Just cfgInfo ->
        let numNodes = fromIntegral $ HashSet.size (cfgInfo ^. #nodes)
            target = max 1 (floor (numNodes * pathSamplingFactor))
        in Just (func, target)

  -- Initialize sample counters and per-function visit counts for path diversity
  countsRef <- newIORef (HashMap.empty :: HashMap Function Word64)
  visitCountsRef <- newIORef (HashMap.empty :: HashMap Function (VisitCounts PilNode))
  iterRef <- newIORef (0 :: Word64)

  let solver = chooseSolver actuallyUseSolver

  -- Main loop
  let loop = do
        mFunc <- pickWeightedFunc countsRef targets workingSet
        case mFunc of
          Nothing -> do
            debug "Steady-state: all functions have met sample targets"
            return ()
          Just func -> do
            -- Get accumulated visit counts for this function
            allVisitCounts <- readIORef visitCountsRef
            let funcVisitCounts = fromMaybe emptyVisitCounts (HashMap.lookup func allVisitCounts)

            -- Sample 1 path using visit counts for diversity
            mResult <- sampleSinglePathWithVisitCounts store func funcVisitCounts
            case mResult of
              Nothing -> do
                -- Mark as fully sampled so we don't pick it again
                modifyIORef' countsRef (HashMap.insert func (fromMaybe 1 $ HashMap.lookup func targets))
              Just (path, updatedVisitCounts) -> do
                -- Update visit counts for this function
                modifyIORef' visitCountsRef (HashMap.insert func updatedVisitCounts)

                let typeHints = fromMaybe HashMap.empty (HashMap.lookup func funcToTypeHintsMap)
                    pprep = mkPathPrepWithTypeHints typeHints [] path

                -- Get fresh callable prims snapshot
                cprimsSnapshot <- CM.getSnapshot (store ^. #callablePrims)

                -- Check path against all prims concurrently
                forConcurrently_ prims $ \prim ->
                  steadyStateCheckPathForPrim maxResultsPerPath solver store cprimsSnapshot func pprep prim doSquash

            -- Increment sample count
            modifyIORef' countsRef (HashMap.insertWith (+) func 1)

            -- Check report interval
            modifyIORef' iterRef (+ 1)
            iter <- readIORef iterRef
            when (iter `mod` reportInterval == 0) $ do
              debug $ "Steady-state iteration " <> show iter
              outputCallback

            loop

  loop

  -- Final output
  debug "Steady-state: final output"
  outputCallback
