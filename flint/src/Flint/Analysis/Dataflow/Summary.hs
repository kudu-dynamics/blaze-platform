-- | Computes per-function data flow summaries from PIL/CFG.
--
-- A 'FuncSummary' records which function parameters can flow to which
-- outputs (other params via pointer writes, or the return value).
-- These summaries are computed intraprocedurally, then composed
-- bottom-up across the call graph to answer interprocedural queries.
--
-- The analysis is flow-insensitive: it treats the function body as an
-- unordered set of statements. Sound but potentially imprecise.
module Flint.Analysis.Dataflow.Summary
  ( -- * Building summaries
    computeSummary
  , computeAllSummaries
  , buildDataflowStore
    -- * Lazy persistent cache
  , setupDataflowCache
  , getDataflowSummary
  , getComposedDataflowStore
  , computeSummaryForRef
    -- * Interprocedural composition
  , composeSummariesBottomUp
  , composeWithCallGraph
    -- * Call site extraction
  , extractCallSites
  , computeCallArgMapping
    -- * Helpers
  , collectParams
  , collectReturnVars
  , buildDefUseMap
  , reachableFrom
  ) where

import Flint.Prelude

import Flint.Types.Analysis.Dataflow
import Flint.Types.Cfg.Store (CfgInfo, CfgStore)
import qualified Flint.Cfg.Store as Store
import qualified Blaze.Types.PersistentCalc as PC
import qualified Flint.Types.CachedCalc as CC

import Blaze.Types.Cfg (PilCfg, getNodeData)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (PilVar, Stmt, Expression)
import qualified Blaze.Pil.Analysis as PilA
import qualified Blaze.Pil.Analysis.Path as PathA
import Blaze.Types.Function
  ( FuncRef(..)
  , Function
  , FuncParamInfo(..)
  , toFunctionRef
  , toExternFunctionRef
  )
import qualified Blaze.Graph as G
import Blaze.Types.CallGraph (CallGraph)

import Data.Graph (stronglyConnComp, SCC(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


-- | Collect all PIL statements from a PilCfg (across all basic blocks,
-- call nodes, etc.)
collectAllStmts :: PilCfg -> [Stmt]
collectAllStmts cfg =
  concatMap getNodeData . HashSet.toList $ G.nodes cfg

-- | Replace @LOAD (STACK_ADDR off)@ in expression trees with @VAR arg_N@
-- references to a synthesized @isParam=True@ PilVar. Complements
-- 'promoteStackLocals' for the read-only stack slots that hold
-- stack-passed parameters (which never get a matching 'Store', so
-- the upstream pass leaves them as raw LOADs).
--
-- Only positive stack offsets are materialised — those are the
-- caller-pushed arguments on x86 cdecl and similar ABIs. Negative
-- offsets are locals and either get promoted upstream or stay as
-- memory accesses.
materializeStackParams :: [Stmt] -> [Stmt]
materializeStackParams = fmap goStmt
  where
    goStmt :: Stmt -> Stmt
    goStmt (Pil.Stmt addr stmt) = Pil.Stmt addr (fmap goExpr stmt)

    goExpr :: Expression -> Expression
    goExpr e = case e ^. #op of
      Pil.LOAD (Pil.LoadOp src)
        | Pil.STACK_ADDR so <- src ^. #op
        , let off = so ^. #offset
        , off >= 0
        -> Pil.Expression (e ^. #size)
             (Pil.VAR (Pil.VarOp (stackParamPilVar (so ^. #ctx) (e ^. #size) (coerce off))))
      op -> Pil.Expression (e ^. #size) (fmap goExpr op)

    stackParamPilVar :: Pil.Ctx -> Pil.Size Expression -> Int64 -> PilVar
    stackParamPilVar sctx exprSz off = Pil.PilVar
      { Pil.size = coerce exprSz
      , Pil.ctx = Just sctx
      , Pil.version = Just 1
      , Pil.symbol = "arg_" <> show off
      , Pil.isParam = True
      , Pil.location = Pil.StackMemory off
      }

-- | Identify parameters: PilVars with @isParam = True@, indexed by
-- positional order from 'Function.params'. This matches the call-site
-- argument position, which is required for interprocedural composition
-- to be sound.
--
-- For each @Function.params[i]@, look up the matching @PilVar@ by symbol
-- name. PilVar symbols are populated from the param name by Ghidra
-- (see 'Blaze.Cfg.Interprocedural.getParamSym'). If a positional param
-- has no matching PilVar in the body (dead parameter), the index is
-- simply omitted.
--
-- Falls back to alphabetical ordering only when 'Function.params' is
-- empty (rare — unresolved function).
collectParams :: Function -> [Stmt] -> HashMap Int PilVar
collectParams func stmts =
  let allVars = PilA.getDefinedVars stmts <> PilA.getRefVars stmts
      paramVars = filter (^. #isParam) $ HashSet.toList allVars
      bySymbol = HashMap.fromList [ (v ^. #symbol, v) | v <- paramVars ]
      funcParamNames = fmap paramInfoName (func ^. #params)

      -- After 'promoteStackLocals' runs on a stack-calling-convention
      -- binary (cdecl x86), stack-passed params surface as PilVars
      -- with @isParam=True@ and @location = StackMemory N@ where N
      -- is the caller-frame offset. Sorting by offset gives positional
      -- order: param 0 = lowest positive offset (first caller-pushed
      -- arg on an ascending-address ABI).
      sortedByLocation = sortOn locKey paramVars
      locKey pv = case pv ^. #location of
        -- Stack params keyed by their offset; negatives should not
        -- appear here because 'mkStackPilVar' marks only positive
        -- offsets as isParam, but be defensive.
        Pil.StackMemory o -> (0 :: Int, o, pv ^. #symbol)
        Pil.Register _    -> (1, 0, pv ^. #symbol)
        _                 -> (2, 0, pv ^. #symbol)
  in case funcParamNames of
       -- No declared params — nothing to index.
       []    -> mempty
       names ->
         -- Primary path: names in Function.params match the symbols on
         -- isParam=True PilVars. Works for register-calling-convention
         -- ABIs where Ghidra names them after the register.
         let byName = HashMap.fromList
               [ (idx, pv)
               | (idx, name) <- zip [0..] names
               , Just pv <- [HashMap.lookup name bySymbol]
               ]
         in if not (HashMap.null byName)
            then byName
            -- Fallback: no name matches (typical for stack-promoted
            -- x86 params where the lifter produces @arg_8@ etc., not
            -- the source-level name). Map positional
            -- 'Function.params' onto 'paramVars' sorted by location.
            -- We take the first @length names@ PilVars in location
            -- order; extra PilVars (shouldn't happen, but …) drop.
            else HashMap.fromList
                   $ zip [0 ..] (take (length names) sortedByLocation)
  where
    paramInfoName :: FuncParamInfo -> Text
    paramInfoName = \case
      FuncParamInfo pinfo  -> pinfo ^. #name
      FuncVarArgInfo pinfo -> pinfo ^. #name

-- | Collect PilVars that appear in return statements.
collectReturnVars :: [Stmt] -> HashSet PilVar
collectReturnVars = HashSet.fromList . concatMap extractRetVar
  where
    extractRetVar (Pil.Stmt _ (Pil.Ret retOp)) =
      PilA.getVarsFromExpr_ (retOp ^. #value)
    extractRetVar _ = []

-- | Build a def-use map: for each PilVar that is defined, record which
-- PilVars it depends on (the vars referenced in its defining expression).
--
-- This is simpler than the full DataDependenceGraph — we just need
-- a HashMap from defined var to the set of vars it uses.
buildDefUseMap :: [Stmt] -> HashMap PilVar (HashSet PilVar)
buildDefUseMap = foldl' go HashMap.empty
  where
    go acc (Pil.Stmt _ stmt) = case stmt of
      Pil.Def defOp ->
        let dest = defOp ^. #var
            uses = HashSet.fromList $ PilA.getVarsFromExpr_ (defOp ^. #value)
        in HashMap.insert dest uses acc
      Pil.DefPhi phiOp ->
        let dest = phiOp ^. #dest
            uses = HashSet.fromList $ phiOp ^. #src
        in HashMap.insert dest uses acc
      _ -> acc

-- | Compute the transitive closure of reachability through the def-use map.
-- Given a starting set of vars, find all vars that can be reached by
-- following def-use chains backwards (i.e. which defs ultimately depend
-- on the starting vars).
reachableFrom :: HashMap PilVar (HashSet PilVar) -> HashSet PilVar -> HashSet PilVar
reachableFrom defUseMap seeds = go seeds seeds
  where
    -- BFS/fixed-point: expand from frontier
    go visited frontier
      | HashSet.null frontier = visited
      | otherwise =
          let -- For each var in frontier, find all vars that USE it
              -- (i.e. vars whose definition references a frontier var)
              newReachable = HashSet.fromList
                [ dest
                | (dest, uses) <- HashMap.toList defUseMap
                , not (HashSet.null (HashSet.intersection uses frontier))
                , not (HashSet.member dest visited)
                ]
          in go (HashSet.union visited newReachable) newReachable

-- | Compute a 'FuncSummary' for a single function from its CfgInfo.
-- Flow-insensitive: ignores block ordering and asks "can data from
-- param P reach return/other-param?" transitively through def-use chains.
computeSummary :: DataflowConfig -> FuncRef -> Function -> CfgInfo -> FuncSummary
computeSummary _config funcRef func cfgInfo =
  let cfg = cfgInfo ^. #cfg
      rawStmts = collectAllStmts cfg
      -- Two passes needed to surface stack-passed params as PilVars:
      --   1. Upstream 'promoteStackLocals' handles slots that are both
      --      read AND written (typical locals, and params the function
      --      decrements/increments).
      --   2. Our own 'materializeStackParams' handles /read-only/
      --      stack slots — Ghidra's lifter now emits stack-passed
      --      params as LOAD(STACK_ADDR off) with no corresponding
      --      Store, so upstream promotion misses them entirely.
      allStmts = materializeStackParams . PathA.promoteStackLocals $ rawStmts
      params = collectParams func allStmts
      retVars = collectReturnVars allStmts
      defUseMap = buildDefUseMap allStmts

      -- Direct + transitive global loads per defined var.
      varDirectLoads = computeVarDirectLoads allStmts
      varTransitive = computeVarTransitiveLoads varDirectLoads defUseMap

      -- For each parameter, compute which vars are reachable
      paramFlows = HashMap.mapWithKey (computeParamFlows defUseMap retVars params) params

      -- Flatten into FlowEdges
      edges = concatMap snd $ HashMap.toList paramFlows

      -- Store-based flows. Now also captures param->global and
      -- global->global writes through fixed addresses.
      storeEdges = computeStoreFlows allStmts defUseMap params varTransitive

      -- Global-read flows: if a return variable transitively loads
      -- from a global, add a Global -> Return edge.
      globalReturnEdges =
        [ FlowEdge (GlobalEndpoint g) ReturnEndpoint DerefReadFlow
        | rv <- HashSet.toList retVars
        , g  <- HashSet.toList $ fromMaybe HashSet.empty
                               $ HashMap.lookup rv varTransitive
        ]

      -- Extract call sites with pre-computed arg/ret mappings
      sites = extractCallSites defUseMap params retVars varTransitive allStmts
  in FuncSummary
      { funcRef   = funcRef
      , flowEdges = edges <> storeEdges <> globalReturnEdges
      , paramVars = params
      , retVars   = retVars
      , callSites = sites
      , computed  = True
      }

-- | For a single parameter, determine what it flows to.
computeParamFlows
  :: HashMap PilVar (HashSet PilVar)  -- ^ def-use map
  -> HashSet PilVar                   -- ^ return vars
  -> HashMap Int PilVar               -- ^ all params
  -> Int                              -- ^ this param's index
  -> PilVar                           -- ^ this param's PilVar
  -> [FlowEdge]
computeParamFlows defUseMap retVars allParams paramIdx paramVar =
  let -- Everything reachable from this param
      reachable = reachableFrom defUseMap (HashSet.singleton paramVar)

      -- Does it reach any return var?
      retEdges =
        [ FlowEdge (ParamEndpoint paramIdx) ReturnEndpoint DirectFlow
        | not . HashSet.null $ HashSet.intersection reachable retVars
        ]

      -- Does it reach any other param? (for output params / pointer writes)
      paramEdges =
        [ FlowEdge (ParamEndpoint paramIdx) (ParamEndpoint otherIdx) DirectFlow
        | (otherIdx, otherVar) <- HashMap.toList allParams
        , otherIdx /= paramIdx
        , HashSet.member otherVar reachable
        ]
  in retEdges <> paramEdges

-- | Detect flows through Store statements:
--
--   * @Store(ptrDerivedFromParam, value)@: param-to-param DerefWriteFlow
--     when the store address and stored value are derived from
--     different parameters.
--
--   * @Store(CONST_PTR g, value)@: a write to a fixed global address.
--     Emits @ParamEndpoint -> GlobalEndpoint g@ (for each caller param
--     feeding the value) and @GlobalEndpoint g' -> GlobalEndpoint g@
--     (for each distinct global loaded into the value).
computeStoreFlows
  :: [Stmt]
  -> HashMap PilVar (HashSet PilVar)
  -> HashMap Int PilVar
  -> HashMap PilVar (HashSet Address)   -- ^ var -> transitive global loads
  -> [FlowEdge]
computeStoreFlows stmts defUseMap params varTransitive =
  let paramSet = HashSet.fromList $ HashMap.elems params
      paramLookup = HashMap.fromList
        [ (v, idx) | (idx, v) <- HashMap.toList params ]
      invertedMap = invertDefUse defUseMap
  in concatMap (checkStore paramSet paramLookup invertedMap) stmts
  where
    checkStore paramSet paramLookup invMap (Pil.Stmt _ (Pil.Store storeOp)) =
      let addrExpr = storeOp ^. #addr
          valExpr  = storeOp ^. #value
          valVars  = HashSet.fromList $ PilA.getVarsFromExpr_ valExpr
          valReachesParams = HashSet.intersection paramSet
            $ reachableFrom invMap valVars
          valGlobals = expressionGlobalDeps varTransitive valExpr
      in case extractConstPtrAddr addrExpr of
           Just globalDst ->
             -- Write lands at a fixed global address.
             [ FlowEdge (ParamEndpoint srcIdx) (GlobalEndpoint globalDst) DerefWriteFlow
             | srcVar <- HashSet.toList valReachesParams
             , Just srcIdx <- [HashMap.lookup srcVar paramLookup]
             ] ++
             [ FlowEdge (GlobalEndpoint srcG) (GlobalEndpoint globalDst) DerefWriteFlow
             | srcG <- HashSet.toList valGlobals
             , srcG /= globalDst
             ]
           Nothing ->
             let addrVars = HashSet.fromList $ PilA.getVarsFromExpr_ addrExpr
                 addrReachesParams = HashSet.intersection paramSet
                   $ reachableFrom invMap addrVars
             in [ FlowEdge
                    (ParamEndpoint srcIdx)
                    (ParamEndpoint dstIdx)
                    DerefWriteFlow
                | srcVar <- HashSet.toList valReachesParams
                , Just srcIdx <- [HashMap.lookup srcVar paramLookup]
                , dstVar <- HashSet.toList addrReachesParams
                , Just dstIdx <- [HashMap.lookup dstVar paramLookup]
                , srcIdx /= dstIdx
                ]
    checkStore _ _ _ _ = []

-- | Invert the def-use map: for each var, find what defines depend on it.
-- This lets us trace backwards from a use to its defining params.
invertDefUse :: HashMap PilVar (HashSet PilVar) -> HashMap PilVar (HashSet PilVar)
invertDefUse = HashMap.foldlWithKey' go HashMap.empty
  where
    go acc dest uses = foldl' addEdge acc (HashSet.toList uses)
      where
        addEdge m src = HashMap.insertWith HashSet.union src (HashSet.singleton dest) m

-- ---------------------------------------------------------------------------
-- Global-address extraction
-- ---------------------------------------------------------------------------

-- | If an address expression resolves to a fixed global address (with
-- offset baked in), return that address. Walks through @FIELD_ADDR@
-- and @ARRAY_ADDR@ wrappers that appear around a @CONST_PTR@ or
-- @GLOBAL_PTR@ base — typical when Ghidra lifts accesses to fields of
-- a global struct.
--
-- For FIELD_ADDR, the constant offset is added so different fields of
-- the same global base map to distinct 'Address' values. For
-- ARRAY_ADDR, the variable index means we can't pinpoint a specific
-- element, so we just use the base address.
extractConstPtrAddr :: Expression -> Maybe Address
extractConstPtrAddr e = case e ^. #op of
  Pil.CONST_PTR op  -> Just . intToAddr $ op ^. #constant
  Pil.GLOBAL_PTR op -> Just . intToAddr $ op ^. #constant
  Pil.FIELD_ADDR op -> do
    base <- extractConstPtrAddr (op ^. #baseAddr)
    let ByteOffset off = op ^. #offset
    Just $ base & #offset %~ (+ off)
  Pil.ARRAY_ADDR op ->
    -- Variable index: collapse to the base.
    extractConstPtrAddr (op ^. #base)
  _ -> Nothing

-- | Walk an expression tree and collect the addresses of every @LOAD@
-- whose source resolves (via 'extractConstPtrAddr') to a fixed global
-- address. Covers direct @LOAD (CONST_PTR _)@ as well as the common
-- @LOAD (FIELD_ADDR (GLOBAL_PTR _) _)@ struct-field pattern.
extractGlobalLoads :: Expression -> HashSet Address
extractGlobalLoads e = case e ^. #op of
  Pil.LOAD op ->
    let inner = op ^. #src
        direct = maybe HashSet.empty HashSet.singleton (extractConstPtrAddr inner)
    in direct <> extractGlobalLoads inner
  op ->
    foldMap extractGlobalLoads op

-- | Per-defined-var: the global addresses loaded /directly/ in that
-- var's defining expression (not via other vars).
computeVarDirectLoads :: [Stmt] -> HashMap PilVar (HashSet Address)
computeVarDirectLoads = foldl' go HashMap.empty
  where
    go acc (Pil.Stmt _ stmt) = case stmt of
      Pil.Def defOp ->
        let dest = defOp ^. #var
            globals = extractGlobalLoads (defOp ^. #value)
        in if HashSet.null globals then acc
           else HashMap.insertWith HashSet.union dest globals acc
      _ -> acc

-- | Transitive version of 'computeVarDirectLoads' — for each var, the
-- union of directly-loaded globals along its backwards def-use chain.
computeVarTransitiveLoads
  :: HashMap PilVar (HashSet Address)     -- ^ direct loads per var
  -> HashMap PilVar (HashSet PilVar)       -- ^ def-use map (dest -> uses)
  -> HashMap PilVar (HashSet Address)
computeVarTransitiveLoads directLoads defUseMap =
  let invMap = invertDefUse defUseMap
      allVars = HashSet.toList
        $ HashSet.fromList (HashMap.keys defUseMap)
       <> HashSet.fromList (HashMap.keys directLoads)
  in HashMap.fromList
       [ (v, union)
       | v <- allVars
       , let ancestors = reachableFrom invMap (HashSet.singleton v)
             union = foldMap lookupDirect (HashSet.toList ancestors)
       , not (HashSet.null union)
       ]
  where
    lookupDirect u = fromMaybe HashSet.empty $ HashMap.lookup u directLoads

-- | For an expression, the union of its direct global loads plus the
-- transitive loads inherited from any PilVars it references.
expressionGlobalDeps
  :: HashMap PilVar (HashSet Address)
  -> Expression
  -> HashSet Address
expressionGlobalDeps varTransitive e =
  let direct = extractGlobalLoads e
      viaVars = foldMap
        (\v -> fromMaybe HashSet.empty $ HashMap.lookup v varTransitive)
        (PilA.getVarsFromExpr_ e)
  in direct <> viaVars

-- ---------------------------------------------------------------------------
-- Call site extraction
-- ---------------------------------------------------------------------------

-- | Extract call site details from PIL statements, with pre-computed
-- argument and return mappings. This runs during intraprocedural summary
-- computation when the def-use map is available.
extractCallSites
  :: HashMap PilVar (HashSet PilVar)  -- ^ def-use map
  -> HashMap Int PilVar               -- ^ caller params
  -> HashSet PilVar                   -- ^ caller return vars
  -> HashMap PilVar (HashSet Address) -- ^ var -> transitive global loads
  -> [Stmt]                           -- ^ all statements
  -> [CallSiteDetail]
extractCallSites defUseMap callerParams callerRetVars varTransitive =
  let -- Precompute: for each caller param, everything that transitively
      -- depends on it. Shared across all call sites in this function.
      paramReachable = HashMap.map (reachableFrom defUseMap . HashSet.singleton) callerParams
  in mapMaybe (extractOne paramReachable)
  where
    extractOne paramReachable stmt = do
      callStmt <- Pil.mkCallStatement stmt
      let dest = Pil.getCallDest callStmt
      ref <- callDestToFuncRef dest
      let argMap = computeCallArgMapping paramReachable callerParams
                     varTransitive (callStmt ^. #args)
          retMap = computeReturnMapping defUseMap callerParams callerRetVars
                     (callStmt ^. #resultVar)
      return CallSiteDetail
        { calleeRef  = ref
        , argMapping = argMap
        , retMapping = retMap
        , callAddr   = stmt ^. #addr
        }

    callDestToFuncRef :: Pil.CallDest Pil.Expression -> Maybe FuncRef
    callDestToFuncRef = \case
      Pil.CallFunc f    -> Just . InternalRef $ toFunctionRef f
      Pil.CallExtern ef -> Just . ExternalRef $ toExternFunctionRef ef
      _                 -> Nothing  -- indirect/unknown calls

-- | Compute the mapping from callee param indices to caller endpoints.
-- For each argument expression at position @i@, determines which caller
-- parameters contributed to it by tracing through def-use chains.
--
-- Takes a precomputed per-param reachable set to avoid recomputing
-- reachability for every argument of every call site.
--
-- Returns @[(calleeParamIdx, callerEndpoint)]@ — each entry says
-- "callee param i receives data from caller endpoint ep".
computeCallArgMapping
  :: HashMap Int (HashSet PilVar)
    -- ^ Precomputed: for each caller param index, the set of vars
    -- transitively reachable from (= transitively dependent on) that param
  -> HashMap Int PilVar               -- ^ caller's params
  -> HashMap PilVar (HashSet Address) -- ^ var -> transitive global loads
  -> [Expression]                     -- ^ argument expressions at the call site
  -> [(Int, FlowEndpoint)]
computeCallArgMapping paramReachable callerParams varTransitive argExprs =
  let paramLookup = HashMap.fromList
        [ (v, idx) | (idx, v) <- HashMap.toList callerParams ]
      paramSet = HashSet.fromList $ HashMap.elems callerParams
  in concatMap (mapOneArg paramLookup paramSet) (zip [0..] argExprs)
  where
    mapOneArg paramLookup paramSet (calleeIdx, argExpr) =
      let argVars = HashSet.fromList $ PilA.getVarsFromExpr_ argExpr
          reachableFromParams =
            [ callerIdx
            | (callerIdx, reached) <- HashMap.toList paramReachable
            , not . HashSet.null $ HashSet.intersection reached argVars
            ]
          directParams =
            [ callerIdx
            | argVar <- HashSet.toList argVars
            , HashSet.member argVar paramSet
            , Just callerIdx <- [HashMap.lookup argVar paramLookup]
            ]
          paramSourceIndices = HashSet.toList . HashSet.fromList
                                $ directParams <> reachableFromParams
          -- Global-origin sources: if any part of argExpr (directly or
          -- through its PilVars) transitively loads from a global, note
          -- that global as an argument source.
          argGlobals = expressionGlobalDeps varTransitive argExpr
      in [ (calleeIdx, ParamEndpoint callerIdx)
         | callerIdx <- paramSourceIndices
         ] ++
         [ (calleeIdx, GlobalEndpoint g)
         | g <- HashSet.toList argGlobals
         ]

-- | Compute which caller endpoint receives the return value from a call.
-- If the call has a resultVar and that var reaches a caller return or param,
-- we track where the return value goes.
computeReturnMapping
  :: HashMap PilVar (HashSet PilVar)  -- ^ def-use map
  -> HashMap Int PilVar               -- ^ caller's params
  -> HashSet PilVar                   -- ^ caller's return vars
  -> Maybe PilVar                     -- ^ result var from the call
  -> Maybe FlowEndpoint
computeReturnMapping defUseMap callerParams callerRetVars = \case
  Nothing -> Nothing
  Just rv ->
    let reached = reachableFrom defUseMap (HashSet.singleton rv)
    in if not . HashSet.null $ HashSet.intersection reached callerRetVars
       then Just ReturnEndpoint
       else -- Check if it reaches another param (output param pattern)
            let paramHits =
                  [ ParamEndpoint idx
                  | (idx, pv) <- HashMap.toList callerParams
                  , HashSet.member pv reached
                  ]
            in listToMaybe paramHits


-- ---------------------------------------------------------------------------
-- Interprocedural composition
-- ---------------------------------------------------------------------------

-- | Compose callee summaries into a caller's summary at each call site.
-- Uses the pre-computed argument/return mappings stored in CallSiteDetail
-- to translate callee flow edges into caller terms.
composeAtCallSites
  :: HashMap FuncRef FuncSummary  -- ^ all summaries (callees already resolved)
  -> FuncSummary                  -- ^ caller's current summary
  -> FuncSummary
composeAtCallSites allSummaries callerSummary =
  let transitiveEdges = concatMap composeOneCallSite (callerSummary ^. #callSites)
      -- Deduplicate: the same transitive edge might be produced by multiple
      -- call sites or already exist from the intraprocedural phase
      existingEdges = HashSet.fromList (callerSummary ^. #flowEdges)
      newEdges = HashSet.toList . HashSet.fromList
                   $ filter (not . (`HashSet.member` existingEdges)) transitiveEdges
  in callerSummary & #flowEdges %~ (<> newEdges)
  where
    composeOneCallSite site =
      let calleeSummary = fromMaybe (emptySummary (site ^. #calleeRef))
                            $ HashMap.lookup (site ^. #calleeRef) allSummaries
      in if not (calleeSummary ^. #computed) || null (calleeSummary ^. #flowEdges)
         then []
         else translateCalleeEdges
                (site ^. #argMapping) (site ^. #retMapping)
                (calleeSummary ^. #flowEdges)

    -- A callee param may have multiple caller-side sources (two caller
    -- params both contributing to one arg expression, or a param plus
    -- a global). Collapse via 'fromListWith (<>)' so every source
    -- survives; translate each callee edge against every combination
    -- of resolved sources.
    translateCalleeEdges argMapping mRetEp calleeEdges =
      let argMap :: HashMap Int [FlowEndpoint]
          argMap = HashMap.fromListWith (<>)
                     [ (i, [ep]) | (i, ep) <- argMapping ]
      in concatMap (translateEdge argMap mRetEp) calleeEdges

    translateEdge argMap mRetEp calleeEdge =
      let fromSources = resolveSources argMap (calleeEdge ^. #from)
          toSources   = resolveSources' argMap mRetEp (calleeEdge ^. #to)
      in [ FlowEdge f t (calleeEdge ^. #kind)
         | f <- fromSources
         , t <- toSources
         , f /= t
         ]

    resolveSources argMap = \case
      ParamEndpoint i  -> fromMaybe [] $ HashMap.lookup i argMap
      ReturnEndpoint   -> []
      GlobalEndpoint a -> [GlobalEndpoint a]

    resolveSources' argMap mRetEp = \case
      ParamEndpoint i  -> fromMaybe [] $ HashMap.lookup i argMap
      ReturnEndpoint   -> maybeToList mRetEp
      GlobalEndpoint a -> [GlobalEndpoint a]

-- | Bottom-up interprocedural composition using topological sort.
-- Processes functions from callees to callers so that callee summaries
-- are fully resolved before being inlined into callers.
composeSummariesBottomUp
  :: HashMap FuncRef FuncSummary
  -> CfgStore
  -> IO (HashMap FuncRef FuncSummary)
composeSummariesBottomUp initialSummaries store = do
  cg <- Store.getCallGraph store
  pure $ composeWithCallGraph cg initialSummaries

-- | Pure variant of 'composeSummariesBottomUp'. Takes the call graph
-- and the initial summaries, and returns composed summaries with
-- transitive edges inlined. Exported for tests that can construct
-- synthetic fixtures without a 'CfgStore'.
composeWithCallGraph
  :: CallGraph
  -> HashMap FuncRef FuncSummary
  -> HashMap FuncRef FuncSummary
composeWithCallGraph cg initialSummaries =
  let nodeList = HashSet.toList $ G.nodes cg
      sccs = stronglyConnComp
        [ (n, n, HashSet.toList $ G.succs n cg)
        | n <- nodeList
        ]
      -- stronglyConnComp returns in reverse topological order (sinks first)
      -- which is what we want: callees processed before callers
  in foldl' processSCC initialSummaries sccs
  where
    processSCC summaries (AcyclicSCC funcRef) =
      case HashMap.lookup funcRef summaries of
        Nothing -> summaries
        Just summary ->
          let composed = composeAtCallSites summaries summary
          in HashMap.insert funcRef composed summaries

    processSCC summaries (CyclicSCC funcRefs) =
      iterateComposition (5 :: Int) summaries funcRefs

    iterateComposition 0 summaries _ = summaries
    iterateComposition n summaries funcRefs =
      let summaries' = foldl' (\s ref ->
            case HashMap.lookup ref s of
              Nothing -> s
              Just summary ->
                let composed = composeAtCallSites s summary
                in HashMap.insert ref composed s
            ) summaries funcRefs
      in if summaries' == summaries
         then summaries
         else iterateComposition (n - 1) summaries' funcRefs


-- ---------------------------------------------------------------------------
-- Building the full store
-- ---------------------------------------------------------------------------

-- | Compute intraprocedural summaries for all functions, going through
-- the persistent cache on 'CfgStore'. The first call for a function
-- runs 'computeSummary' and persists to LMDB; subsequent calls in the
-- same session hit the in-memory tier, and calls across sessions hit
-- LMDB.
computeAllSummaries
  :: DataflowConfig
  -> CfgStore
  -> IO (HashMap FuncRef FuncSummary)
computeAllSummaries config store = do
  setupDataflowCache store config
  internals <- Store.getInternalFuncs store
  allRefs   <- Store.getFuncs store

  -- Internal functions go through the per-function cache (PC.get fires
  -- the registered default on miss, hits LMDB/memory on hit).
  internalSummaries <- forM internals $ \fref -> do
    let ref = InternalRef fref
    s <- fromMaybe (emptySummary ref) <$> PC.get ref (store ^. #dataflowCache)
    return (ref, s)

  -- Externs get empty summaries (we don't model extern bodies). They
  -- live under ExternalRef keys so lookups by name don't collide with
  -- statically-linked internal functions of the same name.
  let externSummaries =
        [ (ref, emptySummary ref)
        | ref <- allRefs
        , case ref of ExternalRef _ -> True; _ -> False
        ]

  return $ HashMap.fromList (internalSummaries <> externSummaries)

-- | Register the per-function dataflow summary computation AND the
-- composed-store default on the given 'CfgStore'. Idempotent — safe to
-- call from multiple entry points. Both defaults fire lazily on first
-- access.
setupDataflowCache :: CfgStore -> DataflowConfig -> IO ()
setupDataflowCache store config = do
  PC.setDefault (computeSummaryForRef store config) (store ^. #dataflowCache)
  CC.setDefault (\() -> buildDataflowStore config store)
                (store ^. #composedDataflowCache)

-- | Fetch the fully-composed 'DataflowStore' for this 'CfgStore',
-- building it on first access and caching afterwards. Every consumer
-- that holds a 'CfgStore' — the flint CLI, the shell, the MCP
-- server — shares the same composed store, so the composition cost
-- is paid at most once per binary load.
getComposedDataflowStore :: CfgStore -> DataflowConfig -> IO DataflowStore
getComposedDataflowStore store config = do
  setupDataflowCache store config
  fromJust <$> CC.get () (store ^. #composedDataflowCache)

-- | Look up or compute a single function's intra-procedural summary.
-- Hits the in-memory cache, then LMDB, then falls back to a fresh
-- 'computeSummary' on a CFG miss.
getDataflowSummary :: CfgStore -> DataflowConfig -> FuncRef -> IO FuncSummary
getDataflowSummary store config ref = do
  setupDataflowCache store config
  fromMaybe (emptySummary ref) <$> PC.get ref (store ^. #dataflowCache)

-- | The default compute function wired into 'dataflowCache'. Resolves
-- the function and its CFG, then calls 'computeSummary'. External
-- functions return an empty summary (we don't model extern bodies yet).
computeSummaryForRef
  :: CfgStore
  -> DataflowConfig
  -> FuncRef
  -> IO FuncSummary
computeSummaryForRef store config = \case
  ref@(InternalRef fref) -> do
    mFunc <- Store.resolveFunction store fref
    case mFunc of
      Nothing   -> return (emptySummary ref)
      Just func -> do
        mCfgInfo <- Store.getFuncCfgInfo store func
        case mCfgInfo of
          Nothing      -> return (emptySummary ref)
          Just cfgInfo -> return $ computeSummary config ref func cfgInfo
  ref@(ExternalRef _) -> return (emptySummary ref)

-- | Build a complete DataflowStore from a CfgStore.
-- This is the main entry point:
--   1. Computes intraprocedural summaries for all functions
--   2. Composes them bottom-up across the call graph
--   3. Packages everything with the config
buildDataflowStore :: DataflowConfig -> CfgStore -> IO DataflowStore
buildDataflowStore config store = do
  intraSummaries <- computeAllSummaries config store
  composedSummaries <- composeSummariesBottomUp intraSummaries store
  return DataflowStore
    { summaries = composedSummaries
    , config    = config
    }
