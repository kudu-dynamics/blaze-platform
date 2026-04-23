-- | Core types for interprocedural data flow analysis.
--
-- This module defines the representations for function summaries (which
-- parameters flow to which outputs) and the configuration knobs that
-- control how the analysis runs.
--
-- Design note: this layer is /pure data flow/. It does not know about
-- taint policies, severity levels, or CWE numbers. A separate taint
-- layer can interpret the reachability results produced here.
module Flint.Types.Analysis.Dataflow
  ( -- * Flow edges
    FlowEdge(..)
  , FlowKind(..)
    -- * Function summaries
  , FuncSummary(..)
  , emptySummary
    -- * Call site detail
  , CallSiteDetail(..)
    -- * Configuration
  , DataflowConfig(..)
  , defaultDataflowConfig
    -- * Interprocedural state
  , DataflowStore(..)
    -- * Query results
  , FlowPath(..)
  , FlowQuery(..)
  , FlowResult(..)
    -- * Endpoints
  , FlowEndpoint(..)
    -- * Persistence transport
  , FuncSummaryTransport(..)
  , summaryToTransport
  , summaryFromTransport
  ) where

import Flint.Prelude

import Blaze.Types.Function (FuncRef)
import Blaze.Types.Pil (PilVar)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


-- | Describes the kind of data flow between two endpoints.
data FlowKind
  = DirectFlow
    -- ^ Value is copied directly (e.g. @x = y@)
  | DerefReadFlow
    -- ^ Value flows through a memory read (e.g. @x = *p@)
  | DerefWriteFlow
    -- ^ Value flows through a memory write (e.g. @*p = y@)
  | CallArgFlow
    -- ^ Value flows into a function call argument
  | CallReturnFlow
    -- ^ Value flows from a function return value
  | PhiFlow
    -- ^ Value flows through an SSA phi node (control-flow merge)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, Serialize)

-- | An endpoint of a data flow edge — either a function parameter
-- (identified by position) or a return value.
data FlowEndpoint
  = ParamEndpoint Int
    -- ^ Function parameter at zero-based index
  | ReturnEndpoint
    -- ^ The function's return value
  | GlobalEndpoint Address
    -- ^ A global memory location
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, Serialize)

-- | A single data flow edge within a function summary.
-- Records that data can flow from one endpoint to another,
-- with a particular kind of transformation.
data FlowEdge = FlowEdge
  { from :: FlowEndpoint
  , to   :: FlowEndpoint
  , kind :: FlowKind
  } deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, Serialize)

-- | Summary of data flow within a single function.
-- Computed once per function, then composed bottom-up across the call graph.
data FuncSummary = FuncSummary
  { funcRef   :: FuncRef
    -- ^ The function this summary describes
  , flowEdges :: [FlowEdge]
    -- ^ Intra-procedural flow edges (param->param, param->return, etc.)
    -- After interprocedural composition, also includes transitive edges
    -- produced by inlining callee summaries.
  , paramVars :: HashMap Int PilVar
    -- ^ Mapping from param index to the PIL variable.
    -- NOTE: param indices are assigned by sorting PIL param vars
    -- alphabetically by symbol name — they may not match the function's
    -- actual parameter positions. Use 'Function.params' for positional info.
  , retVars   :: HashSet PilVar
    -- ^ PIL variables that appear in return statements
  , callSites :: [CallSiteDetail]
    -- ^ Call sites found in this function's PIL. Used for interprocedural
    -- composition — maps which caller expressions flow into which callee params.
  , computed  :: Bool
    -- ^ False for stubs / extern functions with no body
  } deriving (Eq, Show, Generic)

-- | Information about a call site within a function's PIL, pre-computed
-- with argument-to-parameter mappings for interprocedural composition.
data CallSiteDetail = CallSiteDetail
  { calleeRef   :: FuncRef
    -- ^ Which function is being called
  , argMapping  :: [(Int, FlowEndpoint)]
    -- ^ Pre-computed mapping: (callee param index, caller endpoint that
    -- provides the value). Computed during intraprocedural summary by
    -- tracing which caller params reach each argument expression.
  , retMapping  :: Maybe FlowEndpoint
    -- ^ Where the return value goes in the caller (if any). 'Nothing' if
    -- the call has no result variable or the result isn't used in a
    -- tracked way.
  , callAddr    :: Address
    -- ^ Address of the call instruction
  } deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

-- | Empty summary for functions we can't analyze (extern, no CFG).
emptySummary :: FuncRef -> FuncSummary
emptySummary ref = FuncSummary
  { funcRef   = ref
  , flowEdges = []
  , paramVars = mempty
  , retVars   = mempty
  , callSites = []
  , computed  = False
  }

-- | Configuration for the data flow analysis engine.
--
-- The analysis is currently always flow-insensitive (it treats the
-- statements of each function as an unordered set). A flow-sensitive
-- mode that respects CFG ordering is planned but not yet implemented.
data DataflowConfig = DataflowConfig
  { maxCallDepth   :: Int
    -- ^ Maximum interprocedural call chain depth (prevents infinite
    -- expansion in recursive code)
  , followExterns  :: Bool
    -- ^ Whether to attempt to model extern functions. If False, externs
    -- are treated as opaque (no flow edges).
  } deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

defaultDataflowConfig :: DataflowConfig
defaultDataflowConfig = DataflowConfig
  { maxCallDepth   = 10
  , followExterns  = False
  }

-- | Cached store of function summaries for the entire binary.
data DataflowStore = DataflowStore
  { summaries :: HashMap FuncRef FuncSummary
    -- ^ Per-function summaries (computed or stubbed)
  , config    :: DataflowConfig
  } deriving (Show, Generic)

-- | A query asking "can data flow from source to sink?"
data FlowQuery = FlowQuery
  { source     :: (FuncRef, FlowEndpoint)
    -- ^ Where the data originates (function + param/return)
  , sink       :: (FuncRef, FlowEndpoint)
    -- ^ Where the data must reach
  , maxDepth   :: Maybe Int
    -- ^ Override maxCallDepth for this query
  } deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | A step in an interprocedural flow path.
data FlowPath = FlowPath
  { function :: FuncRef
    -- ^ The function this step occurs in
  , edge     :: FlowEdge
    -- ^ The intra-procedural flow edge
  } deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Result of a flow query.
data FlowResult = FlowResult
  { query      :: FlowQuery
  , reachable  :: Bool
    -- ^ Whether data can flow from source to sink
  , paths      :: [[FlowPath]]
    -- ^ Witness paths (may be empty if reachable is False, or multiple
    -- if there are several routes)
  } deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Persistence transport
-- ---------------------------------------------------------------------------

-- | Serialisation-friendly mirror of 'FuncSummary'. 'cereal' does not
-- provide instances for 'HashMap' or 'HashSet', so we materialise those
-- fields into association lists / lists on disk. 'summaryToTransport'
-- and 'summaryFromTransport' round-trip between the two representations.
data FuncSummaryTransport = FuncSummaryTransport
  { funcRef   :: FuncRef
  , flowEdges :: [FlowEdge]
  , paramVars :: [(Int, PilVar)]
  , retVars   :: [PilVar]
  , callSites :: [CallSiteDetail]
  , computed  :: Bool
  } deriving (Eq, Show, Generic)
  deriving anyclass (Serialize)

summaryToTransport :: FuncSummary -> FuncSummaryTransport
summaryToTransport s = FuncSummaryTransport
  { funcRef   = s ^. #funcRef
  , flowEdges = s ^. #flowEdges
  , paramVars = HashMap.toList (s ^. #paramVars)
  , retVars   = HashSet.toList (s ^. #retVars)
  , callSites = s ^. #callSites
  , computed  = s ^. #computed
  }

summaryFromTransport :: FuncSummaryTransport -> FuncSummary
summaryFromTransport t = FuncSummary
  { funcRef   = t ^. #funcRef
  , flowEdges = t ^. #flowEdges
  , paramVars = HashMap.fromList (t ^. #paramVars)
  , retVars   = HashSet.fromList (t ^. #retVars)
  , callSites = t ^. #callSites
  , computed  = t ^. #computed
  }
