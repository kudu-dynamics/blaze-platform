---

# Flint Codebase Reference

## Project Structure (monorepo: blaze-platform)

```
blaze-platform/
├── flint/           -- Vulnerability detection tool (this package)
├── blaze/           -- Core binary analysis library (CFG, PIL, paths, graphs)
├── ghidra-haskell/  -- JNI bindings to Ghidra (binary analysis backend)
├── binaryninja-haskell/  -- BinaryNinja backend (compile-time optional)
```

## Flint Pipeline (high level)

1. **Load binary** via Ghidra (or BinaryNinja) backend
2. **Build CfgStore** — thread-safe cache of CFGs, functions, analysis results
3. **Onion passes** — iterate over functions, sample paths, check patterns
4. **Collect CallableWMI results** from store → output as JSON (FlintResult)

## Key Flint Files

| File | Purpose |
|------|---------|
| `app/flint/Main.hs` | CLI entry, binary loading, onion orchestration, JSON output |
| `src/Flint/Query.hs` | Path sampling, onion analysis, pattern checking (~1500 lines, core logic) |
| `src/Flint/Analysis/Path/Matcher.hs` | Pattern DSL matcher (StmtPattern → path → MatcherState) |
| `src/Flint/Analysis/Path/Matcher/Primitives.hs` | CallableWMI creation from match results |
| `src/Flint/Analysis/Path/Matcher/Primitives/Library.hs` | Primitive definitions (freeHeap, doubleFreePrim, etc.) |
| `src/Flint/Types/Analysis/Path/Matcher.hs` | MatcherState, StmtPattern, ExprPattern types |
| `src/Flint/Types/Analysis/Path/Matcher/Primitives.hs` | PrimSpec, CallableWMI, FuncVar, FuncVarExpr types |
| `src/Flint/Types/Query.hs` | Query types (QueryTarget, QueryExpandAll, etc.) |
| `src/Flint/App.hs` | Backend abstraction (Ghidra vs BinaryNinja) |

## Key Flint Types

```haskell
-- A vulnerability pattern to search for
data PrimSpec = PrimSpec { name :: Text, vars :: ..., locations :: ... }

-- A concrete instance of a pattern found in a binary
data CallableWMI = CallableWMI
  { prim :: PrimSpec, func :: Func, callDest :: Func
  , varMapping :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
  , locations :: HashMap (Symbol Address) (Either ExternFunction Address)
  , linkedVars :: HashSet FuncVar }

-- How external inputs flow in
data FuncVar = Arg Word64 | Ret | Global Pil.Expression

-- Pattern DSL
data StmtPattern = Stmt _ | AvoidUntil _ | Where _ _ | Necessarily _ _
  | Location _ _ | Primitive _ _ | Star | And _ _ | Or _ _ | EndOfPath

-- Output
data FlintResult = FlintResult { baseAddress :: Address, callableWMIs :: [(Text, [CallableWMIBlob])] }
```

## Key Query.hs Functions (onion flow)

- `onionFlow` (~line 1277) — main loop: run N onion passes
- `steadyStateOnionFlow` — stochastic mode with weighted function sampling
- `onionSinglePass` (~line 1228) — one pass over all functions
- `onionCheckFunc` — sample paths from one function, check each against prims
- `onionCheckPathForPrim` (~line 1111) — match single path to single prim, create CallableWMI if match
- `checkPathForPrims` / `checkFuncForPrims'` — concurrent multi-prim checking variants
- `samplesFromQuery` — sample N paths through a function via various query strategies

## Key Blaze Types & Modules

```haskell
-- CFG
data Cfg n = Cfg { graph :: ControlFlowGraph n, rootId :: NodeId UUID, nextCtxIndex :: CtxId }
type PilCfg = Cfg PilNode
type PilNode = CfNode [Stmt]

-- CFG Nodes
data CfNode a = BasicBlock _ | Call _ | EnterFunc _ | LeaveFunc _ | Grouping _

-- Paths (linear root-to-leaf through CFG)
data Path a = Path { nextCtxIndex, outerCtx, path :: AlgaPath BranchType UUID a }
type PilPath = Path (CfNode [Stmt])

-- PIL Statements
data Statement expr = Def _ | Store _ | Call _ | BranchCond _ | Jump _ | Ret _ | Constraint _ | ...

-- PIL Expressions
data Expression = Expression { size :: Size Expression, op :: ExprOp Expression }
-- ExprOp has 60+ variants: VAR, LOAD, ADD, CMP_E, CALL, CONST, FIELD_ADDR, etc.

-- Variables
data PilVar = PilVar { size, ctx, version, symbol, isParam, location :: VarLocation }

-- Call destinations
data CallDest expr = CallAddr _ | CallFunc Function | CallExtern ExternFunction | CallExpr expr | ...
```

## Key Blaze Files

| File | Purpose |
|------|---------|
| `blaze/src/Blaze/Types/Cfg.hs` | Cfg, CfNode, BranchType |
| `blaze/src/Blaze/Types/Pil.hs` | Statement, Expression, ExprOp (the big one) |
| `blaze/src/Blaze/Types/Cfg/Path.hs` | PilPath type |
| `blaze/src/Blaze/Types/Function.hs` | Function, ExternFunction |
| `blaze/src/Blaze/Types/Pil/Common.hs` | PilVar, Ctx, CtxId, Symbol |
| `blaze/src/Blaze/Path.hs` | Path sampling algorithms (random, containing, sequence) |
| `blaze/src/Blaze/Cfg/Path.hs` | CFG-specific path sampling (sampleRandomPathsContaining) |
| `blaze/src/Blaze/Pil/Analysis.hs` | Variable analysis (defined vars, referenced vars, memory groups) |
| `blaze/src/Blaze/Cfg/Loop.hs` | Loop detection, backedges, unrolling |
| `blaze/src/Blaze/Cfg/Interprocedural.hs` | Call expansion, context handling |
| `blaze/src/Blaze/Types/CachedMap.hs` | Thread-safe STM cache (used by CfgStore) |
| `blaze/src/Blaze/Types/Graph.hs` | Graph typeclass (succs, preds, reachable, dominators) |

## ghidra-haskell

- JNI bindings to Ghidra via inline-java (tweag/jvm). Requires JDK 17+.
- Runs a single JVM instance; all ops go through `Ghidra` monad.
- Extracts: functions, CFGs, P-code (raw + high/decompiled), variables, types, xrefs, basic blocks.
- Key modules: `Ghidra.Core` (JVM lifecycle), `Ghidra.State` (DB open/analyze), `Ghidra.Function`, `Ghidra.Pcode`, `Ghidra.BasicBlock`, `Ghidra.Variable`, `Ghidra.GhidraDataTypes`.
- BinaryNinja backend exists at `binaryninja-haskell/`, compile-time toggled via `FLINT_SUPPORT_BINARYNINJA`.

## CfgStore (thread-safe analysis cache)

- Lives in Query.hs / blaze's CachedMap
- Stores CFGs, callablePrims, analysis results per function
- `callablePrims :: CachedMap (PrimSpec, Func) (HashSet CallableWMI)` — where all results accumulate
- Thread-safe via STM; supports concurrent onion passes

## Results Flow

```
Pattern match on path → MatcherState (bound vars, locations)
  → mkCallableWMI → CallableWMI
    → stored in CfgStore.callablePrims
      → toFlintResult → FlintResult { baseAddress, callableWMIs }
        → JSON output
```

## Building & Running

- Haskell project using Cabal (check `flint.cabal` for deps and flags)
- `flint` binary is the main CLI (`app/flint/Main.hs`)
- `flint-shell` is the interactive REPL (`app/flint-shell/`)
- Key CLI options: `--onion-depth`, `--path-sampling-factor`, `--do-not-use-solver`, backend selection

## flint-mcp (MCP Server)

- **Source**: `app/flint-mcp/Main.hs`
- **Library**: [`mcp-server`](https://github.com/drshade/haskell-mcp-server) (git dep in `stack.yaml`)
- **Build**: `stack build flint:flint-mcp`
- **Run**: `flint-mcp [--backend BACKEND] [--doNotUseSolver] [--analysisDb DB] [-t TYPEHINTS]`
- **Transport**: stdio (JSON-RPC over stdin/stdout, logs to stderr)
- Reuses the same `ShellState`, `CfgStore`, and shell command infrastructure as `flint-shell`
- Tool calls are dispatched through `dispatchCommand` from `Flint.Shell.Command`

### MCP Tools Exposed

| Tool | Shell Equivalent | Description |
|------|-----------------|-------------|
| `load_binary` | — | Load a binary file; resets all state |
| `list_functions` | `functions [filter]` | List/filter functions in the binary |
| `sample_paths` | `sample <func> [count] [@ addrs]` | Sample execution paths from a function |
| `show_paths` | `show <path_ids>` | Display PIL statements for paths |
| `pshow_path` | `pshow <path_id> [addrs]` | Raw Haskell PIL types |
| `reduce_paths` | `reduce <path_ids>` | Copy/constant propagation |
| `free_paths` | `free <path_ids>` | Release cached paths |
| `list_paths` | `paths` | List cached paths |
| `list_wmis` | `wmis` | List vulnerability patterns |
| `check_wmi` | `check-wmi <name> <path_ids>` | Check paths against vuln patterns |
| `solve_paths` | `solve <path_ids>` | Z3 SMT satisfiability check |
| `set_solver` | `set solver on\|off` | Toggle solver on/off |

### Typical AI Workflow
`load_binary` → `list_functions` → `sample_paths` → `reduce_paths` → `show_paths` → `check_wmi`

### Driving flint-mcp from CLI

flint-mcp uses JSON-RPC over stdio. Write all commands to a file and pipe them in — no sleeps needed, the server processes them sequentially and responds immediately:

```bash
cat > /tmp/mcp_cmds.txt << 'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"claude","version":"1.0"}}}
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"load_binary","arguments":{"file_path":"path/to/binary.gzf"}}}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"list_functions","arguments":{"filter":""}}}
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"sample_paths","arguments":{"function":"main","count":"5"}}}
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"reduce_paths","arguments":{"path_ids":"0 1 2 3 4"}}}
{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"show_paths","arguments":{"path_ids":"5 6 7 8 9"}}}
EOF

stack exec flint-mcp -- < /tmp/mcp_cmds.txt 2>/dev/null
```

- **stdout**: JSON-RPC responses (one per line, matching request IDs)
- **stderr**: log messages (loading, request processing) — redirect to `/dev/null` or a log file
- The `initialize` handshake (id 1) must be the first message
- Path IDs are sequential across the session: if you sample 5 paths (IDs 0-4), then `reduce_paths` creates new paths starting at ID 5
- Reduced paths are generally much easier to read — always reduce before showing
- WMI names are case-insensitive (e.g. `danglingPtr` or `danglingptr` both work)
- Use `all` as the WMI name to check all primitives at once
