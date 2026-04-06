# Blaze Platform

## Version 0.26.0406b
- C AST decompiler: new `CStmt`/`CExpr` types (14 statement constructors, 14 expression constructors) that transform Ghidra's raw ClangAST token stream into structured, readable C99 source code
  + `convertFunction` / `convertBlock` / `convertExpr` — recursive transformer handling nested control flow, multi-arg function calls, prefix/postfix/binary operators, casts, pointer/array/field access, compound assignments, for-loop init declarations
  + `renderStmts` / `renderExpr` — indented C source renderer
  + Every node carries `[AddrRange]` annotations linking back to binary addresses
  + Handles inconsistent Ghidra token types via text-based matching (`isTokenText`) instead of type-specific checks
- `decompile_function` MCP tool and `decomp` shell command — takes function name or hex address, returns decompiled C with full control flow structure (for, while, do-while, if/else, switch/case)
- `BinaryImporter.decompileFunction` typeclass method with Ghidra backend implementation
- Test binary (`loop-test.gzf`) and 16 new tests: unit tests for C AST construction + integration tests decompiling real functions (nested_for, bubble_sort, find_first_negative, classify, test_switch, process)

## Version 0.26.0406a
- Update `DevelopersGuide.md` to include dependency introduced by LMDB

## Version 0.26.0405
- `--context-depth N` option for `sample` command (shell + MCP) — samples paths entering the target function through N levels of callers, using reverse call graph traversal and targeted callsite sampling
- `!!` view suffix for `show` and `psum` — strips outer calling context stmts, shows only the target function with resolved caller args in the header (e.g. `process_auth(ctx=conn, hdr=buf+sVar3)`)
- `PathViewMode` type (`ViewReduced` / `ViewRaw` / `ViewContextStripped`) replaces the boolean raw flag in `PathRef`
- `CallerContext` type in `CachedPath` stores target function stmt addresses and param bindings for the `!!` view
- Fix exponential expression blowup in `aggressiveExpand` — `exprNodeCount` caps unfolded tree size at 64 nodes for both `varSubstMap` and `memSubstMap`; oversized Defs kept as named variables instead of inlined
- Fix `expandToTargetsStrategy` failing on Call node targets — Call nodes no longer require a random return node in the path sequence, which caused ~60% failure rate on functions with multiple returns
- `context_depth` parameter for `sample_paths` MCP tool

## Version 0.26.0403b
- `psum` command (shell + MCP) — path summary showing only calls and non-stack-local memory ops, filtering out scalar comparisons, loop counters, and stack bookkeeping
- `global_xrefs` command (shell + MCP) — find functions referencing a global address or named symbol via Ghidra's reference manager (no decompilation needed)
- `Ghidra.Symbol` module — JNI bindings to Ghidra's SymbolTable for global symbol lookup by name
- `BinaryImporter.lookupGlobalSymbol` — resolve symbol names to addresses
- `ShellState` gains `xrefsTo` and `lookupSymbol` callbacks for on-demand Ghidra queries

## Version 0.26.0403a
- FlintDB persistent analysis cache — decompilation results survive across sessions via LMDB + cereal binary serialization (`.flintdb` files auto-created next to `.gzf`)
- `PersistentCalc` type wraps `CachedCalc` with optional LMDB persistence layer; drop-in replacement API
- `Blaze.Persist.Lmdb` module — LMDB wrapper with dedicated bound writer thread for safe concurrent writes
- `Blaze.Concurrent.WorkerPool` — shared bounded parallelism via QSem, auto-sized to CPUs - 2
- `analyze-all` command in flint-shell and flint-mcp — parallel pre-analysis of all functions
- `Serialize` instances (cereal) for the full PIL/Cfg type hierarchy (~100 types)
- Warm load benchmarks: Dive_Logger 82 funcs in 4s, udapi-server 643 funcs in 16s, curl 1866 funcs in 43s — no Ghidra needed
- DB sizes ~3x smaller than JSON: Dive_Logger 5MB, udapi-server 253MB, curl 860MB

## Version 0.26.0402
- Renamed `FunctionMarker`/`FuncMarker` → `FunctionRef`/`FuncRef` throughout
- CFG cache keys reverted to `Function` (not `FunctionRef`) — CFG computation requires decompilation anyway, so marker keys added needless `toFunctionRef`/`resolveFunction` roundtrips
- `CachedCalc.defaultCalc` — optional `(k -> IO v)` fallback for unknown keys, so caches auto-compute on demand without pre-registering every key
- CfgStore CFG-layer caches (`cfgCache`, `acyclicCfgCache`, `acyclicDescendantsCache`, `callSitesInFuncCache`) use `defaultCalc` instead of pre-registered `setCalc` per function
- `CfgStore.getCallSitesInFunc` getter for call sites within a function's CFG
- `getFunction` handles `EXTERNAL` address space via `mkExternalAddress` — previously failed for extern addresses, breaking `externFuncCalc` resolution
- `computeAttackSurfaceWorkingSet` takes `[FunctionRef]` and resolves lazily — only decompiles attack surface entry points + BFS callees, not all functions
- Added dynamic taint propagation in `flint-shell` and `flint-mcp`
- Added dynamic pattern generation in `flint-shell` and `flint-mcp`

## Version 0.26.0401
- `FunctionRef` / `FuncRef` types in Blaze — lightweight function refs without params, used for call graphs and CfgStore function lists
- Lazy decompilation: `getFunctions` no longer decompiles every function at startup; params are resolved on demand through `CfgStore.funcCalc` when a function's CFG is first accessed
- `CachedCalc` migrated from Flint to Blaze with deadlock fix: `TMVar v` → `TMVar (Either SomeException v)` so waiting threads always wake up on error; stderr logging before re-throw
- `CachedCalc.getOrCompute` — compute-once semantics with caller-supplied computation
- GhidraImporter uses `CachedCalc` for `highFnCalc` instead of `CachedMap`; pattern matches replaced with lens access throughout Ghidra import modules
- `CfgStore.funcCalc :: CachedCalc FunctionRef Function` for lazy ref-to-full-function resolution

## Version 0.26.0330
- Fix target sampling (`sample func @ 0xaddr`) failing when xref addresses point to raw instructions folded into high P-code CALL operations by the decompiler
- BasicBlock address ranges now extend to cover all raw instruction addresses via varnode `pcAddress` fields (`mkCfNodePcodeBlock`) and gap-filling after call splitting (`inheritOrigBounds`)
- Acyclic `StrictDescendantsMap` cache in `CfgStore` — used by `expandToTargetsStrategy` so the loop sampler's `filterReachable` doesn't claim nodes are reachable through back edges it won't follow
- `inspect` shell command and `inspect_address` MCP tool — shows raw instruction, Ghidra basic block range, and P-code at any address (`Ghidra.Inspect` module, `BinaryImporter.inspectAddress` typeclass method)

## Version 0.26.0327b
- Fix xrefs reporting thunk functions that don't exist in the function list — resolve thunks in string xrefs and follow thunks for internal function call sites (not just externs)
- Remove `tryResolveAsExternThunk` heuristic that misclassified default-named functions (e.g. `FUN_0042b904`) as externs if they called any extern; trust Ghidra's `isThunk` instead

## Version 0.26.0327
- Loop summarization replaces loop unrolling as the default sampler — single abstract iteration with `_looping`/`_exit` vars and entry/exit constraints
- Indented `----> loop` / `<---- end loop` display with unique CtxIds, nests with interprocedural expansion
- `--unrollLoops` flag to use old unrolling behavior for comparison
- Function param names in path headers, suppress `@0` on default-context vars

## Version 0.26.0326
- Fix O(2^n) type checker blowup: type-check raw stmts BEFORE aggressiveExpand, so the checker sees VAR references (O(1)) instead of exponentially duplicated expression trees from copy propagation. cgc_malloc paths: 40s -> 14ms. Dive_Logger max live: 1079MB -> 266MB.
- `IsExpression` typeclass in Blaze enabling `aggressiveExpand` to work on both plain `Expression` and typed `InfoExpression` statements
- Generalized `substExprInExpr`, `aggressiveExpand`, and related helpers to work on any `IsExpression` type
- Fix redundant `Star` before `AvoidUntil` in 3 vulnerability pattern definitions (removes O(n^2) matcher behavior)
- `typecheck` command for flint-shell and flint-mcp (use `N!` for raw/unexpanded comparison)
- Bang patterns on StateT accumulators in blaze (SequenceChooserState, ConstraintGenState, UnifyState, etc.)

## Version 0.26.0320b
- JNI batching: Java helpers (`PcodeHelper.java`) collapse per-varnode and per-op JNI calls into one call per block (`extractBlockData`). Pcode extraction 3.8x faster (20.1s → 5.3s on miniupnpd)
- Call graph function cache: `getCallGraphCached` pre-builds `HashMap Address Func` from known functions, skipping redundant `getFuncFromJFunction` JNI calls. Call graph 95x faster (26.6s → 0.28s on miniupnpd)
- Lazy `AddressSpace` caching in `HighVarCache` — unknown spaces fetched once via JNI, then cached
- `mkAddressFromParts` constructs addresses from packed offset+spaceId using cached `AddressSpaceMap`, avoiding JNI
- `opcodeMap` maps Ghidra opcode ints to `BarePcodeOp`, replacing `getMnemonic` String round-trips
- `[timing] getCallGraph` logging in `CfgStore` (forced when `--profileSampling` enabled)
- Build: `make res/pcode-helper.jar` in ghidra-haskell, Dockerfile updated

## Version 0.26.0320a
- Initial implementation for execution path reasoning

## Version 0.26.0319b
- PilCfg loading perf: `calcStrictDescendantsMap` uses SCC decomposition + Integer bitsets instead of N independent BFS traversals (~25x faster for 160 nodes, ~20x for 712 nodes)
- Hash `CfNode` by UUID only instead of entire statement list (speeds up all PilCfg HashMap/HashSet ops)
- Hash `Function`/`ExternFunction` by address only instead of full struct including params
- Cache `addressSpaceMap` in `GhidraState` — computed once at DB open, not per block
- Cache `HighVariable` processing per function and `Address.NO_ADDRESS` lookup — eliminates thousands of redundant JNI calls

## Version 0.26.0319a
- Path sampling perf: smart `CfgInfo` caching avoids redundant UUID regeneration (4-130x speedup on warm cache)
- Removed dead `CfgInfo` fields (`descendantsMap`, `acyclicStrictDescendantsMap`) — never read, each O(n²)
- Split `acyclicCfg` into separate lazy `CachedCalc` so `expandAllStrategy` never pays for it
- `--profileSampling` flag for flint, flint-shell, flint-mcp — prints per-path timing breakdown to stderr
- Deterministic UUID offsetting for duplicate call expansions (`getOffsetFuncCfgInfo`) — same expansion pattern always produces identical UUIDs, enabling `nub` to dedup across call sites
- Fix `expand_call` and `pshow` address matching on 32-bit binaries (was assuming 64-bit address space)

## Version 0.26.0317
- Collapse nested `ARRAY_ADDR`/`FIELD_ADDR` ops in path reduction (e.g. `ptr[1][1][1]` → `ptr[3]`)
- Fix extern callsite lookup: follow through PLT/thunk stubs via `getFunctionThunkAddresses`
  + New `callSitesToFuncCache` for callee→callers lookup
- `calls` command (renamed from `functions-calling`) works for both internal and extern functions
- `sample` command: reordered args to `sample [count] <func>`, fixed address space on target addrs
- Filter out low-address strings
- Tests for path sampling

## Version 0.26.0316d
- `strings` / `string-xrefs` commands in shell and MCP for string navigation
  + `strings` lists all strings; filter by quoted substring or look up by hex address
  + `string-xrefs` finds functions referencing a string address; quoted filter xrefs all matches grouped by string
  + Alias `sxrefs` for `string-xrefs`
- `XrefImporter` typeclass in Blaze (`Blaze.Import.Xref`) with Ghidra backend implementation
- `getStringsMap` method on `BinaryImporter` typeclass
- String map and xrefs precalculated at init and cached in `CfgStore`

## Version 0.26.0316c
- Resolve constant strings in Ghidra backend

## Version 0.26.0316b
- `functions-calling` / `functions_calling` command in shell and MCP: find call sites to a given extern function

## Version 0.26.0316a
- `functions` command lists external/imported functions alongside internals; `-e`/`-i` flags to filter

## Version 0.26.0314d
- Undid the "pure func whitelist" in `aggressiveExpand`

## Version 0.26.0314c
- Added `expand` / `expand_call` command to shell and MCP for interprocedural path expansion
  + Expand a callsite in a cached path by sampling fresh callee paths or stitching in specific cached paths
  + Caller args are propagated into callee via EnterFunc/LeaveFunc nodes
  + Supports chaining: expand multiple callsites in the same path sequentially
  + Validates callsite address, callee function match, and rejects indirect calls
- `CachedPath` now preserves the graph-structured `PilPath` for use in expansion

## Version 0.26.0314b
- Added `tag` / `free-untagged` commands to shell and MCP for naming paths with human-readable tags
- Tag names can be used in place of numeric path IDs in all commands (`show`, `free`, `solve`, `check-wmi`, `pshow`)

## Version 0.26.0314a
- Reduced paths are now the default in shell and MCP; use `N!` suffix for raw/unreduced view
- HLint cleanup in Shell.Types

## Version 0.26.0213b
- Dynamically generate buildkit

## Version 0.26.0313a
- Eliminated duplication in `DevelopersGuide.md`

## Version 0.26.0312b
- Added `Bound` constructor to `ExprPattern` for referencing previously bound vars
- Changed `CallsPrimitive` and `SubPrimitive` to take `[(Symbol, ExprPattern)]` instead of `HashMap`

## Version 0.26.0312a
- Changed `Prim.stmtPattern` and `BugMatch.pathPattern` from `[StmtPattern]` to `StmtPattern`
- Updated matcher API (`match_`, `match`, `singleMatch`, etc.) to take single `StmtPattern`

## Version 0.26.0310
- Inline primitive matching through SubPrimitive

## Version 0.26.0309b
- added MCP version of Flint

## Version 0.26.0309a
- Initial references abstraction integration with call-based suboperations.
- Documentation improvements with CVE example.

## Version 0.26.0305b
- Added `danglingPtr` WMI pattern with tests
- Fixed `Star` in relation to `AvoidUntil`. It should be in the `until`.

## Version 0.26.0305a
- Fixed targeted path sampling (`sample func N @ addr`) failing with `NoReqNodesCanBeReached` in functions with loops
  + `expandToTargetsStrategy` now uses an acyclic `StrictDescendantsMap` matching the acyclic CFG it traverses

## Version 0.26.0304d
- Fixed CfgStore so that it lazily loads Cfgs for functions.
- Now initial load times are much faster for `flint-shell`

## Version 0.26.0304c
- Added steady-state onion analysis mode (`--steadyState`)
  + Stochastically picks functions and samples paths on-demand instead of pre-sampling all paths up front
  + Weighted random function selection: larger, under-sampled functions are prioritized
  + Per-function visit count tracking for path diversity (uses existing `VisitCounts`/`SequenceChooserState` machinery)
  + Immediate per-insert squashing of CallableWMIs
  + Periodic intermediate results written to output file (overwrites each time)
  + Terminates when all functions meet their sample target
- Added attack surface option (`--attackSurface FILE`, `--attackSurfaceDepth N`)
  + BFS from user-specified entry functions through `CfgInfo.calls` (avoids expensive CallGraph)
  + Limits analysis to functions within N hops of the attack surface
- Added `--reportInterval N` to control how often intermediate results are output
- Changed existing `onionFlow` to sample paths on-demand per pass instead of pre-caching all paths
- Added `sampleSinglePathWithVisitCounts` to `Flint.Cfg.Path` for visit-count-aware path sampling

## Version 0.26.0304b
- Moved blacklist filtering into `Store.initWithTypeHints` so blacklisted functions are never loaded or sampled
  + Blacklist is now applied at store initialization, filtering out functions before CFGs are fetched
  + Removed downstream blacklist filtering from `onionFlow`

## Version 0.26.0304a
- Added `flint-shell`, an interactive shell for exploring binaries
  + Commands: `functions`, `sample`, `show`, `reduce`, `solve`, `free`, `paths`, `wmis`, `check-wmi`
  + Path caching by integer ID for referring to sampled paths across commands
  + Targeted sampling with `@` syntax to sample paths through specific addresses
  + Haskeline-based REPL with tab completion, history, and command aliases
  + Bracket/range syntax for path IDs: `[0..5]`, `[1, 3, 7-10]`
  + Command layer decoupled from REPL for future MCP server reuse

## Version 0.26.0219
- Restored functionality of --isKernelModule option

## Version 0.26.0216
- Added pure tests for some WMIs in LibarySpec
- Fixed bug where the type of the return var in a Def Call wasn't being set properly in the Matcher
  This prevented matching the return of a function call to any later uses.

## Version 0.26.0126b
- Added Loop analysis type modules for HighCfg and HighCfNode
- Added HighCfg Dot graph visualizer module

## Version 0.26.0126a
- Initial addition of references abstraction. Not yet integrated into analysis
process.

## Version 0.26.0123a
- Fixed bug where `onionSinglePass` used non-squased CallableWMI snapshot instead
of the squashed CallableWMI snapshot.
- Allow users to disable the `squashCallableWMIs` feature by passing a `--noSquash`
command-line option.

## Version 0.26.0122b
- Added more documentation in `DevelopersGuide.md` related to the SMT output. Also,
added table of contents inside the `CHANGELOG.md`.

## Version 0.26.0122a
- Removed some `-02` optimization flags from `.gitlab-ci.yml` and the
`-fsimpl-tick-factor=200` GHC flag that boosted the tick limit for
[`BranchContextSpec.hs`](./blaze/test/general/Blaze/Cfg/Solver/BranchContextSpec.hs).

## Version 0.26.0121b
- Fixed `GLOBAL` formatting in Flint's `SMTish` module. Previously, it only
showed the symbol of the global, or the address if there was no symbol. Now
we show it as a JSON record field with both name and address.

## Version 0.26.0121a
- Corrected `.gitignore` to ignore `stack.yaml.lock` and added changelog
update enforcement to CI.

## Version 0.26.0120b
- Eliminated unnecessary duplication of function name data in `CallOp`s and
`TailCallOp`s.

## Version 0.26.0120a
- import Ghidra types
  + optionally import inferred types through Importer
  + ie you can specify good types for a func in Ghidra, then tell Flint to use the Ghidra types for that function
  + improved our own type inference

## Version 0.26.0115
- Created SMTish module that converts PIL expressions in Flint output to an SMT-like syntax
- includes CLI option

## Version 0.26.0107
- This change incorporates global detection into a CallableWMI like WriteToGlobalKernel.
- Instead of flagging every CONST_PTR as a global variable, we modify Pil to detect global addresses that reside in `.data` and `.bss`. We ignore `.rodata` due to the data being read-only.

## Version 0.25.1208
- Allow case matching on type of exprs in Flint.
- Pipes inferred types along with Stmts into matcher
- Added patterns for matching on types

## Version 0.25.1122
- Added a "blacklist" feature to flint
  + sometimes flint encounters a nasty function that it hangs on
  + this feature allows users at the CLI to specify functions not to check

## Version 0.25.1120
- This change adds a function called sqCallableWMI that removes CallableWMIs that point to the same location. We now merge these into one CallableWMI (unique to one location). Also, we only extract constraints and varMappings that are the same across all duplicate CallableWMIs.

## Version 0.25.1118
- Produce STACK_LOCAL_ADDR Pil statements for PTRSUB pcodeops that get the address of a local variable.

## Version 0.25.1027
- Added AddressSpace to Address type. Now we can tell if an address is in stack, regular RAM, etc

## Version 0.25.1001
- Swapped out hand-made matcher backend to use LogicT. This gives us more control over backtracking and fetching multiple results.

