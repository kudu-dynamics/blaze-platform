# Blaze Platform

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

