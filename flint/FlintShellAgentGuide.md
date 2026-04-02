# Flint Shell Agent Guide

## Prerequisites

### MCP Setup

When using Flint via MCP (`.mcp.json`), the JVM classpath uses relative paths
(`res/ghidra.jar`, `res/pcode-helper.jar`) that must resolve from the `flint/`
directory. The `cwd` field in `.mcp.json` is not always honored by MCP
launchers, so use a wrapper script to guarantee the correct working directory.

1. Create `flint/run-mcp.sh`:

```bash
#!/bin/bash
cd "$(dirname "$0")" || exit 1
exec stack exec flint-mcp -- "$@"
```

2. Make it executable:

```bash
chmod +x flint/run-mcp.sh
```

3. Point `.mcp.json` (at the repo root) at the wrapper:

```json
{
  "mcpServers": {
    "flint-mcp": {
      "type": "stdio",
      "command": "/absolute/path/to/flint/run-mcp.sh",
      "args": [],
      "cwd": "/absolute/path/to/flint"
    }
  }
}
```

4. Restart the Agentic session after any change to `.mcp.json` or the jar
   files — MCP servers cannot be reloaded mid-session.

**Symptom if this is wrong:** `load_binary` and `list_functions` work, but
`sample_paths` fails with `java.lang.NoClassDefFoundError: PcodeHelper`. This
happens because the JVM starts lazily (only when sampling needs pcode analysis),
and by that point the relative classpath cannot find the jar.

---

This guide is for an agent using Flint via MCP first and `flint-shell` second
to verify bug shapes quickly from a binary, a `.gzf`, or a crash site.
If the current agent runtime does **not actually expose** the Flint MCP tools,
that counts as MCP being unavailable for this investigation, and you should
switch to `flint-shell --verbosity Debug` immediately instead of pretending MCP
is usable.
The default operating mode is that the user gives you a target and says "go
find bugs." The goal is not to write a perfect spec on the first try. The goal
is to:

- find the right function and path
- anchor the investigation on the crash site or suspicious address when one is available
- teach Flint the smallest missing semantic fact
- reuse built-ins when they fit
- write `prim-def` only for the bug-specific part

## Golden Rules

- Treat sampled path IDs as ephemeral. Re-run `paths` after every new sample.
- Prefer semantic anchors over raw addresses: allocator/copy/free/use sites,
  helper calls, string references, and field updates.
- If a path through an allocation site keeps landing on the null-return or
  bailout branch, retarget the later semantic anchor first. The later anchor
  usually forces the earlier setup.
- Prefer MCP for the main workflow. Use `flint-shell` as a fallback, for local
  debugging, or when you need interactive exploration that is awkward over MCP.
- Whenever this guide says to use `flint-shell`, it means
  `flint-shell --verbosity Debug`.
- The shell/MCP command pairs in this guide are alternatives, not instructions
  to drive both interfaces concurrently.
- On MCP, the first binary-touching command is `load_binary`. In
  `flint-shell`, the binary is loaded when you start the shell.

## CVE Bug Hunt Workflow

When the user gives you a CVE number and a binary, follow this order before you
start doing serious binary work:

1. **Look up the CVE.**  Use a web search for the CVE ID (e.g.
   `CVE-2015-3824 libstagefright`) and read the advisory, any linked patches,
   and write-ups.  You need:
   - the **vulnerable function name** (e.g. `MPEG4Extractor::parseChunk`)
   - the **bug class** (integer overflow, use-after-free, stack overflow, …)
   - the **triggering operations** (e.g. `operator.new[]` with an overflowed
     size followed by `memcpy` with the original size)
   - any **patch diffs** — these tell you exactly which checks were missing
   - the **source code** surrounding the bug if available — this is the
     Rosetta Stone for mapping the vulnerability to binary-level constructs

2. **Map the source pattern to binary-level names.**  The CVE source or patch
   shows C/C++ code.  You need to translate that into what Ghidra / Flint will
   actually see in the binary:
   - `new uint8_t[expr]` → `operator.new[](expr, nothrow)` (two args;
     the second is the nothrow sentinel — match it with `_` in prim-def)
   - `malloc(expr)` → `malloc(expr)` (one arg)
   - `memcpy(dst, src, len)` → `memcpy(dst, src, len)`
   - wrapper calls may appear instead of the standard name — use `show`
     output to confirm the actual call names Flint sees

   If the source shows `new uint8_t[size + chunk_size]` followed by
   `memcpy(buffer, data, size)`, the binary pattern you are looking for is
   `operator.new[]` followed by `memcpy` where the allocation pointer is
   reused.

3. **Load the binary and do first-pass discovery.**  Prefer MCP for this.
   Start with `load_binary`.  Use `flint-shell --verbosity Debug` only if MCP
   is unavailable or you need interactive fallback.  Then use discovery
   commands before writing patterns:
   - `functions [filter]` / `list_functions`
   - `calls <func_name>` for obvious anchors like `memcpy`, `malloc`, `free`,
     and `__memcpy_chk`
   - `strings` / `string-xrefs` if the CVE patch mentions literals or atom names
   - `input-genesis` if the vulnerable entry point is unknown

4. **Match CVE details to binary functions.**  Use `functions [filter]` with
   keywords from the CVE (the vulnerable function name, not just the feature
   area).  Be precise: if the CVE says `parseChunk`, look for `parseChunk` —
   do not assume a nearby helper like `extract3GPPGlobalDescriptions` is the
   bug site just because it handles related data.
   If names collide, use the address Flint prints.  For example, Stagefright
   has both `MPEG4Extractor::parseChunk` and `MPEG4Source::parseChunk`; only
   the extractor one contains CVE-2015-3824.

5. **Sample a candidate function once before targeting addresses.**  Do a
   small untargeted sample first using **one interface**:
   - MCP: `sample_paths(function=<func_addr>, count=10)`
   - shell: `sample 10 <func_addr>`

   Then inspect with `paths`, `show <id>`, `show <id!>`, and `pshow <id>` to
   learn the call landscape in the current binary.

6. **Derive interesting addresses from the current binary.**  Good sources:
   - callsites returned by `calls memcpy`, `calls free`, `calls malloc`, etc.
   - callsites you see in `show` / `show !` / `pshow`
   - the crash PC or a patch diff, if the CVE already gives one
   - nearby strings, helper calls, or field accesses mentioned in the patch

   Use `inspect <addr>` / `inspect_address` to confirm the address is really
   the operation you think it is.

7. **Resample with targeted addresses.**  Use
   `sample [count] <func_or_addr> @ <addr1> <addr2>` to force paths through
   the crash site or semantic anchors.  Prefer the **later** anchor first
   (copy/use/free site) if sampling through the earlier one keeps producing
   error-handling paths.

8. **Disable the solver before matching.**  Run `set solver off` before
   `check-wmi`.  The Z3 solver can reject valid matches by over-constraining
   the path.  Always start with the solver off for initial pattern matching;
   you can re-enable it later to prune false positives if needed.

9. **Try built-ins before custom patterns.**  Run `check-wmi all <id>` on the
   concrete path you just inspected.  If a built-in or existing primitive hits,
   reuse it.

10. **Teach the smallest missing semantic fact.**  If a wrapper function is
   hiding the operation you care about, add `stdlib-add` or `taint-add`
   before writing a larger `prim-def`.

11. **Write the prim-def based on the bug class**, not on what you see in a
   random path.  Classic shapes:
   - **Integer-overflow heap overflow:**
     `{ * ; alloc: def p = call operator.new[](sz, _) ; * ; copy: call memcpy(@p, src, len) }`
   - **Use-after-free:**
     `{ * ; free: call free(p) ; * ; use: def v = call @p(args) }`
   - **Format string:**
     `{ * ; fmt: call printf(user_input) }`

12. **Confirm with `check-wmi`** on the sampled paths from the current run.

### Common mistakes

- **Skipping the CVE lookup.**  Without it you are guessing which function
  contains the bug.  A five-second web search saves minutes of sampling the
  wrong code.
- **Copying example addresses from this guide into a new binary.**  Every new
  investigation must derive its own function addresses, callsites, and path IDs.
- **Anchoring on feature-area functions instead of the vulnerable function.**
  Related helpers (e.g. `extract3GPP*`) may touch the same data but not
  contain the actual overflow / UAF / format-string.
- **Writing a pattern before sampling the current binary.**  Always sample and
  inspect at least one current path before you commit to a custom `prim-def`.
- **Writing a pattern for the wrong bug class.**  If the CVE says "integer
  overflow → heap overflow," your prim-def must capture alloc-then-copy, not
  an unbounded-write loop.
- **Never doing the first untargeted sample.**  Large functions like
  `parseChunk` have hundreds of paths, but that first small sample is often
  how you discover which addresses to target next.
- **Targeting only the early anchor when the success path is gated later.**
  If the allocator site keeps returning the null-check path, resample through
  the later copy/use site.
- **Leaving the solver on during initial matching.**  The Z3 solver can reject
  valid pattern matches by over-constraining path feasibility.  Always
  `set solver off` before your first `check-wmi` pass.  Turn it back on only
  after you have confirmed the pattern matches and want to prune infeasible
  paths.
- **Not mapping source-level constructs to binary-level names.**  The CVE
  source says `new uint8_t[n]` but the binary has `operator.new[](n, nothrow)`
  with two arguments.  Always inspect `show <id>` output to confirm the exact
  call names and argument counts before writing a prim-def.

## Fast Loop

Default order:

1. if the user gave an address, resolve the containing function and prefer targeted sampling
2. if the user gave only a name, discover candidate functions and check for duplicates
3. if you do not have a reliable address yet, sample the function once without targets to learn the current call landscape
4. derive the interesting address from the current binary, then re-sample through that address
5. inspect the sampled paths with `show <id>`, then `show <id!>` if reduction hid something important
6. **disable the solver** with `set solver off` before any matching
7. try built-ins on the actual path ID with `check-wmi all <id>`
8. add missing taint or stdlib semantics
9. write the smallest `prim-def` that captures the bug shape
10. re-run matching on the actual path IDs from `paths`

Do not assume path IDs or path counts are stable. Use `paths` after sampling.
Do not assume function names are unique. When names collide, sample by function
address rather than by bare name.

## Bug Hunt Inputs

The best inputs from the user are:

- a binary or `.gzf`
- a **CVE ID** — if provided, always look it up before doing binary work
  (see "CVE Bug Hunt Workflow" above)
- a crash PC, suspicious address, or block address
- a suspected function name if they have one
- the desired output, such as "explain the bug" or "give me a matching `prim-def`"

If the user gives a CVE ID, follow the CVE Bug Hunt Workflow first.  The CVE
advisory and any linked patches will tell you exactly which function to target
and what bug class to look for.

If the user gives only a binary and says "go find bugs," start from likely
entry points and input-bearing code. `input-genesis`, `functions`, and `calls`
are the right first tools.

## Discovery-First Binary Workflow

When you have a binary but **no exact address yet**, do this:

1. choose one interface
   - preferred: MCP
   - fallback: `flint-shell --verbosity Debug`
2. load the binary
   - MCP: `load_binary`
   - shell: start `flint-shell --verbosity Debug /path/to/binary`
3. discover candidate functions
   - `functions <name>`
   - `calls memcpy`, `calls free`, `calls malloc`, `calls __memcpy_chk`
   - `strings <keyword>` and `string-xrefs "<literal>"`
   - `input-genesis` if the entry path is unclear
4. sample a candidate function without target addresses
   - MCP: `sample_paths(function=<func_addr>, count=3)`
   - shell: `sample 3 <func_addr>`
5. inspect the current sample
   - `paths`
   - `show <id>`
   - `show <id!>`
   - `pshow <id>`
6. derive the current-binary addresses for the interesting operations
7. re-sample through the later semantic anchor
   - MCP: `sample_paths(function=<func_addr>, count=10, addresses="<copy_or_use_addr>")`
   - shell: `sample 10 <func_addr> @ <copy_or_use_addr>`
8. disable the solver and match
   - `set solver off`
   - `check-wmi all <id>`
   - only then add `taint-add`, `stdlib-add`, or a custom `prim-def`

MCP tool sequence:

- `load_binary`
- `list_functions`
- `calls`
- `sample_paths`
- `list_paths`
- `show_paths`
- `pshow_path`
- `inspect_address`
- `set_solver`
- `check_wmi`

## Targeted Sampling Workflow

When the user gives an address, make that the anchor:

1. identify the likely function with `functions [filter]` if needed
2. if the name is duplicated, use the function start address, not the name
3. sample through the target address with `sample [count] <func_or_addr> @ <addr>`
4. run `paths`
5. inspect `show <id>` and then `show <id!>` if the reduced path is too lossy
6. **disable the solver** with `set solver off`
7. run `check-wmi all <id>` before writing a custom pattern
8. write the smallest `prim-def` that still mentions the target semantic anchors

Example below is shown in `flint-shell` because it is shorter to read. Assume
the shell was started as `flint-shell --verbosity Debug`. In MCP, use the
equivalent tools on the same sequence of ideas; do not run both at once.

Example:

```text
flint> functions parseChunk
0x<addr_a> parseChunk
0x<addr_b> parseChunk
flint> sample 3 0x<addr_b>
flint> paths
flint> show <path_id>
flint> inspect <interesting_addr>
flint> sample 10 0x<addr_b> @ <later_anchor_addr>
flint> paths
flint> show <path_id>
flint> show <path_id!>
flint> set solver off
flint> check-wmi all <path_id>
flint> prim-def AllocThenCopy { * ; alloc: def p = call operator.new[](sz, _) ; * ; copy: call memcpy(@p, src, len) }
flint> check-wmi AllocThenCopy <path_id>
```

Notes:

- the stable anchors are the function address and the target address, not the sampled path ID
- use the path IDs printed by the current `paths` output, not IDs from an older run
- if `sample <name> @ <addr>` finds nothing and the function name is duplicated, retry with the function start address
- if `show <id>` does not contain the callsite you care about, `show <id!>` usually answers whether reduction hid it
- if the first target address only gives you an error-handling path, retarget the later semantic anchor and sample again

## Worked Example: CVE-2015-3824

This is the shortest successful workflow for the `tx3g` Stagefright bug shape.
Use it as a model for similar integer-overflow heap-overflow CVEs.

Source-side facts from the patch:

- vulnerable function: `MPEG4Extractor::parseChunk`
- bug class: integer overflow leading to undersized heap allocation
- key operations: `new uint8_t[size + chunk_size]` followed by copy into the
  newly allocated buffer
- patch: add an overflow check before the allocation

What this looked like in the analyzed binary:

- `functions parseChunk` returned two candidates
- the correct function was the extractor `parseChunk`, not `MPEG4Source::parseChunk`
- a targeted sample through the `memcpy` callsite produced the winning path
- the matching path used:
  - alloc site: `operator.new[](..., nothrow)`
  - copy site: `memcpy(@p, src, len)`
  - then a later read/write into `buffer + size`

Recommended sequence:

```text
flint> functions parseChunk
0x<source_parseChunk> parseChunk
0x<extractor_parseChunk> parseChunk
flint> calls memcpy
...
parseChunk @ 0x<copy_site>
...
flint> sample 10 0x<extractor_parseChunk> @ 0x<copy_site>
flint> paths
flint> show <path_id>
flint> set solver off
flint> check-wmi all <path_id>
flint> prim-def StagefrightTx3gAllocThenCopy { * ; alloc: def p = call operator.new[](sz, _) ; * ; copy: call memcpy(@p, src, len) }
flint> check-wmi StagefrightTx3gAllocThenCopy <path_id>
```

What success looks like:

- the path should branch on the `tx3g` atom
- the path should hit `operator.new[]` first
- then `memcpy` using the allocation result as destination
- then a later append/read into `buffer + size`

If you sample the wrong `parseChunk`, you will still get plausible MP4 logic,
but not the alloc-then-copy bug shape.

## Discovery Commands

Use these before guessing:

- `functions [-i|-e] [filter]`: find likely targets by name
- `calls <func_name>`: find callers of an allocator, copy, or wrapper
- `input-genesis [count] [--depth N]`: auto-find common input sources and sample upward from them
- `wmis`: list built-in and user-defined primitives
- `stdlib-list`: list current known-function semantic mappings
- `sample [count] <func_or_addr> @ <addr...>`: target a function and require sampled paths to pass through specific addresses

Use these after sampling:

- `paths`: list cached paths and their IDs
- `show <id>`: show the reduced path
- `show <id!>`: show the raw path if reduction hid something important
- `pshow <id>`: show PIL/Haskell statement structure when normal `show` is too lossy
- `inspect <addr>` / `inspect_address`: confirm that a current-binary address is really the callsite you think it is
- `set solver off`: **always do this before matching** — the solver causes false negatives
- `check-wmi all <id>`: quickly test built-ins and existing primitives on one concrete path before writing a new pattern

## What To Look For In `show`

When reading a path, look for:

- whether the path actually passes through the address the user cares about
- where attacker-controlled data enters
- whether the source is a return value, argument, or global-derived value
- the exact allocator, copy, free, or formatting call names
- whether the interesting operation is direct or behind a wrapper
- which values need to be linked later with `@name`

If the path uses a wrapper like `my_alloc`, prefer `stdlib-add` before writing a larger pattern.

## Choose The Smallest Tool

### `taint-add`

Use `taint-add` when a custom function propagates attacker-controlled data and
Flint does not already know that.

Examples:

```text
taint-add custom_read src:0 ret
taint-add recv_packet src:0 ret
taint-add getenv other:env ret
```

Parameter forms:

- `arg:N`
- `src:N`
- `dst:N`
- `ret`
- `other:<name>`

### `stdlib-add`

Use `stdlib-add` when a custom function is really an existing semantic
primitive under another name.

Examples:

```text
stdlib-add my_alloc allocHeap ptr=ret size=arg:0
stdlib-add my_copy copyMem dest_ptr=arg:0 src_ptr=arg:1 len=arg:2
stdlib-add my_free freeHeap ptr=arg:0
```

Use this before `prim-def` when the function is just a wrapper around an
allocator, copy, free, format function, or other known semantic operation.

### `prim-def`

Use `prim-def` only when:

- no existing primitive already captures the bug shape
- `taint-add` alone is not enough
- `stdlib-add` alone is not enough

Good uses:

- one-off exploit-shape verification
- bug classes not already encoded in Flint
- matching a project-specific semantic sequence

Bad uses:

- re-encoding standard semantics that `stdlib-add` already models
- writing a giant path-specific script for one sampled path

## Address Sanity

Flint addresses, Ghidra addresses, and external disassembly addresses may not
always line up one-for-one. Before concluding that the address is wrong:

- confirm you are in the right containing function
- compare the surrounding call sequence, not just the raw address
- use semantic anchors like `operator.new[]`, `memcpy`, virtual reads, or helper calls
- retry sampling with the function start address if the name is duplicated

If Flint and the external disassembler disagree, the right question is usually
"am I looking at the same basic block?" rather than "are these exact numbers
identical?"

In practice, raw ELF / objdump addresses may differ from Flint / Ghidra
addresses by a load base.  Derive the offset once from a known semantic anchor
inside the same function, then keep using Flint addresses for sampling.

## Heap Overflow Demo

Run this from `flint/`:

```text
./.stack-work/dist/x86_64-linux/ghc-9.10.3/build/flint-shell/flint-shell --verbosity Debug ../demo_bins/taint_tests/heap_overflow
```

Suggested workflow:

```text
flint> functions sample
flint> sample sample_me
flint> paths
flint> show 0
flint> set solver off
flint> taint-add custom_read src:0 ret
flint> prim-def TaintedHeapAllocThenCopy { * ; alloc: def p = call malloc(sz[tainted:global]) ; * ; copy: call memcpy(@p, src, len) }
flint> check-wmi TaintedHeapAllocThenCopy 0
```

Notes:

- use the path IDs that `paths` prints; do not assume `0..1` will always exist
- `TaintedHeapAllocThenCopy` is a high-signal exploratory pattern, not a formal proof of overflow
- there is no built-in heap overflow primitive in the current codebase
- prefer `check-wmi all <id>` on the concrete path before jumping to a custom pattern

If the path uses an allocator wrapper:

```text
flint> stdlib-add my_alloc allocHeap ptr=ret size=arg:0
flint> prim-def TaintedHeapAllocThenCopy { * ; alloc: def p = call my_alloc(sz[tainted:global]) ; * ; copy: call memcpy(@p, src, len) }
flint> check-wmi TaintedHeapAllocThenCopy 0
```

## Minimal `prim-def` Template

These are the forms an agent will use most often:

- `*`
- `label: ...`
- `def dst = expr`
- `def dst = call func(args)`
- `call func(args)`
- `store dst value`
- `ret expr`
- `constraint expr`
- `@name` to reuse an earlier binding
- `name[tainted]` or `name[tainted:global]` to require taint

Example:

```text
prim-def TaintedHeapAllocThenCopy {
  * ;
  alloc: def p = call malloc(sz[tainted:global]) ;
  * ;
  copy: call memcpy(@p, src, len)
}
```

This produces:

- primitive name: `TaintedHeapAllocThenCopy`
- vars: `p`, `sz`, `src`, `len`
- locations: `alloc`, `copy`

## Pattern Writing Rules

- Prefer stable semantic anchors over raw statement-by-statement recreation.
- Use `stdlib-add` before writing a custom primitive for wrapper functions.
- Bind only the values you actually need later.
- Use labels only for locations you care about in the result.
- Use `*` to skip irrelevant path noise.
- Keep `prim-def` short. If it starts looking like a miniature decompiler dump, stop and simplify.
- Do not over-claim what a match proves.

## What Good Output Looks Like

After `sample`, `paths` should show entries like:

```text
path 0 (N stmts)
```

After `check-wmi`, a successful match looks like:

```text
MATCH: TaintedHeapAllocThenCopy in sample_me
```

`No match` means you should inspect the path again, not immediately make the
pattern bigger.

## Common Failure Modes

### No match after `taint-add`

- You may have added taint on the wrong argument or direction.
- The path you sampled may not contain the callsite you care about.
- The custom function name may differ from the imported symbol Flint sees.
- Next commands: `show <id>`, `show <id!>`, `calls <func_name>`

### No paths sampled through the target address

- You may have sampled the wrong duplicate-named function.
- The address may come from a different base or loader view than Flint uses.
- The path count may be too low to hit the interesting branch.
- Next commands: `functions <name>`, `sample <count> <func_addr> @ <addr>`, `paths`, `show <id!>`

### `calls <func_name>` is awkward for demangled C++ names

- Demangled C++ names with spaces, commas, or brackets can be inconvenient to pass through the shell.
- Prefer `calls memcpy`, `calls malloc`, `calls __memcpy_chk`, and similar simple anchors first.
- For C++ allocators like `operator.new[](..., nothrow)`, it is often easier to discover the callsite from `show <id!>` or external disassembly, then target that address directly.

### No match after `stdlib-add`

- The primitive var names must match the target `PrimSpec`.
- The function may have a different argument order than the standard API.
- The real call name may be mangled or wrapped differently.
- Next commands: `stdlib-list`, `show <id>`, `wmis`

### `prim-def` parses but never matches

- **The solver may be rejecting valid matches.**  This is the most common
  cause of false negatives.  Run `set solver off` and retry before
  investigating anything else.
- Your path may use a wrapper function, so add `stdlib-add` first.
- Your pattern may be too concrete.
- Your pattern may describe a suspicious shape, but the sampled path does not actually have the semantic relation you intended.
- The reduced path may have hidden the statement you are matching on; inspect `show <id!>`.
- Next commands: `set solver off`, `show <id>`, `show <id!>`, `pshow <id>`, `check-wmi all <id>`

## When To Read Code

- `flint/src/Flint/Shell/Commands/Taint.hs`: REPL commands for `taint-add`, `taint-remove`, and `taint-reset`
- `flint/src/Flint/Shell/Commands/StdLib.hs`: REPL commands for `stdlib-add` and stdlib semantic mappings
- `flint/src/Flint/Shell/Commands/PrimDef.hs`: parser and builder for the shell `prim-def` DSL
- `flint/src/Flint/Shell/Commands/WMI.hs`: REPL entry point for `check-wmi`
- `flint/src/Flint/Analysis/Path/Matcher/Primitives/Library.hs`: built-in primitive library and matcher patterns
- `flint/src/Flint/Analysis/Path/Matcher/Primitives/Library/StdLib.hs`: concrete function-name mappings like `malloc -> allocHeap`
- `flint/src/Flint/Analysis/Path/Matcher/Primitives/Library/PrimSpec.hs`: abstract primitive contracts, including required vars and locations
