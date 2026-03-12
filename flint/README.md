# Flint

- [Dependencies](#dependencies)
- [Overview of output](#overview-of-output)
- [Interactive Shell](#interactive-shell)
- [Batch Usage](#batch-usage)
- [Steady-State Mode](#steady-state-mode)
- [MCP Server](#mcp-server)

A program that uses [Blaze](../blaze) to find Weird Machine instructions.

# Installation and development

First, clean and build your changes in `blaze-platform`:

```bash
stack clean
stack build
```

Next, you can run `flint` using `stack exec` inside `blaze-platform/flint`:

```bash
$ cd flint/
$ stack exec -- flint
Missing: INPUT_FILE

Usage: flint [--backend BACKEND] [-o|--outputToFile OUTPUT_TO_FILE]
             [--doNotUseSolver] [--onionDepth ONION_DEPTH]
             [--pathSamplingFactor PATH_SAMPLING_FACTOR] [--isKernelModule]
             [--analysisDb ANALYSIS_DB] [--verbosity VERBOSITY]
             [-f|--filterFuncs FILTER_FUNCS_FILE]
             [-b|--blacklist BLACKLIST_FILE] [--outputSMTish] INPUT_FILE

  Static path-based analysis to find bugs.
```

For a PIL output, you will want to use  `--outputToFile` and `--verbosity`. For anything complex, you'll probably want to use `--doNotUseSolver`. Also, some of our test files like `dive_logger` currently require us to use `--blacklist` because our analysis gets stuck on some functions.

Here is an example output for `onion_test` (`flint/res/test_bins/onion_test/onion_test`):

```bash
stack exec -- flint -o test.json --verbosity Debug res/test_bins/
onion_test/onion_test
Opening binary with default backend (Ghidra)[DEBUG]
Getting paths for: "_init"[DEBUG]
Got 5 paths.[DEBUG]
Getting paths for: "FUN_00401020"[DEBUG]
Got 3 paths.[DEBUG]
Getting paths for: "FUN_004010a0"[DEBUG]
Got 3 paths.[DEBUG]
Getting paths for: "_start"[DEBUG]
Got 0 paths.[DEBUG]
Getting paths for: "_dl_relocate_static_pie"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "deregister_tm_clones"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "register_tm_clones"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "__do_global_dtors_aux"[DEBUG]
Got 5 paths.[DEBUG]
Getting paths for: "free1"[DEBUG]
Got 2 paths.[DEBUG]
Getting paths for: "free2"[DEBUG]
Got 2 paths.[DEBUG]
Getting paths for: "calls_free2"[DEBUG]
Got 2 paths.[DEBUG]
Getting paths for: "double_free1"[DEBUG]
Got 3 paths.[DEBUG]
Getting paths for: "malloc1"[DEBUG]
Got 2 paths.[DEBUG]
Getting paths for: "calloc1"[DEBUG]
Got 2 paths.[DEBUG]
Getting paths for: "use_after_free"[DEBUG]
Got 4 paths.[DEBUG]
Getting paths for: "a_write"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "use_after_free2"[DEBUG]
Got 5 paths.[DEBUG]
Getting paths for: "int_overflow_func"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "not_int_overflow_func"[DEBUG]
Got 3 paths.[DEBUG]
Getting paths for: "controlled_indirect_call"[DEBUG]
Got 3 paths.[DEBUG]
Getting paths for: "_copy_from_user"[DEBUG]
Got 1 paths.[DEBUG]
Getting paths for: "unbounded_copy_from_user1"[DEBUG]
Got 5 paths.[DEBUG]
Getting paths for: "unbounded_copy_from_user2"[DEBUG]
Got 4 paths.[DEBUG]
Getting paths for: "main"[DEBUG]
Got 21 paths.[DEBUG]
Getting paths for: "_fini"[DEBUG]
Got 1 paths.[DEBUG]
Finished sampling paths[DEBUG]
Wrote results to "test.json"
```

It produces a `test.json` file you can look into.

---

### Using the SMT Solver

In order to modify Flint's output, you will need to use different flags.

The `--outputSMTish` flag makes an output almost compatible with downstream tools. Also, you can use the `--pathSamplingFactor` to increase the how many paths we enumerate through. We can also use `-f` to filter on functions we want to show in the JSON results.

A challenge binary was created to test the SMT output. It's located in [`blaze-platform/demo_bins/demo1/demo1`](../demo_bins/demo1/demo1).

If you want to look at the source code, you can find it in [`blaze-platform/demo_bins/demo1/demo1.c`](../demo_bins/demo1/demo1.c).

You can run generate SMT-like output, compatible with other weird machine research programs, with the following command:
```bash
$ echo "menu" > functions.txt
stack run -- flint --outputSMTish --pathSamplingFactor 10 -f functions.txt -o test-smt.json ../demo_bins/demo1/demo1
```

Now, you can look at the `test-smt.json` file to observe the output:
```bash
$ test-smt.json
{
    "baseAddress": {
        "offset": 4194304,
        "space": {
            "addressableUnitSize": 1,
            "name": {
                "tag": "Ram"
            },
            "ptrSize": 8
        }
    },
    "operations": [
        {
            "name": "ControlledIndirectCall",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "call_dest": "(DEREF 64 (ARRAY_ADDR ((FIELD_ADDR (DEREF 64 (GLOBAL sys)) 0x0000000000000000)) (DEREF 64 (FIELD_ADDR (DEREF 64 (GLOBAL ctrl)) 0x0000000000000050)) 8))"
                        },
                        "id": "-1560613651385522566",
                        "type": "ControlledIndirectCall"
                    }
                ],
                "preconditions": [
                    "(bvnot ((= (ARG 0) 0x00000003)))",
                    "(bvslt (ARG 0) 0x00000004)",
                    "(bvnot ((= (ARG 0) 0x00000001)))",
                    "(= (ARG 0) 0x00000002)"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        },
        {
            "name": "Write",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "ptr": "(ARRAY_ADDR ((FIELD_ADDR (DEREF 64 (GLOBAL ctrl)) 0x0000000000000000)) ((ARG 1)) 8)",
                            "value": "(ARG 2)"
                        },
                        "id": "5506292366957372219",
                        "type": "Write"
                    }
                ],
                "preconditions": [
                    "(bvnot ((= (ARG 0) 0x00000003)))",
                    "(bvslt (ARG 0) 0x00000004)",
                    "(= (ARG 0) 0x00000001)",
                    "(bvnot ((bvslt (ARG 1) 0x0000000000000000)))",
                    "(bvnot ((bvslt 0x000000000000000a (ARG 1))))"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        },
        {
            "name": "Write",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "ptr": "(FIELD_ADDR (DEREF 64 (GLOBAL sys)) 0x0000000000000030)",
                            "value": "(ARG 1)"
                        },
                        "id": "7055323186741156031",
                        "type": "Write"
                    }
                ],
                "preconditions": [
                    "(= (ARG 0) 0x00000003)"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        }
    ]
}
```

---

#### Write Primitive 1

`demo1.c` snippet:
```c
struct SystemState {
    void (*handlers[HANDLER_COUNT])();
    long call_target;   // an attacker can write past the array of function pointers
};

...

// arbitarary write to addr
void set_call_target(long addr)
{
    sys->call_target = addr;
}
```

The first write primitive allows us to write an arbitary function pointer to `sys->call_target`. In this case, we can write `admin_mode()` to `call_target`.

`test-smt.json` snippet:
```json
        {
            "name": "Write",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "ptr": "(FIELD_ADDR (DEREF 64 (GLOBAL sys)) 0x0000000000000030)",
                            "value": "(ARG 1)"
                        },
                        "id": "-8904604037364128612",
                        "type": "Write"
                    }
                ],
                "preconditions": [
                    "(= (ARG 0) 0x00000003)"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        }
```

---

#### Write Primitive 2

`demo1.c` snippet:
```c
// oob to handler index
int set_slot(long index, long value)
{
    if (index < 0 || index > SLOT_COUNT)
        return 0;
    ctrl->slots[index] = value; // oob
    return 1;
}
```

The second write primitive uses an off-by-one write to overflow `ctrl->slots[]` into `ctrl->handler_index`.

`test-smt.json` snippet:
```json
        {
            "name": "Write",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "ptr": "(ARRAY_ADDR ((FIELD_ADDR (DEREF 64 (GLOBAL ctrl)) 0x0000000000000000)) ((ARG 1)) 8)",
                            "value": "(ARG 2)"
                        },
                        "id": "4720097406850872539",
                        "type": "Write"
                    }
                ],
                "preconditions": [
                    "(bvnot ((= (ARG 0) 0x00000003)))",
                    "(bvslt (ARG 0) 0x00000004)",
                    "(= (ARG 0) 0x00000001)",
                    "(bvnot ((bvslt (ARG 1) 0x0000000000000000)))",
                    "(bvnot ((bvslt 0x000000000000000a (ARG 1))))"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        }
```

---

#### Indirect Call Primitive

`demo1.c` snippet:
```c
// indirect call
void call_handler()
{
    sys->handlers[ctrl->handler_index]();   // handler_index stores the admin_mode() address
}
```

The indirect call primitive triggers an indirect call that indexes into `sys->handlers[]` to call your controlled pointer.

`test-smt.json` snippet:
```json
       {
            "name": "ControlledIndirectCall",
            "operation": {
                "effects": [
                    {
                        "arguments": {
                            "call_dest": "(DEREF 64 (ARRAY_ADDR ((FIELD_ADDR (DEREF 64 (GLOBAL sys)) 0x0000000000000000)) (DEREF 64 (FIELD_ADDR (DEREF 64 (GLOBAL ctrl)) 0x0000000000000050)) 8))"
                        },
                        "id": "9095671908802974361",
                        "type": "ControlledIndirectCall"
                    }
                ],
                "preconditions": [
                    "(bvnot ((= (ARG 0) 0x00000003)))",
                    "(bvslt (ARG 0) 0x00000004)",
                    "(bvnot ((= (ARG 0) 0x00000001)))",
                    "(= (ARG 0) 0x00000002)"
                ],
                "variables": [
                    "; variables section\n; TODO\n"
                ]
            },
            "trigger": "menu((ARG 0), (ARG 1), (ARG 2));"
        }
```

---

### Describing the SMT output

This JSON output has the following fields:

* `name` - The name for the WMI
* `operation`
    * `effects` - The changes when this oepration executes
        * `arguments` - The function arguments
        *  `type` - The type of effect (`write`, `malloc`, `free`, etc)
    * `preconditions` The conditions its holds
    * `variables` - The variables referenced
    * `trigger` - The function that causes the WMI

---

## Miscellaneous Build Instructions

The following sections are for developers on MacOS. Also, `blaze` supported Binary Ninja backend in the past. This was before Ghidra was open source.

---

### Special side directions for MacOS

- Install the build-essentials stuff from x tools. OSX doesn't have the build-essential package in their repository, but it should automatically prompt the equivalent when trying to download build-essential
- Get Haskell via the install_notes.txt instructions
- Get Homebrew and install openjdk@21. Do not get the oracle version or any other version, if you do you have to remove it
- get the blaze repo
- run getGhidraJar.sh on the ghidra-haskell directory (so your terminal would be ghidra-haskell > ./scripts/getGhidraJar.sh
- run `stack build` and `stack test` in the ghidra-haskell library

# Overview of output

Flint finds "Weird Machine Instructions" (WMIs). Particularly, Flint currently focuses on _callable_ WMIs, meaning that you can trigger the WMI by calling a function, and potentially control it through function inputs, as well as recieve its output through callsites args or a return. Thus, the callable WMIs are written in terms and globals, function inputs, and the function return.

The term `Primitive` refers to a general description, or class, of a capability or operation, and any instances of that Primitive are a WMI of that Primitive. We describe all the primitives implemented in `Flint` in `res/prispec.toml`. Here's an example description for a "double free" primitive:

```
[[prim]]
  name = "doubleFree"
  locations = ["free1", "free2"]
  vars = ["ptr"]
```

Each primitive has a unique name, locations, and variable names. The `locations` are labels to indicate where important parts of the primitive occur in the binary. The `vars`
are names of variables that will be bound to expressions, in relation to a function's arguments, when a WMI is created.

Here's an example WMI for a `doubleFree` primitive:

```
{
  "constraints": [ "ARG_2 != 0" ],
  "func": [
    "double_free1",
      4198867
    ],
  "linkedVars": [
    "ARG_1"
    ],
  "locations": {
    "free1": [
        4198890
        ],
      "free2": [
        4198902
        ]
    },
  "primName": "doubleFree",
  "vars": {
    "ptr": "ARG_1"
    }
}
```

Here's a breakdown of the fields in this Callable WMI:

- `primName` - the name of the primitive of which this WMI is an instance
- `func` - this is the function that contains this WMI. All `ARG_n` are referring to this function's arguments.
- `locations` - these are the locations, in the binary, of instructions that correspond to important parts of this WMI. Here, it's the location of the first and second calls `FreeHeap` WMIs.
- `constraints` - this shows nessecary constraints that must be true for the WMI to pass. These might be collected from a single path, or from the common constraints of multiple paths that trigger the same WMI. This example constraint shows that the second arg of the function `double_free1` must not equal zero.
- `vars` - we can see the that `ptr` that gets double-freed is the first arg of the `double_free1` function. That means: if we call `double_free1`, the pointer we pass to the first arg will get double-freed

# Interactive Shell

Flint includes an interactive shell (`flint-shell`) for exploring binaries, sampling and inspecting paths, and checking for WMIs interactively.

```
stack run flint-shell -- /path/to/your/binary.gzf
```

Once in the shell, type `help` for a list of commands. Here's a typical workflow:

```
flint> functions malloc           # list functions matching "malloc"
flint> sample vulnerable_func 10  # sample 10 paths from a function
flint> show 0                     # pretty-print path 0
flint> reduce [0..9]              # reduce all paths via copy/constant propagation
flint> solve [10..19]             # check satisfiability with Z3
flint> wmis                       # list available WMI primitives
flint> check-wmi doubleFree 10    # check a path for a WMI
flint> free [0..19]               # free cached paths
```

You can also do targeted sampling, where sampled paths must pass through specific addresses:

```
flint> sample my_func @ 0x401000 0x401050
flint> sample my_func 10 @ 0x401000
```

The shell supports tab completion, command history, bracket/range syntax for path IDs (`[0..5]`, `[1, 3, 7-10]`), and aliases for all commands (e.g. `sp` for `sample`, `sh` for `show`).

The command layer is decoupled from the REPL so these same functions can be reused by an MCP server for AI-driven analysis.

# Batch Usage

From this folder, run:

```
stack run flint -- --doNotUseSolver /path/to/your/binary
```

It will output a potentially very long JSON object that contains all the weird machine instructions it finds.

We currently use `--doNotUseSolver` because the SMT solver can get tripped up by many edge cases that we have not yet handled.

For more examples of usage, please see the [DevelopersGuide.md](DevelopersGuide.md).

# Steady-State Mode

The default onion analysis runs N full passes across the entire binary. The steady-state mode (`--steadyState`) instead stochastically picks functions, samples paths on-demand, checks for WMIs immediately, and squashes results incrementally. This avoids the expensive up-front path pre-sampling that can exhaust memory on large binaries.

```
stack run flint -- --steadyState -o results.json /path/to/binary
```

Intermediate results are periodically written to the output file (overwriting each time), so you can inspect progress while the analysis runs. The `--reportInterval` flag controls how often this happens (default: every 100 iterations).

## Attack Surface

You can focus the analysis on functions reachable from known entry points using `--attackSurface`:

```
stack run flint -- --steadyState --attackSurface funcs.txt -o results.json /path/to/binary
```

Where `funcs.txt` contains one function name per line. Flint will BFS from those functions through callsites and only analyze functions within `--attackSurfaceDepth` hops (default: 5).

## Options

| Flag | Default | Description |
|------|---------|-------------|
| `--steadyState` | off | Enable steady-state stochastic analysis |
| `--attackSurface FILE` | none | File with entry function names (one per line) |
| `--attackSurfaceDepth N` | 5 | BFS depth from attack surface entry functions |
| `--reportInterval N` | 100 | Write intermediate results every N iterations |

# MCP Server

Flint includes an MCP (Model Context Protocol) server (`flint-mcp`) that exposes the same analysis tools as the interactive shell over JSON-RPC, enabling AI-driven binary analysis.

## Building

```
stack build flint:flint-mcp
```

## Running

**Stdio transport** (for use with Claude Code, etc.):
```
stack exec flint-mcp -- [--backend Ghidra] [--doNotUseSolver]
```

**HTTP transport**:
```
stack exec flint-mcp -- --http [--port 3000]
```

No binary is loaded on startup — use the `load_binary` tool to load one.

## Claude Code Configuration

To use flint-mcp with Claude Code, add a `.mcp.json` file in your project directory (or `~/.claude/.mcp.json` for global config):

```json
{
  "mcpServers": {
    "flint-mcp": {
      "type": "stdio",
      "command": "stack",
      "args": ["exec", "flint-mcp", "--"],
      "cwd": "/path/to/blaze-platform/flint"
    }
  }
}
```

Adjust `cwd` to point to your `flint` directory (where `stack.yaml` lives). You can also pass extra flags via `args`, e.g. `["exec", "flint-mcp", "--", "--doNotUseSolver"]`.

Once configured, Claude Code will automatically start the MCP server and expose the flint tools. Use `/mcp` in Claude Code to verify the server is connected.

## Available Tools

| Tool | Description |
|------|-------------|
| `load_binary` | Load a binary file for analysis (resets all state) |
| `list_functions` | List/filter functions in the binary |
| `sample_paths` | Sample execution paths from a function |
| `show_paths` | Display PIL statements for paths |
| `pshow_path` | Show raw Haskell PIL types for a path |
| `reduce_paths` | Simplify paths via copy/constant propagation |
| `solve_paths` | Check path satisfiability with Z3 |
| `list_wmis` | List available WMI vulnerability primitives |
| `check_wmi` | Check paths against a WMI primitive (or `all` to check all) |
| `set_solver` | Toggle the Z3 solver on/off |
| `list_paths` | List cached paths |
| `free_paths` | Free cached paths to release memory |
| `exit` | Shut down the server |

## Typical AI Workflow

```
load_binary → list_functions → sample_paths → reduce_paths → show_paths → check_wmi
```

## Driving from the CLI

Write JSON-RPC commands to a file and pipe them in:

```bash
cat > /tmp/mcp_cmds.txt << 'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"load_binary","arguments":{"file_path":"path/to/binary.gzf"}}}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"list_functions","arguments":{}}}
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"sample_paths","arguments":{"function":"main","count":"5"}}}
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"reduce_paths","arguments":{"path_ids":"0 1 2 3 4"}}}
{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"check_wmi","arguments":{"wmi_name":"all","path_ids":"5 6 7 8 9"}}}
EOF

stack exec flint-mcp -- < /tmp/mcp_cmds.txt 2>/dev/null
```

- **stdout**: JSON-RPC responses (one per line)
- **stderr**: log messages — redirect to `/dev/null` or a log file
- The `initialize` handshake must be the first message
- Path IDs are sequential: sampling 5 paths gives IDs 0-4, then `reduce_paths` creates new paths starting at ID 5

Distribution A. (Approved for public release; distribution unlimited.)

[ee]: https://en.wikipedia.org/wiki/Weird_machine
