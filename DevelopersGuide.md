- [Installing Haskell Dependecies](#installing-haskell-dependecies)
- [Building blaze-platform Monorepo](#building-blaze-platform-monorepo)
- [Rapid Prototyping with Hspec library](#rapid-prototyping-with-hspec-library)
- [blaze-platform Versioning](#blaze-platform-versioning)
- [Using JSON Output as Control Group](#using-json-output-as-control-group)
    - [How To Actually Run flint](#how-to-actually-run-flint)
        - [Using the SMT Solver](#using-the-smt-solver)
    - [Describing the SMT output](#describing-the-smt-output)
- [Miscellaneous Build Instructions](#miscellaneous-build-instructions)
    - [Special side directions for MacOS](#special-side-directions-for-macos)
    - [Legacy Build Instructions](#legacy-build-instructions)
        - [Binary Ninja](#binary-ninja)

## Installing Haskell Dependecies

First, install the following packages:

```bash
sudo apt install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
```

Then, install the script:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Everything is generally default. However, if you plan on using `vscode`, make sure you install the Haskell Language Server.

In `blaze-platform`, we use `stack` for building and testing. Install `stack` using the `ghcup`:

```bash
ghcup install stack
```

Next, you will need to clone `z3`:

```bash
git clone https://github.com/Z3Prover/z3.git
```

Then, build and install `z3`:

```bash
cd z3
./configure; mkdir build
cd build
make
make install
```

## Building `blaze-platform` Monorepo

Now, clone `blaze-platform`:

```bash
git clone <repository link>.git
cd blaze-platform
```

First, build `blaze` before anything else:

```bash
cd blaze
stack build
stack test
```

Next, `cd` into `ghidra-haskell` and create an environment variable for `libjvm.so`:

```bash
export PATH=$JDK/bin/${PATH:+:$PATH}
export LD_LIBRARY_PATH=$JDK/lib/server/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
```

Then, go ahead and `build` inside `ghidra-haskell`:

```bash
make res/ghidra.jar
stack build
stack test
```

Finally, build `flint` and `test` everything inside `blaze-platform`:

```bash
cd ..
stack build
stack test
```

## Rapid Prototyping with `Hspec` library

You can quickly add features to `blaze`, `flint`, or `ghidra-haskell` by creating or adding to a `*spec*.hs` file. This will allow rapid development using `Hspec` library.

You can `find` all the `*spec*.hs` files with the following command below:

```bash
find . -iname "*spec*.hs" ! -path "*dist*"
```

You can run all test with `stack test`. However, its best to just run specific test using the `--match` flag.

```bash
stack test --test-arguments "--match MatcherSpec"
```

## `blaze-platform` Versioning

`blaze-platform` versioning uses the format "N.YY.MMDDx" where "N" is the major version (0 before release),
"YY" is the last two digits of the year, "MMDD" is the numerical month and date concatenated, and "x" is the
alphabetical index of the version within the same day. For example, the third version of `blaze-platform`
before release committed on June 3, 2007 would be "0.07.0603c".

## Using JSON Output as Control Group

I used the `*.json` output as the, "Do my changes work", file.

### How To Actually Run `flint`

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

You can also use this command that locates all the test files used for `flint` in `blaze-platform/flint`:

```bash
find res -type f -exec file {} + | grep ELF | sed 's/:.*//'
res/demo/cb/electronictrading
res/test_bins/intercfg/intercfg
res/test_bins/shim/shim_main
res/test_bins/shim/shim_libc.so
res/test_bins/onion_atm/atm
res/test_bins/onion_atm/libatm2.so
res/test_bins/demo1/demo1
res/test_bins/onion_test/onion_test_aarch64
res/test_bins/onion_test/onion_test
res/test_bins/dirty_benchmark/dirty_benchmark
res/test_bins/juliet/CWE416/CWE416_Use_After_Free__return_freed_ptr_01-bad
```

Here is an example output for `onion_test`:

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

#### Using the SMT Solver

In order to modify Flint's output, you will need to use different flags.

The `--outputSMTish` flag makes an output an almost compatible SMT output. Also, you can use the `--pathSamplingFactor` to increase the how many paths we enumerate through. We can also use `-f` to filter on functions we want to show in the JSON results.

A challenge binary was created to test the SMT output. It's located in `blaze-platform/flint/res/test_bins/demo`.

You can run generate SMT-like output, compatible with other weird machine research programs, with the following command:
```bash
$ echo "menu" > functions.txt 
stack run -- flint --outputSMTish --pathSamplingFactor 10 -f functions.txt -o test-smt.json res/test_bins/demo1/demo1
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

### Describing the SMT output

This JSON output has multiple fields related to SMT output.

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

### Special side directions for MacOS

- Install the build-essentials stuff from x tools. OSX doesn't have the build-essential package in their repository, but it should automatically prompt the equivalent when trying to download build-essential
- Get Haskell via the install_notes.txt instructions
- Get Homebrew and install openjdk@21. Do not get the oracle version or any other version, if you do you have to remove it
- get the blaze repo
- run getGhidraJar.sh on the ghidra-haskell directory (so your terminal would be ghidra-haskell > ./scripts/getGhidraJar.sh
- run `stack build` and `stack test` in the ghidra-haskell library

---

### Legacy Build Instructions

#### Binary Ninja

You must have binaryninja installed somewhere, and must be able to be headless:
```bash
Set to version `3.5.4526`
```

Now, make links from some dir on your LD_LIBARY_PATH (or /usr/lib) to these binja libs
```bash
libbinaryninjacore.so -> /path/to/binaryninja/libbinaryninjacore.so.1
libbinaryninjacore.so.1 -> /path/to/binaryninja/libbinaryninjacore.so.1
```

Now, get corresponding binja api:
```bash
git clone https://github.com/Vector35/binaryninja-api.git
cd binaryninja-ai
git checkout beb79f0a7aa38bdf6818056d8c21c6e599456be8
git submodule update --init --recursive
cmake -S . -B build
cmake --build build -j8
```

Now, link this from some dir on your library path:
```bash
libbinaryninjaapi.a -> /path/to/binaryninja-api/build/out/libbinaryninjaapi.a
```

Now add this environment variable:
```
export BINJA_PLUGINS="/path/to//binaryninja/plugins"
```

Now, refresh your `.bashrc`

```bash
source ~/.bashrc
```

Now, copy binja license to 
```bash
cp license.dat ~/.binaryninja/license.dat
```

Now, build `blaze-platform`:
```bash
cd blaze-platform
git checkout 10-sample-plan
stack build

cd flint
stack test
```
