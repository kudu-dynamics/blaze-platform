# Flint

- [Dependencies](#dependencies)
- [Overview of output](#overview-of-output)
- [Usage](#usage)

A program that uses [Blaze](../blaze) to find Weird Machine instructions.

# Installation and development

See [DevelopersGuide.md](DevelopersGuide.md) for instructions setting up and using Flint.

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

# Usage

From this folder, run:

```
stack run flint -- --doNotUseSolver /path/to/your/binary
```

It will output a potentially very long JSON object that contains all the weird machine instructions it finds.

We currently use `--doNotUseSolver` because the SMT solver can get tripped up by many edge cases that we have not yet handled.

For more examples of usage, please see the [DevelopersGuide.md](DevelopersGuide.md).

Distribution A. (Approved for public release; distribution unlimited.)

[ee]: https://en.wikipedia.org/wiki/Weird_machine
