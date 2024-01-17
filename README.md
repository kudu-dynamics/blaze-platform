# Flint

A program that uses [Blaze][blaze] to find [emergent execution][ee] primitives.

# Dependencies

The repository structure of `Flint` assumes that the repositories for the following dependencies are located adjacently.

For example:
```
folder/flint
folder/blaze
folder/binaryninja-haskell
folder/ghidra-haskell
```

`Flint` can be compiled using either the `cabal` or `stack` toolchains.

`Flint` which uses `Blaze` supports both `binaryninja` and `ghidra` backends.

The `binaryninja` backend requires a headless version of `binaryninja` to be installed, the `binaryninja-haskell` package, and for the `binaryninja/libbinaryninjacore.so` file to be available.

The `ghidra` backend requires the `ghidra-haskell` package as well as for the Java Development Kit (jdk) `jdk/lib/server/libjvm.so` file to be available.

Each of these dependencies can be specified at build time using the `--extra-lib-dirs` flag.

For example:
- `cabal build --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server`
- `stack build --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server`

# To-do

- Use a single cabal.project file and Cabal flags and conditionals to specify whether to compile -O0 or not.
- Tell projectile to ignore `dist-newstyle` build dir. Also, consider renaming to a hidden folder.


Distribution A. (Approved for public release; distribution unlimited.)

[blaze]: https://github.com/kudu-dynamics/blaze
[ee]: https://en.wikipedia.org/wiki/Weird_machine
