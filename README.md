# Flint

A program that uses [Blaze][blaze] to find [emergent execution][ee] primitives.

# Dependencies

The repository structure of `Flint` assumes that the repositories for the following dependencies are located adjacently:

- [blaze][blaze]
- [binaryninja-haskell][bn-hs]
- [ghidra-haskell][g-hs]
- [binary-analysis][b-a]

For example:
```
.
├── flint
├── blaze
├── binaryninja-haskell
├── ghidra-haskell
└── binary-analysis
```

`Flint` can be compiled using either `cabal` or `stack`.

`Flint` uses `Blaze` for its binary lifting and analysis, which supports both `binaryninja` and `ghidra` as backends.

The `binaryninja` backend requires a headless version of Binary Ninja to be installed, which provides `libbinaryninjacore.so`.

The `ghidra` backend a Java Development Kit (JDK) to be installed. See the [binaryninja-haskell][bn-hs] installation instructions.

Each of these dependencies can be specified at build time using the `--extra-lib-dirs` flag.

For example:
- `cabal build --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server`
- `stack build --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server`

# To-do

- Use a single cabal.project file and Cabal flags and conditionals to specify whether to compile -O0 or not.
- Tell projectile to ignore `dist-newstyle` build dir. Also, consider renaming to a hidden folder.


Distribution A. (Approved for public release; distribution unlimited.)

[blaze]: https://github.com/kudu-dynamics/blaze
[bn-hs]: https://github.com/kudu-dynamics/binaryninja-haskell
[g-hs]: https://github.com/kudu-dynamics/ghidra-haskell
[b-a]: https://github.com/kudu-dynamics/binary-analysis
[ee]: https://en.wikipedia.org/wiki/Weird_machine
