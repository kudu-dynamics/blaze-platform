# Flint

A program that uses [Blaze](../blaze) to find [emergent execution][ee] primitives.

# Dependencies

`Flint` uses `Blaze` for its binary lifting and analysis, which supports both `binaryninja` and `ghidra` as backends.

The optional `binaryninja` backend requires a headless version of Binary Ninja to be installed, which provides `libbinaryninjacore.so`. See the [binaryninja-haskell](../binaryninja-haskell) installation instructions. To use the Binary Ninja backend with Flint, add `--flag flint:binaryninja` to `stack build`, or `-f binaryninja` to `cabal build`.

The `ghidra` backend requires a Java Development Kit (JDK) to be installed. See the [ghidra-haskell](../ghidra-haskell) installation instructions.

Each of these dependencies can be specified at build time using the `--extra-lib-dirs` flag.

For example (without Binary Ninja support):
- `cabal build --extra-lib-dirs=/path/to/jdk/lib/server flint`
- `stack build --extra-lib-dirs=/path/to/jdk/lib/server flint`

- `cabal build -f binaryninja --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server flint`
- `stack build --flag flint:binaryninja --extra-lib-dirs=/path/to/binaryninja --extra-lib-dirs=/path/to/jdk/lib/server flint`

# To-do
- Tell projectile to ignore `dist-newstyle` build dir. Also, consider renaming to a hidden folder.


Distribution A. (Approved for public release; distribution unlimited.)

[ee]: https://en.wikipedia.org/wiki/Weird_machine
