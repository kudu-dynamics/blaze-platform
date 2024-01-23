# Ghidra Bindings for Haskell

This library provides bindings to Ghidra in Haskell.

# Dependencies

`ghidra-haskell` is reliant on the `inline-java/jvm` and `inline-java/jni` packages and requires the Java Development Kit (JDK) to be installed. Ghidra 10.2 and beyond (the latest version at the time of writing is 11.0) requires JDK 17, though we have had no problems using JDKs up to 21. The rest of these instructions assumes the JDK is installed at `$JDK`.

[GHCup](https://www.haskell.org/ghcup/) should also be installed to obtain the `stack` toolchain in order to build `ghidra-haskell`.

## Linux

The `$JDK/lib/server/libjvm.so` binary file must be present on disk.
Users should add the following to either their shell initialization (e.g., `~/.bashrc`) or to an environment manager (e.g., [direnv][https://direnv.net/]).

```sh
export PATH=$JDK/bin/${PATH:+:$PATH}
export LD_LIBRARY_PATH=$JDK/lib/server/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
```

## MacOS

Similar to Linux, except expect to find `$JDK/lib/server/libjvm.dylib`.
So far, setting `DYLD_LIBRARY_PATH` does not seem necessary.

```sh
export PATH=$JDK/bin/${PATH:+:$PATH}
```

# Install

Clone `ghidra-haskell` and [binary-analysis](https://github.com/kudu-dynamics/binary-analysis) into the same parent folder.

```sh
cd ghidra-haskell
make res/ghidra.jar
stack build
stack test
```

Distribution A. (Approved for public release; distribution unlimited.)
