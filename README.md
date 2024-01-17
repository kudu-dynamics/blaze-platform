# Ghidra Bindings for Haskell

This library provides bindings to Ghidra in Haskell.

# Dependencies

`ghidra-haskell` is reliant on the `inline-java/jvm` and `inline-java/jni` packages and requires the Java Development Kit (jdk) to be installed.

On Linux systems, the `jdk-17/lib/server/libjvm.so` binary file must be present on disk and the library path should be added as an environment variable.

On Linux, users should add the following to their `.bashrc` to permit usage of the JDK.

```
export PATH=/path/to/jdk-17/bin/:$PATH
export LD_LIBRARY_PATH=/path/to/jdk-17/lib/server/
```

[GHCup](https://www.haskell.org/ghcup/) should also be installed to obtain the `stack` toolchain in order to build `ghidra-haskell`.

# Install

Clone `ghidra-haskell` and `binary-analysis` (https://github.com/kudu-dynamics/binary-analysis) into the same parent folder.

```
cd ghidra-haskell
make res/ghidra.jar
stack build
stack test
```

`make res/ghidra.jar` downloads Ghidra and create a jar.

Distribution A. (Approved for public release; distribution unlimited.)
