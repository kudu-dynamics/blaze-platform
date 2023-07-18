# Ghidra Bindings for Haskell

This library provides bindings to Ghidra in Haskell.

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
