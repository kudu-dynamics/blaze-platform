# Binary Ninja bindings for Haskell

Provides access to Binary Ninja from Haskell through the Binary Ninja core shared library.

## Usage
* Generate a C-spec compliant binaryninjacore.h header file. 's header file is not compliant. This issue was reported and the response "will not fix."
* Ensure the BN core shared library is placed/installed correctly such that it can be found during linking of the bindings.
* With the fixed binayninjacore.h header file placed in the `res/` directory, the bindings can now be built.
