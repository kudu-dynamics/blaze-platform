# Binary Ninja bindings for Haskell

Provides access to Binary Ninja from Haskell through the Binary Ninja shared library.

## Usage
* Ensure the BN core shared library is placed/installed correctly such that it can be found during linking of the bindings.
* Use `stack build` and `stack test` to build or test the package, respectively.
* Note: When updates to the Binary Ninja API are made, a C-compliant `binaryninjacore.h` header file must be generated. Place in the `res/` directory and the bindings can now be built.

Approved for Public Release, Distribution Unlimited.
