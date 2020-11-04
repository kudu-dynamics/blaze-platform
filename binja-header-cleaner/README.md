# BN Header Cleaner 

Tool to make the binaryninjacore.h header file compliant (which spec?) for building the Binary Ninja bindings for Haskell. C specification compliance is required by c2hs which is used to help generate the bindings.

## Usage
Run `stack run clean-binja-header` from the project directory. Without any arguments, the program will print out the usage information:

`clean-binja-header path/to/original/binaryninjacore.h path/to/new/binaryninjacore.h`

The program expects two arguments: a path to the original binaryninjacore.h file and a path to output the reformatted binaryninjacore.h file that will be used to generate the BN Haskell bindings.

When running `clean-binja-header` via `stack`, the command will look like:

`stack run clean-binja-header -- path/to/original/binaryninjacore.h path/to/new/binaryninjacore.h`
