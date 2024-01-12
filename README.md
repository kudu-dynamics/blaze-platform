# Blaze

# Quick Start Guide

Welcome to Blaze! Below are the things you'll need to get started. This is assuming you are running a Ubuntu/Debian system, otherwise
modifications will need to be made.

## Haskell

### Use ghcup to install GHC and/or Stack
1. To get ghcup run `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
2. You can list the available tools and versions from ghcup with `ghcup list`
3. Install stack: `ghcup install stack`

## Haskell bindings for Binary Ninja
1. Install Binary Ninja. You'll need a license that supports headless mode.
2. to your `~/.profile` add `export BINJA_PLUGINS=<path-to-binja-plugin-folder>`

## Z3
1. Get z3 from `https://github.com/Z3Prover/z3.git`
2. run `./configure; mkdir build`
3. run `cd build; make`, then `make install`

## Blaze
1. The directory structure should look like `blaze/blaze`, `blaze/binaryninja-haskell`, `blaze/ghidra-haskell`, `blaze/binary-analysis`
2. Run `stack build` from the blaze project.
4. To test Blaze run `stack test` within the `blaze` directory.


Distribution A. (Approved for public release; distribution unlimited.)
