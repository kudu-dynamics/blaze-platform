# Blaze


# Quick Start Guide

Welcome to Blaze! Below are the things you'll need to get started. This is assuming you are running a Ubuntu/Debian system, otherwise
modifications will need to be made.

## Haskell

### Stack
1. To get Stack run `curl -sSL https://get.haskellstack.org/ | sh`
2. Stack will tell you where it installs make sure this is on your PATH

### Ghcup
1. To get ghcup run `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
2. As of this writing ghc 8.10.7 is the default installed by this tool.  List the available versions with `ghcup list`
3. Currently blaze uses ghc 8.10.7. To install, and use this version type `ghcup install 8.10.7`

## Haskell bindings for Binary Ninja
1. Install Binary Ninja. You'll need a license that supports headless mode.
2. to your `~/.profile` add `export BINJA_PLUGINS=<path-to-binja-plugin-folder>`


## Z3
1. Get z3 with `https://github.com/Z3Prover/z3.git`
2. run `./configure; mkdir build`
3. run `cd build; make`, then `make install`


## Blaze
1. The directory structure should look like `blaze/blaze`, `blaze/binaryninja-haskell`, `blaze/binary-analysis`
2. Run `stack build` from the blaze project.
4. To test Blaze run `stack test` within the `blaze` directory.


Approved for Public Release, Distribution Unlimited.
