# Hinja

Tool to generate Haskell and Idris bindings and datatypes for binaryninja core.

Strategy:
* clean binaryninjacore.h file so language-c can parse it (needed by c2hs)
* use language-c parser to generate records for Structs, function declarations (enum modules can already be generated using ParseEnums)


# Quick Start Guide

Welcome to blaze! Below are the things you'll need to get started. This is assuming you are running a Ubuntu/Debian system, otherwise
modifications will need to be made.


## Haskell

### Stack
1. To get stack run `curl -sSL https://get.haskellstack.org/ | sh`
2. stack will tell you where it installs make sure this is on your PATH

### Ghcup
1. To get ghcup run `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
2. As of this writing ghc 8.8.3 is the default installed by this tool.  List the available versions with `ghcup list`
3. Currently blaze uses ghc 8.8.2. To install, and use this version type `ghcup install 8.8.2`

### Haskell Ide Engine (hie)
1. Install depedencies first (this assumes debian 10/ubuntu 18.10 or later) with `sudo apt install libicu-dev libncurses-dev libgmp-dev`
2. To download run `git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules`
3. cd into the hie directory and run `./install.hs`
4. You're next step depends on your development environment. To use hie with Vscode simply go to the plugin store, search the full name, and install hie.


## Binja
1. Install Binja. You'll need a license.
2. to your ~/.profile add `export BINJA_PLUGINS=<path-to-binja-plugin-folder>`


## z3
1. Get z3 with `https://github.com/Z3Prover/z3.git`
2. run `./configure; mkdir build`
3. run `cd build; make`, then `make install`


## blaze
1. you will need to clone binja-header-cleaner, hs-binary-analysis, and haskell-binja from the project repo.
2. the directory structure should look like blaze/haskell-blaze, blaze/haskell-binja, blaze/binja-header-cleaner, blaze/hs-binary-analysis
3. from blaze directory run `touch File.hs`
3. run `stack build` on all three projects with the haskell-blaze project last.
4. To test all steps to this point run `stack test :blaze-test` within haskell-blaze directory.