# flint

A program that uses Blaze to find EE primitives.

## Using with Emacs

This project uses `cabal` instead of `stack`. To set your `haskell-mode` in emacs to use `cabal`, run this command:

```
(setq haskell-process-type 'cabal-repl)
```

You can do this easily in three steps:
1. Load up this README.md in emacs.
2. Move cursor to right after closing paren of line above
3. Press `C-x C-e`

# TODO

## devops

- Use a single cabal.project file and Cabal flags and conditionals to specify whether to compile -O0 or not.
- Tell projectile to ignore `dist-newstyle` build dir. Also, consider renaming to a hidden folder.


Distribution A. (Approved for public release; distribution unlimited.)
