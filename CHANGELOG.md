# Blaze Platform

- [Version 0.26.0123a](#version-0260123a)
- [Version 0.26.0122b](#version-0260122b)
- [Version 0.26.0122a](#version-0260122a)
- [Version 0.26.0121b](#version-0260121b)
- [Version 0.26.0121a](#version-0260121a)
- [Version 0.26.0120a](#version-0260120a)

---

## Version 0.26.0126a
- Initial addition of references abstraction. Not yet integrated into analysis
process.

## Version 0.26.0123a
- Fixed bug where `onionSinglePass` used non-squased CallableWMI snapshot instead
of the squashed CallableWMI snapshot.
- Allow users to disable the `squashCallableWMIs` feature by passing a `--noSquash`
command-line option.

## Version 0.26.0122b
- Added more documentation in `DevelopersGuide.md` related to the SMT output. Also,
added table of contents inside the `CHANGELOG.md`.

## Version 0.26.0122a
- Removed some `-02` optimization flags from `.gitlab-ci.yml` and the
`-fsimpl-tick-factor=200` GHC flag that boosted the tick limit for
[`BranchContextSpec.hs`](./blaze/test/general/Blaze/Cfg/Solver/BranchContextSpec.hs).

## Version 0.26.0121b
- Fixed `GLOBAL` formatting in Flint's `SMTish` module. Previously, it only
showed the symbol of the global, or the address if there was no symbol. Now
we show it as a JSON record field with both name and address.

## Version 0.26.0121a
- Corrected `.gitignore` to ignore `stack.yaml.lock` and added changelog
update enforcement to CI.

## Version 0.26.0120a
- Eliminated unnecessary duplication of function name data in `CallOp`s and
`TailCallOp`s.
