# Blaze Platform

## Version 0.26.0121b
- Fixed `GLOBAL` formatting in Flint's `SMTish` module. Previously, it only showed the symbol of the global, or the address if there was no symbol. Now we show it as a JSON record field with both name and address.

## Version 0.26.0121a
- Corrected `.gitignore` to ignore `stack.yaml.lock` and added changelog
update enforcement to CI.

## Version 0.26.0120a
- Eliminated unnecessary duplication of function name data in `CallOp`s and
`TailCallOp`s.
