# Hinja

Tool to generate Haskell and Idris bindings and datatypes for binaryninja core.

Strategy:
* clean binaryninjacore.h file so language-c can parse it (needed by c2hs)
* use language-c parser to generate records for Structs, function declarations (enum modules can already be generated using ParseEnums)

