TAGS: $(wildcard ghidra-haselll/** binaryninja-haskell/** blaze/** flint/**)
	hasktags -R -e ghidra-haskell binaryninja-haskell blaze flint

ghidra-haskell/res/ghidra.jar: ghidra-haskell/Makefile
	$(MAKE) -C ghidra-haskell res/ghidra.jar
