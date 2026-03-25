TAGS: $(wildcard ghidra-hasell/** binaryninja-haskell/** blaze/** flint/**)
	hasktags -R -e ghidra-haskell binaryninja-haskell blaze flint

ghidra-haskell/res/ghidra.jar: ghidra-haskell/Makefile
	$(MAKE) -C ghidra-haskell res/ghidra.jar

ghidra-haskell/res/pcode-helper.jar: ghidra-haskell/java/PcodeHelper.java ghidra-haskell/res/ghidra.jar
	$(MAKE) -C ghidra-haskell res/pcode-helper.jar

res/pcode-helper.jar: ghidra-haskell/res/pcode-helper.jar
	cp ghidra-haskell/res/pcode-helper.jar res/pcode-helper.jar
