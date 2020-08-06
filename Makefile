help:
	@cat Makefile

build:
	stack build

test-haskell-binja:
	stack test

test-binja-header-cleaner:
	cd binja-header-cleaner && stack test

test: test-haskell-binja test-binja-header-cleaner

header:
	cd binja-header-cleaner && stack run ${BLAZE_BINJA_API}/binaryninjacore.h ../res/binaryninjacore.h
