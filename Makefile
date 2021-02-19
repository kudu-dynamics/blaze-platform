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
	[ -n "$${BLAZE_BINJA_API}" ] || { echo '$$BLAZE_BINJA_API is not set' >/dev/stderr && exit 1 ; }
	cd binja-header-cleaner && stack run ${BLAZE_BINJA_API}/binaryninjacore.h ../res/binaryninjacore.h

hlint:
	hlint src test demo

