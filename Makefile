help:
	@cat Makefile

build:
	stack build

header:
	cd binja-header-cleaner && stack run ${BLAZE_BINJA_API}/binaryninjacore.h ../res/binaryninjacore.h
