help:
	@cat Makefile

build:
	stack build

header:
	stack run gen-clean-binja-header ${BLAZE_BINJA_API}/binaryninjacore.h
