help:
	@cat Makefile

submodule-init:
	git submodule init
	git submodule update

submodule-update:
	git submodule update --remote --merge

build:
	stack build

hlint:
	hlint src test demo
