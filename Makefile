.PHONY: build test test-general test-binja copy-tests hlint docs clean

BUILD_TYPE ?= dev
CABAL_OPTIONS ?=
CABAL_BUILD_OPTIONS ?=
CABAL_HADDOCK_OPTIONS ?=

cabal_options_dev := --project-file=cabal.project.dev $(CABAL_OPTIONS)
cabal_options_release := $(CABAL_OPTIONS)
cabal_options := $(cabal_options_$(BUILD_TYPE))


build:
	cabal $(cabal_options) build $(CABAL_BUILD_OPTIONS)

stack-build-dev:
	stack --stack-yaml stack-dev.yaml build --test --no-run-tests

stack-build-release:
	stack --stack-yaml stack-dev.yaml build --test --no-run-tests

dev:
	cabal $(cabal_options_dev) build $(CABAL_BUILD_OPTIONS)

run:
	cabal $(cabal_options) run flint

hlint:
	hlint src test demo

tags:
	hasktags -R -e . ../ghidra-haskell ../binaryninja-haskell ../binary-analysis ../blaze

clean:
	cabal $(cabal_options) clean
