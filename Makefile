.PHONY: build test-ghidra-haskell test copy-tests hlint docs clean

BUILD_TYPE ?= dev
STACK_OPTIONS ?=
STACK_BUILD_OPTIONS ?=
STACK_HADDOCK_OPTIONS ?=

stack_options_dev := --stack-yaml stack-dev.yaml $(STACK_OPTIONS)
stack_options_release := $(STACK_OPTIONS)
stack_options := $(stack_options_$(BUILD_TYPE))
stack_options_bhc := $(patsubst %.yaml,../%.yaml,$(stack_options))

stackage_snapshot := $(shell grep -oE '^resolver: .*$$' stack.yaml | sed -E -e 's/resolver:\s*//' -e 's/\s*$$//')
haddock_remote := https://www.stackage.org/haddock/${stackage_snapshot}/

build:
	stack $(stack_options) build --test --no-run-tests $(STACK_BUILD_OPTIONS)

res/ghidra.jar:
	scripts/getGhidraJar.sh

test-ghidra-haskell: build res/ghidra.jar
	.ci/scripts/run_test.py $$(stack $(stack_options) path --dist-dir)/build/ghidra-test/ghidra-test

test: test-ghidra-haskell

copy-tests: build
	if ! [ -d "$${TEST_BIN_DEST_DIR}" ]; then echo "TEST_BIN_DEST_DIR does not exist or is not a directory" >&2; exit 2; fi
	cp $$(stack $(stack_options) path --dist-dir)/build/ghidra-test/ghidra-test "$${TEST_BIN_DEST_DIR}"

hlint:
	hlint src test app

docs:
	stack $(stack_options) haddock $(STACK_HADDOCK_OPTIONS)
	mkdir -p docs
	bash -c ' \
		shopt -s nullglob && \
		cp -ar $$(stack $(stack_options) path --haddock $(STACK_HADDOCK_OPTIONS) --local-doc-root)/{binary-analysis-*,ghidra-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
		'
	find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"$(haddock_remote)"'\1|g'
	find docs/ -maxdepth 1 -type d -printf '%P\n' | \
		while read d; do \
			find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"$(haddock_remote)$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
		done

clean:
	stack $(stack_options) clean
