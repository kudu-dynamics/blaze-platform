.PHONY: build test copy-tests hlint docs clean

BUILD_TYPE ?= dev
STACK_OPTIONS ?=
STACK_BUILD_OPTIONS ?=
STACK_HADDOCK_OPTIONS ?=

stack_options_dev := --stack-yaml stack-dev.yaml $(STACK_OPTIONS)
stack_options_release := $(STACK_OPTIONS)
stack_options := $(stack_options_$(BUILD_TYPE))

stackage_snapshot := $(shell grep -oE '^resolver: .*$$' stack.yaml | sed -E -e 's/resolver:\s*//' -e 's/\s*$$//')
haddock_remote := https://www.stackage.org/haddock/${stackage_snapshot}/

build:
	stack $(stack_options) build --test --no-run-tests $(STACK_BUILD_OPTIONS)

test: build
	.ci/scripts/run_test.py $$(stack $(stack_options) path --dist-dir)/build/blaze-test/blaze-test

copy-tests: build
	if ! [ -d "$${TEST_BIN_DEST_DIR}" ]; then echo "TEST_BIN_DEST_DIR does not exist or is not a directory" >&2; exit 2; fi
	cp $$(stack $(stack_options) path --dist-dir)/build/blaze-test/blaze-test "$${TEST_BIN_DEST_DIR}"

hlint:
	hlint src test demo

docs:
	stack $(stack_options) haddock $(STACK_HADDOCK_OPTIONS)
	mkdir -p docs
	bash -c ' \
		shopt -s nullglob && \
		cp -ar $$(stack $(stack_options) path --haddock --local-doc-root ${STACK_HADDOCK_OPTIONS})/{binary-analysis-*,binaryninja-*,blaze-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
		'
	find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"${haddock_remote}"'\1|g'
	find docs/ -maxdepth 1 -type d -printf '%P\n' | \
		while read d; do \
			find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"${haddock_remote}$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
		done

clean:
	stack $(stack_options) clean
