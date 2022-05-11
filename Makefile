.PHONY: build ops test-haskell-binja test-binja-header-cleaner test copy-tests header hlint docs clean

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

ops:
	stack $(stack_options) run gen-mlil-op-modules src/Binja/Types/MLIL/Op

test-haskell-binja: build
	.ci/scripts/run_test.py $$(stack $(stack_options) path --dist-dir)/build/binja-test/binja-test

test-binja-header-cleaner: build
	cd binja-header-cleaner && \
		../.ci/scripts/run_test.py \
			$$(stack $(stack_options_bhc) path --dist-dir)/build/binja-header-cleaner-test/binja-header-cleaner-test

test: test-haskell-binja test-binja-header-cleaner

copy-tests: build
	if ! [ -d "$${TEST_BIN_DEST_DIR}" ]; then echo "TEST_BIN_DEST_DIR does not exist or is not a directory" >&2; exit 2; fi
	cp $$(stack $(stack_options) path --dist-dir)/build/binja-test/binja-test "$${TEST_BIN_DEST_DIR}"
	cd binja-header-cleaner && \
		cp $$(stack $(stack_options_bhc) path --dist-dir)/build/binja-header-cleaner-test/binja-header-cleaner-test "$${TEST_BIN_DEST_DIR}"

header:
	[ -n "$${BLAZE_BINJA_API}" ] || { echo '$$BLAZE_BINJA_API is not set' >/dev/stderr && exit 1 ; }
	cd binja-header-cleaner && \
		stack $(stack_options_bhc) run $(BLAZE_BINJA_API)/binaryninjacore.h ../res/binaryninjacore.h

hlint:
	hlint src test demo
	hlint binja-header-cleaner/{src,test}

docs:
	stack $(stack_options) haddock $(STACK_HADDOCK_OPTIONS)
	mkdir -p docs
	bash -c ' \
		shopt -s nullglob && \
		cp -ar $$(stack $(stack_options) path --haddock $(STACK_HADDOCK_OPTIONS) --local-doc-root)/{binary-analysis-*,binaryninja-*,binja-header-cleaner-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
		'
	find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"$(haddock_remote)"'\1|g'
	find docs/ -maxdepth 1 -type d -printf '%P\n' | \
		while read d; do \
			find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"$(haddock_remote)$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
		done

clean:
	stack $(stack_options) clean
