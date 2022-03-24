.PHONY: help build ops test-haskell-binja test-binja-header-cleaner test header hlint

stackage_snapshot := $(shell grep -oE '^resolver: .*$$' stack.yaml | sed -E -e 's/resolver:\s*//' -e 's/\s*$$//')
haddock_remote := https://www.stackage.org/haddock/${stackage_snapshot}/

build:
	stack build --test --no-run-tests

help:
	@cat Makefile

ops:
	stack run gen-mlil-op-modules src/Binja/Types/MLIL/Op

test-haskell-binja: build
	.ci/scripts/run_test.py $$(stack path --dist-dir)/build/binja-test/binja-test

test-binja-header-cleaner: build
	cd binja-header-cleaner && \
		../.ci/scripts/run_test.py \
			$$(stack path --dist-dir)/build/binja-header-cleaner-test/binja-header-cleaner-test

test: test-haskell-binja test-binja-header-cleaner

header:
	[ -n "$${BLAZE_BINJA_API}" ] || { echo '$$BLAZE_BINJA_API is not set' >/dev/stderr && exit 1 ; }
	cd binja-header-cleaner && stack run ${BLAZE_BINJA_API}/binaryninjacore.h ../res/binaryninjacore.h

hlint:
	hlint src test demo
	hlint binja-header-cleaner/{src,test}

docs: src/ package.yaml stack.yaml
	stack haddock
	mkdir -p docs
	bash -c ' \
		shopt -s nullglob && \
		cp -ar $$(stack path --haddock --local-doc-root)/{binary-analysis-*,binaryninja-*,binja-header-cleaner-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
		'
	find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"${haddock_remote}"'\1|g'
	find docs/ -maxdepth 1 -type d -printf '%P\n' | \
		while read d; do \
			find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"${haddock_remote}$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
		done
