stackage_snapshot := $(shell grep -oE '^resolver: .*$$' stack.yaml | sed -E -e 's/resolver:\s*//' -e 's/\s*$$//')
haddock_remote := https://www.stackage.org/haddock/${stackage_snapshot}/

.PHONY: build help test submodule-init submodule-update hlint

build:
	stack build --test --no-run-tests

test: build
	.ci/scripts/run_test.py $$(stack path --dist-dir)/build/blaze-test/blaze-test

help:
	@cat Makefile

submodule-init:
	git submodule init
	git submodule update

submodule-update:
	git submodule update --remote --merge

hlint:
	hlint src test demo

docs: src/ package.yaml stack.yaml
	stack haddock
	mkdir -p docs
	bash -c ' \
		shopt -s nullglob && \
		cp -ar $$(stack path --haddock --local-doc-root)/{binary-analysis-*,binaryninja-*,blaze-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
		'
	find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"${haddock_remote}"'\1|g'
	find docs/ -maxdepth 1 -type d -printf '%P\n' | \
		while read d; do \
			find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"${haddock_remote}$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
		done
