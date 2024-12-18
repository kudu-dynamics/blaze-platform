set positional-arguments

stack_options := ""
stack_build_options := ""
stack_haddock_options := ""

stackage_snapshot := `grep -oE '^resolver: .*$' stack.yaml | sed -E -e 's/resolver:\s*//' -e 's/\s*$//'`
haddock_remote := "https://www.stackage.org/haddock/{{stackage_snapshot}}/"

default:
    just --list

build:
    stack {{stack_options}} build --test --no-run-tests {{stack_build_options}}

test-all: test-ghidra test-binaryninja test-blaze test-flint
test-ghidra: (test "ghidra:ghidra-test")
test-binaryninja: (test "binaryninja:binja-test") (test "binja-header-cleaner:binja-header-cleaner-test")

test-blaze: test-blaze-general test-blaze-ghidra test-blaze-binaryninja
test-blaze-general: (test "blaze:blaze-general-test")
test-blaze-ghidra: (test "blaze:blaze-ghidra-importer-test")
test-blaze-binaryninja: (test "blaze-binaryninja:blaze-binja-test")

test-flint: test-flint-general test-flint-binaryninja
test-flint-general: (test "flint:flint-general-test")
test-flint-binaryninja: (test "flint-binaryninja:flint-binaryninja-test")

test target:
    stack {{stack_options}} test {{stack_build_options}} {{target}}

hlint *hlint_args:
    #!/usr/bin/env bash
    hlint "$@" \
        ghidra-haskell/{src,test,app} \
        binaryninja-haskell/{src,test} \
        binaryninja-haskell/binja-header-cleaner/{src,app} \
        blaze/{src,test,app} \
        blaze/blaze-binaryninja/{src,test,demo,app} \
        flint/{src,test,demo,app} \
        flint/flint-binaryninja/test

# docs:
#     stack {{stack_options}} haddock $(STACK_HADDOCK_OPTIONS)
#     mkdir -p docs
#     bash -c ' \
#         shopt -s nullglob && \
#         cp -ar $$(stack {{stack_options}} path --haddock --local-doc-root ${STACK_HADDOCK_OPTIONS})/{binary-analysis-*,binaryninja-*,blaze-*,doc-index*,index.html,*.css,*.js,*.png} docs/ \
#         '
#     find docs/ -name '*.html' -type f -print0 | xargs -0 sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"${haddock_remote}"'\1|g'
#     find docs/ -maxdepth 1 -type d -printf '%P\n' | \
#         while read d; do \
#             find docs/ -name '*.html' -type f -exec sed -i 's|<a href="'"${haddock_remote}$$d"'/docs|<a href="../'"$$d"'|g' {} \; ; \
#         done

tags:
    hasktags -R -e ghidra-haskell binaryninja-haskell blaze flint

sub-tags: tags
    cp TAGS blaze/TAGS
    cp TAGS flint/TAGS
    cp TAGS binaryninja-haskell/TAGS
    cp TAGS ghidra-haskell/TAGS

clean:
    stack {{stack_options}} clean
