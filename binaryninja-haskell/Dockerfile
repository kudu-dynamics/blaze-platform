# Build with `docker build .. -f Dockerfile`, i.e., it needs binary-analysis in the context
ARG CI_REGISTRY
ARG CI_PROJECT_NAMESPACE
ARG BLAZE_BINARYNINJA_HASKELL_BASE_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/devops/binaryninja-haskell-base:latest

FROM ${BLAZE_BINARYNINJA_HASKELL_BASE_IMAGE} as main
ARG BUILD_TYPE=dev
ARG STACK_BUILD_OPTIONS=

# Copy binary-analysis
COPY /binary-analysis/ /blaze/build/binary-analysis/

# Copy binaryninja-haskell project definition for building dependencies
COPY \
    /binaryninja-haskell/stack*.yaml \
    /binaryninja-haskell/package.yaml \
    /binaryninja-haskell/Makefile \
    /blaze/build/binaryninja-haskell/
COPY /binaryninja-haskell/binja-header-cleaner/ /blaze/build/binaryninja-haskell/binja-header-cleaner/

WORKDIR /blaze/build/binaryninja-haskell

# Build binaryninja-haskell dependencies only
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=bnhs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=bnhs-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=bnhs-bnhshc-stackwork,target=/blaze/build/binaryninja-haskell/binja-header-cleaner/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --only-dependencies" \
        BUILD_TYPE="${BUILD_TYPE}" \
        build

# Copy binary-analysis and binaryninja-haskell source dist
COPY /binary-analysis/ /blaze/src/binary-analysis/
COPY /binaryninja-haskell/ /blaze/src/binaryninja-haskell/

# Copy and build binaryninja-haskell
COPY /binaryninja-haskell/ /blaze/build/binaryninja-haskell/
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=bnhs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=bnhs-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=bnhs-bnhshc-stackwork,target=/blaze/build/binaryninja-haskell/binja-header-cleaner/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --copy-bins" \
        BUILD_TYPE="${BUILD_TYPE}" \
        TEST_BIN_DEST_DIR="${HOME}/.local/bin" \
        copy-tests

FROM main as docs
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=bnhs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=bnhs-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=bnhs-bnhshc-stackwork,target=/blaze/build/binaryninja-haskell/binja-header-cleaner/.stack-work \
    make docs

FROM main
