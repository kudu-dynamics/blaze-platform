# Build with `docker build .. -f Dockerfile`, i.e., it needs binary-analysis in the context
ARG CI_REGISTRY
ARG CI_PROJECT_NAMESPACE
ARG BLAZE_HASKELL_JAVA_BASE_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/devops/haskell-java-base:latest

FROM ${BLAZE_HASKELL_JAVA_BASE_IMAGE} as ghidra-jar
COPY /ghidra-haskell/scripts/ /blaze/build/ghidra-haskell/scripts/
COPY /ghidra-haskell/Makefile /blaze/build/ghidra-haskell/Makefile
WORKDIR /blaze/build/ghidra-haskell
RUN mkdir res
RUN make res/ghidra.jar

FROM ${BLAZE_HASKELL_JAVA_BASE_IMAGE} as main
ARG BUILD_TYPE=dev
ARG STACK_BUILD_OPTIONS=

# Copy binary-analysis
COPY /binary-analysis/ /blaze/build/binary-analysis/

# Copy ghidra-haskell project definition for building dependencies
COPY \
    /ghidra-haskell/stack*.yaml \
    /ghidra-haskell/package.yaml \
    /ghidra-haskell/Makefile \
    /blaze/build/ghidra-haskell/

WORKDIR /blaze/build/ghidra-haskell

# Build ghidra-haskell dependencies only
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=ghs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=ghs-ghs-stackwork,target=/blaze/build/ghidra-haskell/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --only-dependencies" \
        BUILD_TYPE="${BUILD_TYPE}" \
        build

# Copy binary-analysis and ghidra-haskell source dist
COPY /binary-analysis/ /blaze/src/binary-analysis/
COPY /ghidra-haskell/ /blaze/src/ghidra-haskell/

# Copy and build ghidra-haskell
COPY /ghidra-haskell/ /blaze/build/ghidra-haskell/
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=ghs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=ghs-ghs-stackwork,target=/blaze/build/ghidra-haskell/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --copy-bins" \
        BUILD_TYPE="${BUILD_TYPE}" \
        TEST_BIN_DEST_DIR="${HOME}/.local/bin" \
        copy-tests
COPY --from=ghidra-jar /blaze/build/ghidra-haskell/res/ghidra.jar /blaze/build/ghidra-haskell/res/ghidra.jar

FROM main as docs
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=ghs-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=ghs-ghs-stackwork,target=/blaze/build/ghidra-haskell/.stack-work \
    make docs

FROM main
