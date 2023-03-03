# syntax=docker/dockerfile:1.4

ARG CI_REGISTRY
ARG CI_PROJECT_NAMESPACE
ARG BLAZE_BINARYNINJA_HASKELL_BASE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/devops/binaryninja-haskell-base:latest
ARG BLAZE_BINARYNINJA_HASKELL_BINDINGS=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/binaryninja-haskell/binaryninja-haskell:latest
ARG GHIDRA_HASKELL=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/ghidra-haskell/ghidra-haskell:latest

FROM ${GHIDRA_HASKELL} as ghidra-haskell

FROM ${BLAZE_BINARYNINJA_HASKELL_BASE} as z3
RUN --mount=type=cache,id=blaze-apt,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,id=blaze-apt-lists,target=/var/apt/lists,sharing=locked \
    apt update -yq &&                 \
    apt install -yq --no-install-recommends \
        cmake                         \
        ninja-build                   \
        python3-distutils  # z3 relies on this for some reason

# Build and install z3
RUN git clone --depth=1 https://github.com/Z3Prover/z3 /tmp/z3
RUN mkdir /tmp/z3/build && \
    cd /tmp/z3/build && \
    cmake -G Ninja -D CMAKE_INSTALL_PREFIX:PATH=/usr/local .. && \
    cmake --build . && \
    cmake --install .


FROM ${BLAZE_BINARYNINJA_HASKELL_BINDINGS} as main
ARG BUILD_TYPE=dev
ARG STACK_BUILD_OPTIONS=

RUN --mount=type=cache,id=blaze-apt,target=/var/cache/apt,sharing=private \
    --mount=type=cache,id=blaze-apt-lists,target=/var/apt/lists,sharing=private \
<<EOF
    apt update -yq
    apt install -yq --no-install-recommends openjdk-11-jdk
EOF

ENV LIBRARY_PATH="${LIBRARY_PATH:+"$LIBRARY_PATH:"}/usr/lib/jvm/java-11-openjdk-amd64/lib/server"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+"$LD_LIBRARY_PATH:"}/usr/lib/jvm/java-11-openjdk-amd64/lib/server"
ENV C_INCLUDE_PATH="${LIBRARY_PATH:+"$LIBRARY_PATH:"}/usr/lib/jvm/java-11-openjdk-amd64/include:/usr/lib/jvm/java-11-openjdk-amd64/include/linux"

COPY --from=ghidra-haskell /blaze/build/ghidra-haskell/ /blaze/build/ghidra-haskell/

# Copy project definition for building dependencies
COPY \
    stack*.yaml \
    package.yaml \
    Makefile \
    /blaze/build/blaze/

WORKDIR /blaze/build/blaze

# Build dependencies only
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --only-dependencies" \
        BUILD_TYPE="${BUILD_TYPE}" \
        build

# Copy source dist
COPY ./ /blaze/src/blaze

# Copy and build
COPY ./ /blaze/build/blaze
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    make \
        STACK_BUILD_OPTIONS="${STACK_BUILD_OPTIONS} --copy-bins" \
        BUILD_TYPE="${BUILD_TYPE}" \
        TEST_BIN_DEST_DIR="${HOME}/.local/bin" \
        copy-tests

# Copy in Z3
COPY --from=z3 /usr/local/bin/z3 /usr/local/bin/z3
# Think we don't need z3-the-library, just z3-the-executable, but uncomment these if we do
# COPY --from=z3 /usr/local/include/z3* /usr/local/include/
# COPY --from=z3 /usr/local/lib/libz3* /usr/local/lib/
# COPY --from=z3 /usr/local/lib/pkgconfig/z3.pc /usr/local/lib/pkgconfig/z3.pc

FROM main as docs
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    make docs

FROM main
