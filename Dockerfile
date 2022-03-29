ARG CI_REGISTRY
ARG CI_PROJECT_NAMESPACE
ARG BLAZE_BINARYNINJA_HASKELL_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/binaryninja-haskell/binaryninja-haskell:latest

FROM ${BLAZE_BINARYNINJA_HASKELL_IMAGE} as main
RUN --mount=type=cache,id=blaze-apt,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,id=blaze-apt-lists,target=/var/apt/lists,sharing=locked \
    apt update -yq &&                 \
    apt install -yq --no-install-recommends \
        cmake                         \
        ninja-build                   \
        python3-distutils  # z3 relies on this for some reason

# Build and install z3
RUN cd /tmp && \
    git clone --depth=1 https://github.com/Z3Prover/z3 && \
    mkdir z3/build
RUN cd /tmp/z3/build && \
    cmake -GNinja ..
RUN cd /tmp/z3/build && \
    ninja
RUN cd /tmp/z3/build && \
    ninja install && \
    cd / && \
    rm -rf /tmp/z3

# Copy project definition for building dependencies
COPY \
    stack.yaml \
    package.yaml \
    /blaze/build/blaze/

WORKDIR /blaze/build/blaze

# Build dependencies only
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    stack build --only-dependencies --ghc-options -fdiagnostics-color=always

# Copy source dist
COPY ./ /blaze/src/blaze

# Copy and build
COPY ./ /blaze/build/blaze
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    stack build --test --no-run-tests --copy-bins --ghc-options -fdiagnostics-color=always && \
    cp $(stack path --dist-dir)/build/blaze-test/blaze-test ~/.local/bin

FROM main as docs
RUN --mount=type=cache,id=blaze-stackroot,target=/root/.stack \
    --mount=type=cache,id=blaze-ba-stackwork,target=/blaze/build/binary-analysis/.stack-work \
    --mount=type=cache,id=blaze-bnhs-stackwork,target=/blaze/build/binaryninja-haskell/.stack-work \
    --mount=type=cache,id=blaze-blaze-stackwork,target=/blaze/build/blaze/.stack-work \
    make docs

FROM main
