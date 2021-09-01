ARG CI_REGISTRY
ARG CI_PROJECT_NAMESPACE
ARG BLAZE_BINARYNINJA_HASKELL_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/binaryninja-haskell/binaryninja-haskell:latest

FROM ${BLAZE_BINARYNINJA_HASKELL_IMAGE}
RUN apt update && apt install -yq \
    cmake \
    ninja-build \
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

RUN z3 --version && which z3

RUN stack upgrade --binary-only

COPY ./ /blaze/build/blaze
WORKDIR /blaze/build/blaze
RUN stack build --test --no-run-tests --ghc-options -fdiagnostics-color=always
