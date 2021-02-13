# Build with `docker build .. -f Dockerfile`, i.e., it needs binary-analysis in the context
ARG DEPS_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/binaryninja-haskell/binaryninja-haskell-deps:latest

FROM ${DEPS_IMAGE}

COPY /binary-analysis/ /blaze/binary-analysis/
COPY /binaryninja-haskell/ /blaze/binaryninja-haskell/

RUN cd /blaze/binaryninja-haskell && \
    stack build --only-dependencies --test --no-run-tests
