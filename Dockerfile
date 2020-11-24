ARG DOCKER_REGISTRY=gitlab.mp.kududyn.io:5005
ARG TAG=latest
ARG HASKELL_BINARYNINJA_BASE_TAG=${TAG}
ARG HASKELL_BINARYNINJA_BASE_IMAGE=${DOCKER_REGISTRY}/blaze/blaze-system/haskell-binaryninja-base:${HASKELL_BINARYNINJA_BASE_TAG}

FROM ${HASKELL_BINARYNINJA_BASE_IMAGE}
ARG STACK_ROOT
ENV STACK_ROOT=${STACK_ROOT}
RUN stack path
RUN false

COPY binary-analysis haskell-binja binja-header-cleaner /src
COPY blaze.cabal package.yaml stack.yaml stack.yaml.lock /src/blaze
WORKDIR /src/blaze
RUN stack build --only-dependencies
