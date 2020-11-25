ARG DOCKER_REGISTRY=gitlab.mp.kududyn.io:5005
ARG HASKELL_BINARYNINJA_BASE_TAG=master
ARG HASKELL_BINARYNINJA_BASE_IMAGE=${DOCKER_REGISTRY}/blaze/blaze-system/haskell-binaryninja-base:${HASKELL_BINARYNINJA_BASE_TAG}

FROM ${HASKELL_BINARYNINJA_BASE_IMAGE}

RUN apt update && apt install -y git

COPY binary-analysis /src/binary-analysis
COPY haskell-binja /src/haskell-binja
COPY binja-header-cleaner /src/binja-header-cleaner

COPY blaze/package.yaml blaze/stack.yaml /src/blaze/
WORKDIR /src/blaze

RUN stack build --only-dependencies
