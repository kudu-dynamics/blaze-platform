# Build with `docker build .. -f Dockerfile`, i.e., it needs binary-analysis in the context
ARG BLAZE_BINARYNINJA_HASKELL_BASE_IMAGE=${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/devops/binaryninja-haskell-base:latest

FROM ${BLAZE_BINARYNINJA_HASKELL_BASE_IMAGE} as base

COPY /binary-analysis/ /blaze/binary-analysis/
COPY /binaryninja-haskell/ /blaze/binaryninja-haskell/

# Update Binary Ninja. Purposely put after the COPY instructions so that we're always
# pulling latest binary ninja

ARG BLAZE_BINJA_CHANNEL=dev
ARG BLAZE_BINJA_VERSION=LATEST
ARG BLAZE_BINJA_API_COMMIT=origin/dev
RUN /usr/local/bin/binja_api_update "${BLAZE_BINJA_API}" "${BLAZE_BINJA_API_COMMIT}"
RUN python3 /usr/local/bin/binjaupdater.py "${BLAZE_BINJA_CHANNEL}" "${BLAZE_BINJA_VERSION}"

WORKDIR /blaze/binaryninja-haskell
RUN stack build --test --no-run-tests --ghc-options -fdiagnostics-color=always
