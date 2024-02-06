# syntax=docker/dockerfile:1.4

FROM ubuntu:24.04 AS base
SHELL ["/bin/bash", "-c"]
ENV DEBIAN_FRONTEND=noninteractive
RUN --mount=type=cache,target=/var/cache/apt,sharing=private \
<<EOF
    apt update -yq
    packages=(
        # downloads (curl, git, etc)
        ca-certificates
        # running Binary Ninja
        dbus
        # Ghidra
        openjdk-17-jdk
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF
RUN echo '/usr/lib/jvm/java-17-openjdk-amd64/lib/server' >/etc/ld.so.conf.d/jvm.conf && ldconfig

# Artifacts:
#   - /binaryninja
#   - /binaryninja-api
#   - /root/.binaryninja/license.dat
#   - /etc/ld.so.conf.d/binaryninja.conf
# Caveats:
#   - Probably run `ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so`
FROM --platform=linux/amd64 base AS binaryninja-builder
ARG BINARYNINJA_CHANNEL=release
ARG BINARYNINJA_VERSION=3.4.4271
ARG BINARYNINJA_API_VERSION=v3.4.4271-stable

RUN --mount=type=cache,target=/var/cache/apt,sharing=private \
<<EOF
    packages=(
        git               # cloning binaryninja-api
        libarchive-tools  # extracting BinaryNinja.zip
        python3           # updating Binary Ninja
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN git clone --recursive --filter=blob:none --also-filter-submodules \
    -b "${BINARYNINJA_API_VERSION}" \
    https://github.com/Vector35/binaryninja-api.git \
    /binaryninja-api

RUN --mount=type=bind,source=BinaryNinja.zip,target=/BinaryNinja.zip \
    --mount=type=bind,source=license.dat,target=/root/.binaryninja/license.dat \
    --mount=type=bind,source=.docker/binary_ninja_updater.py,target=/binary_ninja_updater.py \
<<EOF
    mkdir /binaryninja
    bsdtar xvf /BinaryNinja.zip --strip-components=1 -C /binaryninja
    PYTHONPATH=/binaryninja/python python3 -u /binary_ninja_updater.py "${BINARYNINJA_CHANNEL}" "${BINARYNINJA_VERSION}"
EOF


FROM base AS haskell
RUN --mount=type=cache,target=/var/cache/apt,sharing=private \
<<EOF
    packages=(
        # ghc build requirements
        build-essential
        curl
        libffi-dev
        libgmp-dev
        libncurses-dev
        libnuma-dev
        # haskell package requirements
        git
        pkg-config
        zlib1g-dev
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN <<EOF
    set -o pipefail
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
EOF
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin${PATH:+:$PATH}"


FROM haskell AS hlint
RUN <<EOF
    cabal update
    mkdir -p ~/.local/bin
    cabal install hlint \
        --constraint 'hlint == 3.5.*' \
        --overwrite-policy=always \
        --install-method=copy \
        --installdir ~/.local/bin
EOF


FROM haskell AS builder
COPY --from=binaryninja-builder /binaryninja /binaryninja
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
RUN cat <<EOF >root/.stack/config.yaml
extra-lib-dirs:
  - /usr/lib/jvm/java-17-openjdk-amd64/lib/server
extra-include-dirs:
  - /usr/lib/jvm/java-17-openjdk-amd64/include
  - /usr/lib/jvm/java-17-openjdk-amd64/include/linux
EOF

WORKDIR /build

# Copy stack.yaml and package.yaml files so we can build dependencies in a
# cached layer
COPY stack.yaml /build/stack.yaml
COPY /binaryninja-haskell/package.yaml \
      binaryninja-haskell/package.yaml
COPY /binaryninja-haskell/binja-header-cleaner/package.yaml \
      binaryninja-haskell/binja-header-cleaner/package.yaml
COPY /ghidra-haskell/package.yaml \
      ghidra-haskell/package.yaml
COPY /blaze/package.yaml \
      blaze/package.yaml
COPY /flint/package.yaml \
      flint/package.yaml

RUN --mount=type=cache,target=/root/.stack,sharing=private \
    stack build --only-dependencies binaryninja binja-header-cleaner ghidra blaze flint

COPY . /build
# TODO copy to controlled location
RUN mkdir -p /root/.local/bin
RUN --mount=type=cache,target=/root/.stack,sharing=private \
    stack build --test --no-run-tests --copy-bins
