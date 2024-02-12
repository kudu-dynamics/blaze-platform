# syntax=docker/dockerfile:1.4

ARG BINARYNINJA_CHANNEL=release
ARG BINARYNINJA_VERSION=3.4.4271
# ARG BINARYNINJA_API_VERSION=v3.4.4271-stable

FROM ubuntu:23.10 as before-base-deps
SHELL ["/bin/bash", "-c"]
ENV DEBIAN_FRONTEND=noninteractive
RUN <<EOF
    set -euxo pipefail
    apt update -yq
    packages=(
        ca-certificates           # Downloads (curl, git, etc)
        fonts-dejavu-core         # Smallest font that satisfies openjdk dependencies
        dbus                      # Binary Ninja
        openjdk-17-jre-headless   # Ghidra
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF
RUN echo '/usr/lib/jvm/java-17-openjdk-amd64/lib/server' >/etc/ld.so.conf.d/jvm.conf && ldconfig


# Artifacts:
#   - /out/z3
FROM before-base-deps as z3-builder
RUN <<EOF
    set -euxo pipefail
    packages=(
        curl              # Downloading z3.zip
        libarchive-tools  # Extracting z3.zip
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF
RUN <<EOF
    set -euxo pipefail
    mkdir -p /out /out/extracted
    curl --proto '=https' --tlsv1.2 -fsSL \
        'https://github.com/Z3Prover/z3/releases/download/z3-4.12.5/z3-4.12.5-x64-glibc-2.35.zip' \
        >/out/z3.zip
    echo 'f036574d5e2029c9204fff3503cfe68ddf41fa6fdebb39beed99e1bf355b7fee /out/z3.zip' | sha256sum -c
    bsdtar xvf /out/z3.zip -C /out/extracted --strip-components=1
    mv /out/extracted/bin/z3 /out/z3
    rm -rf /out/z3.zip /out/extracted/
EOF


# Artifacts:
#   - /out/binaryninja
# Caveats:
#   - Run `ln -s $outpath/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so`
#     and `ln -s $outpath/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1`
FROM --platform=linux/amd64 before-base-deps as binaryninja-builder
ARG BINARYNINJA_CHANNEL
ARG BINARYNINJA_VERSION
# ARG BINARYNINJA_API_VERSION

RUN <<EOF
    set -euxo pipefail
    packages=(
        libarchive-tools  # Extracting BinaryNinja.zip
        python3           # Updating Binary Ninja
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN --mount=type=bind,source=BinaryNinja.zip,target=/BinaryNinja.zip \
    --mount=type=bind,source=license.dat,target=/root/.binaryninja/license.dat \
    --mount=type=bind,source=.docker/binary_ninja_updater.py,target=/binary_ninja_updater.py \
<<EOF
    set -euxo pipefail
    mkdir -p /out/binaryninja
    bsdtar xvf /BinaryNinja.zip --strip-components=1 -C /out/binaryninja
    PYTHONPATH=/out/binaryninja/python python3 -u /binary_ninja_updater.py "${BINARYNINJA_CHANNEL}" "${BINARYNINJA_VERSION}"
EOF


FROM before-base-deps as base
COPY --from=z3-builder /out/z3 /usr/local/bin/z3
COPY --from=binaryninja-builder /out/binaryninja /binaryninja
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1
ENV BINJA_PLUGINS=/binaryninja/plugins


# # Artifacts:
# #   - /out/bin/z3
# FROM base as z3-builder
# RUN <<EOF
#     set -euxo pipefail
#     packages=(
#         build-essential
#         cmake
#         ninja-build
#         git
#         python3-distutils
#     )
#     apt install -yq --no-install-recommends "${packages[@]}"
# <<EOF
# RUN <<EOF
#     git clone --depth=1 https://github.com/Z3Prover/z3 /tmp/z3
#     mkdir /tmp/z3/build
#     cd /tmp/z3/build
#     cmake -G Ninja -D CMAKE_INSTALL_PREFIX:PATH=/out ..
#     cmake --build .
#     cmake --install .
# EOF


FROM base as haskell
RUN <<EOF
    set -euxo pipefail
    packages=(
        # GHC build dependencies
        build-essential
        curl
        libffi-dev
        libgmp-dev
        libncurses-dev
        libnuma-dev

        # Haskell dependencies
        git
        pkg-config
        zlib1g-dev

        # Making ghidra.jar
        unzip
        openjdk-17-jdk-headless

        # Running tests
        python3
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN <<EOF
    set -euxo pipefail
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
EOF
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin${PATH:+:$PATH}"

RUN <<EOF
    set -euxo pipefail
    curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh \
    | bash -s -- --to /usr/local/bin
EOF


FROM haskell as hlint
RUN <<EOF
    set -euxo pipefail
    cabal update
    mkdir -p ~/.local/bin
    cabal install hlint \
        --constraint 'hlint == 3.5.*' \
        --overwrite-policy=always \
        --install-method=copy \
        --installdir ~/.local/bin
EOF


# Artifacts:
#   - /out/bin/*
#   - /out/test/*
#   - /out/run-tests
FROM haskell as builder

RUN cat <<EOF >root/.stack/config.yaml
extra-lib-dirs:
  - /usr/lib/jvm/java-17-openjdk-amd64/lib/server
extra-include-dirs:
  - /usr/lib/jvm/java-17-openjdk-amd64/include
  - /usr/lib/jvm/java-17-openjdk-amd64/include/linux
EOF

RUN --mount=type=bind,source=ghidra-haskell/scripts/getGhidraJar.sh,target=/getGhidraJar.sh \
<<EOF
    set -euxo pipefail
    mkdir res
    ./getGhidraJar.sh
    mv res/ghidra.jar .
    rm -d res
EOF

# COPY --from=binaryninja-builder /binaryninja /binaryninja
# RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
# RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1
# ENV BINJA_PLUGINS=/binaryninja/plugins

# COPY --from=z3-builder /out/bin/z3 /usr/local/bin/z3

WORKDIR /build

# Copy stack.yaml and package.yaml files so we can build dependencies in a
# cached layer
COPY stack.yaml stack.yaml
COPY binaryninja-haskell/package.yaml \
     binaryninja-haskell/package.yaml
COPY binaryninja-haskell/binja-header-cleaner/package.yaml \
     binaryninja-haskell/binja-header-cleaner/package.yaml
COPY ghidra-haskell/package.yaml \
     ghidra-haskell/package.yaml
COPY blaze/package.yaml \
     blaze/package.yaml
COPY flint/package.yaml \
     flint/package.yaml

RUN stack build --only-dependencies binaryninja binja-header-cleaner ghidra blaze flint

COPY ./ ./
RUN mv /ghidra.jar ghidra-haskell/res/ghidra.jar
RUN mkdir -p /out/bin /out/test
RUN <<EOF
    set -euxo pipefail
    stack --local-bin-path /out/bin build --test --no-run-tests --copy-bins

    echo 'set -euxo pipefail' >/out/run-tests
    echo 'cd /build' >>/out/run-tests
    chmod 755 /out/run-tests
    dist_dir="$(stack path --dist-dir)"
    function copytest() {
        cp -t /out/test "$1"/"${dist_dir}"/build/"$2"/"$2"
        echo "( cd $1 && /build/.docker/run_test.py /out/test/$2 \"\$@\" )" >>/out/run-tests
    }
    copytest ghidra-haskell ghidra-test
    copytest binaryninja-haskell binja-test
    copytest binaryninja-haskell/binja-header-cleaner binja-header-cleaner-test
    copytest blaze blaze-general-test
    copytest blaze blaze-ghidra-test
    copytest blaze blaze-binja-test
    copytest flint flint-tests
EOF


FROM base as deliver
# COPY ./ /src/
COPY --from=builder /out/ /out/
COPY --from=builder /build/ghidra-haskell/res/ghidra.jar /out/res/ghidra.jar
WORKDIR /out
