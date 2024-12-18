# syntax=docker/dockerfile:1.4@sha256:9ba7531bd80fb0a858632727cf7a112fbfd19b17e94c4e84ced81e24ef1a0dbc

ARG BINARYNINJA_CHANNEL=release
ARG BINARYNINJA_VERSION=3.4.4271
ARG HLINT_VERSION=3.6.1
# ARG BINARYNINJA_API_VERSION=v3.4.4271-stable

FROM ubuntu:noble as before-base-deps
SHELL ["/bin/bash", "-c"]
ENV DEBIAN_FRONTEND=noninteractive
RUN <<EOF
    set -euxo pipefail
    apt update -yq
    packages=(
        ca-certificates           # Downloads (curl, git, etc)
        fonts-dejavu-core         # Smallest font that satisfies openjdk dependencies
        dbus                      # Binary Ninja
        libharfbuzz0b             # JDK libfontmanager needs this but openjdk-21-jre-headless made it recommended only
        openjdk-21-jre-headless   # Ghidra
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF
RUN echo '/usr/lib/jvm/java-21-openjdk-amd64/lib/server' >/etc/ld.so.conf.d/jvm.conf && ldconfig


# Artifacts:
#   - /out/z3
FROM before-base-deps as z3-builder
RUN <<EOF
    set -euxo pipefail
    apt update -yq
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

RUN <<EOF
    set -euxo pipefail
    apt update -yq
    packages=(
        unzip    # Extracting BinaryNinja.zip
        python3  # Updating Binary Ninja
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN mkdir -p /out
RUN --mount=type=bind,source=BinaryNinja.zip,target=/BinaryNinja.zip \
    unzip /BinaryNinja.zip 'binaryninja/*' -d /out
RUN --mount=type=bind,source=license.dat,target=/root/.binaryninja/license.dat \
    --mount=type=bind,source=.docker/binary_ninja_updater.py,target=/binary_ninja_updater.py \
    PYTHONPATH=/out/binaryninja/python python3 -u /binary_ninja_updater.py "${BINARYNINJA_CHANNEL}" "${BINARYNINJA_VERSION}"


# Artifacts:
#   - /out/ghidra.jar
FROM before-base-deps as ghidra-jar-builder
RUN <<EOF
    set -euxo pipefail
    apt update -yq
    packages=(
        curl
        openjdk-21-jdk-headless
        unzip
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF
RUN mkdir -p /out
RUN --mount=type=bind,source=ghidra-haskell/scripts/getGhidraJar.sh,target=/getGhidraJar.sh \
    /getGhidraJar.sh /out/ghidra.jar


FROM before-base-deps as base
COPY --from=z3-builder /out/z3 /usr/local/bin/z3
COPY --from=ghidra-jar-builder /out/ghidra.jar /out/res/ghidra.jar


FROM base as haskell
RUN <<EOF
    set -euxo pipefail
    apt update -yq
    packages=(
        # GHC build dependencies
        build-essential
        curl
        libffi-dev
        libgmp-dev
        libncurses-dev
        libnuma-dev

        # Haskell dependencies
        git                      # Cloning inline-java and binary-analysis
        openjdk-21-jdk-headless  # All we need this for is jni.h. Sigh
        pkg-config
        yq                       # Determining ghc version from stack.yaml
        zlib1g-dev

        # Running tests
        python3
    )
    apt install -yq --no-install-recommends "${packages[@]}"
EOF

RUN <<EOF
    set -euxo pipefail
    curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh \
    | bash -s -- --to /usr/local/bin
EOF

ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin${PATH:+:$PATH}"
RUN --mount=type=bind,source=stack.yaml,target=/stack.yaml \
<<EOF
    set -euxo pipefail
    if compiler_string="$(yq -r .compiler /stack.yaml)"; then
        export BOOTSTRAP_HASKELL_GHC_VERSION="$(echo "${compiler_string}" | sed 's/^ghc-//')"
    fi
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
      sh
EOF

# Install hlint
ARG HLINT_VERSION
RUN <<EOF
    set -euxo pipefail
    temp="$(mktemp -d)"
    cd "$temp"
    curl -fsSL --proto =https --tlsv1.2 https://github.com/ndmitchell/hlint/releases/download/v"$HLINT_VERSION"/hlint-"$HLINT_VERSION"-x86_64-linux.tar.gz | \
        tar -xz
    mkdir -p ~/.local/bin
    cp hlint-"$HLINT_VERSION"/hlint ~/.local/bin/hlint
    chmod +x ~/.local/bin/hlint
    cd /
    rm -rf "$temp"
EOF


FROM haskell as just-deps
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
COPY blaze/blaze-binaryninja/package.yaml \
     blaze/blaze-binaryninja/package.yaml
COPY flint/package.yaml \
     flint/package.yaml
COPY flint/flint-binaryninja/package.yaml \
     flint/flint-binaryninja/package.yaml

RUN stack build --color always --ghc-options="${OPTIM}" --only-dependencies \
    binaryninja binja-header-cleaner ghidra blaze blaze/blaze-binaryninja flint flint/flint-binaryninja


FROM just-deps as just-deps-binaryninja
COPY --from=binaryninja-builder /out/binaryninja /binaryninja
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1
ENV BINJA_PLUGINS=/binaryninja/plugins


# Artifacts:
#   - /out/bin/*
#   - /out/test/*
#   - /out/run-tests
FROM just-deps as builder
ARG OPTIM=-O0

COPY ./ ./
RUN ln -s /out/res/ghidra.jar ghidra-haskell/res/ghidra.jar
RUN mkdir -p /out/bin
# Build all packages that do not depend on binaryninja
RUN stack --local-bin-path /out/bin build --color always --ghc-options="${OPTIM}" --test --no-run-tests --copy-bins \
    ghidra blaze flint
RUN mkdir -p /out/test
RUN <<EOF
    set -euxo pipefail

    echo 'set -euxo pipefail' >/out/run-tests
    echo 'cd /build' >>/out/run-tests
    chmod 755 /out/run-tests
    dist_dir="$(stack path --dist-dir)"
    function copytest() {
        cp -t /out/test "$1"/"${dist_dir}"/build/"$2"/"$2"
        echo "( cd $1 && /build/.docker/run_test.py /out/test/$2 \"\$@\" )" >>/out/run-tests
    }
    copytest ghidra-haskell ghidra-test
    copytest blaze blaze-general-test
    copytest blaze blaze-ghidra-importer-test
    copytest flint flint-general-test
EOF


# Artifacts:
#   - /out/bin/*
#   - /out/test/*
#   - /out/run-tests
FROM builder as builder-binaryninja
COPY --from=binaryninja-builder /out/binaryninja /binaryninja
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1
ENV BINJA_PLUGINS=/binaryninja/plugins
# Build the rest of the packages that were not built in `builder`
RUN stack --local-bin-path /out/bin build --color always --ghc-options="${OPTIM}" --test --no-run-tests --copy-bins
RUN <<EOF
    set -euxo pipefail

    dist_dir="$(stack path --dist-dir)"
    function copytest() {
        cp -t /out/test "$1"/"${dist_dir}"/build/"$2"/"$2"
        echo "( cd $1 && /build/.docker/run_test.py /out/test/$2 \"\$@\" )" >>/out/run-tests
    }
    copytest binaryninja-haskell binja-test
    copytest binaryninja-haskell/binja-header-cleaner binja-header-cleaner-test
    copytest blaze/blaze-binaryninja blaze-binja-test
    copytest blaze/blaze-binaryninja blaze-binaryninja-importer-test
    copytest flint/flint-binaryninja flint-binaryninja-test
EOF


FROM base as deliver-binaryninja
COPY --from=builder-binaryninja /out/ /out/
COPY --from=builder-binaryninja /build/ghidra-haskell/res/ghidra.jar /out/res/ghidra.jar
COPY --from=builder-binaryninja /binaryninja /binaryninja
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so
RUN ln -s /binaryninja/libbinaryninjacore.so.1 /usr/lib/libbinaryninjacore.so.1
ENV BINJA_PLUGINS=/binaryninja/plugins
WORKDIR /out


FROM base as deliver
COPY --from=builder /out/ /out/
COPY --from=builder /build/ghidra-haskell/res/ghidra.jar /out/res/ghidra.jar
WORKDIR /out
