#!/usr/bin/env bash

case "$#" in
    0)
        output=res/ghidra.jar
        ;;
    1)
        output="$1"
        ;;
    *)
        echo "USAGE: $0 [OUTPUT_PATH]" >&2
        exit 2
esac

set -euxo pipefail


GHIDRA_RELEASE_ZIP="https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_11.3.1_build/ghidra_11.3.1_PUBLIC_20250219.zip"
GHIDRA_SHA256="bcda0a9de8993444766cc255964c65c042b291ddaf6c50d654e316e442b441fa"

d="$(mktemp -d)"
mkdir -p "$d"/extracted
function cleanup() {
    rm -r "$d"
}
trap cleanup EXIT

curl -fSL --proto '=https' --tlsv1.2 "${GHIDRA_RELEASE_ZIP}" >"$d"/ghidra.zip
#echo "${GHIDRA_SHA256} /$d/ghidra.zip" | sha256sum -c
unzip "$d"/ghidra.zip -d "$d"/extracted
"$d"/extracted/ghidra*/support/buildGhidraJar -output "$output"
