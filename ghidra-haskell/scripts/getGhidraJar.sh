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

GHIDRA_RELEASE_ZIP="https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_10.3_build/ghidra_10.3_PUBLIC_20230510.zip"
GHIDRA_SHA256="4e990af9b22be562769bb6ce5d4d609fbb45455a7a2f756167b8cdcdb75887fc"

d="$(mktemp -d)"
mkdir -p "$d"/extracted
function cleanup() {
    rm -r "$d"
}
trap cleanup EXIT

curl -fSL --proto '=https' --tlsv1.2 "${GHIDRA_RELEASE_ZIP}" >"$d"/ghidra.zip
echo "${GHIDRA_SHA256} /$d/ghidra.zip" | sha256sum -c
unzip "$d"/ghidra.zip -d "$d"/extracted
cd "$d"/extracted/ghidra*
./support/buildGhidraJar -output "$output"
