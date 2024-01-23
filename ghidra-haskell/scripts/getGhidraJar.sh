#!/usr/bin/env bash

set -euxo pipefail

GHIDRA_RELEASE_ZIP="https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_10.3_build/ghidra_10.3_PUBLIC_20230510.zip"
GHIDRA_SHA256="4e990af9b22be562769bb6ce5d4d609fbb45455a7a2f756167b8cdcdb75887fc"

wget -O /tmp/ghidra.zip $GHIDRA_RELEASE_ZIP
echo "${GHIDRA_SHA256} */tmp/ghidra.zip" | shasum -c -a 256
mkdir -p /tmp/ghidra_zip
unzip /tmp/ghidra.zip -d /tmp/ghidra_zip
cd res && /tmp/ghidra_zip/ghidra*/support/buildGhidraJar
rm /tmp/ghidra.zip
rm -r /tmp/ghidra_zip
