#!/usr/bin/env bash

set -euxo pipefail

GHIDRA_RELEASE_ZIP="https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_10.2.3_build/ghidra_10.2.3_PUBLIC_20230208.zip"
GHIDRA_SHA256="daf4d85ec1a8ca55bf766e97ec43a14b519cbd1060168e4ec45d429d23c31c38"

wget -O /tmp/ghidra.zip $GHIDRA_RELEASE_ZIP
echo "${GHIDRA_SHA256} /tmp/ghidra.zip" | sha256sum -c
mkdir -p /tmp/ghidra_zip
unzip /tmp/ghidra.zip -d /tmp/ghidra_zip
cd res && /tmp/ghidra_zip/ghidra*/support/buildGhidraJar
rm /tmp/ghidra.zip
rm -r /tmp/ghidra_zip
