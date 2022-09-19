#!/usr/bin/env bash


GHIDRA_RELEASE_ZIP="https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_10.1.5_build/ghidra_10.1.5_PUBLIC_20220726.zip"
GHIDRA_SHA256="17db4ba7d411d11b00d1638f163ab5d61ef38712cd68e462eb8c855ec5cfb5ed"

wget -O /tmp/ghidra.zip $GHIDRA_RELEASE_ZIP
echo "${GHIDRA_SHA256} /tmp/ghidra.zip" | sha256sum -c
mkdir -p /tmp/ghidra_zip
unzip /tmp/ghidra.zip -d /tmp/ghidra_zip
cd res && /tmp/ghidra_zip/ghidra*/support/buildGhidraJar
rm /tmp/ghidra.zip

# # is it frowned upon to use rm -rf for cleanup in a script?
# rm -rf /tmp/ghidra_zip
