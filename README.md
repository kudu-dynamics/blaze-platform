# Blaze Platform

Monorepo for Blaze, Flint, and binary lifter backends

## Running

### Using Docker

`docker-compose run --env DOCKER_REGISTRY={ DOCKER_REGISTRY_URL } { SERVICE_NAME } [ ARGS... ]`

## Building

### Using Docker

As simple as `docker build .` or `docker-compose build`.
You can increase the GHC optimization level with `--build-arg OPTIM=-O2` for example (the default is `-O0`).

For now, Blaze has a hard dependency on both ghidra-haskell as well as binaryninja-haskell, even if you only plan on using one or the other at run-time.
We have plans to make these soft dependencies, but what this means is, for now, you need to have both Binary Ninja and Ghidra available.
Luckily, Ghidra is a FOSS project, and we can just grab it while building the Docker image.
Unfortunately, there isn't any way to download Binary Ninja headlessly[^download-binary-ninja-headlessly], so you'll need to provide your own.
Simply copy `BinaryNinja.zip` and `license.dat` (commercial or headless license) into this directory before running any `docker build` or `docker-compose build`/`docker-compose up` commands.
The version of `BinaryNinja.zip` doesn't matter, only that it's a linux build.

[^download-binary-ninja-headlessly]: There is [the official version switcher script](https://github.com/Vector35/binaryninja-api/blob/661c77ab75f1365910e925640577c36dc47c47c7/python/examples/version_switcher.py), but that needs an existing installation to use, so we can't bootstrap with it. There is also [the official headless download script](https://github.com/Vector35/binaryninja-api/blob/661c77ab75f1365910e925640577c36dc47c47c7/scripts/download_headless.py), but that _only_ works for "headless" licenses, and not for "commercial" licenses.

> [!NOTE]
>
> Because of our hard dependency on Binary Ninja, which only provides AMD64 builds for Linux, our whole Docker image is forced to be AMD64.
> If you run a different architecture (e.g., on Apple Silicon devices), then Docker has to emulate AMD64 (e.g., through QEMU, Rosetta, etc).
> This impacts build times significantly.
> For instance, on a 2023 M2 MacBook Pro, building from scratch takes about 40 minutes.
> You may want to consider using a Docker remote context with an AMD64 machine.
