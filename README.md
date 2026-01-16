# Blaze Platform

Monorepo for Blaze, Flint, and binary lifter backends

## Preparing Development Environment
If you want to use set up an environment to contribute to Flint, follow this [DevelopersGuide.md](DevelopersGuide.md) guide.

## Running

### Running with Docker

`DOCKER_REGISTRY` is an optional environment variable that is used in [`docker-compose.yaml`](docker-compose.yaml) to refer to a Docker registry that stores the images built by [`Dockerfile`](Dockerfile) and [`.gitlab-ci.yml`](.gitlab-ci.yml).
If you use a Docker registry, set this environment variable (e.g., `export DOCKER_REGISTRY=gitlab.example.com:1234`) to use the images stored in the registry.
Otherwise, leave it unset.

`docker-compose run { SERVICE_NAME } [ ARGS... ]`

### From source

For the most part, `stack run { TARGET } [ ARGS... ]`, but see each package's `README.md` for more information.

## Building

### Building with Docker

As simple as `docker build --platform=linux/amd64 .` or `docker-compose build`.
See the comments about `DOCKER_REGISTRY` in the [Running > Using Docker](#using-docker) section.
You can increase the GHC optimization level with `--build-arg OPTIM=-O2` for example (the default is `-O0`).

#### Blaze with Ghidra

The following commands build the blaze tooling with ghidra.
Ghidra is a FOSS project, and we can just grab it while building the Docker images.

##### Ghidra Blaze with Unit Tests

`docker build -t blaze .`
(This works since it is the last image in the Dockerfile)

OR

`docker build -t blaze --target deliver-tests .`

##### Ghidra Blaze without Unit Tests

`docker build -t blaze --target deliver .`

## Running Flint Using Docker

Flint:

```
docker run --rm -v /your/local/path/:/whatever:ro blaze /out/bin/flint --doNotUseSolver /whatever/somebinary
```

There's also a `-o yourfile.json` option to output the results direclty to JSON. 

"Flint Classic" is an older version of Flint that has the kernel module option to check for dangling handlers. Use the `--isKernelModule` for `.ko` files.

```
docker run --rm -v /your/local/path/:/whatever:ro blaze /out/bin/flint_classic --doNotUseSolver /whatever/somebinary
```
