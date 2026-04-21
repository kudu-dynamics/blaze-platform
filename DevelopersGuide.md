- [Installing Haskell Dependecies](#installing-haskell-dependecies)
- [Installing Java and Setting Environment Variables](#installing-java-and-setting-environment-variables)
- [Building blaze-platform Monorepo](#building-blaze-platform-monorepo)
- [Rapid Prototyping with Hspec library](#rapid-prototyping-with-hspec-library)
- [Contributing to blaze-platform](#contributing-to-blaze-platform)
- [blaze-platform Versioning](#blaze-platform-versioning)
- [How To Actually Run flint](#how-to-actually-run-flint)
- [Miscellaneous Build Instructions](#miscellaneous-build-instructions)
    - [Special side directions for MacOS](#special-side-directions-for-macos)
    - [Legacy Build Instructions](#legacy-build-instructions)
        - [Binary Ninja](#binary-ninja)

---

## Installing Haskell Dependecies

First, install the following packages:

```bash
sudo apt install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev liblmdb-dev
```

Then, install the script:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Everything is generally default. However, if you plan on using `vscode`, make sure you install the Haskell Language Server.

Then, go ahead and refresh your shell:

```bash
source ~/.bashrc
```

In `blaze-platform`, we use `stack` for building and testing. Install `stack` using the `ghcup`:

```bash
ghcup install stack
```

Next, you will need to clone `z3`:

```bash
git clone https://github.com/Z3Prover/z3.git
```

Then, build and install `z3`:

```bash
cd z3
./configure
mkdir -p build
cd build
```

Now, install `z3`:

```bash
make install
```

---

# Installing Java and Setting Environment Variables

Now, you can install Java 21:

```bash
sudo apt install openjdk-21-jdk
```

Then, create environment variables for `libjvm.so`:
```bash
export JDK="$(dirname "$(dirname "$(readlink -f "$(which javac)")")")"
export PATH=$JDK/bin/${PATH:+:$PATH}
export LD_LIBRARY_PATH=$JDK/lib/server/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
```

## Building `blaze-platform` Monorepo

Now, clone `blaze-platform`:

```bash
git clone <repository link>.git
cd blaze-platform
```

First, build `blaze` before anything else:

```bash
cd blaze
stack build
stack test
```

Next, `cd` into `ghidra-haskell` and create an environment variable for `libjvm.so`:

```bash
export PATH=$JDK/bin/${PATH:+:$PATH}
export LD_LIBRARY_PATH=$JDK/lib/server/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
```

Then, go ahead and `build` inside `ghidra-haskell`:

```bash
make res/ghidra.jar
stack build
stack test
```

Finally, build `flint` and `test` everything inside `blaze-platform`:

```bash
cd ..
stack build
stack test
```

---

## Rapid Prototyping with `Hspec` library

You can quickly add features to `blaze`, `flint`, or `ghidra-haskell` by creating or adding to a `*spec*.hs` file. This will allow rapid development using `Hspec` library.

You can `find` all the `*spec*.hs` files with the following command below:

```bash
find . -iname "*spec*.hs" ! -path "*dist*"
```

You can run all test with `stack test`. However, its best to just run specific test using the `--match` flag.

```bash
stack test --test-arguments "--match MatcherSpec"
```

---

## Contributing to `blaze-platform`

Make sure to check tests and warnings before making a PR by runnning `just ci`.

The dependencies for this can be installed with `sudo apt install hlint just`.

---

## `blaze-platform` Versioning

`blaze-platform` versioning uses the format "N.YY.MMDDx" where "N" is the major version (0 before release),
"YY" is the last two digits of the year, "MMDD" is the numerical month and date concatenated, and "x" is the
alphabetical index of the version within the same day. For example, the third version of `blaze-platform`
before release committed on June 3, 2007 would be "0.07.0603c".

---

## How To Actually Run `flint`

See [`flint`'s `README.md`](flint/README.md) for instructions on running `flint`.

## Miscellaneous Build Instructions

The following sections are for developers on MacOS. Also, `blaze` supported Binary Ninja backend in the past. This was before Ghidra was open source.

---

### Special side directions for MacOS

- Install the build-essentials stuff from x tools. OSX doesn't have the build-essential package in their repository, but it should automatically prompt the equivalent when trying to download build-essential
- Get Haskell via the install_notes.txt instructions
- Get Homebrew and install openjdk@21. Do not get the oracle version or any other version, if you do you have to remove it
- get the blaze repo
- run getGhidraJar.sh on the ghidra-haskell directory (so your terminal would be ghidra-haskell > ./scripts/getGhidraJar.sh
- run `stack build` and `stack test` in the ghidra-haskell library
