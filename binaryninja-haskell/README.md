# Binary Ninja bindings for Haskell

Provides access to Binary Ninja from Haskell through the Binary Ninja shared library.

## Dependencies

The minimum required Binary Ninja version is `3.4.4271` with a headless license.

In order to successfully build the `binaryninja-haskell` bindings, `libbinaryninjacore.so` must be available.

On Linux, by default, this is typically found at `/path/to/binaryninja/libbinaryninjacore.so.1` and requires creating a symbolic link with the command: `ln -s libbinaryninjacore.so.1 libbinarycore.so`

On Linux, the following environment variables must also be present. Users can update their `.bashrc` with the following environment variables:

```
export BINJA_PLUGINS=/path/to/binaryninja/plugins
```

When building the project with `stack`, it may be necessary to provide the path to `binaryninja` using the command: `stack build --extra-lib-dirs=/path/to/binaryninja`.

## Usage
* Ensure the BN core shared library is placed/installed correctly such that it can be found during linking of the bindings.
* Use `stack build` and `stack test` to build or test the package, respectively.
* Note: When updates to the Binary Ninja API are made, a C-compliant `binaryninjacore.h` header file must be generated. Place in the `res/` directory and the bindings can now be built.

## Hacking

### Upgrading supported Binary Ninja version
If a new version of Binary Ninja is released that either breaks ABI backwards-compatibility, or adds a new desirable feature, then the bindings need to be updated.
First, users should clone https://github.com/Vector35/binaryninja-api and checkout the tag that corresponds to a Binary Ninja release.
For instance, Binary Ninja 3.4.4271 corresponds to the `v3.4.4271-stable` tag.

The `binaryninjacore.h` header file from `binaryninja-api` may be used to update the bindings of `binaryninja-haskell`. However, first they must be cleaned using `binja-header-cleaner`.

From the `binja-header-cleaner` directory within this repository, the following command may be run:

```
stack run clean-binja-header -- /path/to/binaryninja-api/binaryninjacore.h res/binaryninjacore.h
```


Approved for Public Release, Distribution Unlimited.
