# blockcat

[![Build Status](https://travis-ci.org/TerrorJack/blockcat.svg)](https://travis-ci.org/TerrorJack/blockcat)

## Building

### Building `binaryen`

`blockcat` depends on the development version of [`binaryen`](https://github.com/WebAssembly/binaryen):

```sh
# Dependencies: git, cmake, ninja and a C++11-compliant compiler
git clone --depth 1 https://github.com/WebAssembly/binaryen.git
cd binaryen
cmake -G Ninja .
ninja
sudo ninja install
```

Windows users are recommended to use the `MINGW64` shell of [MSYS2](http://www.msys2.org/) to build `binaryen`. The dependencies can be installed with `pacman --needed -S git mingw-w64-x86_64-{cmake,ninja,gcc}`.

### Building `blockcat`

`blockcat` is not on Hackage yet.

Building with [`stack`](https://docs.haskellstack.org/en/stable/README/): simply `stack build`.

Building with [`cabal`](https://cabal.readthedocs.io/en/latest/): first use [`hpack`](https://github.com/sol/hpack) to generate `blockcat.cabal` from `package.yaml`.
