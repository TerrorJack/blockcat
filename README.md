# blockcat

[![Build Status](https://travis-ci.org/TerrorJack/blockcat.svg)](https://travis-ci.org/TerrorJack/blockcat)

## Building

### Building `binaryen`

`blockcat` assumes the development version of [`binaryen`](https://github.com/WebAssembly/binaryen) is installed to `/opt/binaryen`:

```sh
# Dependencies: git, cmake, ninja and a C++11-compliant compiler
git clone --depth 1 https://github.com/WebAssembly/binaryen.git
cd binaryen
cmake -DCMAKE_INSTALL_PREFIX=/opt/binaryen -G Ninja .
ninja install
```

Windows users are assumed to have [MSYS2](http://www.msys2.org/) installed at `C:\msys64`, so `/opt/binaryen` translates to `C:\msys64\opt\binaryen`. The above shell script works under the `MINGW64` shell (the dependencies can be installed with `pacman --needed -S git mingw-w64-x86_64-{cmake,ninja,gcc}`).

The default path of `binaryen` is hard-wired in [`package.yaml`](package.yaml).

### Building `blockcat`

`blockcat` is not on Hackage yet.

Building with [`stack`](https://docs.haskellstack.org/en/stable/README/): simply `stack build`.

Building with [`cabal`](https://cabal.readthedocs.io/en/latest/): first use [`hpack`](https://github.com/sol/hpack) to generate `blockcat.cabal` from `package.yaml`.
