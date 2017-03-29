# blockcat

[![Build Status](https://travis-ci.org/TerrorJack/blockcat.svg)](https://travis-ci.org/TerrorJack/blockcat)

## Building

`blockcat` bundles a development version of [`binaryen`](https://github.com/WebAssembly/binaryen) with it. Just set the `BINARYEN_LIBDIR` environment variable (there's no default) and `binaryen` will be built during `Cabal` pre-configure phase. You can specify a C++11-compliant compiler with the `CXX` environment variable (defaults to `g++`).

Building with [`stack`](https://docs.haskellstack.org/en/stable/README/): `stack --extra-lib-dirs $BINARYEN_LIBDIR build`.

Building with [`cabal`](https://cabal.readthedocs.io/en/latest/): first use [`hpack`](https://github.com/sol/hpack) to generate `blockcat.cabal` from `package.yaml`. You also need to pass `$BINARYEN_LIBDIR` as extra library path.

`ghci` linking is problematic on Windows as of now. We're still crafting the `Cabal` custom builder, hopefully things can work without extra flags and environment variables in the future.
