# blockcat

[![Build Status](https://travis-ci.org/TerrorJack/blockcat.svg)](https://travis-ci.org/TerrorJack/blockcat)

## Building

`blockcat` bundles a development version of [`binaryen`](https://github.com/WebAssembly/binaryen) with it. Just set the `BINARYEN_LIBDIR` environment variable (there's no default) and `binaryen` will be built during `Cabal` pre-configure phase. You can specify a C++11-compliant compiler with the `CXX` environment variable (defaults to `clang++` on Darwin/FreeBSD, `g++` otherwise).

Building with [`stack`](https://docs.haskellstack.org/en/stable/README/): `stack build`. `stack ghci` needs to pass `--extra-lib-dirs=$BINARYEN_LIBDIR`, because `stack` does not use the `Cabal` custom builder for interactive mode.

Building with [`cabal`](https://cabal.readthedocs.io/en/latest/): first use [`hpack`](https://github.com/sol/hpack) to generate `blockcat.cabal` from `package.yaml`. `cabal build` and `cabal repl` works.

On Windows, `ghci` doesn't work as of now. We're still crafting the `Cabal` custom builder, hopefully things can work on all platforms without extra flags and environment variables in the future.
