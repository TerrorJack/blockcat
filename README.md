# blockcat

[![Build Status](https://travis-ci.org/TerrorJack/blockcat.svg)](https://travis-ci.org/TerrorJack/blockcat)

## Building

`blockcat` bundles a development version of [`binaryen`](https://github.com/WebAssembly/binaryen) with it. `binaryen` is built during `Cabal` configure phase. You can specify a C++11-compliant compiler with the `CXX` environment variable (defaults to `clang++` on Darwin/FreeBSD, `g++` otherwise).

Building with [`stack`](https://docs.haskellstack.org/en/stable/README/): `stack build`. `stack ghci` needs to pass `--extra-lib-dirs $libdir`, where `$libdir` is typically something like `blockcat-0.0.1/.stack-work/install/x86_64-linux/nightly-2017-03-25/8.0.2/lib`.

Building with [`cabal`](https://cabal.readthedocs.io/en/latest/): first use [`hpack`](https://github.com/sol/hpack) to generate `blockcat.cabal` from `package.yaml`. `cabal build` and `cabal repl` works.

`ghci` doesn't work on Windows yet. We're still crafting the `Cabal` custom builder, hopefully things can work on all platforms without extra flags in the future.
