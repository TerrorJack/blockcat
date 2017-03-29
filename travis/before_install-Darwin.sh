#!/bin/sh

export CXX=`which clang++`
curl -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
stack config set install-ghc --global true
stack --no-terminal --extra-lib-dirs $BINARYEN_LIBDIR build --test --bench --only-dependencies
