#!/bin/sh

export CXX=`which clang++-5.0`
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack config set system-ghc --global true
stack --no-terminal --extra-lib-dirs $BINARYEN_LIBDIR build --test --bench --only-dependencies
