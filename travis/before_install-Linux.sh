#!/bin/sh

curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack config set system-ghc --global true
stack --no-terminal build --test --bench --only-dependencies
