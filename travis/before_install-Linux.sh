#!/bin/sh

cd $TRAVIS_BUILD_DIR
if [ ! -d "binaryen/.git" ]; then
  rm -rf binaryen
  git clone --depth 1 https://github.com/WebAssembly/binaryen.git
fi
cd binaryen
git pull
cmake -DCMAKE_C_COMPILER=`which clang-5.0` -DCMAKE_CXX_COMPILER=`which clang++-5.0` -DCMAKE_INSTALL_PREFIX=~/.local -G Ninja .
ninja install

cd $TRAVIS_BUILD_DIR
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack config set system-ghc --global true
stack config set install-ghc --global true
stack --no-terminal build --test --bench --only-dependencies
