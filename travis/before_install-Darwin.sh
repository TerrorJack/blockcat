#!/bin/sh

sudo brew update
sudo brew upgrade
sudo brew install ninja

cd $TRAVIS_BUILD_DIR
travis_retry curl -L https://github.com/WebAssembly/binaryen/archive/master.zip -o binaryen-master.zip
unzip -q binaryen-master.zip
cd binaryen-master
cmake -DCMAKE_C_COMPILER=`which clang` -DCMAKE_CXX_COMPILER=`which clang++` -DCMAKE_INSTALL_PREFIX=~/.local -G Ninja .
ninja
ninja install

cd $TRAVIS_BUILD_DIR
export PATH=~/.local/bin:/opt/ghc/8.0.2/bin:$PATH
travis_retry curl -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
stack config set system-ghc --global true
stack config set install-ghc --global true
stack --no-terminal build --test --bench --only-dependencies
