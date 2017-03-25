#!/bin/sh

brew update
brew upgrade
brew install ninja

cd $TRAVIS_BUILD_DIR
if [ ! -d "binaryen/.git" ]; then
  rm -rf binaryen
  git clone --depth 1 https://github.com/WebAssembly/binaryen.git
fi
cd binaryen
git pull
cmake -DCMAKE_C_COMPILER=`which clang` -DCMAKE_CXX_COMPILER=`which clang++` -DCMAKE_INSTALL_PREFIX=~/.local -G Ninja .
ninja install

cd $TRAVIS_BUILD_DIR
curl -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
stack config set install-ghc --global true
stack --no-terminal --extra-include-dirs ~/.local/include --extra-lib-dirs ~/.local/lib build --test --bench --only-dependencies
