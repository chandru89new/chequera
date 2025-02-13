#!/bin/sh

# install ghcup
brew install ghcup
export PATH=$PATH:"$HOME/.ghcup/bin"

# install latest ghc and cabal
ghcup install ghc $GHC_VERSION
ghcup set ghc $GHC_VERSION
ghcup install cabal $CABAL_VERSION
ghcup set cabal $CABAL_VERSION

# update cabal dependencies
cabal update

# build binary
make clean-build

# reduce binary size
strip $(readlink -f ./release/chequera)

# get version
VERSION=$(./release/chequera --version)

# tar-zip binary
tar -czvf chequera_darwin_arm64_v$VERSION.tar.gz -C ./release $(readlink -f ./release/chequera)