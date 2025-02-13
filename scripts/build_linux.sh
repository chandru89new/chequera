#!/bin/sh

# install ghcup
sudo apt-get update
sudo apt-get install -y curl g++ libgmp-dev make # Ensure dependencies
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y
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
echo "VERSION=$VERSION" >> $GITHUB_ENV

# tar-zip binary
tar -czvf chequera_linux_amd64_v$VERSION.tar.gz -C ./release $(readlink -f ./release/chequera)