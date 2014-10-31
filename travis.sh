#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){ "$@" || "$@" || "$@"; }

if [ "$GHCVER" != "" ]; then
    retry sudo add-apt-repository -y ppa:hvr/ghc
    retry sudo apt-get update
    retry sudo apt-get install ghc-$GHCVER cabal-install-1.18 happy-1.19.4 alex-3.1.3
    export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/1.18/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:/home/travis/.cabal/bin:$PATH
fi

retry cabal update
ghc-pkg expose binary || true # on GHC 7.2 it is installed, but not exposed
retry cabal install --only-dependencies --enable-tests
retry git clone https://github.com/ndmitchell/neil
(cd neil && retry cabal install --flags=small)
neil test --install
if [ -e travis.hs ]; then
    runhaskell travis.hs
fi
git diff --exit-code # check regenerating doesn't change anything
