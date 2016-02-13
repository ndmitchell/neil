#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){ "$@" || "$@" || "$@"; }

#####################################################################
## GHC SETUP

export PATH=/opt/ghc/$(ls /opt/ghc)/bin:/opt/cabal/$(ls /opt/cabal)/bin:$PATH
if [ -d /opt/happy ]; then
    export PATH=/opt/happy/$(ls /opt/happy)/bin:$PATH
fi
if [ -d /opt/alex ]; then
    export PATH=/opt/alex/$(ls /opt/alex)/bin:$PATH
fi
if [ "$GHCVER" = "7.2.2" ]; then
    # on GHC 7.2 it is installed, but not exposed
    sudo /opt/ghc/$GHCVER/bin/ghc-pkg expose binary
fi


#####################################################################
## PACKAGE SETUP

retry cabal update
retry cabal install --only-dependencies --enable-tests || FAIL=1
if [ "$GHCVER" = "head" ] && [ "$FAIL" = "1" ]; then
    FAIL=
    retry cabal install --only-dependencies --enable-tests --allow-newer || FAIL=1
    if [ "$FAIL" = "1" ]; then
        echo Failed because some dependencies failed to install, not my fault
        exit
    fi
fi


#####################################################################
## NEIL SETUP

retry git clone https://github.com/ndmitchell/neil
(cd neil && retry cabal install --flags=small)
if [ -e travis.hs ]; then
    # ensure that reinstalling this package won't break the test script
    mkdir travis
    ghc --make travis.hs -outputdir travis -o travis/travis
fi


#####################################################################
## EXECUTE

FLAGS=
if [ "$GHCVER" = "head" ]; then
    FLAGS=--no-warnings
fi
neil test --install $FLAGS
if [ -e travis.hs ]; then
    travis/travis
fi
git diff --exit-code # check regenerating doesn't change anything
