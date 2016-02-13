#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){ "$@" || "$@" || "$@"; }

#####################################################################
## GHC SETUP

export PATH=~/.cabal/bin:/opt/ghc/$(ls /opt/ghc)/bin:/opt/cabal/$(ls /opt/cabal)/bin:$PATH
if [ -d /opt/happy ]; then
    export PATH=/opt/happy/$(ls /opt/happy)/bin:$PATH
fi
if [ -d /opt/alex ]; then
    export PATH=/opt/alex/$(ls /opt/alex)/bin:$PATH
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

(cd neil && retry cabal install --flags=small --verbose=3)
ls -l /home/travis/.cabal/bin
echo ~
which neil
/home/travis/.cabal/bin/neil --version || true
neil --version || true
echo $PATH
export PATH=/home/travis/.cabal/bin:$PATH
/home/travis/.cabal/bin/neil --version || true
neil --version || true
chmod +x /home/travis/.cabal/bin/neil
/home/travis/.cabal/bin/neil --version || true
neil --version

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
