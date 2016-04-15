#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){ "$@" || "$@" || "$@"; }
timer(){
    set +x;
    local before=$(date +%s);
    set -x;
    "$@";
    set +x;
    local after=$(date +%s);
    echo Timing: $(expr $after - $before) spent doing $@;
    set -x;
}


if [ "$GHCVER" != "" ]; then
    if [ "$GHCVER" = "head" ]; then
        CABALVER=head
    elif [ "$GHCVER" = "8.0.1" ]; then
        CABALVER=1.24
    elif [ "$GHCVER" = "7.10.3" ]; then
        CABALVER=1.22
    else
        CABALVER=1.20
    fi
    retry sudo add-apt-repository -y ppa:hvr/ghc
    retry sudo apt-get update
    retry sudo apt-get install ghc-$GHCVER cabal-install-$CABALVER happy-1.19.4 alex-3.1.3
    export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:/home/travis/.cabal/bin:$PATH
fi

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
retry git clone https://github.com/ndmitchell/neil
(cd neil && retry cabal install --flags=small)
if [ -e travis.hs ]; then
    # ensure that reinstalling this package won't break the test script
    mkdir travis
    ghc --make travis.hs -outputdir travis -o travis/travis
fi
FLAGS=
if [ "$GHCVER" = "head" ]; then
    FLAGS=--no-warnings
fi
timer neil test --install $FLAGS
if [ -e travis.hs ]; then
    timer travis/travis
fi
git diff --exit-code # check regenerating doesn't change anything
