#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -u # warn about unitialised variables
set -x # echo each line

function retry () { "$@" || "$@" || "$@"; }

retry cabal update
retry cabal install --only-dependencies --enable-tests
retry git clone https://github.com/ndmitchell/neil
(cd neil && retry cabal install --flags=small)
export PATH=/home/travis/.cabal/bin:$PATH
neil test --install
if [ -e travis.hs ]; then
    runhaskell travis.hs
fi
git diff --exit-code # check regenerating doesn't change anything
