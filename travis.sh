#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
travis_retry cabal update
travis_retry cabal install --only-dependencies --enable-tests
git clone https://github.com/ndmitchell/neil
(cd neil && travis_retry cabal install --flags=small)
neil test --install --verbose
if [ -e travis.hs ]; then
    runhaskell travis.hs
fi
git diff --exit-code # check regenerating doesn't change anything
