#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
cabal update
cabal install --only-dependencies --enable-tests
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install --flags=small --verbose)
export PATH=~/.cabal/bin:$PATH
echo $PATH
~/.cabal/bin/neil --help
neil --help
neil test --install --verbose
if [ -e travis.hs ]; then
    runhaskell travis.hs
fi
git diff --exit-code # check regenerating doesn't change anything
