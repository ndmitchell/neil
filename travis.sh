#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
git clone https://github.com/ndmitchell/neil
(cd neil && (cabal install --flags=small || cabal install --flags=small || cabal install --flags=small))
neil test --install
if [ -e travis.hs ]; then
    runhaskell travis.hs
fi
