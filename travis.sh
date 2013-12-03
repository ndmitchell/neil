#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
export PATH=/home/travis/.cabal/bin:$PATH
cabal install alex
cabal install happy
cabal install haddock
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install)
neil test
