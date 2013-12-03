#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
export PATH=/home/travis/.cabal/bin:$PATH
export PATH=/home/travis/.ghc-multi/7.6.3/bin:$PATH
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install)
neil test
