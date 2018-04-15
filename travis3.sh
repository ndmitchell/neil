#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

echo /opt/cabal/*.*
echo /opt/ghc/*.*
echo $HOME/.cabal/bin:/opt/ghc/*.*/bin:/opt/cabal/*.*/bin:$PATH
echo $HOME/.cabal/bin:/opt/ghc/$(echo /opt/ghc/*.*)/bin:/opt/cabal/$(echo /opt/cabal/*.*)/bin:$PATH
export "PATH=$HOME/.cabal/bin:/opt/ghc/$(echo /opt/ghc/*.*)/bin:/opt/cabal/$(echo /opt/cabal/*.*)/bin:$PATH"

cabal update && cabal install
