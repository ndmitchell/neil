#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install)
neil test --install
if [ -e Travis.sh ]; then
	runhaskell Travis.hs
fi
