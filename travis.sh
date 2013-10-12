#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line
echo Using the 'neil test' bootstrap script
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install)
neil test
