#!/bin/bash -e
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
git clone https://github.com/ndmitchell/neil
(cd neil && cabal install)
neil test
