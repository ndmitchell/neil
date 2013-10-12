#!/bin/bash -e
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
cabal configure --enable-tests
cabal build
cabal test
