#!/bin/bash
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

exit 1 # Not supported

retry(){ "$@" || "$@" || "$@"; }
retry git clone https://github.com/ndmitchell/neil
ls neil
chmod +x neil/travis2.sh
sh neil/travis2.sh
