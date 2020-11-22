#!/bin/sh
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

retry(){
    ($@) && return
    sleep 15
    ($@) && return
    sleep 15
    $@
}
timer(){
    set +x
    local before=$(date +%s)
    set -x
    $@
    set +x
    local after=$(date +%s)
    echo Timing: $(expr $after - $before) spent doing $@
    set -x
}

# make sure we hlint check before running the tests, in case they generate non-compliant hlint
if [ "$HLINT_ARGUMENTS" = "" ]; then
    HLINT_ARGUMENTS=.
fi
curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s $HLINT_ARGUMENTS --with-group=extra --with-group=future

ghc --version
cabal --version
# happy --version
# alex --version
haddock --version

if [ "$HASKELL_DEPENDENCIES" != "" ]; then
    retry cabal build $HASKELL_DEPENDENCIES
fi

# Install dependencies
retry cabal build --only-dependencies --enable-tests $CABALFLAGS

# Install the neil tool
retry git clone -n "https://github.com/ndmitchell/neil" .neil
(cd .neil && git checkout && retry cabal install --allow-newer --flags=small --verbose --installdir=. --install-method=copy)

timer .neil/neil test --install --cabal2

# Run any additional tests, written in Haskell
if [ -e travis.hs ]; then
    timer cabal run -- runhaskell travis.hs
fi

# Check regenerating doesn't change anything
git diff --exit-code
