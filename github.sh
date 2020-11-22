#!/bin/sh
# This script is invoked from my GitHub-CI commands
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
haddock --version

if [ "$HASKELL_DEPENDENCIES" != "" ]; then
    retry cabal new-build $HASKELL_DEPENDENCIES
fi

# Install dependencies
retry cabal new-build --only-dependencies --enable-tests $CABALFLAGS

# Install the neil tool
retry git clone --depth=1 "https://github.com/ndmitchell/neil" .neil
(cd .neil && retry cabal new-install --allow-newer --flags=small --installdir=. --install-method=copy --overwrite-policy=always)

timer .neil/neil test --install --cabal2
export PATH="$HOME/.cabal/bin:/home/runner/.cabal/bin:/c/Users/runneradmin/AppData/Roaming/cabal/bin:$PATH"

# Run any additional tests, written in Haskell
if [ -e travis.hs ]; then
    # We want to run travis.hs with the extra package in scope
    # Best way I can do that is by hijacking the Main.hs of .neil
    cp travis.hs .neil/src/Main.hs
    (cd .neil && cabal new-install --allow-newer --flags=small --installdir=. --install-method=copy --overwrite-policy=always)
    .neil/neil
fi

# Check regenerating doesn't change anything
git diff --exit-code
