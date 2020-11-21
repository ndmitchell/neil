#!/bin/sh
# This script is invoked from my Travis-CI commands
# It bootstraps to grab the 'neil' tool and run 'neil test'
set -e # exit on errors
set -x # echo each line

GITHUB_USER=$1
COMMIT=$2

if [ -z "$GITHUB_USER" ]; then
    GITHUB_USER=ndmitchell
fi

if [ -z "$COMMIT" ]; then
    COMMIT=master
fi

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

ghc-pkg list

if [ "$HASKELL_DEPENDENCIES" != "" ]; then
    retry cabal v1-install $HASKELL_DEPENDENCIES
fi

retry cabal v1-install --only-dependencies --enable-tests $CABALFLAGS || FAIL=1
if [ "$GHC_HEAD" = "1" ] && [ "$FAIL" = "1" ]; then
    FAIL=
    retry cabal v1-install --only-dependencies --enable-tests --force-reinstalls --allow-newer || FAIL=1
    if [ "$FAIL" = "1" ]; then
        echo Failed because some dependencies failed to install, not my fault
        exit
    fi
fi
ghc-pkg list

retry git clone -n "https://github.com/$GITHUB_USER/neil" .neil
(cd .neil && git checkout $COMMIT && retry cabal install --allow-newer --flags=small --verbose)

export PATH="$HOME/.cabal/bin:/home/runner/.cabal/bin:$PATH"

if [ -e travis.hs ]; then
    # ensure that reinstalling this package won't break the test script
    mkdir travis
    ghc --make travis.hs -outputdir travis -o travis/travis
fi

timer neil test --install

if [ -e travis.hs ]; then
    timer travis/travis
fi
git diff --exit-code # check regenerating doesn't change anything
