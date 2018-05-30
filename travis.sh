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
curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s $HLINT_ARGUMENTS

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    # Try and use the Cabal that ships with the same GHC version
    if [ "$GHCVER" = "head" ]; then
        CABALVER=2.0
        # Should be HEAD but see https://github.com/haskell/cabal/issues/5108
    else
        CABALVER=2.0
    fi
    retry sudo add-apt-repository -y ppa:hvr/ghc
    # Sometimes apt-get update fails silently, but then apt-get install fails loudly, so retry both
    update_install(){
        apt-get update && apt-get install ghc-$GHCVER cabal-install-$CABALVER happy-1.19.4 alex-3.1.3
    }
    retry sudo update_install
    export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH
    retry cabal update
else
    brew update
    brew install ghc cabal-install
    retry cabal update
    cabal install alex happy
fi
export PATH=$HOME/.cabal/bin:$PATH

ghc --version
cabal --version
happy --version
alex --version
haddock --version

if [ "$GHCVER" = "" ]; then
    # Only happens on Mac where ghc is installed via brew
    export GHCVER=$(ghc --numeric-version)
fi
if [ "$GHCVER" = "head" ]; then
    export GHC_HEAD=1
fi
if [ "$GHCVER" = "8.2.2" ]; then
    export GHC_STABLE=1
fi

retry cabal install --only-dependencies --enable-tests --force-reinstalls || FAIL=1
if [ "$GHC_HEAD" = "1" ] && [ "$FAIL" = "1" ]; then
    FAIL=
    retry cabal install --only-dependencies --enable-tests --force-reinstalls --allow-newer || FAIL=1
    if [ "$FAIL" = "1" ]; then
        echo Failed because some dependencies failed to install, not my fault
        exit
    fi
fi

retry git clone -n "https://github.com/$GITHUB_USER/neil" .neil
(cd .neil && git checkout $COMMIT && retry cabal install --flags=small)

if [ -e travis.hs ]; then
    # ensure that reinstalling this package won't break the test script
    mkdir travis
    ghc --make travis.hs -outputdir travis -o travis/travis
fi

FLAGS=
if [ "$GHCVER" = "head" ]; then
    FLAGS=--no-warnings
fi
timer neil test --install $FLAGS

if [ -e travis.hs ]; then
    timer travis/travis
fi
git diff --exit-code # check regenerating doesn't change anything

# Generate artifacts for release
mkdir travis-release
if [ "$GHC_STABLE" = "1" ] || [ "$TRAVIS_OS_NAME" = "osx" ]; then
    neil binary
    if [ -d dist/bin ]; then
        cp dist/bin/* travis-release
    fi
fi
if [ "$GHC_STABLE" = "1" ]; then
    cabal sdist
    cp dist/*.tar.gz travis-release
fi
