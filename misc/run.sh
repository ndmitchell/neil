#!/bin/sh
# This script is invoked from my Travis commands
# It bootstraps to grab the a binary release and run it
set -e # exit on errors

PACKAGE=$1
if [ -z "$PACKAGE" ]; then
    echo No arguments provided, please pass the project name as the first argument
    exit 1
fi
shift

case "$(uname)" in
    "Darwin")
        OS=osx;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

if [ "$OS" = "windows" ]; then
    EXT=.zip
    ESCEXT=\.zip
else
    EXT=.tar.gz
    ESCEXT=\.tar\.gz
fi

echo Downloading and running $PACKAGE...
# Don't go for the API since it hits the Appveyor GitHub API limit and fails
RELEASES=$(curl --silent --show-error https://api.github.com/repos/ndmitchell/$PACKAGE/releases)
FILENAME=$(echo $RELEASES | grep -o '\"[^\"]*-x86_64-'$OS$ESCEXT'\"' | sed s/\"//g | head -n1)
echo DEBUG: FILENAME = $FILENAME
VERSION=$(echo $FILENAME | sed -n 's@.*-\(.*\)-x86_64-'$OS$ESCEXT'@\1@p')
echo DEBUG: VERSION = $VERSION
URL=https://github.com/ndmitchell/$PACKAGE/releases/download/v$VERSION/$FILENAME
echo DEBUG: URL = $URL
TEMP=$(mktemp -d .$PACKAGE-XXXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

retry(){
    ($@) && return
    sleep 15
    ($@) && return
    sleep 15
    $@
}

retry curl --progress-bar --location -o$TEMP/$PACKAGE$EXT $URL
if [ "$OS" = "windows" ]; then
    7z x -y $TEMP/$PACKAGE$EXT -o$TEMP -r > /dev/null
else
    tar -xzf $TEMP/$PACKAGE$EXT -C$TEMP
fi
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
