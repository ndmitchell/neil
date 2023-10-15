#!/bin/sh
# This script is invoked from my Travis commands
# It bootstraps to grab the a binary release and run it
set -e # exit on errors

PACKAGE_ARG=$1
if [ -z "$PACKAGE_ARG" ]; then
    echo No arguments provided, please pass the project name as the first argument
    exit 1
fi
shift

# The PACKAGE_ARG can optionally have a release version separated by the @ character.
case ${PACKAGE_ARG} in
    *@*)
        PACKAGE=$(echo "$PACKAGE_ARG" | cut -d'@' -f1)
        RELEASE_VERSION=$(echo "$PACKAGE_ARG" | cut -d'@' -f2)
        ;;
    *)
        PACKAGE="$PACKAGE_ARG"
        RELEASE_VERSION=""
        ;;
esac

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
echo DEBUG: INFO $OS$ESCEXT
echo DEBUG: RELEASED $(echo $RELEASES | grep -o '\"https://[^\"]*-x86_64-'$OS$ESCEXT'\"')
URL=$(echo $RELEASES | grep -o '\"https://[^\"]*-x86_64-'$OS$ESCEXT'\"' | sed s/\"//g | grep "$RELEASE_VERSION" | head -n1)

if [ "$OS" = "osx" ] && [ "$URL" = "" ]; then
    echo FAILED ON MAC
    echo $RELEASES
    echo END OF CURL OUTPUT
    exit 1
fi

echo DEBUG: URL = $URL
VERSION=$(echo $URL | sed -n 's@.*-\(.*\)-x86_64-'$OS$ESCEXT'@\1@p')
echo DEBUG: VERSION = $VERSION
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

echo Fetching $PACKAGE from $URL...
retry curl --progress-bar --location -o$TEMP/$PACKAGE$EXT $URL
if [ "$OS" = "windows" ]; then
    7z x -y $TEMP/$PACKAGE$EXT -o$TEMP -r > /dev/null
else
    tar -xzf $TEMP/$PACKAGE$EXT -C$TEMP
fi
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
