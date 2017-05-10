#!/bin/sh
# This script is invoked from my Travis commands
# It bootstraps to grab the a binary release and run it
set -e # exit on errors

PACKAGE=$1
shift

if [ -z "$PACKAGE" ]; then
    echo No arguments provided, please pass the project name as the first argument
    exit 1
fi

echo Downloading and running $PACKAGE...
RELEASES=$(curl --silent https://api.github.com/repos/ndmitchell/$PACKAGE/releases)
URL=$(echo $RELEASES | sed -e 's/.*"browser_download_url" *: *"\([^"]*-x86_64-linux\.tar\.gz\)".*/\1/')
VERSION=$(echo $URL | sed -e 's/.*-\([\.0-9]\+\)-x86_64-linux\.tar\.gz/\1/')
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
