#!/usr/bin/env bash

GREEN="\033[32m"
YELLOW="\033[33m"
END="\033[0m"

set -e

ROOT=`git rev-parse --show-toplevel`
pushd $ROOT > /dev/null

VERSION=`tools/get_version.sh`

BUILD_BASE="build_tlang"
BUILD_FOLDER="$BUILD_BASE/tlang"
rm -rf $BUILD_FOLDER
mkdir -p $BUILD_FOLDER

# Build project with sbt
echo -e "Building project with sbt"
#sbt universal:packageBin

# Copy files
echo -e "Copying needed files"
ZIP_NAME="core-$VERSION"
tar -xzf "target/universal/$ZIP_NAME.zip" -C $BUILD_FOLDER
cp -r "$BUILD_FOLDER/$ZIP_NAME/lib" "$BUILD_FOLDER/lib"
rm -rf "$BUILD_FOLDER/$ZIP_NAME"

cp -r stdlib "$BUILD_FOLDER/stdlib"
cp -r tools/executables "$BUILD_FOLDER/bin"

# Make final zip file
ZIP="tlang-$VERSION.tar.gz"
echo -e "Creating tar ball ${YELLOW}$ZIP${END}"
tar -czf $ZIP -C $BUILD_BASE .
rm -rf $BUILD_BASE
echo -e "${GREEN}Successfully built!${END}"

popd > /dev/null
