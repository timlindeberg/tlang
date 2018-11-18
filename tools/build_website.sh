GREEN="\033[32m"
YELLOW="\033[33m"
END="\033[0m"

ROOT=`git rev-parse --show-toplevel`

pushd $ROOT > /dev/null

ZIP_NAME="backend-$VERSION"

BACKEND_FOLDER="$ROOT/modules/backend"
FRONTEND_FOLDER="$ROOT/modules/frontend"

cd "$FRONTEND_FOLDER"
echo "Building frontend with yarn"
yarn build

if [ $? -ne 0 ]; then
	exit 1
fi

echo "Gziping assets and putting them in public folder"

PUBLIC_FOLDER="$BACKEND_FOLDER/public"
rm -rf $PUBLIC_FOLDER
mkdir $PUBLIC_FOLDER
cp -r build/static/ $PUBLIC_FOLDER
cp build/* $PUBLIC_FOLDER

cd $PUBLIC_FOLDER

# gzip all the files
for TYPE in js html css map ico eot svg xml json ttf; do
	find . -type f -iname "*.$TYPE" -exec gzip --best --keep {} \;		
done

echo "Building backend with sbt"
cd "$ROOT"
sbt "project backend" universal:packageBin

echo "Copying needed files to build directory"

BUILD_FOLDER="build_tlang"
rm -rf $BUILD_FOLDER
mkdir -p $BUILD_FOLDER

cp -r documentation "$BUILD_FOLDER/documentation"
cp -r "$BACKEND_FOLDER/docker" "$BUILD_FOLDER/docker"


VERSION=`tools/get_version.sh`
ZIP_NAME="backend-$VERSION"
tar -xzf "$BACKEND_FOLDER/target/universal/$ZIP_NAME.zip" -C $BUILD_FOLDER
for FOLDER in lib bin conf; do 
	cp -r "$BUILD_FOLDER/$ZIP_NAME/$FOLDER" "$BUILD_FOLDER/$FOLDER"
done
rm -rf "$BUILD_FOLDER/$ZIP_NAME"

ZIP="tlang-website-$VERSION.tar.gz"
echo "Creating zip ${YELLOW}$ZIP${END}"
tar -czf $ZIP -C $BUILD_FOLDER .
rm -rf $BUILD_FOLDER

echo "${GREEN}Successfully built!${END}"
