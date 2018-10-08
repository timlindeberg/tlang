ROOT=`git rev-parse --show-toplevel`

PUBLIC_FOLDER="$ROOT/modules/backend/public"

cd "$ROOT/modules/frontend"
#yarn build

rm -rf $PUBLIC_FOLDER
mkdir $PUBLIC_FOLDER
cp -r build/static/ $PUBLIC_FOLDER
cp build/* $PUBLIC_FOLDER

cd "$ROOT"
#sbt "project backend" universal:packageBin