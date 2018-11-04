ROOT=`git rev-parse --show-toplevel`

PUBLIC_FOLDER="$ROOT/modules/backend/public"


cd "$ROOT/modules/frontend"
#yarn build

if [ $? -ne 0 ]; then
	exit 1
fi

rm -rf $PUBLIC_FOLDER
mkdir $PUBLIC_FOLDER
cp -r build/static/ $PUBLIC_FOLDER
cp build/* $PUBLIC_FOLDER

cd $PUBLIC_FOLDER

for TYPE in js html css map ico eot svg xml json ttf; do
	find . -type f -iname "*.$TYPE" -exec gzip --best --keep {} \;		
done

cd "$ROOT"
sbt "project backend" universal:packageBin