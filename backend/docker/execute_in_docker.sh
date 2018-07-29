#!/bin/bash

OUT_DIR="$1"
CLASS="$2"
ID="$3"

OUTPUT_DIR="out"

docker run \
	--volume $OUT_DIR:/home/tlang/$OUTPUT_DIR:ro \
	--name $ID \
	--net=none \
	--memory 128mb \
	--cpus 0.2 \
	exec \
	java -cp $OUTPUT_DIR:stdlib $CLASS
