#!/bin/bash

CLASS_PATH="$1"
ID="$2"

BASE=${CLASS_PATH##*/}
CLASS=${BASE%.*}

docker run \
	--volume $CLASS_PATH:/home/tlang/$CLASS.class:ro \
	--name $ID \
	--net=none \
	--memory 128mb \
	--cpus 0.2 \
	exec \
	java $CLASS
