#!/bin/bash

real_path () {
  TARGET_FILE="$1"
  FIX_CYGPATH="$2"

  cd "$(dirname "$TARGET_FILE")"
  TARGET_FILE=$(basename "$TARGET_FILE")

  COUNT=0
  while [ -L "$TARGET_FILE" -a $COUNT -lt 100 ]
  do
      TARGET_FILE=$(readlink "$TARGET_FILE")
      cd "$(dirname "$TARGET_FILE")"
      TARGET_FILE=$(basename "$TARGET_FILE")
      COUNT=$(($COUNT + 1))
  done

  echo "$(pwd -P)/$TARGET_FILE"
}


T_HOME="$(dirname "$(real_path "$0")")/.."
export T_HOME

T_LIBS="$T_HOME/lib/*"
