#!/usr/bin/env bash

ROOT=`git rev-parse --show-toplevel`
cat "$ROOT/modules/utils/src/main/resources/version.txt"
