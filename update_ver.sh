#!/bin/bash

set -e

VER_FILE=VERSION
FILE_0="app/Main.hs"
FILE_1="crak-chess.cabal"
FILE_2="README.md"
FILE_3="LEEME.md"
FILES="${FILE_0} ${FILE_1} ${FILE_2} ${FILE_3}" 

echo "Updating version in: ${FILES}"

read VER < $VER_FILE

sed -i -e \
"s/version = [\"][0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+[\"]/version = \"${VER}\"/g" \
  $FILE_0

sed -i -e "s/version: [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/version: ${VER}/g" \
	$FILE_1

sed -i -e "s/version [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/version ${VER}/g" \
	$FILE_2

sed -i -e "s/versión [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/versión ${VER}/g" \
	$FILE_3

echo "done!"

