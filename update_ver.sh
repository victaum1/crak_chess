#!/bin/bash

set -e

VER_FILE=VERSION
FILES_2_UPDATE_EN="crak-chess.cabal README.md"
FILES_2_UPDATE_SP="LEEME.md"

FILES="${FILES_2_UPDATE_EN} ${FILES_2_UPDATE_SP}"

echo "Updating version in $FILES ..."

read VER < $VER_FILE

for item in $FILES; do
	sed -i -e "s/version: [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/version: ${VER}/g" $item
done

for item in $FILES; do
	sed -i -e "s/version [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/version ${VER}/g" $item
done

for item in $FILES; do
	sed -i -e "s/versión [0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/versión ${VER}/g" $item
done

echo "done!"

