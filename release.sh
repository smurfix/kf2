#!/bin/sh
set -e

VERSION="${1}"
if [ -z "${VERSION}" ]
then
	echo "usage: ${0} version"
	exit 1
fi

make clean

SRC="kf-${VERSION}-src"
mkdir "${SRC}"
cp -avit "${SRC}/" fraktal_sft formula cl ldbl64 common jpeg_static.cpp preprocessor.hs Makefile README "${0}"
zip -0 -r "${SRC}.zip" "${SRC}/"

BIN="kf-${VERSION}"
mkdir "${BIN}"
make -j 8
strip kf.exe
cp -avit "${BIN}/" kf.exe "${SRC}.zip"
cp -avi README "${BIN}/kf.txt"
zip -9 -r "${BIN}.zip" "${BIN}/"
gpg -b "${BIN}.zip"

ls -1sh "${BIN}.zip"*
