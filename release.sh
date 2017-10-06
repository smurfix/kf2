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
make formula/formula.cpp
cp -avit "${SRC}/" fraktal_sft formula cl ldbl64 common preprocessor.hs Makefile 32.mk 64.mk README.md "${0}"
zip -0 -r "${SRC}.zip" "${SRC}/"
BIN="kf-${VERSION}"
mkdir "${BIN}"
cp -avit "${BIN}/" "${SRC}.zip"

make -j 8 SYSTEM=32
strip kf.exe
cp -avi kf.exe "${BIN}/kf.32.exe"

make clean

make -j 8 SYSTEM=64
strip kf.exe
cp -avi kf.exe "${BIN}/kf.64.exe"

make README.pdf

cp -avi README.md "${BIN}/kf.txt"
cp -avi README.pdf "${BIN}/kf.pdf"
zip -9 -r "${BIN}.zip" "${BIN}/"
gpg -b "${BIN}.zip"

ls -1sh "${BIN}.zip"*
