#!/bin/sh
# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2020 Claude Heiland-Allen
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
set -e

VERSION="${1}"
if [ -z "${VERSION}" ]
then
	echo "usage: ${0} version"
	exit 1
fi

make clean

SRC="${VERSION}-src"
mkdir "${SRC}"
make formula/formula.cpp cl/formula.cpp
cp -avit "${SRC}/" fraktal_sft formula cl common utils preprocessor.hs Makefile 32.mk 64.mk 64+.mk native.mk README.md LICENSE.md prepare.sh prepare-msys.sh "${0}"
zip -0 -r "${SRC}.zip" "${SRC}/"
BIN="${VERSION}"
mkdir "${BIN}"
cp -avit "${BIN}/" "${SRC}.zip" utils/stratify.m utils/resizeKFB.m

make -j 32 SYSTEM=64
cp -avi kf.exe "${BIN}/kf.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.exe"
strip "${BIN}/kf.exe"
strip "${BIN}/kf-tile.exe"

make README.pdf
make LICENSE.pdf

cp -avi README.md "${BIN}/kf.txt"
cp -avi README.pdf "${BIN}/kf.pdf"
cp -avi LICENSE.md "${BIN}/LICENSE.txt"
cp -avi LICENSE.pdf "${BIN}/LICENSE.pdf"
7zr a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${BIN}.7z" "${BIN}/"
gpg -b "${BIN}.7z"

ls -1sh "${BIN}.7z"*
