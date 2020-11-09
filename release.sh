#!/bin/sh
# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2019 Claude Heiland-Allen
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

NCPUS="$(( $(nproc) * 2 ))"

SRC="${VERSION}-src"
mkdir "${SRC}"

make clean
make formula/formula.cpp cl/formula.cpp
cp -avit "${SRC}/" fraktal_sft formula cl common utils preprocessor.hs Makefile 32.mk 64.mk 64+.mk native.mk README.md LICENSE.md prepare.sh prepare-msys.sh "${0}"
zip -0 -r "${SRC}.zip" "${SRC}/"
BIN="${VERSION}"
mkdir "${BIN}"
cp -avit "${BIN}/" "${SRC}.zip" utils/stratify.m utils/resizeKFB.m

make clean
make formula/formula.cpp cl/formula.cpp
make -j "${NCPUS}" SYSTEM=32
cp -avi kf.exe "${BIN}/kf.32.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.32.exe"
strip "${BIN}/kf.32.exe"
strip "${BIN}/kf-tile.32.exe"

make clean
make formula/formula.cpp cl/formula.cpp
make -j "${NCPUS}" SYSTEM=64
cp -avi kf.exe "${BIN}/kf.64.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.64.exe"
strip "${BIN}/kf.64.exe"
strip "${BIN}/kf-tile.64.exe"

make README.pdf
make LICENSE.pdf
pandoc -f markdown -t html -s --toc --metadata "title=Kalles Fraktaler 2+ Manual" README.md -o manual.html

cp -avi README.md "${BIN}/kf.txt"
cp -avi README.pdf "${BIN}/kf.pdf"
cp -avi LICENSE.md "${BIN}/LICENSE.txt"
cp -avi LICENSE.pdf "${BIN}/LICENSE.pdf"
7zr a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${BIN}.7z" "${BIN}/"
gpg -b "${BIN}.7z"

ls -1sh "${BIN}.7z"*
