#!/bin/sh
# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2021 Claude Heiland-Allen
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
cp -avit "${SRC}/" fraktal_sft formula formulas cl common palettes utils glad preprocessor.hs Makefile armv7.mk aarch64.mk i686.mk x86_64.mk x86_64+.mk x86_64+native.mk README.md LICENSE.md prepare.sh prepare-msys.sh "${0}"
zip -0 -r "${SRC}.zip" "${SRC}/"
BIN="${VERSION}"
mkdir "${BIN}"
cp -avit "${BIN}/" "${SRC}.zip" palettes formulas

if false
then
make clean
make -j "${NCPUS}" SYSTEM=armv7
cp -avi kf.exe "${BIN}/kf.armv7.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.armv7.exe"
armv7-w64-mingw32-strip "${BIN}/kf.armv7.exe"
armv7-w64-mingw32-strip "${BIN}/kf-tile.armv7.exe"
fi

if false
then
make clean
make -j "${NCPUS}" SYSTEM=aarch64
cp -avi kf.exe "${BIN}/kf.aarch64.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.aarch64.exe"
aarch64-w64-mingw32-strip "${BIN}/kf.aarch64.exe"
aarch64-w64-mingw32-strip "${BIN}/kf-tile.aarch64.exe"
fi

make clean
make -j "${NCPUS}" SYSTEM=i686
cp -avi kf.exe "${BIN}/kf.i686.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.i686.exe"
i686-w64-mingw32-strip "${BIN}/kf.i686.exe"
i686-w64-mingw32-strip "${BIN}/kf-tile.i686.exe"

make clean
make -j "${NCPUS}" SYSTEM=x86_64
cp -avi kf.exe "${BIN}/kf.x86_64.exe"
cp -avi kf-tile.exe "${BIN}/kf-tile.x86_64.exe"
x86_64-w64-mingw32-strip "${BIN}/kf.x86_64.exe"
x86_64-w64-mingw32-strip "${BIN}/kf-tile.x86_64.exe"

make README.pdf
make LICENSE.pdf
make manual.html

cp -avi README.md "${BIN}/kf.txt"
cp -avi README.pdf "${BIN}/kf.pdf"
cp -avi LICENSE.md "${BIN}/LICENSE.txt"
cp -avi LICENSE.pdf "${BIN}/LICENSE.pdf"
7zr a -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on "${BIN}.7z" "${BIN}/"
gpg -b "${BIN}.7z"

ls -1sh "${BIN}.7z"*
