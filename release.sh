#!/bin/sh
# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2018 Claude Heiland-Allen
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

SRC="kf-${VERSION}-src"
mkdir "${SRC}"
make formula/formula.cpp
cp -avit "${SRC}/" fraktal_sft formula cl common preprocessor.hs Makefile 32.mk 64.mk README.md prepare.sh "${0}"
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
