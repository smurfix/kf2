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
NCPUS="$(( $(nproc) * 2 ))"
export CPPFLAGS=-D__USE_MINGW_ANSI_STDIO
export LDFLAGS="-static-libgcc -static-libstdc++"
if [ "x$1" = "xdl" ]
then
mkdir -p ~/win64/src
mkdir -p ~/win32/src
cp -avft ~/win64/src *.patch
cp -avft ~/win32/src *.patch
# download
cd ~/win64/src
wget -c https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.7z
wget -c https://gmplib.org/download/gmp/gmp-6.2.0.tar.lz
wget -c https://www.mpfr.org/mpfr-current/mpfr-4.1.0.tar.xz
#wget -c https://www.mpfr.org/mpfr-current/allpatches
wget -c https://zlib.net/zlib-1.2.11.tar.xz
wget -c https://jpegclub.org/support/files/jpegsrc.v6b2.tar.gz
wget -c https://download.sourceforge.net/libpng/libpng-1.6.37.tar.xz
wget -c https://download.osgeo.org/libtiff/tiff-4.1.0.tar.gz
wget -c https://ftp.gnu.org/gnu/gsl/gsl-2.6.tar.gz
wget -c https://www.cairographics.org/releases/pixman-0.38.4.tar.gz
wget -c https://github.com/g-truc/glm/releases/download/0.9.9.8/glm-0.9.9.8.7z
wget -c https://github.com/AcademySoftwareFoundation/openexr/archive/v2.5.3.tar.gz -O openexr-2.5.3.tar.gz
git clone https://github.com/meganz/mingw-std-threads.git || ( cd mingw-std-threads && git pull )
git clone https://github.com/martijnberger/clew.git || ( cd clew && git pull )
cp -avft ~/win32/src *z mingw-std-threads clew # allpatches
elif [ "x$1" = "x64" ]
then
# gmp 64
cd ~/win64/src
tar xf gmp-*.tar.lz
cd gmp-*/
CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" MAKE="mingw32-make" ./configure --build=x86_64-w64-mingw32 --enable-fat --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
mingw32-make check
# mpfr 64
cd ~/win64/src
tar xf mpfr-*.tar.xz
cd mpfr-*/
#patch -N -Z -p1 < ../allpatches
./configure --prefix=$HOME/win64 --with-gmp-build=../gmp-6.2.0 --enable-static --disable-shared
make -j $NCPUS # mingw32-make does not work here
make install
make check
# zlib 64
cd ~/win64/src
tar xf zlib-*.tar.xz
cd zlib-*/
mingw32-make -fwin32/Makefile.gcc -j $NCPUS
BINARY_PATH="$HOME/win64/bin" INCLUDE_PATH="$HOME/win64/include" LIBRARY_PATH="$HOME/win64/lib" mingw32-make install -fwin32/Makefile.gcc
# png 64
cd ~/win64/src
tar xf libpng-*.tar.xz
cd libpng-*/
./configure --disable-shared CPPFLAGS="$CPPFLAGS -I$HOME/win64/include" LDFLAGS="$LDFLAGS -L$HOME/win64/lib" --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
# jpeg 64
cd ~/win64/src
tar xf jpegsrc.v6b2.tar.gz
cd jpeg-6b2
./configure --disable-shared --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
# tiff 64
cd ~/win64/src
tar xf tiff-*.tar.gz
cd tiff-*/
./configure --disable-shared --disable-lzma --disable-zstd --disable-jpeg --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
# gsl 64
cd ~/win64/src
tar xf gsl-*.tar.gz
cd gsl-*/
./configure --disable-shared --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
make check # mingw32-make does not work here
# pixman 64
cd ~/win64/src
tar xf pixman-*.tar.gz
cd pixman-*/
LDFLAGS=-L$HOME/win64/lib ./configure --disable-shared --disable-openmp --prefix=$HOME/win64
mingw32-make -j $NCPUS
mingw32-make install
mingw32-make check
# boost 64
cd ~/win64/src
7zr x boost*.7z
cd ~/win64/include
rm -f boost
cp -r ../src/boost*/boost/ .
# glm 64
cd ~/win64/src
7zr x glm*.7z
cd ~/win64/include
rm -f glm
cp -r ../src/glm*/glm/ .
# mingw-std-threads 64
cd ~/win64/include
rm -f mingw-std-threads
cp -r ../src/mingw-std-threads .
# openexr 64
cd ~/win64/src
tar xf openexr-*.tar.gz
cd openexr-*/
patch -p1 < $(ls ../openexr-*.patch)
mkdir -p build
cd build
cmake -G"MSYS Makefiles" -DBUILD_SHARED_LIBS=OFF -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake -DCMAKE_AR=/mingw64/bin/ar -DCMAKE_CXX_FLAGS=-I$HOME/win64/include -DZLIB_INCLUDE_DIR=$HOME/win64/include -DZLIB_LIBRARY=$HOME/win64/lib/libz.a -DCMAKE_INSTALL_PREFIX=$HOME/win64 ..
mingw32-make -j $NCPUS
mingw32-make install
# clew 64
#nop
else
  echo "usage:"
  echo "  $0 dl    # download sources"
  echo "  $0 64    # build 64bit libs"
  exit 1
fi
