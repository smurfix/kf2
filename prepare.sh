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
NCPUS=32
export CPPFLAGS=-D__USE_MINGW_ANSI_STDIO
mkdir -p ~/win64/src
mkdir -p ~/win32/src
# download
cd ~/win64/src
wget -c https://dl.bintray.com/boostorg/release/1.67.0/source/boost_1_67_0.7z
wget -c https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz
wget -c http://www.mpfr.org/mpfr-current/mpfr-4.0.1.tar.xz
wget -c http://www.mpfr.org/mpfr-current/allpatches
wget -c https://zlib.net/zlib-1.2.11.tar.xz
wget -c http://jpegclub.org/support/files/jpegsrc.v6b2.tar.gz
wget -c ftp://ftp-osl.osuosl.org/pub/libpng/src/libpng16/libpng-1.6.34.tar.xz
wget -c https://download.osgeo.org/libtiff/tiff-4.0.9.tar.gz
wget -c ftp://ftp.gnu.org/gnu/gsl/gsl-2.5.tar.gz
cp -avft ~/win32/src *z allpatches
# gmp 64
cd ~/win64/src
tar xf gmp-*.tar.lz
cd gmp-*/
./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
make check
# gmp 32
cd ~/win32/src
tar xf gmp-*.tar.lz
cd gmp-*/
./configure --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
make check
# mpfr 64
cd ~/win64/src
tar xf mpfr-*.tar.xz
cd mpfr-*/
patch -N -Z -p1 < ../allpatches
./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win64 --with-gmp-build=../gmp-6.1.2 --enable-static --disable-shared
make -j $NCPUS
make install
make check
# mpfr 32
cd ~/win32/src
tar xf mpfr-*.tar.xz
cd mpfr-*/
patch -N -Z -p1 < ../allpatches
./configure --host=i686-w64-mingw32 --prefix=$HOME/win32 --with-gmp-build=../gmp-6.1.2 --enable-static --disable-shared
make -j $NCPUS
make install
make check
# zlib 64
cd ~/win64/src
tar xf zlib-*.tar.xz
cd zlib-*/
CC=x86_64-w64-mingw32-gcc ./configure --static --prefix=$HOME/win64
CC=x86_64-w64-mingw32-gcc make -j $NCPUS
CC=x86_64-w64-mingw32-gcc make install
# zlib 32
cd ~/win32/src
tar xf zlib-*.tar.xz
cd zlib-*/
CC=i686-w64-mingw32-gcc ./configure --static --prefix=$HOME/win32
CC=i686-w64-mingw32-gcc make -j $NCPUS
CC=i686-w64-mingw32-gcc make install
# png 64
cd ~/win64/src
tar xf libpng-*.tar.xz
cd libpng-*/
./configure --disable-shared --host=x86_64-w64-mingw32 CPPFLAGS=-I$HOME/win64/include LDFLAGS=-L$HOME/win64/lib --prefix=$HOME/win64
make -j $NCPUS
make install
# png 32
cd ~/win32/src
tar xf libpng-*.tar.xz
cd libpng-*/
./configure --disable-shared --host=i686-w64-mingw32 CPPFLAGS=-I$HOME/win32/include LDFLAGS=-L$HOME/win32/lib --prefix=$HOME/win32
make -j $NCPUS
make install
# jpeg 64
cd ~/win64/src
tar xf jpegsrc.v6b2.tar.gz
cd jpeg-6b2
./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
# jpeg 32
cd ~/win32/src
tar xf jpegsrc.v6b2.tar.gz
cd jpeg-6b2
./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
# tiff 64
cd ~/win64/src
tar xf tiff-*.tar.gz
cd tiff-*/
./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
# tiff 32
cd ~/win32/src
tar xf tiff-*.tar.gz
cd tiff-*/
./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
# gsl 64
cd ~/win64/src
tar xf gsl-*.tar.gz
cd gsl-*/
./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
ln -s gsl-histogram.exe gsl-histogram # hack for test suite
make check
# gsl 32
cd ~/win32/src
tar xf gsl-*.tar.gz
cd gsl-*/
./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
ln -s gsl-histogram.exe gsl-histogram # hack for test suite
make check
# boost
cd ~/win64/src
7zr x boost*.7z
cd ~/win64/include
ln -s ../src/boost*/boost/
cd ~/win32/include
ln -s ../../win64/src/boost*/boost/
