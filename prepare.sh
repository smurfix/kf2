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
NCPUS=32
export CPPFLAGS=-D__USE_MINGW_ANSI_STDIO
mkdir -p ~/win64/src
mkdir -p ~/win32/src
cp -avft ~/win64/src *.patch
cp -avft ~/win32/src *.patch
# download
cd ~/win64/src
wget -c https://dl.bintray.com/boostorg/release/1.70.0/source/boost_1_70_0.7z
wget -c https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz
wget -c https://www.mpfr.org/mpfr-current/mpfr-4.0.2.tar.xz
wget -c https://www.mpfr.org/mpfr-current/allpatches
wget -c https://zlib.net/zlib-1.2.11.tar.xz
wget -c https://jpegclub.org/support/files/jpegsrc.v6b2.tar.gz
wget -c https://download.sourceforge.net/libpng/libpng-1.6.37.tar.xz
wget -c https://download.osgeo.org/libtiff/tiff-4.0.10.tar.gz
wget -c ftp://ftp.gnu.org/gnu/gsl/gsl-2.5.tar.gz
wget -c https://www.cairographics.org/releases/pixman-0.38.4.tar.gz
wget -c https://github.com/g-truc/glm/releases/download/0.9.9.5/glm-0.9.9.5.7z
wget -c https://github.com/openexr/openexr/archive/v2.2.1.tar.gz
cp -avft ~/win32/src *z allpatches
# gmp 64
cd ~/win64/src
tar xf gmp-*.tar.lz
cd gmp-*/
CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=x86_64-pc-linux-gnu --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
make check
# gmp 32
cd ~/win32/src
tar xf gmp-*.tar.lz
cd gmp-*/
CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=x86_64-pc-linux-gnu --host=i686-w64-mingw32 --prefix=$HOME/win32
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
make -k check || echo "expected 2 FAILs (multifit_nlinear, multilarge_nlinear)"
# pixman 64
cd ~/win64/src
tar xf pixman-*.tar.gz
cd pixman-*/
CC=x86_64-w64-mingw32-gcc LDFLAGS=-L$HOME/win64/lib ./configure --disable-shared --disable-openmp --prefix=$HOME/win64
make -j $NCPUS
make install
make check || echo "expected 1 FAIL (thread-test)"
# pixman 32
cd ~/win32/src
tar xf pixman-*.tar.gz
cd pixman-*/
CC=i686-w64-mingw32-gcc LDFLAGS=-L$HOME/win32/lib ./configure --disable-shared --disable-openmp --prefix=$HOME/win32
make -j $NCPUS
make install
make check || echo "expected 1 FAIL (thread-test)"
# boost
cd ~/win64/src
7zr x boost*.7z
cd ~/win64/include
rm -f boost
ln -s ../src/boost*/boost/
cd ~/win32/include
rm -f boost
ln -s ../../win64/src/boost*/boost/
# glm
cd ~/win64/src
7zr x glm*.7z
cd ~/win64/include
rm -f glm
ln -s ../src/glm*/glm/
cd ~/win32/include
rm -f glm
ln -s ../../win64/src/glm*/glm/
# openexr 64
export WINEPATH=/usr/lib/gcc/x86_64-w64-mingw32/8.3-win32/
cd ~/win64/src
tar xf v2.2.1.tar.gz
cd openexr-*/
patch -p1 < "../openexr-2.2.1.patch"
cd IlmBase
./bootstrap
./configure --disable-shared --disable-threading --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
cd ../OpenEXR
./bootstrap
CPPFLAGS="$CPPFLAGS -I$HOME/win64/include -I$HOME/win64/include/OpenEXR" LDFLAGS="-L$HOME/win64/lib" LIBS="-lHalf" ./configure --disable-shared --disable-threading --disable-ilmbasetest --disable-posix-sem --disable-imfexamples --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
# openexr 32
export WINEPATH=/usr/lib/gcc/i686-w64-mingw32/8.3-win32/
cd ~/win32/src
tar xf v2.2.1.tar.gz
cd openexr-*/
patch -p1 < "../openexr-2.2.1.patch"
cd IlmBase
./bootstrap
./configure --disable-shared --disable-threading --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
cd ../OpenEXR
./bootstrap
CPPFLAGS="$CPPFLAGS -I$HOME/win32/include -I$HOME/win32/include/OpenEXR" LDFLAGS="-L$HOME/win32/lib" LIBS="-lHalf" ./configure --disable-shared --disable-threading --disable-ilmbasetest --disable-posix-sem --disable-imfexamples --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
