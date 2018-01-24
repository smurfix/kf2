#!/bin/sh
set -e
NCPUS=8
export CPPFLAGS=-D__USE_MINGW_ANSI_STDIO
mkdir -p ~/win64/src
mkdir -p ~/win32/src

cd ~/win64/src
wget -c https://dl.bintray.com/boostorg/release/1.66.0/source/boost_1_66_0.7z
wget -c https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz
wget -c https://ftp.gnu.org/gnu/mpfr/mpfr-3.1.6.tar.xz
wget -c https://zlib.net/zlib-1.2.11.tar.xz
wget -c http://www.ijg.org/files/jpegsrc.v6b.tar.gz
wget -c ftp://ftp-osl.osuosl.org/pub/libpng/src/libpng16/libpng-1.6.34.tar.xz
cp -avft ~/win32/src *z

cd ~/win64/src
tar xf gmp-6.1.2.tar.lz
cd gmp-6.1.2
./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win64
make -j $NCPUS
make install
make check

cd ~/win32/src
tar xf gmp-6.1.2.tar.lz
cd gmp-6.1.2
./configure --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
make check

cd ~/win64/src
tar xf mpfr-3.1.6.tar.xz
cd mpfr-3.1.6
patch -N -Z -p1 < ../mpfr-3.1.6.patch
./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win64 \
  --with-gmp-build=../gmp-6.1.2 --enable-static --disable-shared
make -j $NCPUS
make install
make check

cd ~/win32/src
tar xf mpfr-3.1.6.tar.xz
cd mpfr-3.1.6
./configure --host=i686-w64-mingw32 --prefix=$HOME/win32 \
  --with-gmp-build=../gmp-6.1.2 --enable-static --disable-shared
make -j $NCPUS
make install
make check

cd ~/win64/src
tar xf zlib-1.2.11.tar.xz
cd zlib-1.2.11
CC=x86_64-w64-mingw32-gcc ./configure --static --prefix=$HOME/win64
CC=x86_64-w64-mingw32-gcc make -j 8
CC=x86_64-w64-mingw32-gcc make install

cd ~/win32/src
tar xf zlib-1.2.11.tar.xz
cd zlib-1.2.11
CC=i686-w64-mingw32-gcc ./configure --static --prefix=$HOME/win32
CC=i686-w64-mingw32-gcc make -j 8
CC=i686-w64-mingw32-gcc make install

cd ~/win64/src
tar xf libpng-1.6.34.tar.xz
cd libpng-1.6.34
./configure --disable-shared --host=x86_64-w64-mingw32 \
  CPPFLAGS=-I$HOME/win64/include LDFLAGS=-L$HOME/win64/lib \
  --prefix=$HOME/win64
make -j $NCPUS
make install

cd ~/win32/src
tar xf libpng-1.6.34.tar.xz
cd libpng-1.6.34
./configure --disable-shared --host=i686-w64-mingw32 \
  CPPFLAGS=-I$HOME/win32/include LDFLAGS=-L$HOME/win32/lib \
  --prefix=$HOME/win32
make -j $NCPUS
make install

cd ~/win64/src
tar xf jpegsrc.v6b.tar.gz
cd jpeg-6b
./configure --disable-shared CC=x86_64-w64-mingw32-gcc \
  --prefix=$HOME/win64
make -j $NCPUS
cp -av libjpeg.a ~/win64/lib
cp -av jpeglib.h jconfig.h jmorecfg.h jpegint.h jerror.h ~/win64/include

cd ~/win32/src
tar xf jpegsrc.v6b.tar.gz
cd jpeg-6b
./configure --disable-shared CC=i686-w64-mingw32-gcc \
  --prefix=$HOME/win32
make -j $NCPUS
cp -av libjpeg.a ~/win32/lib
cp -av jpeglib.h jconfig.h jmorecfg.h jpegint.h jerror.h ~/win32/include

cd ~/win64/src
7zr x boost*.7z
cd ~/win64/include
ln -s ../src/boost*/boost/

cd ~/win32/include
ln -s ../../win64/src/boost*/boost/
