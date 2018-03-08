#!/bin/sh
set -e
NCPUS=8
export CPPFLAGS=-D__USE_MINGW_ANSI_STDIO
mkdir -p ~/win64/src
mkdir -p ~/win32/src
# download
cd ~/win64/src
wget -c https://dl.bintray.com/boostorg/release/1.66.0/source/boost_1_66_0.7z
wget -c https://gmplib.org/download/gmp/gmp-6.1.2.tar.lz
wget -c http://www.mpfr.org/mpfr-current/mpfr-4.0.1.tar.xz
wget -c https://zlib.net/zlib-1.2.11.tar.xz
wget -c http://jpegclub.org/support/files/jpegsrc.v6b2.tar.gz
wget -c ftp://ftp-osl.osuosl.org/pub/libpng/src/libpng16/libpng-1.6.34.tar.xz
cp -avft ~/win32/src *z
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
./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win64 --with-gmp-build=../gmp-6.1.2 --enable-static --disable-shared
make -j $NCPUS
make install
make check
# mpfr 32
cd ~/win32/src
tar xf mpfr-*.tar.xz
cd mpfr-*/
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
cd ~/win64/src
tar xf jpegsrc.v6b2.tar.gz
cd jpeg-6b2
./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win32
make -j $NCPUS
make install
# boost
cd ~/win64/src
7zr x boost*.7z
cd ~/win64/include
ln -s ../src/boost*/boost/
cd ~/win32/include
ln -s ../../win64/src/boost*/boost/
