#!/bin/bash
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
NCPUS="$(( $(nproc) * 2 ))"
export CPPFLAGS="-D__USE_MINGW_ANSI_STDIO=1 -DWINVER=0x501 -D_WIN32_WINNT=0x501"
export LDFLAGS="-static-libgcc -static-libstdc++ -static -Wl,-Bstatic -lstdc++ -Wl,-Bdynamic"
ALL_ARCH="x86_64 i686 aarch64 armv7"
ALL_LIBS="gmp mpfr zlib png jpeg tiff gsl pixman glm mingw-std-threads openexr clew boost"
if [[ "x$1" = "x" ]]
then
  ACTION="dl ${ALL_ARCH}"
else
  ACTION="$1"
fi
if [[ "x$2" = "x" ]]
then
  PREPARE="${ALL_LIBS}"
else
  PREPARE="$2"
fi
if [[ "${ACTION}" =~ "-h" ]]
then
  echo "usage:"
  echo "  $0             # download and build everything"
  echo "  $0 dl          # download sources"
  echo "  $0 \$arch       # build all libraries for one architecture"
  echo "    # supported architectures:"
  echo "    ${ALL_ARCH}"
  echo "  $0 \$arch \$lib  # build one library for one architecture"
  echo "    # supported libraries:"
  echo "    ${ALL_LIBS}"
  exit 0
fi
if [[ "${ACTION}" =~ "dl" ]]
then
  mkdir -p ~/win/src
  cp -avft ~/win/src *.patch
  # download
  cd ~/win/src
  wget -c https://dl.bintray.com/boostorg/release/1.75.0/source/boost_1_75_0.7z
  wget -c https://gmplib.org/download/gmp/gmp-6.2.1.tar.lz
  wget -c https://www.mpfr.org/mpfr-current/mpfr-4.1.0.tar.xz
  #wget -c https://www.mpfr.org/mpfr-current/allpatches
  wget -c https://zlib.net/zlib-1.2.11.tar.xz
  wget -c https://jpegclub.org/support/files/jpegsrc.v6b2.tar.gz
  wget -c https://download.sourceforge.net/libpng/libpng-1.6.37.tar.xz
  wget -c https://download.osgeo.org/libtiff/tiff-4.1.0.tar.gz
  wget -c https://ftp.gnu.org/gnu/gsl/gsl-2.6.tar.gz
  wget -c https://www.cairographics.org/releases/pixman-0.38.4.tar.gz
  wget -c https://github.com/g-truc/glm/releases/download/0.9.9.8/glm-0.9.9.8.7z
  wget -c https://github.com/AcademySoftwareFoundation/openexr/archive/v2.5.5.tar.gz -O openexr-2.5.5.tar.gz
  wget -c https://github.com/glfw/glfw/releases/download/3.3.3/glfw-3.3.3.zip
  git clone https://github.com/meganz/mingw-std-threads.git || ( cd mingw-std-threads && git pull )
  git clone https://github.com/martijnberger/clew.git || ( cd clew && git pull )
fi
if [[ "${ACTION}" =~ "x86_64" ]]
then
  if [[ "${PREPARE}" =~ "gmp" ]]
  then
    # gmp 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/gmp-6.2.1.tar.lz
    cd gmp-6.2.1/
    CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=x86_64-pc-linux-gnu --host=x86_64-w64-mingw32 --enable-fat --prefix=$HOME/win/x86_64
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "mpfr" ]]
  then
    # mpfr 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/mpfr-4.1.0.tar.xz
    cd mpfr-4.1.0/
    #patch -N -Z -p1 < ../allpatches
    ./configure --host=x86_64-w64-mingw32 --prefix=$HOME/win/x86_64 --with-gmp-build=../gmp-6.2.1 --enable-static --disable-shared
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "zlib" ]]
  then
    # zlib 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/zlib-1.2.11.tar.xz
    cd zlib-1.2.11/
    CC=x86_64-w64-mingw32-gcc ./configure --static --prefix=$HOME/win/x86_64
    CC=x86_64-w64-mingw32-gcc make -j $NCPUS
    CC=x86_64-w64-mingw32-gcc make install
  fi
  if [[ "${PREPARE}" =~ "png" ]]
  then
    # png 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    cd libpng-1.6.37/
    ./configure --disable-shared --host=x86_64-w64-mingw32 CPPFLAGS="$CPPFLAGS -I$HOME/win/x86_64/include" LDFLAGS="$LDFLAGS -L$HOME/win/x86_64/lib" --prefix=$HOME/win/x86_64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "jpeg" ]]
  then
    # jpeg 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/jpegsrc.v6b2.tar.gz
    cd jpeg-6b2/
    ./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win/x86_64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "tiff" ]]
  then
    # tiff 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/tiff-4.1.0.tar.gz
    cd tiff-4.1.0/
    ./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win/x86_64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "gsl" ]]
  then
    # gsl 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/gsl-2.6.tar.gz
    cd gsl-2.6/
    ./configure --disable-shared --host=x86_64-w64-mingw32 --prefix=$HOME/win/x86_64
    make -j $NCPUS
    make install
    rm -f gsl-histogram
    ln -s gsl-histogram.exe gsl-histogram # hack for test suite
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "pixman" ]]
  then
    # pixman 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/pixman-0.38.4.tar.gz
    cd pixman-0.38.4/
    CC=x86_64-w64-mingw32-gcc LDFLAGS=-L$HOME/win/x86_64/lib ./configure --disable-shared --disable-openmp --prefix=$HOME/win/x86_64
    make SUBDIRS="pixman test" -j $NCPUS
    make SUBDIRS="pixman test" install
    make SUBDIRS="pixman test" check -k || echo "expected 1 FAIL (thread-test)"
  fi
  if [[ "${PREPARE}" =~ "boost" ]]
  then
    # boost 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    7zr x ~/win/src/boost_1_75_0.7z
    cd ~/win/x86_64/include
    rm -f boost
    ln -s ../src/boost_1_75_0/boost/
  fi
  if [[ "${PREPARE}" =~ "glm" ]]
  then
    # glm 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    7zr x ~/win/src/glm-0.9.9.8.7z
    cd ~/win/x86_64/include
    rm -f glm
    ln -s ../src/glm/glm/
  fi
  if [[ "${PREPARE}" =~ "mingw-std-threads" ]]
  then
    # mingw-std-threads 64
    mkdir -p ~/win/x86_64/include
    cd ~/win/x86_64/include
    rm -f mingw-std-threads
    ln -s ~/win/src/mingw-std-threads
  fi
  if [[ "${PREPARE}" =~ "openexr" ]]
  then
    # openexr 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    tar xaf ~/win/src/openexr-2.5.5.tar.gz
    cd openexr-2.5.5/
    #patch -p1 < ~/win/src/openexr-2.4.0.patch
    sed -i "s/#ifdef _WIN32/#if 0/g" OpenEXR/IlmImf/ImfStdIO.cpp
    mkdir -p build
    cd build
    cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake -DCMAKE_CXX_FLAGS=-I$HOME/win/x86_64/include -DZLIB_INCLUDE_DIR=$HOME/win/x86_64/include -DZLIB_LIBRARY=$HOME/win/x86_64/lib/libz.a -DCMAKE_INSTALL_PREFIX=$HOME/win/x86_64 ..
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "glfw" ]]
  then
    # glfw 64
    mkdir -p ~/win/x86_64/src
    cd ~/win/x86_64/src
    unzip ~/win/src/glfw-3.3.3.zip
    cd glfw-3.3.3/
    mkdir -p build
    cd build
    cmake -DCMAKE_TOOLCHAIN_FILE=../CMake/x86_64-w64-mingw32.cmake -DCMAKE_INSTALL_PREFIX=${HOME}/win/x86_64/ -DBUILD_SHARED_LIBS=OFF -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF -DGLFW_USE_HYBRID_HPG=ON ..
    make -j $NCPUS
    make install
  fi
  # clew 64
  # nop
fi
if [[ "${ACTION}" =~ "i686" ]]
then
  if [[ "${PREPARE}" =~ "gmp" ]]
  then
    # gmp 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/gmp-6.2.1.tar.lz
    cd gmp-6.2.1/
    CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=x86_64-pc-linux-gnu --host=i686-w64-mingw32 --enable-fat --prefix=$HOME/win/i686
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "mpfr" ]]
  then
    # mpfr 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/mpfr-4.1.0.tar.xz
    cd mpfr-4.1.0/
    #patch -N -Z -p1 < ../allpatches
    ./configure --host=i686-w64-mingw32 --prefix=$HOME/win/i686 --with-gmp-build=../gmp-6.2.1 --enable-static --disable-shared
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "zlib" ]]
  then
    # zlib 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/zlib-1.2.11.tar.xz
    cd zlib-1.2.11/
    CC=i686-w64-mingw32-gcc ./configure --static --prefix=$HOME/win/i686
    CC=i686-w64-mingw32-gcc make -j $NCPUS
    CC=i686-w64-mingw32-gcc make install
  fi
  if [[ "${PREPARE}" =~ "png" ]]
  then
    # png 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    cd libpng-1.6.37/
    ./configure --disable-shared --host=i686-w64-mingw32 CPPFLAGS="$CPPFLAGS -I$HOME/win/i686/include" LDFLAGS="$LDFLAGS -L$HOME/win/i686/lib" --prefix=$HOME/win/i686
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "jpeg" ]]
  then
    # jpeg 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/jpegsrc.v6b2.tar.gz
    cd jpeg-6b2/
    ./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win/i686
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "tiff" ]]
  then
    # tiff 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/tiff-4.1.0.tar.gz
    cd tiff-4.1.0/
    ./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win/i686
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "gsl" ]]
  then
    # gsl 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/gsl-2.6.tar.gz
    cd gsl-2.6/
    ./configure --disable-shared --host=i686-w64-mingw32 --prefix=$HOME/win/i686
    make -j $NCPUS
    make install
    rm -f gsl-histogram
    ln -s gsl-histogram.exe gsl-histogram # hack for test suite
    make -k check || echo "expected 2 FAILs (multifit_nlinear, multilarge_nlinear)"
  fi
  if [[ "${PREPARE}" =~ "pixman" ]]
  then
    # pixman 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xaf ~/win/src/pixman-0.38.4.tar.gz
    cd pixman-0.38.4/
    patch -p1 < ~/win/src/pixman-0.38.4-i686.patch
    CC=i686-w64-mingw32-gcc LDFLAGS=-L$HOME/win/i686/lib ./configure --disable-shared --disable-openmp --prefix=$HOME/win/i686
    make SUBDIRS="pixman test" -j $NCPUS
    make SUBDIRS="pixman test" install
    make SUBDIRS="pixman test" check -k || echo "expected 1 FAIL (thread-test)"
  fi
  if [[ "${PREPARE}" =~ "boost" ]]
  then
    # boost 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    7zr x ~/win/src/boost_1_75_0.7z
    cd ~/win/i686/include
    rm -f boost
    ln -s ../src/boost_1_75_0/boost/
  fi
  if [[ "${PREPARE}" =~ "glm" ]]
  then
    # glm 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    7zr x ~/win/src/glm-0.9.9.8.7z
    cd ~/win/i686/include
    rm -f glm
    ln -s ../src/glm/glm/
  fi
  if [[ "${PREPARE}" =~ "mingw-std-threads" ]]
  then
    # mingw-std-threads 32
    mkdir -p ~/win/i686/include
    cd ~/win/i686/include
    rm -f mingw-std-threads
    ln -s ~/win/src/mingw-std-threads
  fi
  if [[ "${PREPARE}" =~ "openexr" ]]
  then
    # openexr 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    tar xf ~/win/src/openexr-2.5.5.tar.gz
    cd openexr-2.5.5/
    #patch -p1 < ~/win/src/openexr-2.4.0.patch
    sed -i "s/#ifdef _WIN32/#if 0/g" OpenEXR/IlmImf/ImfStdIO.cpp
    sed -i "s/x86_64/i686/g" cmake/Toolchain-mingw.cmake
    mkdir -p build
    cd build
    cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake -DCMAKE_CXX_FLAGS=-I$HOME/win/i686/include -DZLIB_INCLUDE_DIR=$HOME/win/i686/include -DZLIB_LIBRARY=$HOME/win/i686/lib/libz.a -DCMAKE_INSTALL_PREFIX=$HOME/win/i686 ..
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "glfw" ]]
  then
    # glfw 32
    mkdir -p ~/win/i686/src
    cd ~/win/i686/src
    unzip ~/win/src/glfw-3.3.3.zip
    cd glfw-3.3.3/
    mkdir -p build
    cd build
    cmake -DCMAKE_TOOLCHAIN_FILE=../CMake/i686-w64-mingw32.cmake -DCMAKE_INSTALL_PREFIX=${HOME}/win/i686/ -DBUILD_SHARED_LIBS=OFF -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF -DGLFW_USE_HYBRID_HPG=ON ..
    make -j $NCPUS
    make install
  fi
  # clew 32
  #nop
fi
if [[ "${ACTION}" =~ "aarch64" ]]
then
  if [[ "${PREPARE}" =~ "gmp" ]]
  then
    # gmp 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/gmp-6.2.1.tar.lz
    cd gmp-6.2.1/
    CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=aarch64-pc-linux-gnu --host=aarch64-w64-mingw32 --enable-fat --prefix=$HOME/win/aarch64
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "mpfr" ]]
  then
    # mpfr 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/mpfr-4.1.0.tar.xz
    cd mpfr-4.1.0/
    #patch -N -Z -p1 < ../allpatches
    ./configure --host=aarch64-w64-mingw32 --prefix=$HOME/win/aarch64 --with-gmp-build=../gmp-6.2.1 --enable-static --disable-shared
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "zlib" ]]
  then
    # zlib 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/zlib-1.2.11.tar.xz
    cd zlib-1.2.11/
    CC=aarch64-w64-mingw32-gcc AR=aarch64-w64-mingw32-ar RANLIB=aarch64-w64-mingw32-ranlib ./configure --static --prefix=$HOME/win/aarch64
    CC=aarch64-w64-mingw32-gcc AR=aarch64-w64-mingw32-ar RANLIB=aarch64-w64-mingw32-ranlib make -j $NCPUS -k || echo
    CC=aarch64-w64-mingw32-gcc AR=aarch64-w64-mingw32-ar RANLIB=aarch64-w64-mingw32-ranlib make install
  fi
  if [[ "${PREPARE}" =~ "png" ]]
  then
    # png 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    cd libpng-1.6.37/
    ./configure --disable-shared --host=aarch64-w64-mingw32 CPPFLAGS="$CPPFLAGS -I$HOME/win/aarch64/include" LDFLAGS="$LDFLAGS -L$HOME/win/aarch64/lib" --prefix=$HOME/win/aarch64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "jpeg" ]]
  then
    # jpeg 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    tar xaf ~/win/src/jpegsrc.v6b2.tar.gz
    cp -avf libpng-1.6.37/config.sub jpeg-6b2/config.sub
    cd jpeg-6b2/
    ./configure --disable-shared --host=aarch64-w64-mingw32 --prefix=$HOME/win/aarch64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "tiff" ]]
  then
    # tiff 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/tiff-4.1.0.tar.gz
    cd tiff-4.1.0/
    ./configure --disable-shared --host=aarch64-w64-mingw32 --prefix=$HOME/win/aarch64
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "gsl" ]]
  then
    # gsl 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/gsl-2.6.tar.gz
    cd gsl-2.6/
    ./configure --disable-shared --host=aarch64-w64-mingw32 --prefix=$HOME/win/aarch64
    make -j $NCPUS
    make install
    rm -f gsl-histogram
    ln -s gsl-histogram.exe gsl-histogram # hack for test suite
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "pixman" ]]
  then
    # pixman 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/pixman-0.38.4.tar.gz
    cd pixman-0.38.4/
    LDFLAGS=-L$HOME/win/aarch64/lib ./configure --host aarch64-w64-mingw32 --disable-shared --disable-openmp --prefix=$HOME/win/aarch64
    make SUBDIRS="pixman test" -j $NCPUS
    make SUBDIRS="pixman test" install
    make SUBDIRS="pixman test" check -k || echo "expected 1 FAIL (thread-test)"
  fi
  if [[ "${PREPARE}" =~ "boost" ]]
  then
    # boost 64
    mkdir -p ~/win/aarch64/src
    mkdir -p ~/win/aarch64/include
    cd ~/win/aarch64/src
    7zr x ~/win/src/boost_1_75_0.7z
    cd ~/win/aarch64/include
    rm -f boost
    ln -s ../src/boost_1_75_0/boost/
  fi
  if [[ "${PREPARE}" =~ "glm" ]]
  then
    # glm 64
    mkdir -p ~/win/aarch64/src
    mkdir -p ~/win/aarch64/include
    cd ~/win/aarch64/src
    7zr x ~/win/src/glm-0.9.9.8.7z
    cd ~/win/aarch64/include
    rm -f glm
    ln -s ../src/glm/glm/
  fi
  if [[ "${PREPARE}" =~ "mingw-std-threads" ]]
  then
    # mingw-std-threads 64
    mkdir -p ~/win/aarch64/include
    cd ~/win/aarch64/include
    rm -f mingw-std-threads
    ln -s ~/win/src/mingw-std-threads
  fi
  if [[ "${PREPARE}" =~ "openexr" ]]
  then
    # openexr 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    tar xaf ~/win/src/openexr-2.5.5.tar.gz
    cd openexr-2.5.5/
    #patch -p1 < ~/win/src/openexr-2.4.0.patch
    sed -i "s/#ifdef _WIN32/#if 0/g" OpenEXR/IlmImf/ImfStdIO.cpp
    sed -i "s/x86_64/aarch64/g" cmake/Toolchain-mingw.cmake
    mkdir -p build
    cd build
    cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake -DCMAKE_CXX_FLAGS=-I$HOME/win/aarch64/include -DZLIB_INCLUDE_DIR=$HOME/win/aarch64/include -DZLIB_LIBRARY=$HOME/win/aarch64/lib/libz.a -DCMAKE_INSTALL_PREFIX=$HOME/win/aarch64 ..
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "glfw" ]]
  then
    # glfw 64
    mkdir -p ~/win/aarch64/src
    cd ~/win/aarch64/src
    unzip ~/win/src/glfw-3.3.3.zip
    cd glfw-3.3.3/
    sed -i "s/x86_64/aarch64/g" CMake/x86_64-w64-mingw32.cmake
    mkdir -p build
    cd build
    cmake -DCMAKE_TOOLCHAIN_FILE=../CMake/x86_64-w64-mingw32.cmake -DCMAKE_INSTALL_PREFIX=${HOME}/win/aarch64/ -DBUILD_SHARED_LIBS=OFF -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF -DGLFW_USE_HYBRID_HPG=ON ..
    make -j $NCPUS
    make install
  fi
  # clew 64
  # nop
fi
if [[ "${ACTION}" =~ "armv7" ]]
then
  if [[ "${PREPARE}" =~ "gmp" ]]
  then
    # gmp 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/gmp-6.2.1.tar.lz
    cd gmp-6.2.1/
    CC_FOR_BUILD="gcc" CPP_FOR_BUILD="gcc -E" ./configure --build=x86_64-pc-linux-gnu --host=armv7-w64-mingw32 --disable-assembly --prefix=$HOME/win/armv7
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "mpfr" ]]
  then
    # mpfr 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/mpfr-4.1.0.tar.xz
    cd mpfr-4.1.0/
    #patch -N -Z -p1 < ../allpatches
    ./configure --host=armv7-w64-mingw32 --prefix=$HOME/win/armv7 --with-gmp-build=../gmp-6.2.1 --enable-static --disable-shared
    make -j $NCPUS
    make install
    make check -k || echo
  fi
  if [[ "${PREPARE}" =~ "zlib" ]]
  then
    # zlib 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/zlib-1.2.11.tar.xz
    cd zlib-1.2.11/
    CC=armv7-w64-mingw32-gcc AR=armv7-w64-mingw32-ar RANLIB=armv7-w64-mingw32-ranlib ./configure --static --prefix=$HOME/win/armv7
    CC=armv7-w64-mingw32-gcc AR=armv7-w64-mingw32-ar RANLIB=armv7-w64-mingw32-ranlib make -j $NCPUS
    CC=armv7-w64-mingw32-gcc AR=armv7-w64-mingw32-ar RANLIB=armv7-w64-mingw32-ranlib make install
  fi
  if [[ "${PREPARE}" =~ "png" ]]
  then
    # png 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    cd libpng-1.6.37/
    ./configure --disable-shared --host=armv7-w64-mingw32 CPPFLAGS="$CPPFLAGS -I$HOME/win/armv7/include" LDFLAGS="$LDFLAGS -L$HOME/win/armv7/lib" --prefix=$HOME/win/armv7
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "jpeg" ]]
  then
    # jpeg 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/libpng-1.6.37.tar.xz
    tar xaf ~/win/src/jpegsrc.v6b2.tar.gz
    cp -avf libpng-1.6.37/config.sub jpeg-6b2/config.sub
    cd jpeg-6b2/
    ./configure --disable-shared --host=armv7-w64-mingw32 --prefix=$HOME/win/armv7
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "tiff" ]]
  then
    # tiff 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/tiff-4.1.0.tar.gz
    cd tiff-4.1.0/
    ./configure --disable-shared --host=armv7-w64-mingw32 --prefix=$HOME/win/armv7
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "gsl" ]]
  then
    # gsl 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/gsl-2.6.tar.gz
    cd gsl-2.6/
    ./configure --disable-shared --host=armv7-w64-mingw32 --prefix=$HOME/win/armv7
    make -j $NCPUS
    make install
    rm -f gsl-histogram
    ln -s gsl-histogram.exe gsl-histogram # hack for test suite
    make -k check || echo "expected 2 FAILs (multifit_nlinear, multilarge_nlinear)"
  fi
  if [[ "${PREPARE}" =~ "pixman" ]]
  then
    # pixman 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xaf ~/win/src/pixman-0.38.4.tar.gz
    cd pixman-0.38.4/
    LDFLAGS=-L${HOME}/win/armv7/lib ./configure --host armv7-w64-mingw32 --disable-shared --disable-openmp --prefix=$HOME/win/armv7
    make SUBDIRS="pixman test" -j $NCPUS
    make SUBDIRS="pixman test" install
    make SUBDIRS="pixman test" check -k || echo "expected 1 FAIL (thread-test)"
  fi
  if [[ "${PREPARE}" =~ "boost" ]]
  then
    # boost 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    7zr x ~/win/src/boost_1_75_0.7z
    cd ~/win/armv7/include
    rm -f boost
    ln -s ../src/boost_1_75_0/boost/
  fi
  if [[ "${PREPARE}" =~ "glm" ]]
  then
    # glm 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    7zr x ~/win/src/glm-0.9.9.8.7z
    cd ~/win/armv7/include
    rm -f glm
    ln -s ../src/glm/glm/
  fi
  if [[ "${PREPARE}" =~ "mingw-std-threads" ]]
  then
    # mingw-std-threads 32
    mkdir -p ~/win/armv7/include
    cd ~/win/armv7/include
    rm -f mingw-std-threads
    ln -s ~/win/src/mingw-std-threads
  fi
  if [[ "${PREPARE}" =~ "openexr" ]]
  then
    # openexr 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    tar xf ~/win/src/openexr-2.5.5.tar.gz
    cd openexr-2.5.5/
    #patch -p1 < ~/win/src/openexr-2.4.0.patch
    sed -i "s/#ifdef _WIN32/#if 0/g" OpenEXR/IlmImf/ImfStdIO.cpp
    sed -i "s/x86_64/armv7/g" cmake/Toolchain-mingw.cmake
    mkdir -p build
    cd build
    cmake -DBUILD_SHARED_LIBS=OFF -DCMAKE_TOOLCHAIN_FILE=../cmake/Toolchain-mingw.cmake -DCMAKE_CXX_FLAGS=-I$HOME/win/armv7/include -DZLIB_INCLUDE_DIR=$HOME/win/armv7/include -DZLIB_LIBRARY=$HOME/win/armv7/lib/libz.a -DCMAKE_INSTALL_PREFIX=$HOME/win/armv7 ..
    make -j $NCPUS
    make install
  fi
  if [[ "${PREPARE}" =~ "glfw" ]]
  then
    # glfw 32
    mkdir -p ~/win/armv7/src
    cd ~/win/armv7/src
    unzip ~/win/src/glfw-3.3.3.zip
    cd glfw-3.3.3/
    sed -i "s/i686/armv7/g" CMake/i686-w64-mingw32.cmake
    mkdir -p build
    cd build
    cmake -DCMAKE_TOOLCHAIN_FILE=../CMake/i686-w64-mingw32.cmake -DCMAKE_INSTALL_PREFIX=${HOME}/win/armv7/ -DBUILD_SHARED_LIBS=OFF -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF -DGLFW_USE_HYBRID_HPG=ON ..
    make -j $NCPUS
    make install
  fi
  # clew 32
  #nop
fi
