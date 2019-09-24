WINPREFIXPLUS ?= $(HOME)/win64+
WINPREFIX ?= $(HOME)/win64
SIMD ?= 4
COMPILE ?= x86_64-w64-mingw32-g++ -march=native
LINK ?= x86_64-w64-mingw32-g++
WINDRES ?= x86_64-w64-mingw32-windres
AR ?= x86_64-w64-mingw32-ar
AR2 ?= x86_64-w64-mingw32-ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= x86_64-w64-mingw32-gcc
