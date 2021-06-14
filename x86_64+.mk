WINPREFIXPLUS ?= $(HOME)/win/x86_64+
WINPREFIX ?= $(HOME)/win/x86_64
SIMD ?= 2
OPENCL ?= 1
# require 'haswell' (Intel Haswell), 'bdver4' (AMD Excavator), or 'eden-x4' (VIA)
COMPILE ?= x86_64-w64-mingw32-g++ -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2 -mavx -mavx2
LINK ?= x86_64-w64-mingw32-g++
WINDRES ?= x86_64-w64-mingw32-windres
WINDRES2 ?= $(WINDRES)
AR ?= x86_64-w64-mingw32-ar
AR2 ?= x86_64-w64-mingw32-ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= x86_64-w64-mingw32-gcc
