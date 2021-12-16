WINPREFIXPLUS ?= /usr
WINPREFIX ?= /usr
SIMD ?= 4
OPENCL ?= 0
COMPILE ?= g++ -march=native -fPIC -DKF_EMBED -I ./embed/
LINK ?= g++
LIBS ?= -lHalf -lmpfr -ldl
WINDRES ?= false
WINDRES2 ?= false
AR ?= ar
AR2 ?= ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= gcc
