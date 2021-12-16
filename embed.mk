WINPREFIXPLUS ?= /usr
WINPREFIX ?= /usr
SIMD ?= 4
OPENCL ?= 0
COMPILE ?= g++ -march=native -fPIC -DKF_EMBED -I ./embed/
LINK ?= g++
LIBS ?= -lHalf -lmpfr -lgsl -lIlmImf -ljpeg -ltiff -lpng -lpixman-1 -ldl -lpthread
WINDRES ?= false
WINDRES2 ?= false
AR ?= ar
AR2 ?= ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= gcc
STD ?= c++17
