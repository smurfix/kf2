WINPREFIXPLUS ?= /usr
WINPREFIX ?= /usr
SIMD ?= 4
OPENCL ?= 1
COMPILE ?= g++ -march=native
TYPEFLAGS ?= -march=native -fPIC -I ./embed/ -g -Og
LINK ?= g++ -g
LIBS ?= -lHalf -lmpfr -lgsl -lIlmImf -lglfw -lclew -lOpenCL -ljpeg -ltiff -lpng -lpixman-1 -ldl -lpthread
WINDRES ?= false
WINDRES2 ?= false
AR ?= ar
AR2 ?= ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= gcc
STD ?= c++17
