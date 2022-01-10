WINPREFIXPLUS ?= /usr
WINPREFIX ?= /usr
SIMD ?= 4
OPENCL ?= 1
COMPILE ?= g++ -march=native
TYPEFLAGS ?= -D__USE_MINGW_ANSI_STDIO=1 -DWINVER=0x501 -D_WIN32_WINNT=0x501
# -g -Og
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
