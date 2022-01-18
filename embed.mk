WINPREFIXPLUS ?= /usr
WINPREFIX ?= /usr
SIMD ?= 4
OPENCL ?= 1
COMPILE ?= g++-12 -march=native
TYPEFLAGS ?= -march=native -fPIC -I ./embed/ -DKF_EMBED
# -g -Og
LINK ?= g++-12
LIBS ?= -lHalf -lmpfr -lgsl -lIlmImf -lglfw -lclew -lOpenCL -ljpeg -ltiff -lpng -lpixman-1 -ldl -lpthread
WINDRES ?= false
WINDRES2 ?= false
AR ?= ar
AR2 ?= ranlib
XSLTPROC ?= xsltproc
RM ?= rm -f
GCC ?= gcc
DEBUG ?= -O0 -g -D_DEBUG
