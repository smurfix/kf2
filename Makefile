WINPREFIX ?= $(HOME)/win64
CLEWPREFIX ?= ../clew
COMPILE := x86_64-w64-mingw32-g++
LINK := x86_64-w64-mingw32-g++
FLAGS := -Wno-write-strings -pipe -MMD -g -O3 -ffast-math -mfpmath=sse -I$(WINPREFIX)/include -I$(CLEWPREFIX)/include -DKF_THREADED_REFERENCE_BARRIER -Dclew_STATIC
COMPILE_FLAGS := -xc++ $(FLAGS)
LINK_FLAGS := -static-libgcc -static-libstdc++ -Wl,--stack,67108864 -Wl,-subsystem,windows -L$(WINPREFIX)/lib -Ljpeg-6b -ffast-math
LIBS := -lgdi32 -lcomdlg32 -lole32 -loleaut32 -lcomctl32 -luuid -lgmp -ljpeg
XSLTPROC ?= xsltproc
WINDRES ?= x86_64-w64-mingw32-windres
RM := rm -f
AR := x86_64-w64-mingw32-ar
AR2 := x86_64-w64-mingw32-ranlib

FRAKTAL_SOURCES_CPP = \
fraktal_sft/CDecNumber.cpp \
fraktal_sft/CFixedFloat.cpp \
fraktal_sft/dbl_functions.cpp \
fraktal_sft/exp_functions2.cpp \
fraktal_sft/exp_functions.cpp \
fraktal_sft/fraktal_sft.cpp \
fraktal_sft/listbox.cpp \
fraktal_sft/main.cpp \
fraktal_sft/newton.cpp

FRAKTAL_SOURCES_H = \
fraktal_sft/CDecNumber.h \
fraktal_sft/CFixedFloat.h \
fraktal_sft/complex.h \
fraktal_sft/floatexp.h \
fraktal_sft/fraktal_sft.h \
fraktal_sft/listbox.h \
fraktal_sft/resource.h

LDBL_SOURCES_CPP = \
ldbl64/ldbl.cpp

COMMON_SOURCES_CPP = \
common/FolderBrowser.cpp \
common/getimage.cpp \
common/parallell.cpp \
common/StringVector.cpp \
common/tooltip.cpp

COMMON_SOURCES_H = \
common/FolderBrowser.h \
common/getimage.h \
common/parallell.h \
common/StringVector.h \
common/tooltip.h 

FORMULA_SOURCES_CPP = formula/formula.cpp

SOURCES_CPP = $(FRAKTAL_SOURCES_CPP) $(COMMON_SOURCES_CPP) $(LDBL_SOURCES_CPP) $(FORMULA_SOURCES_CPP) cl/opencl.cpp jpeg_static.cpp
SOURCES_C = cl/kf_opencl_source.c $(CLEWPREFIX)/src/clew.c
SOURCES_H = $(FRAKTAL_SOURCES_H) $(COMMON_SOURCES_H) cl/opencl.h $(CLEWPREFIX)/include/clew.h

SOURCES = $(SOURCES_CPP) $(SOURCES_C) $(SOURCES_H)

OBJECTS_CPP := $(patsubst %.cpp,%.o,$(SOURCES_CPP))
OBJECTS_C := $(patsubst %.c,%.o,$(SOURCES_C))
OBJECTS := $(OBJECTS_CPP) $(OBJECTS_C) res.o

DEPENDS := $(patsubst %.o,%.d,$(OBJECTS))

all: kf.exe

clean:
	rm -f $(OBJECTS) $(DEPENDS) $(FORMULA_SOURCES_CPP)
	rm -f cl/kf_opencl_source.c cl/kf_opencl_source.d cl/kf_opencl_source.o cl/kf.cl cl/opencl.d cl/opencl.inc cl/opencl.o
	rm -f preprocessor preprocessor.hi preprocessor.o

kf.exe: $(OBJECTS) jpeg-6b/libjpeg.a
	$(LINK) -o kf.exe $(OBJECTS) $(LINK_FLAGS) $(LIBS)

res.o: fraktal_sft/fraktal_sft.rc
	$(WINDRES) -i fraktal_sft/fraktal_sft.rc -o res.o

%.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

%.o: %.c
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

formula/formula.cpp: formula/common.cpp formula/formula.xsl formula/formula.xml preprocessor
	( cat formula/common.cpp ; $(XSLTPROC) formula/formula.xsl formula/formula.xml | ./preprocessor ) > $@

cl/kf_opencl_source.c: cl/kf.cl cl/s2c.sh
	./cl/s2c.sh kf_opencl_source < cl/kf.cl > cl/kf_opencl_source.c

cl/kf.cl: cl/common.cl cl/formula.xsl formula/formula.xml preprocessor
	( cat cl/common.cl ; $(XSLTPROC) cl/formula.xsl formula/formula.xml | ./preprocessor ) > $@

cabal.sandbox.config:
	( cabal sandbox init ; cabal install parsec )

preprocessor: preprocessor.hs cabal.sandbox.config
	( cabal exec -- ghc -O preprocessor.hs )

cl/opencl.inc: cl/opencl.xsl formula/formula.xml
	$(XSLTPROC) -o $@ cl/opencl.xsl formula/formula.xml

cl/opencl.o: cl/opencl.cpp cl/opencl.h cl/opencl.inc

jpeg_static.o: jpeg_static.cpp jpeg-6b/jconfig.h

jpeg-6b/libjpeg.a: jpeg-6b/jconfig.h
	$(MAKE) -C jpeg-6b libjpeg.a

jpeg-6b/jconfig.h: jpeg-6b
	( cd jpeg-6b ; ./configure CC=x86_64-w64-mingw32-gcc )

jpeg-6b: jpegsrc.v6b.tar.gz
	tar xf jpegsrc.v6b.tar.gz

jpegsrc.v6b.tar.gz:
	wget -c "http://www.ijg.org/files/jpegsrc.v6b.tar.gz"


-include $(DEPENDS)
