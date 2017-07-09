WINPREFIX ?= $(HOME)/win64
CLEWPREFIX ?= ../clew
COMPILE := x86_64-w64-mingw32-g++
LINK := x86_64-w64-mingw32-g++
FLAGS := -Wno-write-strings -pipe -MMD -g -O3 -ffast-math -mfpmath=sse -I$(WINPREFIX)/include -I$(CLEWPREFIX)/include -DKF_THREADED_REFERENCE_BARRIER -Dclew_STATIC
COMPILE_FLAGS := -xc++ $(FLAGS)
HEADER_FLAGS := -xc++-header $(FLAGS)
LINK_FLAGS := -static-libgcc -static-libstdc++ -Wl,--stack,67108864 -Wl,-subsystem,windows -L$(WINPREFIX)/lib
LIBS := -lgdi32 -lcomdlg32 -lole32 -loleaut32 -lcomctl32 -luuid -lgmp
XSLTPROC ?= xsltproc
WINDRES ?= x86_64-w64-mingw32-windres

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

JPEG_SOURCES_CPP = \
jpeg/jpeg_static.cpp

JPEG_SOURCES_C = \
jpeg/cdjpeg.c \
jpeg/jcapimin.c \
jpeg/jcapistd.c \
jpeg/jccoefct.c \
jpeg/jccolor.c \
jpeg/jcdctmgr.c \
jpeg/jchuff.c \
jpeg/jcinit.c \
jpeg/jcmainct.c \
jpeg/jcmarker.c \
jpeg/jcmaster.c \
jpeg/jcomapi.c \
jpeg/jcparam.c \
jpeg/jcphuff.c \
jpeg/jcprepct.c \
jpeg/jcsample.c \
jpeg/jctrans.c \
jpeg/jdapimin.c \
jpeg/jdapistd.c \
jpeg/jdatadst.c \
jpeg/jdatasrc.c \
jpeg/jdcoefct.c \
jpeg/jdcolor.c \
jpeg/jddctmgr.c \
jpeg/jdhuff.c \
jpeg/jdinput.c \
jpeg/jdmainct.c \
jpeg/jdmarker.c \
jpeg/jdmaster.c \
jpeg/jdmerge.c \
jpeg/jdphuff.c \
jpeg/jdpostct.c \
jpeg/jdsample.c \
jpeg/jdtrans.c \
jpeg/jerror.c \
jpeg/jfdctflt.c \
jpeg/jfdctfst.c \
jpeg/jfdctint.c \
jpeg/jidctflt.c \
jpeg/jidctfst.c \
jpeg/jidctint.c \
jpeg/jidctred.c \
jpeg/jmemansi.c \
jpeg/jmemmgr.c \
jpeg/jquant1.c \
jpeg/jquant2.c \
jpeg/jutils.c \
jpeg/rdbmp.c \
jpeg/rdcolmap.c \
jpeg/rdgif.c \
jpeg/rdppm.c \
jpeg/rdrle.c \
jpeg/rdswitch.c \
jpeg/rdtarga.c \
jpeg/transupp.c \
jpeg/wrbmp.c \
jpeg/wrgif.c \
jpeg/wrppm.c \
jpeg/wrrle.c \
jpeg/wrtarga.c

JPEG_SOURCES_H = \
jpeg/cderror.h \
jpeg/cdjpeg.h \
jpeg/jchuff.h \
jpeg/jconfig.h \
jpeg/jdct.h \
jpeg/jdhuff.h \
jpeg/jerror.h \
jpeg/jinclude.h \
jpeg/jmemsys.h \
jpeg/jmorecfg.h \
jpeg/jpegint.h \
jpeg/jpeglib.h \
jpeg/jversion.h \
jpeg/transupp.h

FORMULA_SOURCES_CPP = formula/formula.cpp

SOURCES_CPP = $(FRAKTAL_SOURCES_CPP) $(COMMON_SOURCES_CPP) $(LDBL_SOURCES_CPP) $(JPEG_SOURCES_CPP) $(FORMULA_SOURCES_CPP) cl/opencl.cpp
SOURCES_C = $(JPEG_SOURCES_C) cl/kf_opencl_source.c $(CLEWPREFIX)/src/clew.c
SOURCES_H = $(FRAKTAL_SOURCES_H) $(COMMON_SOURCES_H) $(JPEG_SOURCES_H) cl/opencl.h $(CLEWPREFIX)/include/clew.h

SOURCES = $(SOURCES_CPP) $(SOURCES_C) $(SOURCES_H)

OBJECTS_CPP := $(patsubst %.cpp,%.o,$(SOURCES_CPP))
OBJECTS_C := $(patsubst %.c,%.o,$(SOURCES_C))
OBJECTS := $(OBJECTS_CPP) $(OBJECTS_C) res.o

DEPENDS := $(patsubst %.o,%.d,$(OBJECTS))

all: kf.exe

clean:
	rm -f $(OBJECTS) $(DEPENDS) $(FORMULA_SOURCES_CPP)
	rm -f cl/kf_opencl_source.c cl/kf_opencl_source.d cl/kf_opencl_source.o cl/kf.cl cl/opencl.d cl/opencl.inc cl/opencl.o cl/preprocessor cl/preprocessor.hi cl/preprocessor.o

kf.exe: $(OBJECTS)
	$(LINK) -o kf.exe $(OBJECTS) $(LINK_FLAGS) $(LIBS)

res.o: fraktal_sft/fraktal_sft.rc
	$(WINDRES) -i fraktal_sft/fraktal_sft.rc -o res.o

kf.dll: $(LDBL_SOURCES_CPP)
	$(COMPILE) -o kf.dll $(COMPILE_FLAGS) -shared $(LDBL_SOURCES_CPP) $(LINK_FLAGS) -lgmp

%.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

%.o: %.c
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

formula/formula.cpp: formula/formula.xsl formula/formula.xml
	$(XSLTPROC) -o $@ formula/formula.xsl formula/formula.xml

cl/kf_opencl_source.c: cl/kf.cl cl/s2c.sh
	./cl/s2c.sh kf_opencl_source < cl/kf.cl > cl/kf_opencl_source.c

cl/kf.cl: cl/common.cl cl/formula.xsl formula/formula.xml cl/preprocessor
	( cat cl/common.cl ; $(XSLTPROC) cl/formula.xsl formula/formula.xml | ./cl/preprocessor ) > $@

cl/cabal.sandbox.config:
	( cd cl ; cabal sandbox init ; cabal install parsec )

cl/preprocessor: cl/preprocessor.hs cl/cabal.sandbox.config
	( cd cl ; cabal exec -- ghc preprocessor.hs )

cl/opencl.inc: cl/opencl.xsl formula/formula.xml
	$(XSLTPROC) -o $@ cl/opencl.xsl formula/formula.xml

-include $(DEPENDS)
