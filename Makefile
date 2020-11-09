# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2020 Claude Heiland-Allen
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

# make SYSTEM=native for 64bit (most optimized, non-portable)
# make SYSTEM=64+    for 64bit with recent SIMD extensions
# make SYSTEM=64     for 64bit
# make SYSTEM=32     for 32bit (experimental)
SYSTEM ?= native
include $(SYSTEM).mk

CLEWPREFIX := $(WINPREFIX)/src/clew

FLAGS := -Wall -Wextra -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-function -Wno-cast-function-type -Wno-deprecated-copy -Wno-psabi -MMD -g -ggdb -gdwarf -O3 -fno-var-tracking-assignments -I$(WINPREFIXPLUS)/include -I$(WINPREFIX)/include -I$(WINPREFIX)/include/pixman-1 -I$(WINPREFIX)/include/OpenEXR -D_FILE_OFFSET_BITS=64 -D__USE_MINGW_ANSI_STDIO=1 -DWINVER=0x501 -D_WIN32_WINNT=0x501 -DKF_SIMD=$(SIMD) -I$(CLEWPREFIX)/include
LINK_FLAGS := -static-libgcc -static-libstdc++ -Wl,--stack,67108864 -Wl,-subsystem,windows -L$(WINPREFIXPLUS)/lib -L$(WINPREFIX)/lib
LIBS := -lgdi32 -lcomdlg32 -lole32 -loleaut32 -lcomctl32 -lwininet -lurlmon -luuid -lmpfr -lgmp -ljpeg -ltiff -lpixman-1 $(WINPREFIX)/lib/libpng16.a -lz -lgsl -lgslcblas -lIlmImf-2_5 -lImath-2_5 -lHalf-2_5 -lIex-2_5 -lIexMath-2_5 -lIlmThread-2_5 -lz -static -Wl,-Bstatic -lstdc++ -lpthread -Wl,-Bdynamic

FRAKTAL_SOURCES_CPP = \
fraktal_sft/CDecNumber.cpp \
fraktal_sft/CFixedFloat.cpp \
fraktal_sft/check_for_update.cpp \
fraktal_sft/cmdline.cpp \
fraktal_sft/double_perturbation.cpp \
fraktal_sft/double_reference.cpp \
fraktal_sft/exr.cpp \
fraktal_sft/floatexp_approximation.cpp \
fraktal_sft/floatexp_perturbation.cpp \
fraktal_sft/floatexp_reference.cpp \
fraktal_sft/fraktal_sft.cpp \
fraktal_sft/gradient.cpp \
fraktal_sft/jpeg.cpp \
fraktal_sft/listbox.cpp \
fraktal_sft/long_double_perturbation.cpp \
fraktal_sft/long_double_reference.cpp \
fraktal_sft/main.cpp \
fraktal_sft/main_bailout.cpp \
fraktal_sft/main_color.cpp \
fraktal_sft/main_examine.cpp \
fraktal_sft/main_formula.cpp \
fraktal_sft/main_information.cpp \
fraktal_sft/main_position.cpp \
fraktal_sft/main_ptsatuning.cpp \
fraktal_sft/nanomb1_perturbation.cpp \
fraktal_sft/nanomb1_reference.cpp \
fraktal_sft/nanomb2_perturbation.cpp \
fraktal_sft/nanomb2_reference.cpp \
fraktal_sft/newton.cpp \
fraktal_sft/Parameter.cpp \
fraktal_sft/png.cpp \
fraktal_sft/render.cpp \
fraktal_sft/scale_bitmap.cpp \
fraktal_sft/Settings.cpp \
fraktal_sft/tiff.cpp \
fraktal_sft/tooltip.cpp

FRAKTAL_SOURCES_H = \
fraktal_sft/CDecNumber.h \
fraktal_sft/CFixedFloat.h \
fraktal_sft/check_for_update.h \
fraktal_sft/cmdline.h \
fraktal_sft/colour.h \
fraktal_sft/complex.h \
fraktal_sft/exr.h \
fraktal_sft/floatexp.h \
fraktal_sft/fraktal_sft.h \
fraktal_sft/gradient.h \
fraktal_sft/itercount_array.h \
fraktal_sft/jpeg.h \
fraktal_sft/listbox.h \
fraktal_sft/main.h \
fraktal_sft/main_bailout.h \
fraktal_sft/main_color.h \
fraktal_sft/main_examine.h \
fraktal_sft/main_formula.h \
fraktal_sft/main_information.h \
fraktal_sft/main_position.h \
fraktal_sft/main_ptsatuning.h \
fraktal_sft/nanomb1.h \
fraktal_sft/nanomb1.inc \
fraktal_sft/nanomb2.h \
fraktal_sft/nanomb2.inc \
fraktal_sft/newton.h \
fraktal_sft/png.h \
fraktal_sft/resource.h \
fraktal_sft/scale_bitmap.h \
fraktal_sft/Settings.h \
fraktal_sft/tiff.h \
fraktal_sft/tooltip.h

COMMON_SOURCES_CPP = \
common/FolderBrowser.cpp \
common/getimage.cpp \
common/parallell.cpp \
common/StringVector.cpp \
common/bitmap.cpp \
common/matrix.cpp

COMMON_SOURCES_H = \
common/FolderBrowser.h \
common/getimage.h \
common/parallell.h \
common/StringVector.h \
common/bitmap.h \
common/matrix.h

FORMULA_SOURCES_CPP = formula/formula.cpp

UTILS_SOURCES_CPP = utils/kf-tile.cpp

OPENCL_SOURCES_CPP = cl/opencl.cpp cl/formula.cpp
OPENCL_SOURCES_C = $(CLEWPREFIX)/src/clew.c
OPENCL_SOURCES_H = cl/opencl.h $(CLEWPREFIX)/include/clew.h

SOURCES_CPP = $(FRAKTAL_SOURCES_CPP) $(COMMON_SOURCES_CPP)
SOURCES_C = $(wildcard formula/generated/*.c)
SOURCES_H = $(FRAKTAL_SOURCES_H) $(COMMON_SOURCES_H)

FLAGS_WINDRES = -DKF_SIMD=$(SIMD)

ifeq ($(OPENCL), 1)
FLAGS += -DKF_OPENCL
SOURCES_CPP += $(OPENCL_SOURCES_CPP)
SOURCES_C += $(OPENCL_SOURCES_C)
SOURCES_H += $(OPENCL_SOURCES_H)
FLAGS_WINDRES += -DKF_OPENCL=1
endif

COMPILE_FLAGS := -xc++ -std=c++11 $(FLAGS)

SOURCES = $(SOURCES_CPP) $(SOURCES_C) $(SOURCES_H)

OBJECTS_CPP := $(patsubst %.cpp,%.o,$(SOURCES_CPP)) formula/formula.1.o formula/formula.3.o formula/formula.4.o formula/formula.5.o formula/formula.6.o formula/formula.7.o formula/formula.8.o formula/formula.9.o formula/formula.a.o
OBJECTS_C := $(patsubst %.c,%.o,$(SOURCES_C))
OBJECTS := $(OBJECTS_CPP) $(OBJECTS_C) res.o

DEPENDS := $(patsubst %.o,%.d,$(OBJECTS))

all: kf.exe kf-tile.exe

clean:
	rm -f $(OBJECTS) $(DEPENDS) $(FORMULA_SOURCES_CPP) $(patsubst %.cpp,%.o,$(UTILS_SOURCES_CPP))
	rm -f cl/formula.cpp cl/common_cl.c cl/double_pre_cl.c cl/double_post_cl.c cl/double_post_rc_cl.c cl/double_post_m_cl.c cl/floatexp_pre_cl.c cl/floatexp_post_cl.c cl/floatexp_post_rc_cl.c cl/floatexp_post_m_cl.c
	rm -f preprocessor preprocessor.hi preprocessor.o

kf.exe: $(OBJECTS)
	$(LINK) -o kf.exe $(OBJECTS) $(LINK_FLAGS) $(LIBS)

kf-tile.exe: utils/kf-tile.o
	$(LINK) -o kf-tile.exe utils/kf-tile.o -static-libgcc -static-libstdc++ -L$(WINPREFIX)/lib -lmpfr -lgmp

res.o: fraktal_sft/fraktal_sft.rc fraktal_sft/resource.h
	$(WINDRES) -i fraktal_sft/fraktal_sft.rc -o res.o $(FLAGS_WINDRES)

%.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

%.o: %.c
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

%.1.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS1 -o $@ -c $<

%.3.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS3 -o $@ -c $<

%.4.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS4 -o $@ -c $<

%.5.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS5 -o $@ -c $<

%.6.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS6 -o $@ -c $<

%.7.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS7 -o $@ -c $<

%.8.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS8 -o $@ -c $<

%.9.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASS9 -o $@ -c $<

%.a.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -DPASSA -o $@ -c $<

formula/formula.cpp: formula/common.cpp formula/formula.xsl formula/formula.xml preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; cat formula/common.cpp ; $(XSLTPROC) formula/formula.xsl formula/formula.xml | ./preprocessor ) > $@

cl/%_cl.c: cl/%.cl cl/s2c.sh
	./cl/s2c.sh perturbation_opencl_$* < $< > $@

cl/formula.cpp: cl/formula.xsl formula/formula.xml preprocessor
	( $(XSLTPROC) cl/formula.xsl formula/formula.xml | ./preprocessor ) > $@

preprocessor: preprocessor.hs
	ghc -package parsec -package mtl -O preprocessor.hs

cl/opencl.o: cl/opencl.cpp cl/opencl.h

cl/formula.o: cl/formula.cpp cl/common_cl.c cl/softfloat_cl.c cl/double_pre_cl.c cl/double_post_cl.c cl/double_post_rc_cl.c cl/double_post_m_cl.c cl/floatexp_pre_cl.c cl/floatexp_post_cl.c cl/floatexp_post_rc_cl.c cl/floatexp_post_m_cl.c cl/softfloat_pre_cl.c cl/softfloat_post_cl.c cl/softfloat_post_rc_cl.c cl/softfloat_post_m_cl.c

%.pdf: %.md
	pandoc -f markdown -t latex -V "papersize=a4" -V "geometry=margin=1in" < $< -o $@

-include $(DEPENDS)
