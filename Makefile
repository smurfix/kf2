# Kalles Fraktaler 2
# Copyright (C) 2013-2017 Karl Runmo
# Copyright (C) 2017-2021 Claude Heiland-Allen
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

# make SYSTEM=x86_64+native for 64bit (most optimized, non-portable)
# make SYSTEM=x86_64+       for 64bit with recent SIMD extensions
# make SYSTEM=x86_64        for 64bit
# make SYSTEM=i686          for 32bit (experimental)
# make SYSTEM=aarch64       for 64bit ARM (experimental)
# make SYSTEM=armv7         for 32bit ARM (experimental)
SYSTEM ?= x86_64+native
include $(SYSTEM).mk

CLEWPREFIX := $(HOME)/win/src/clew

FLAGS := -Wall -Wextra -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-function -Wno-cast-function-type -Wno-deprecated-copy -Wno-psabi -MMD -gstabs -O3 -I$(WINPREFIXPLUS)/include -I$(WINPREFIX)/include -I$(WINPREFIX)/include/pixman-1 -I$(WINPREFIX)/include/OpenEXR -D_FILE_OFFSET_BITS=64 -D__USE_MINGW_ANSI_STDIO=1 -DWINVER=0x501 -D_WIN32_WINNT=0x501 -DKF_SIMD=$(SIMD) -I$(CLEWPREFIX)/include -Iglad/include -fno-var-tracking-assignments
LINK_FLAGS := -static-libgcc -static-libstdc++ -Wl,--stack,67108864 -Wl,-subsystem,windows -L$(WINPREFIXPLUS)/lib -L$(WINPREFIX)/lib
LIBS := -lglfw3 -lgdi32 -lcomdlg32 -lole32 -loleaut32 -lcomctl32 -lwininet -lurlmon -luuid -lmpfr -lgmp -ljpeg -ltiff -lpixman-1 $(WINPREFIX)/lib/libpng16.a -lz -lgsl -lgslcblas -lIlmImf-2_5 -lImath-2_5 -lHalf-2_5 -lIex-2_5 -lIexMath-2_5 -lIlmThread-2_5 -lz -static -Wl,-Bstatic -lstdc++ -lpthread -Wl,-Bdynamic

FRAKTAL_SOURCES_CPP = \
fraktal_sft/calculate_perturbation.cpp \
fraktal_sft/calculate_reference.cpp \
fraktal_sft/calculate_reference_threaded.cpp \
fraktal_sft/CDecNumber.cpp \
fraktal_sft/CFixedFloat.cpp \
fraktal_sft/check_for_update.cpp \
fraktal_sft/cmdline.cpp \
fraktal_sft/exr.cpp \
fraktal_sft/floatexp_approximation.cpp \
fraktal_sft/fraktal_sft.cpp \
fraktal_sft/gradient.cpp \
fraktal_sft/hybrid.cpp \
fraktal_sft/jpeg.cpp \
fraktal_sft/listbox.cpp \
fraktal_sft/main_bailout.cpp \
fraktal_sft/main_color.cpp \
fraktal_sft/main_examine.cpp \
fraktal_sft/main_formula.cpp \
fraktal_sft/main_information.cpp \
fraktal_sft/main_numbertype.cpp \
fraktal_sft/main_position.cpp \
fraktal_sft/main_ptsatuning.cpp \
fraktal_sft/main_size.cpp \
fraktal_sft/main_transformation.cpp \
fraktal_sft/nanomb1_perturbation.cpp \
fraktal_sft/nanomb1_reference.cpp \
fraktal_sft/nanomb2_perturbation.cpp \
fraktal_sft/nanomb2_reference.cpp \
fraktal_sft/newton.cpp \
fraktal_sft/opengl.cpp \
fraktal_sft/Parameter.cpp \
fraktal_sft/png.cpp \
fraktal_sft/reference.cpp \
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
fraktal_sft/dual.h \
fraktal_sft/exr.h \
fraktal_sft/fifo.h \
fraktal_sft/floatexp.h \
fraktal_sft/fraktal_sft.h \
fraktal_sft/gradient.h \
fraktal_sft/hybrid.h \
fraktal_sft/itercount_array.h \
fraktal_sft/jpeg.h \
fraktal_sft/listbox.h \
fraktal_sft/main.h \
fraktal_sft/main_bailout.h \
fraktal_sft/main_color.h \
fraktal_sft/main_examine.h \
fraktal_sft/main_formula.h \
fraktal_sft/main_information.h \
fraktal_sft/main_numbertype.h \
fraktal_sft/main_position.h \
fraktal_sft/main_ptsatuning.h \
fraktal_sft/main_size.h \
fraktal_sft/main_transformation.h \
fraktal_sft/nanomb1.h \
fraktal_sft/nanomb1.inc \
fraktal_sft/nanomb2.h \
fraktal_sft/nanomb2.inc \
fraktal_sft/newton.h \
fraktal_sft/opengl.h \
fraktal_sft/png.h \
fraktal_sft/reference.h \
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
common/matrix.cpp \
common/timer.cpp

COMMON_SOURCES_H = \
common/FolderBrowser.h \
common/getimage.h \
common/parallell.h \
common/StringVector.h \
common/bitmap.h \
common/matrix.h \
common/memory.h \
common/timer.h

FORMULA_SOURCES_XML := $(wildcard formula/formula_*_*.xml)
FORMULA_REFERENCE_SOURCES_CPP := \
	$(patsubst %.xml,%.reference.cpp,$(FORMULA_SOURCES_XML)) \
	formula/reference.cpp
FORMULA_PERTURBATIONSIMPLE_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_simple.cpp,$(FORMULA_SOURCES_XML)) \
  formula/perturbation_simple.cpp
FORMULA_PERTURBATIONSIMPLEDERIVATIVES_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_simple_derivatives.cpp,$(FORMULA_SOURCES_XML)) \
  formula/perturbation_simple_derivatives.cpp
FORMULA_PERTURBATIONSIMD_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_simd.cpp,$(FORMULA_SOURCES_XML)) \
  formula/perturbation_simd.cpp
FORMULA_PERTURBATIONSIMDDERIVATIVES_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_simd_derivatives.cpp,$(FORMULA_SOURCES_XML)) \
  formula/perturbation_simd_derivatives.cpp
FORMULA_PERTURBATIONSCALED_SOURCES_CPP := \
  formula/formula_0_2.perturbation_scaled.cpp \
  formula/formula_0_3.perturbation_scaled.cpp \
  formula/formula_1_2.perturbation_scaled.cpp \
  formula/formula_0_2.perturbation_scaled_derivatives.cpp \
  formula/formula_0_3.perturbation_scaled_derivatives.cpp \
  formula/formula_1_2.perturbation_scaled_derivatives.cpp \
  formula/perturbation_scaled.cpp \
  formula/perturbation_scaled_derivatives.cpp
FORMULA_PERTURBATIONOPENCL_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_opencl.cpp,$(FORMULA_SOURCES_XML)) \
  formula/formula_0_2.perturbation_opencl_scaled.cpp \
  formula/formula_0_3.perturbation_opencl_scaled.cpp \
  formula/formula_1_2.perturbation_opencl_scaled.cpp \
  formula/perturbation_opencl.cpp
FORMULA_MISCELLANEOUS_SOURCES_CPP := \
  formula/miscellaneous.cpp
FORMULA_PERTURBATIONCONVERGENTSIMPLE_SOURCES_CPP := \
  $(patsubst %.xml,%.perturbation_convergent_simple.cpp,$(FORMULA_SOURCES_XML)) \
  formula/perturbation_convergent_simple.cpp

FORMULA_REFERENCE_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_REFERENCE_SOURCES_CPP))
FORMULA_PERTURBATIONSIMPLE_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONSIMPLE_SOURCES_CPP))
FORMULA_PERTURBATIONSIMPLEDERIVATIVES_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_SOURCES_CPP))
FORMULA_PERTURBATIONSIMD_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONSIMD_SOURCES_CPP))
FORMULA_PERTURBATIONSIMDDERIVATIVES_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONSIMDDERIVATIVES_SOURCES_CPP))
FORMULA_PERTURBATIONSCALED_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONSCALED_SOURCES_CPP))
FORMULA_MISCELLANEOUS_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_MISCELLANEOUS_SOURCES_CPP))
FORMULA_PERTURBATIONOPENCL_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONOPENCL_SOURCES_CPP))
FORMULA_PERTURBATIONCONVERGENTSIMPLE_OBJECTS := $(patsubst %.cpp,%.o,$(FORMULA_PERTURBATIONCONVERGENTSIMPLE_SOURCES_CPP))

FORMULA_GENERATED_SOURCES_CPP := $(wildcard formula/generated/*.c)
FORMULA_GENERATED_OBJECTS := $(patsubst %.c,%.o,$(FORMULA_GENERATED_SOURCES_CPP))

FORMULA_LIBS := \
	formula/reference.a \
	formula/perturbation_simple.a \
	formula/perturbation_simple_derivatives.a \
	formula/perturbation_simd.a \
	formula/perturbation_simd_derivatives.a \
	formula/perturbation_scaled.a \
	formula/perturbation_opencl.a \
	formula/miscellaneous.a \
	formula/perturbation_convergent_simple.a \

# blank line above

UTILS_SOURCES_CPP = utils/kf-tile.cpp

OPENCL_SOURCES_CPP = cl/opencl.cpp
OPENCL_SOURCES_C = $(CLEWPREFIX)/src/clew.c
OPENCL_SOURCES_H = cl/opencl.h $(CLEWPREFIX)/include/clew.h

SOURCES_CPP = $(FRAKTAL_SOURCES_CPP) $(COMMON_SOURCES_CPP)
SOURCES_C = glad/src/glad.c
SOURCES_H = $(FRAKTAL_SOURCES_H) $(COMMON_SOURCES_H)

FLAGS_WINDRES = -DKF_SIMD=$(SIMD)

ifeq ($(OPENCL), 1)
FLAGS += -DKF_OPENCL
SOURCES_CPP += $(OPENCL_SOURCES_CPP)
SOURCES_C += $(OPENCL_SOURCES_C)
SOURCES_H += $(OPENCL_SOURCES_H)
FLAGS_WINDRES += -DKF_OPENCL=1
endif

SOURCES_H += gl/kf_vert_glsl.h gl/kf_frag_glsl.h

COMPILE_FLAGS := -xc++ -std=c++11 $(FLAGS) -Dclew_STATIC

SOURCES = $(SOURCES_CPP) $(SOURCES_C) $(SOURCES_H)

OBJECTS_CPP := $(patsubst %.cpp,%.o,$(SOURCES_CPP))
OBJECTS_C := $(patsubst %.c,%.o,$(SOURCES_C))
OBJECTS := $(OBJECTS_CPP) $(OBJECTS_C)

UTILS_OBJECTS := $(patsubst %.cpp,%.o,$(UTILS_SOURCES_CPP))

DEPENDS := $(patsubst %.o,%.d,$(OBJECTS))
UTILS_DEPENDS := $(patsubst %.o,%.d,$(UTILS_OBJECTS))
FORMULA_GENERATED_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_GENERATED_OBJECTS))
FORMULA_MISCELLANEOUS_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_MISCELLANEOUS_OBJECTS))
FORMULA_REFERENCE_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_REFERENCE_OBJECTS))
FORMULA_PERTURBATIONSCALED_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONSCALED_OBJECTS))
FORMULA_PERTURBATIONSIMPLE_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONSIMPLE_OBJECTS))
FORMULA_PERTURBATIONSIMPLEDERIVATIVES_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_OBJECTS))
FORMULA_PERTURBATIONSIMD_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONSIMD_OBJECTS))
FORMULA_PERTURBATIONSIMDDERIVATIVES_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONSIMDDERIVATIVES_OBJECTS))
FORMULA_PERTURBATIONOPENCL_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONOPENCL_OBJECTS))
FORMULA_PERTURBATIONCONVERGENTSIMPLE_DEPENDS := $(patsubst %.o,%.d,$(FORMULA_PERTURBATIONCONVERGENTSIMPLE_OBJECTS))

all: kf.exe kf-tile.exe

clean:
	rm -f $(OBJECTS) fraktal_sft/main.o res.o
	rm -f $(DEPENDS) fraktal_sft/main.d
	rm -f $(UTILS_OBJECTS)
	rm -f $(UTILS_DEPENDS)
	rm -f $(FORMULA_GENERATED_OBJECTS)
	rm -f $(FORMULA_GENERATED_DEPENDS)
	rm -f $(FORMULA_MISCELLANEOUS_SOURCES_CPP)
	rm -f $(FORMULA_MISCELLANEOUS_OBJECTS)
	rm -f $(FORMULA_MISCELLANEOUS_DEPENDS)
	rm -f $(FORMULA_REFERENCE_SOURCES_CPP)
	rm -f $(FORMULA_REFERENCE_OBJECTS)
	rm -f $(FORMULA_REFERENCE_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONSCALED_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONSCALED_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONSCALED_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONSIMPLE_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONSIMPLE_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONSIMPLE_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONSIMD_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONSIMD_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONSIMD_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONSIMDDERIVATIVES_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONSIMDDERIVATIVES_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONSIMDDERIVATIVES_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONOPENCL_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONOPENCL_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONOPENCL_DEPENDS)
	rm -f $(FORMULA_PERTURBATIONCONVERGENTSIMPLE_SOURCES_CPP)
	rm -f $(FORMULA_PERTURBATIONCONVERGENTSIMPLE_OBJECTS)
	rm -f $(FORMULA_PERTURBATIONCONVERGENTSIMPLE_DEPENDS)
	rm -f $(FORMULA_LIBS) formula/generated.a kf.a
	rm -f cl/common_cl.c cl/double_pre_cl.c cl/double_pre_c_cl.c cl/double_pre_m_cl.c cl/double_pre_r_cl.c cl/double_post_cl.c cl/double_post_c_cl.c cl/double_post_m_cl.c cl/double_post_r_cl.c cl/floatexp_pre_cl.c cl/floatexp_pre_c_cl.c cl/floatexp_pre_m_cl.c cl/floatexp_pre_r_cl.c cl/floatexp_post_cl.c cl/floatexp_post_c_cl.c cl/floatexp_post_m_cl.c cl/floatexp_post_r_cl.c
	rm -f gl/kf_frag_glsl.h gl/kf_vert_glsl.h
	rm -f preprocessor preprocessor.hi preprocessor.o
	rm -f res.res

kf.a: $(OBJECTS)
	$(AR) rs $@ $(OBJECTS)

kf.exe: kf.a res.o fraktal_sft/main.o $(FORMULA_LIBS) formula/generated.a
	$(LINK) -o kf.exe fraktal_sft/main.o $(FORMULA_LIBS) -Wl,--whole-archive formula/generated.a -Wl,--no-whole-archive kf.a res.o $(FORMULA_LIBS) $(LINK_FLAGS) $(LIBS)

kf-tile.exe: utils/kf-tile.o common/matrix.o
	$(LINK) -o kf-tile.exe utils/kf-tile.o common/matrix.o -static-libgcc -static-libstdc++ -L$(WINPREFIX)/lib -lmpfr -lgmp

res.o: res.res
	$(WINDRES) -J res -i res.res -o res.o

res.res: fraktal_sft/fraktal_sft.rc fraktal_sft/resource.h
	 $(WINDRES2) -i fraktal_sft/fraktal_sft.rc -o res.res $(FLAGS_WINDRES)

%.o: %.cpp
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

%.o: %.c
	$(COMPILE) $(COMPILE_FLAGS) -o $@ -c $<

# brute force rendering

%.brute: %.brute.cpp
	g++ -std=c++17 -Wall -Wextra -pedantic -O3 -march=native -fopenmp -o $@ $< -lmpfr -I/usr/include/OpenEXR -lIlmImf-2_5 -lImath-2_5 -lHalf-2_5 -lIex-2_5 -lIexMath-2_5 -lIlmThread-2_5

%.brute.cpp: %.xml formula/brute1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/brute1.cpp.xsl $< | ./preprocessor ) > $@

# formula implementations

%.reference.cpp: %.xml formula/reference1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/reference1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_simple.cpp: %.xml formula/perturbation_simple1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simple1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_simple_derivatives.cpp: %.xml formula/perturbation_simple_derivatives1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simple_derivatives1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_simd.cpp: %.xml formula/perturbation_simd1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simd1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_simd_derivatives.cpp: %.xml formula/perturbation_simd_derivatives1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simd_derivatives1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_scaled.cpp: %.xml formula/perturbation_scaled1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_scaled1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_scaled_derivatives.cpp: %.xml formula/perturbation_scaled_derivatives1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_scaled_derivatives1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_opencl.cpp: %.xml formula/perturbation_opencl1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_opencl1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_opencl_scaled.cpp: %.xml formula/perturbation_opencl_scaled1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_opencl_scaled1.cpp.xsl $< | ./preprocessor ) > $@

%.perturbation_convergent_simple.cpp: %.xml formula/perturbation_convergent_simple1.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_convergent_simple1.cpp.xsl $< | ./preprocessor ) > $@

# formula dispatchers

formula/reference.cpp: formula/formula.xml formula/reference.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/reference.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_simple.cpp: formula/formula.xml formula/perturbation_simple.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simple.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_simple_derivatives.cpp: formula/formula.xml formula/perturbation_simple_derivatives.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simple_derivatives.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_simd.cpp: formula/formula.xml formula/perturbation_simd.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simd.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_simd_derivatives.cpp: formula/formula.xml formula/perturbation_simd_derivatives.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_simd_derivatives.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_scaled.cpp: formula/formula.xml formula/perturbation_scaled.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_scaled.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_scaled_derivatives.cpp: formula/formula.xml formula/perturbation_scaled_derivatives.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_scaled_derivatives.cpp.xsl $< | ./preprocessor ) > $@

formula/perturbation_opencl.cpp: formula/formula.xml formula/perturbation_opencl.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_opencl.cpp.xsl $< | ./preprocessor ) > $@

formula/miscellaneous.cpp: formula/formula.xml formula/miscellaneous.cpp.xsl
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/miscellaneous.cpp.xsl $< ) > $@

formula/perturbation_convergent_simple.cpp: formula/formula.xml formula/perturbation_convergent_simple.cpp.xsl preprocessor
	( echo "/* this file is autogenerated, do not edit directly */" ; $(XSLTPROC) formula/perturbation_convergent_simple.cpp.xsl $< | ./preprocessor ) > $@

# static libs

formula/reference.a: $(FORMULA_REFERENCE_OBJECTS)
	$(AR) rs $@ $(FORMULA_REFERENCE_OBJECTS)

formula/perturbation_simple.a: $(FORMULA_PERTURBATIONSIMPLE_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONSIMPLE_OBJECTS)

formula/perturbation_simple_derivatives.a: $(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_OBJECTS)

formula/perturbation_simd.a: $(FORMULA_PERTURBATIONSIMD_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONSIMD_OBJECTS)

formula/perturbation_simd_derivatives.a: $(FORMULA_PERTURBATIONSIMDDERIVATIVES_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONSIMDDERIVATIVES_OBJECTS)

formula/perturbation_scaled.a: $(FORMULA_PERTURBATIONSCALED_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONSCALED_OBJECTS)

formula/perturbation_opencl.a: $(FORMULA_PERTURBATIONOPENCL_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONOPENCL_OBJECTS)

formula/miscellaneous.a: $(FORMULA_MISCELLANEOUS_OBJECTS)
	$(AR) rs $@ $(FORMULA_MISCELLANEOUS_OBJECTS)

formula/generated.a: $(FORMULA_GENERATED_OBJECTS)
	$(AR) rs $@ $(FORMULA_GENERATED_OBJECTS)

formula/perturbation_convergent_simple.a: $(FORMULA_PERTURBATIONCONVERGENTSIMPLE_OBJECTS)
	$(AR) rs $@ $(FORMULA_PERTURBATIONCONVERGENTSIMPLE_OBJECTS)


# opencl

cl/%_cl.c: cl/%.cl cl/s2c.sh
	./cl/s2c.sh perturbation_opencl_$* < $< > $@

preprocessor: preprocessor.hs
	ghc -package parsec -package mtl -O preprocessor.hs

cl/opencl.o: cl/opencl.cpp cl/opencl.h

formula/perturbation_opencl.o: formula/perturbation_opencl.cpp cl/common_cl.c cl/double_pre_cl.c cl/double_pre_c_cl.c cl/double_pre_m_cl.c cl/double_pre_r_cl.c cl/double_post_cl.c cl/double_post_c_cl.c cl/double_post_m_cl.c cl/double_post_r_cl.c cl/floatexp_pre_cl.c cl/floatexp_pre_c_cl.c cl/floatexp_pre_m_cl.c cl/floatexp_pre_r_cl.c cl/floatexp_post_cl.c cl/floatexp_post_c_cl.c cl/floatexp_post_m_cl.c cl/floatexp_post_r_cl.c

gl/%_vert_glsl.h: gl/%.vert.glsl gl/s2c.sh
	./gl/s2c.sh $*_vert_glsl < $< > $@

gl/%_frag_glsl.h: gl/%.frag.glsl gl/s2c.sh
	./gl/s2c.sh $*_frag_glsl < $< > $@

fraktal_sft/opengl.o: fraktal_sft/opengl.cpp fraktal_sft/opengl.h gl/kf_vert_glsl.h gl/kf_frag_glsl.h

%.pdf: %.md
	pandoc -f markdown -t latex -V "toc=true" -V "papersize=a4" -V "geometry=margin=1.2in" < $< -o $@

manual.html: README.md
	pandoc README.md -F pandoc-self-links.pl -s --toc --css kf.css -o manual.html

FORMULA_BRUTE_SOURCES_CPP = formula/formula_75_3.brute.cpp formula/formula_76_2.brute.cpp

.PRECIOUS: \
	$(FORMULA_BRUTE_SOURCES_CPP) \
	$(FORMULA_MISCELLANEOUS_SOURCES_CPP) \
	$(FORMULA_REFERENCE_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONSCALED_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONSIMPLE_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONSIMD_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONSIMDDERIVATIVES_SOURCES_CPP) \
	$(FORMULA_PERTURBATIONOPENCL_SOURCES_CPP) \

# blank line above

-include \
	$(DEPENDS) \
	$(FORMULA_GENERATED_DEPENDS) \
	$(FORMULA_MISCELLANEOUS_DEPENDS) \
	$(FORMULA_REFERENCE_DEPENDS) \
	$(FORMULA_PERTURBATIONSCALED_DEPENDS) \
	$(FORMULA_PERTURBATIONSIMPLE_DEPENDS) \
	$(FORMULA_PERTURBATIONSIMPLEDERIVATIVES_DEPENDS) \
	$(FORMULA_PERTURBATIONSIMD_DEPENDS) \
	$(FORMULA_PERTURBATIONSIMDDERIVATIVES_DEPENDS) \
	$(FORMULA_PERTURBATIONCONVERGENTSIMPLE_DEPENDS) \

# blank line above
