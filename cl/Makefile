XSLTPROC ?= xsltproc

all: kf kf.cl

kf: cl_interface.cpp interface.inc
	g++ -std=c++11 -Wall -Wextra -pedantic -O3 -o kf cl_interface.cpp -lOpenCL

kf.cl: common.cl cl_formula.xsl ../formula/formula.xml preprocessor
	( cat common.cl ; $(XSLTPROC) cl_formula.xsl ../formula/formula.xml | ./preprocessor ) > $@

cabal.sandbox.config:
	cabal sandbox init
	cabal install parsec

preprocessor: preprocessor.hs
	cabal exec -- ghc preprocessor.hs

interface.inc: interface.xsl ../formula/formula.xml
	$(XSLTPROC) -o $@ interface.xsl ../formula/formula.xml
