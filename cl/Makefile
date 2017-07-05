XSLTPROC ?= xsltproc

kf.cl: common.cl cl_formula.xsl ../formula/formula.xml preprocessor
	( cat common.cl ; $(XSLTPROC) cl_formula.xsl ../formula/formula.xml | ./preprocessor ) > $@

cabal.sandbox.config:
	cabal sandbox init
	cabal install parsec

preprocessor: preprocessor.hs
	cabal exec -- ghc preprocessor.hs

