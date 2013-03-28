
HS=$(shell find -name "[A-Z]*.hs")

ifeq ($(GHC),)
  GHC=ghc
endif

ifeq ($(GHC_OPTS),)
  GHC_OPTS= -threaded -O2
endif

all: build

#--------------------------------------------------------------------------------

build: interp c cilk spmd

interp: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Interpreter" smvm.hs -o bin/smvm_'$@'.exe

c: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.C" smvm.hs -o bin/smvm_'$@'.exe

cilk: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Cilk" smvm.hs -o bin/smvm_'$@'.exe

cuda: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.CUDA" smvm.hs -o bin/smvm_'$@'.exe

fission: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.BackendKit.FissionTest" smvm.hs -o bin/smvm_'$@'.exe

spmd: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="SPMD" smvm.hs -o bin/smvm_'$@'.exe

#--------------------------------------------------------------------------------

debug-cilk:
	GHC_OPTS="-DDEBUG $(GHC_OPTS)" ${MAKE} cilk

debug-c:
	GHC_OPTS="-DDEBUG $(GHC_OPTS)" ${MAKE} c

#--------------------------------------------------------------------------------

dir:
	mkdir -p bin/


check: $(INFILE)
	head -n $(shell wc -l $(OUTFILE) | awk '{ print $$1 }') $(INFILE) > /tmp/short_uniform.3dpts
	../../pbbs/nBody/common/nbodyCheck /tmp/short_uniform.3dpts $(OUTFILE)


# See ../../README for the "calling conventions".
run:
	$(EXE) $(P1)


deps:
	cabal install mwc-random vector-algorithms


clean:
	rm -f bin/*
