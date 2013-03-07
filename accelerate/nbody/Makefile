
HS=$(shell find -name "[A-Z]*.hs")

ifeq ($(GHC),)
  GHC=ghc-7.6.1
endif

ifeq ($(GHC_OPTS),)
  GHC_OPTS= -threaded -O2
endif

INFILE=/tmp/uniform.3dpts
OUTFILE=/tmp/nbody_out.3dpts

N=100000

all: getinput build

build: interp c cilk cuda

interp: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Interpreter" Main.hs -o bin/naive_'$@'.exe

c: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.C" Main.hs -o bin/naive_'$@'.exe

cilk: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Cilk" Main.hs -o bin/naive_'$@'.exe

debug-cilk:
	GHC_OPTS="-DDEBUG $(GHC_OPTS)" ${MAKE} cilk

cuda: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.CUDA" Main.hs -o bin/naive_'$@'.exe

dir:
	mkdir -p bin/

getinput: $(INFILE)

$(INFILE):
	./inputGenerator/uniform -s -d 3 $(N) $@

check: $(INFILE)
	head -n $(shell wc -l $(OUTFILE) | awk '{ print $$1 }') $(INFILE) > /tmp/short_uniform.3dpts
	../../pbbs/nBody/common/nbodyCheck /tmp/short_uniform.3dpts $(OUTFILE)


# See ../../README for the "calling conventions".
run:
	$(EXE) $(P1)


deps:
	cabal install bytestring-lexing


clean:
	rm -f bin/*
