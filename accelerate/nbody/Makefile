
HS=$(shell find -name "[A-Z]*.hs")

ifeq ($(GHC),)
  GHC=ghc-7.6.1
endif

GHC_OPTS= -threaded -O2

all: getinput build

build: interp c cilk cuda

interp: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Interpreter" Main.hs -o bin/naive_'$@'.exe

c: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.C" Main.hs -o bin/naive_'$@'.exe

cilk: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Cilk" Main.hs -o bin/naive_'$@'.exe

cuda: $(HS) dir
	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.CUDA" Main.hs -o bin/naive_'$@'.exe

dir:
	mkdir -p bin/

getinput: /tmp/uniform.3dpts

/tmp/uniform.3dpts:
	./inputGenerator/uniform -s -d 3 20000 $@

# See ../../README for the "calling conventions".
run:
	$(EXE) $(P1)


deps:
	cabal install bytestring-lexing


clean:
	rm -f bin/*
