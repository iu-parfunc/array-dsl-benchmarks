
HS=$(shell find -name "[A-Z]*.hs")

ifeq ($(GHC),)
  GHC=ghc
endif

ifeq ($(GHC_OPTS),)
  GHC_OPTS= -threaded -O2
endif

INFILE=./uniform.3dpts
OUTFILE=./nbody_out.3dpts
PBBS=../../../pbbs

# Generate 100K points, but we may not use all:
N=100000

# all: getinput build
all: getinput


#--------------------------------------------------------------------------------

# build: interp c cilk spmd cuda
#
# interp: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Interpreter" Main.hs -o bin/naive_'$@'.exe
#
# c: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.C" Main.hs -o bin/naive_'$@'.exe
#
# cilk: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.Cilk" Main.hs -o bin/naive_'$@'.exe
#
# cuda: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.CUDA" Main.hs -o bin/naive_'$@'.exe
#
# fission: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="Data.Array.Accelerate.BackendKit.FissionTest" Main.hs -o bin/naive_'$@'.exe
#
# spmd: $(HS) dir
# 	$(GHC) $(GHC_OPTS) -DACCBACKEND="SPMD" Main.hs -o bin/naive_'$@'.exe

#--------------------------------------------------------------------------------

# debug-cilk:
# 	GHC_OPTS="-DDEBUG $(GHC_OPTS)" ${MAKE} cilk
#
# debug-c:
# 	GHC_OPTS="-DDEBUG $(GHC_OPTS)" ${MAKE} c

#--------------------------------------------------------------------------------

dir:
	mkdir -p bin/

getinput: $(INFILE)

./inputGenerator/uniform:
	$(MAKE) -C inputGenerator uniform

$(INFILE): ./inputGenerator/uniform
	./inputGenerator/uniform -s -d 3 $(N) $@

$(PBBS)/nBody/common/nbodyCheck:
	$(MAKE) -C $(PBBS)/nBody/common

# Invoke the correctness checker for the output file.
check: $(INFILE)
	head -n $(shell wc -l $(OUTFILE) | awk '{ print $$1 }') $(INFILE) > /tmp/short_uniform.3dpts
	$(PBBS)/nBody/common/nbodyCheck /tmp/short_uniform.3dpts $(OUTFILE)


# Standard benchmark size:
P1=10000

# See ../../../README for the "calling conventions".
run1: c
	./bin/naive_c.exe $(P1)
run2: cilk
	./bin/naive_cilk.exe $(P1)
run3: cuda
	./bin/naive_cuda.exe $(P1)
run4: spmd
	./bin/naive_spmd.exe $(P1)

run_all: run1 run2 run3 run4

deps:
	cabal install bytestring-lexing

clean:
	rm -f bin/*

