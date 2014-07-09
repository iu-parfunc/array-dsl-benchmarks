
-- RUN_ARGS?=25000

THREADS=$(word 1, $(RUN_ARGS))
NUM_BODIES=$(word 2, $(RUN_ARGS))

.phony: all
all:

nbody.exe: nbody.c
	icc -O2 -openmp -o nbody.exe nbody.c -std=c99

run: nbody.exe
	OMP_NUM_THREADS=$(THREADS) ./nbody.exe $(NUM_BODIES)

clean: 
