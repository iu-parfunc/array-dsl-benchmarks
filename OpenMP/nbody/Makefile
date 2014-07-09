
-- RUN_ARGS?=25000

THREADS=$(RUN_ARGS[1])
NUM_BODIES=$(RUN_ARGS[2])

.phony: all
all:

nbody.exe: nbody.c
	icc -O2 -openmp -o nbody.exe nbody.c -std=c99

run: nbody.exe
	OMP_NUM_THREADS=$(THREADS) ./nbody.exe $(NUM_BODIES)

clean: 
