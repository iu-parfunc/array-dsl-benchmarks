
RUN_ARGS?=25000

NUM_BODIES=$(RUN_ARGS)

.phony: all
all:

nbody.exe: nbody.c
	icc -O2 -openmp -o nbody.exe nbody.c -std=c99

run: nbody.exe
	./nbody.exe $(NUM_BODIES)


clean:
	rm nbody.exe
