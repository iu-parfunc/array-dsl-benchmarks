
RUN_ARGS?=25000

NUM_BODIES=$(RUN_ARGS)

all: nbody.exe nbody-omp.exe 

nbody.exe: nbody.c
	pgcc -O2 -acc -Minline -Minfo -ta=nvidia,kepler -o nbody.exe nbody.c

nbody-omp.exe: nbody.c
	icc -O2 -openmp -o nbody-omp.exe nbody.c -std=c99

keepCUDA: 
	pgcc -acc -Minline -Minfo -ta=nvidia,kepler,keepptx,keepgpu -o nbody.exe nbody.c

run: nbody$(PROG_SUFFIX).exe
	PGI_ACC_TIME=1 ./nbody$(PROG_SUFFIX).exe $(NUM_BODIES)

#runopt: nbody.exe nbody-omp.exe
#	PGI_ACC_TIME=1 ./nbody.exe $(NUM_BODIES)
#	PGI_ACC_TIME=1 ./nbody-omp.exe $(NUM_BODIES)

clean:
