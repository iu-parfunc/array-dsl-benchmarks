# NUM_BODIES?=25000

all: nbody.exe nbody-omp.exe


nbody.exe: nbody.c
	pgcc -acc -Minline -Minfo -ta=nvidia,kepler -o nbody.exe nbody.c

nbody-omp.exe: nbody.c
	pgcc -mp -Minline -Minfo -o nbody-omp.exe nbody.c

keepCUDA: 
	pgcc -acc -Minline -Minfo -ta=nvidia,kepler,keepptx,keepgpu -o nbody.exe nbody.c

run: nbody.exe nbody-omp.exe
	PGI_ACC_TIME=1 ./nbody.exe # $(NUM_BODIES)
	PGI_ACC_TIME=1 ./nbody-omp.exe # $(NUM_BODIES)

runopt: nbody.exe nbody-omp.exe
	PGI_ACC_TIME=1 ./nbody.exe # $(NUM_BODIES)
	PGI_ACC_TIME=1 ./nbody-omp.exe # $(NUM_BODIES)

clean:
