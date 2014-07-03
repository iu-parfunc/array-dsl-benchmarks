

all: 
	pgcc -acc -Minline -Minfo=accel -ta=nvidia -o blackscholes.exe blackscholes.c

keepCUDA: 
	pgcc -acc -Minline -Minfo=accel -ta=nvidia,keepptx,keepgpu -o blackscholes.exe blackscholes.c

run: blackscholes.exe  
	PGI_ACC_TIME=1 optirun ./blackscholes.exe

runopt: nbody.exe 
	PGI_ACC_TIME=1 optirun ./blackscholes.exe

clean:
