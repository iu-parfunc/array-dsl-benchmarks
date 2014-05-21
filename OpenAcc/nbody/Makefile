

all: 
	pgcc -acc -Minline -Minfo=accel -ta=nvidia -o nbody.exe nbody.c




run: nbody.exe  
	PGI_ACC_TIME=1 optirun ./nbody.exe

runopt: nbody.exe 
	PGI_ACC_TIME=1 optirun ./nbody.exe
