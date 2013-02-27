

EXE=./bin/naive.exe
HS=$(shell find -name "[A-Z]*.hs")

all: getinput build

build: $(EXE)
$(EXE): $(HS)
	mkdir -p bin/
	ghc -threaded -DACCBACKEND="Data.Array.Accelerate.Interpreter" --make -O2 Main.hs -o bin/naive_interp.exe
	ghc -threaded -DACCBACKEND="Data.Array.Accelerate.Cilk" --make -O2 Main.hs -o bin/naive_cilk.exe
	ghc -threaded -DACCBACKEND="Data.Array.Accelerate.CUDA" --make -O2 Main.hs -o bin/naive_cuda.exe

getinput: /tmp/uniform.3dpts

/tmp/uniform.3dpts:
	./inputGenerator/uniform -s -d 3 20000 $@

# See ../../README for the "calling conventions".
run:
	$(EXE) $(P1)


deps:
	cabal install bytestring-lexing
