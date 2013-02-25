CXX=clang++

CXXFLAGS = --std=c++0x -O

#LDFLAGS  = -framework OpenCL
LDFLAGS = -lOpenCL

dotprod: dotprod.o cl++.o
	clang++ -o dotprod dotprod.o cl++.o $(LDFLAGS)
