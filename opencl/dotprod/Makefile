CXX=clang++

CXXFLAGS = --std=c++0x -O

LDFLAGS  = -framework OpenCL

dotprod: dotprod.o cl++.o
	clang++ -o dotprod dotprod.o cl++.o $(LDFLAGS)
