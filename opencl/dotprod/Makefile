#CXX=clang++
CXX=g++

CXXFLAGS = --std=c++0x -O -I../common -I/usr/local/cuda/include

#LDFLAGS  = -framework OpenCL
LDFLAGS = -lOpenCL

OBJS = dotprod.o ../common/cl++.o

.phony: all
all: dotprod-cpu dotprod-gpu

dotprod-cpu: $(OBJS) dotprod-cpu.cpp
	$(CXX) -o dotprod-cpu dotprod-cpu.cpp $(OBJS) $(LDFLAGS) $(CXXFLAGS)

dotprod-gpu: $(OBJS) dotprod-gpu.cpp
	$(CXX) -o dotprod-gpu dotprod-gpu.cpp $(OBJS) $(LDFLAGS) $(CXXFLAGS)

%.d: %.cpp
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$;                 \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(subst .o,.d,$(OBJS))

.phony: clean
clean:
	rm *.o *.d dotprod-cpu dotprod-gpu
