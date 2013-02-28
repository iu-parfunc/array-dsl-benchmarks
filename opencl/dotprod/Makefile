CXX=clang++

CXXFLAGS = --std=c++0x -O -I../common

#LDFLAGS  = -framework OpenCL
LDFLAGS = -lOpenCL

OBJS = dotprod.o ../common/cl++.o

.phony: all
all: dotprod-cpu dotprod-gpu

dotprod-cpu: $(OBJS) dotprod-cpu.cpp
	clang++ -o dotprod-cpu dotprod-cpu.cpp $(OBJS) $(LDFLAGS)

dotprod-gpu: $(OBJS) dotprod-gpu.cpp
	clang++ -o dotprod-gpu dotprod-gpu.cpp $(OBJS) $(LDFLAGS)

%.d: %.cpp
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$;                 \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(subst .o,.d,$(OBJS))

.phony: clean
clean:
	rm *.o *.d dotprod-cpu dotprod-gpu
