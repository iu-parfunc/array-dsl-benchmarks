CXX=clang++

CXXFLAGS = --std=c++0x -O -I../common

#LDFLAGS  = -framework OpenCL
LDFLAGS = -lOpenCL

OBJS = nbody.o ../common/cl++.o

.phony: all
all: nbody-cpu nbody-gpu

nbody-cpu: $(OBJS) nbody-cpu.cpp
	clang++ -o nbody-cpu nbody-cpu.cpp $(OBJS) $(LDFLAGS)

nbody-gpu: $(OBJS) nbody-gpu.cpp
	clang++ -o nbody-gpu nbody-gpu.cpp $(OBJS) $(LDFLAGS)

%.d: %.cpp
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$;                 \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(subst .o,.d,$(OBJS))

.phony: clean
clean:
	rm *.o *.d nbody-cpu nbody-gpu
