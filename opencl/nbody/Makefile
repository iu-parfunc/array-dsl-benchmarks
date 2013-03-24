CXX=clang++

CXXFLAGS = --std=c++0x -O -I../common -I/usr/local/cuda/include -DOPTIMIZED

ifeq ($(shell uname), Darwin)
	LDFLAGS  = -framework OpenCL
else
	LDFLAGS = -lOpenCL
endif

OBJS = nbody.o ../common/cl++.o

.phony: all
all: nbody-cpu nbody-gpu

nbody-cpu: $(OBJS) nbody-cpu.cpp
	$(CXX) -o nbody-cpu nbody-cpu.cpp $(OBJS) $(LDFLAGS) $(CXXFLAGS)

nbody-gpu: $(OBJS) nbody-gpu.cpp
	$(CXX) -o nbody-gpu nbody-gpu.cpp $(OBJS) $(LDFLAGS) $(CXXFLAGS)

%.d: %.cpp
	$(CXX) -M $(CPPFLAGS) $(CXXFLAGS) $< > $@.$$$$;                 \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(subst .o,.d,$(OBJS))

.phony: clean
clean:
	rm *.o *.d nbody-cpu nbody-gpu
