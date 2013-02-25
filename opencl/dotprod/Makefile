CXX=clang++

CXXFLAGS = --std=c++0x -O

#LDFLAGS  = -framework OpenCL
LDFLAGS = -lOpenCL

OBJS = dotprod.o cl++.o

dotprod: $(OBJS)
	clang++ -o dotprod $(OBJS) $(LDFLAGS)

%.d: %.cpp
	$(CXX) -M $(CPPFLAGS) $< > $@.$$$$;                 \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

include $(subst .o,.d,$(OBJS))

.phony: clean
clean:
	rm *.o *.d dotprod
