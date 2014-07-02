
INFILE=./uniform.3dpts

# Generate 100K points, but we may not use all:
N=100000

all: getinput 

#------------------------------------------------------------------------------

getinput: $(INFILE)

./inputgenerator/uniform: 
	(cd inputgenerator/; make uniform)

$(INFILE): ./inputgenerator/uniform
	./inputgenerator/uniform -s -d 3 $(N) $@

