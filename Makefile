
# This is just a shortcut for those in the habit of typing "make":

# Build the run_benchmark executable in this directory:
run_array_dsl_benchmarks.exe: run_benchmark.hs run_benchmark.cabal
	cabal install --bindir=`pwd`  --program-suffix=exe -j

clean:
	rm -f run_array_dsl_benchmarks.exe
