array-dsl-benchmarks
====================

A benchmark suite across multiple array DSLs, covering CPUs and GPUss

General Benchmark Conventions
=============================

Each benchmark exists in a directory `<system>/<benchmark>` and should
build with `make` and run with default parameters via `make run`.

In general benchmarks should print out a line of the form:

    SELFTIMED 3.45
    
Where `3.45` is a time in seconds.  This will be the time used by the
benchmarking framework, and should exclude compile times.  The
framework will also record total process runtime.


An explanation of existing benchmarks
=====================================

Shared Benchmarks
-----------------

 * nbody -- a quadratic nbody that follows PBBS input conventions
 * blackscholes -- the standard blackscholes benchmark as found in the
                   Parsec benchmark suite.  Input is randomly generated.

Accelerate-specific Benchmarks
-------------------------------------

There are a number of temporary benchmarks or microbenchmarks which
you will find in the HSBencher script run_benchmarks.hs.

 * scaleFlops - Attempt to study at what point GPU offload becomes
     viable by varying the arithmetic intensity of a "map" kernel.
    
  - scaleFlops - The first attempt was to vary the size of a large
                arithmetic expression (a bunch of sqrts).
  - scaleFlops2 - The second attempt instead creates a 2D array an
                folds the inner dimm, thus creating an inner
                sequential loop inside the kernel to vary arithmetic intensity.




