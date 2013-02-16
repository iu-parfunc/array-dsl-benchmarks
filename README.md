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


