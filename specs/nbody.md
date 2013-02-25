
Benchmark specification: N-body naive
=====================================

This benchmark is the naive (quadratic) implementation of the N-body
problem.  (We may do a more practical Barnes-Hut implementation as a
separate benchmark.)

nBody is a common benchmark, and we use the Problem-Based-Benchmark-Suite to
standardize on the problem definition and input data sets (see section below).
Note that in the current version of this benchmark the masses (`m1` and `m2`)
are assumed to be `1.0`.  Also, is not actually properly specified by the
benchmark definition below, but all positions and masses (and computations
thereupon) should use double precision (64 bit) floating point.

As always, don't include the time to load the file.  However, do include the
time to copy the array of points from CPU to GPU memory, if timing a discrete
GPU implementation. If JIT compilation is involved, separate that time from the
runtime.

Here we additionally specify, for our array-DSL benchmark, the
specific algorithm that should be used for the naive approach:

Algorithm description, `N` input points
---------------------------------------

 * replicate the `N` points across the columns of an `NxN` matrix
 * replicate the `N` points across the rows of an `NxN` matrix  
 * zip the matrices together using a scalar `accel` function to
   compute the [matrix of] point-to-point accelerations
 * fold a vector-add function over the inner dimension of the matrix

You will notice that this algorithm is especially inefficient.  It computes the
interaction between points `a` and `b` *twice* (both `a~b` and `b~a`).  Further,
to avoid divide-by-zero it will need to have a check of the denominator when
computing an acceleration for a point `a` with itself!  This brings us to the
following improvements:

Optional improvements to algorithm
----------------------------------

 * (1) Manual elimination of intermediates: if your language of choice does not
   eliminate (fuse/deforest) the intermediate matrices, you are encouraged to
   make an additional version which performs the cartesian product (or even the
   cartesian product + fold) in a single step.
   
 * (2) Eliminate redundant computation: if your array language supports it, you may
   perform the acceleration computations *only* for the upper/lower triangular
   matrix (and not the diagonal).  This eliminates the need to hedge against
   divide-by-zero in the scalar `accel` function.  The triangular matrix should
   be unfolded (mirrored) back to a rectangular matrix to allow the same fold
   step. 

PBBS Benchmark specification (COPIED): 
======================================
 
 Nbody Force Calculation (NBODY):

Given n points in 3 dimensions calculate the gravitational force
vector on each point due to all other points. This force vector can be
an approximation. For this benchmark we use a gravitational constant
of 1 giving the force between two particles located at r1 and r2 as :
`F12 = m1 m2 (r2 - r1) / |(r2 - r1)|^3`.  

Input and Output File Formats
-----------------------------

The input and output files need to be in the 3d points file
format. The input file is a set of points specified by their
location. In the current version all weights are assumed to be
unit. The output file specifies the force vector on each point and
therefore should have the same number of entries as the input.

Default Input Distributions
-----------------------------

Each distribution should be run for `n=1,000,000`. The weights used for
average time are given in parentheses. In all cases the accuracy of
the forces need to be within an error bound of 10-6, i.e.,
  `|(F - F')/|F|| < 10-6`, 
where F is the actual force and F' the reported force.

 Points selected at random uniformly within a unit sphere:
 
    uniform -s -d 3 <n> <filename>

 Points selected at random from the Plummer distribution:
 
    plummer -d 3 <n> <filename>
    
 Points selected at random uniformly on the surface of a unit sphere:
 
    uniform -S -d 3 <n> <filename>
