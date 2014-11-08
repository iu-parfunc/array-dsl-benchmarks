

[2013.02.28] {Measuring some initial results}
---------------------------------------------

First, the PBBS version isn't really comparable, being Barnes-Hut.
For reference:

  * `1.2/3.36` seconds (4 vs 1 core) per round on MINE machines for 1M points

Our sequential C backend (Data.Array.Accelerate.C) is starting off at:

  * `2.33s` for N=10K

Whereas monad-par takes `0.5/2.1s` on 4/1 core for 10K, and `3.6s` for 25K.

Hmm... I guess this poor initial result shouldn't be suprising.  Right
now the fold/generate deforestation isn't happening, so we're creating
the big matrix before folding.  Further, it's doing 2X the work it
needs to.


[2013.03.01] {Now with Generate/Reduce fusion}
----------------------------------------------

Note that this benchmark still doesn't do the right thing!  So this
benchmark isn't YET meaningful.  But it is chewing on the right number
of points with the right communication pattern.

Here's the surprising bit.  The sequential C backend seems to have
regressed since yesterday, and now the fused version is only doing as
well as the unfused versio yesterday.  

 * `2.4s` for N=10K, WITH      fold/generate fusion [gcc]
 * `1.53` for N=10K, WITH      fold/generate fusion [icc -fast]  
 * `3.40` for N=10K, *WITHOUT* fold/generate fusion [gcc]
 * `3.26` for N=10K, *WITHOUT* fold/generate fusion [icc -O3]
 * `2.15` for N=10K, *WITHOUT* fold/generate fusion [icc -fast]
 
(FYI, the interpreter take `6.15s` for N=500.)  The CUDA version
crashed before but runs for me on a mine machine (shale) now:

 * `0.67s` for N=10K, CUDA backend ("GeForce GT 430", wimpy GPU)
 
That's not so hot, in that, if the Cilk backend gets a decent speedup
for threading AND vectorizatio, it could be better.  (Btw, the first
run was 3.16 seconds, an extra ~2.5s for compilation/NFS.)  Already,
the Cilk version WITHUOT fusion can parallelize the Generate phase:

 * `1.07s` for N=10K, Cilk backend, no fusion, sequential fold [icc]

UH oh, at 20K it eats up the memory on the machine!  Even at 15K and
with the sequential version it does... weird.  12K ok, 13K segfaults.

Debugging mode:
---------------

Valgrind has nothing to complain about when we run as a standalone
executable (i.e. with a Generate instead of Use). 

And in fact, when I recompile and rerun I'm not seeing the crashes.
It runs 20K (w/fusion) in `6.1s`, ah, but it still crashes at 25K.



[2013.03.01] {A new round of numbers before our meeting}
--------------------------------------------------------

Cilk backend, with fusion (same as sequential for now):
Mine machines:

 * `2.0` N=10K 
 * `8.12s` N=20K   
 * `12.7` N=25K   

Cilk WITHOUT fusion, mine (siltstone):

 * This stopped compiling... vectorizer no longer happy.

Cilk WITHOUT fusion, hive:


Commentary:

  This is pretty inconsistent with before.  The latest round is on the
  CUT DOWN version of the benchmark, which should be simpler.  Why
  would there be a regression at the 10K time?  

  I'm also seeing MASSIVE memory usage when I use the C (not Cilk)
  version.  Hmm.  And the above poor numbers reproduce on another mine
  machine (diorite).



[2013.03.01] {Edward's numbers}
--------------------------------------------------------

Geforce 590GTX:

 * `0.8s` N=20K
 
[2013.03.07] {Adding hacks to C backend to avoid ICC vectorization pitfalls}
----------------------------------------------------------------------------

Bottom line: "__declspec(vector)" is NOT a safe way to vectorize for
machine generated code.  It has arbitrary rules for what will
vectorize and what will throw an error, and I'm not aware of a way to
turn those errors into warnings (i.e. fall back to non-vectorized
code).  For example, this line breaks vectorization:

    if ((bool)(!((bool)((bool)(e11 == e12) && (bool)((bool)(e8 == e7) && (bool)(e10 == e9))))))

But removing the casts allows vectorization to succeed.

For now I'm adding a few hacks to get around this kind of thing, but
ultimately I think it means that the Cilk backend will have to be
restricted to using #pragma ivdep and #pragma vector always, and
inlining everything into a loop rather than having a separate
elemental function.

This may not be so bad though, because in the simple test I performed
(hacking up one of our generated files), ICC generated the EXACT SAME
BINARY, irrespective of whether I called an elemental function from a
_Cilk_for, or whether I inlined that function leaving it to the
pragmas.


[2013.03.07] {Added parallelism to the outer-loop of a multidim for}
--------------------------------------------------------------------

This much SHOULD be safe.  But I'm getting memory explosions.  At
N=10K it runs (1.6s) but even then I see it using 60% of the memory on
the box.  

ACTUALLY... this memory leak seems to be happening even on my
sequential C version.  How far does it go back?  Well... actually the
behavior is a large memory footprint that's constant.  It uses 60% of
3.7gb of memory on N=10K.  Since it should take 800Mb for a 10K matrix
of doubles (and there are THREE doubles, e.g. 2.4gb), this is actually
spot on.  No mystery.  After reenabling fusion the same example uses
extremely little memory either sequentially or with Cilk:

 * `2.06s` N=10K, sequential MINE machine
 * `0.57s` N=10K, cilk, with parallel outer fold loop.
 * `1.29s` N=15K, cilk, with parallel outer fold loop. 
 * `2.28s` N=20K, cilk, with parallel outer fold loop.  
 * `3.57s` N=25K, cilk, with parallel outer fold loop.    

It is successfully vectorizing the INNER for loop.
Ugh, getting some occasional segfaults there though... 

 * `0.86s` N=25K, cilk, with parallel outer fold loop. HIVE
 * `1.24s` N=30K, cilk, with parallel outer fold loop. HIVE 
 * `1.63s` N=35K, cilk, with parallel outer fold loop. HIVE  
 * `2.17s` N=40K, cilk, with parallel outer fold loop. HIVE   

And then at 50K it segfaults reliably. 


[2013.03.23] {Getting numbers from Delta}
--------------------------------------------------------------------

First, with the CUDA backend on ONE Tesla 2070:

 * `4.25s`  N=10K (and then 7.31s... inconsistent, bimodal, saw that again)
 * `6.16s` N=20K
 * `19.69s` N=100K 

Also, it's burning 100% CPU and I can't see a job on nvidia-smi.  HUH?
(but it can't be using the interp because that takes WAY longer)

Is this without Trafo fusion?

Ok, it eventually showed up like this:

    +------------------------------------------------------+                       
    | NVIDIA-SMI 4.304.54   Driver Version: 304.54         |                       
    |-------------------------------+----------------------+----------------------+
    | GPU  Name                     | Bus-Id        Disp.  | Volatile Uncorr. ECC |
    | Fan  Temp  Perf  Pwr:Usage/Cap| Memory-Usage         | GPU-Util  Compute M. |
    |===============================+======================+======================|
    |   0  Tesla C2075              | 0000:02:00.0     Off |                    0 |
    | 30%   55C    P0   149W / 225W |   1%   69MB / 5375MB |     99%      Default |
    +-------------------------------+----------------------+----------------------+
    |   1  Tesla C2075              | 0000:84:00.0     Off |                    0 |
    | 30%   48C   P12    33W / 225W |   0%   10MB / 5375MB |      0%      Default |
    +-------------------------------+----------------------+----------------------+

    +-----------------------------------------------------------------------------+
    | Compute processes:                                               GPU Memory |
    |  GPU       PID  Process name                                     Usage      |
    |=============================================================================|
    |    0     30317  ./test.exe                                            57MB  |
    +-----------------------------------------------------------------------------+

For 100K run (which took 22s though sometimes as little as 16.5).

But... but... it takes 0.7s for 10K on my cruddy GPU in veronica (GT
430)!?  What gives?

Is there any chance the CUDA compile part is taking a crazy long time?
Need to get accelerate to split out compile time.

There is a disscrepancy here because 100K should take one hundred
times as long as 10K due to N^2 complexity.  And yet the running time
is 3X more not 100X more...  

Next round:
----------------------------

Here are times WITHOUT compilation:

 * `0.1s` - 10K 
 * `0.393s` - 20K

Not too shabby!!  But that may not be counting data transfer either.



[2013.03.24] {Building nbody with trafo}
========================================

Trafo does succeed in fusing nbody (fold-zipwith-replicate-replicate)
down to a fold+generate.  But, it generates bad code for the shapes:

    generate (intersect (indexFull (Z :. All :. shapeSize ((shape a0))) (shape a0))
                        (indexFull (Z :. shapeSize ((shape a0)) :. All) (shape a0)))

We'll need to support the Intersect node to support that.

Oh, actually, contrary to my expectation, we didn't retrieve the sizes
statically for this example either.  In fact, we fail to size the
replicates.  There's no reason for that.

