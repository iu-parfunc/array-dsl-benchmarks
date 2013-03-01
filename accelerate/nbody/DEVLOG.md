

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

 * `0.67s` for N=10K, CUDA backend
 
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

