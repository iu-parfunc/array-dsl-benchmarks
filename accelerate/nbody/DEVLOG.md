

[2013.02.28] {Measuring some initial results}

First, the PBBS version isn't really comparable, being Barnes-Hut.
For reference:

  * 1.2/3.36 seconds (4 vs 1 core) per round on MINE machines for 1M points

Our sequential C backend (Data.Array.Accelerate.C) is starting off at:

  * 2.33s for N=10K

Whereas monad-par takes 0.5s/2.1 on 4/1 core for 10K, and 3.6 for 25K.

Hmm... I guess this poor initial result shouldn't be suprising.  Right
now the fold/generate deforestation isn't happening, so we're creating
the big matrix before folding.  Further, it's doing 2X the work it
needs to.


