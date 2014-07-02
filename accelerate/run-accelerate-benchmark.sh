#!/bin/bash

# A script used to launch a benchmark suite, used by our benchmarking servers.
# Assumes all submodules are updated.

set -x 
set -e

# The first argument to this script is usually the root directory for the repo.
if [ "$1" == "" ]; 
then TOP=`pwd`
else TOP=$1/accelerate/
fi
pwd
if [ "$CABAL" == "" ]; 
then CABAL=cabal
fi

HSBENCHER_SANDBOX=$TOP/.cabal-sandbox/
ACC=../accelerate_src
PKGS=" ../HSBencher/hsbencher/ ../HSBencher/hsbencher-fusion/ "
PKGS="$PKGS $ACC/ $ACC/accelerate-backend-kit/backend-kit/ \
                  $ACC/accelerate-backend-kit/icc-opencl/  \
                  $ACC/accelerate-multidev \
                  $ACC/accelerate-cuda/    \
                  "
#                  $ACC/accelerate-backend-kit/simple-cuda

which $CABAL
$CABAL --version
CBLOPTS="--disable-documentation"

# Individual benchmark directories:
DIRS="$TOP/array-dsl-benchmarks/ \
   $TOP/nbody/seq_c \
   $TOP/nbody/cilk  \
   $TOP/nbody/cuda  \
   $TOP/nbody/fission1  \
   $TOP/nbody/spmd1  \
   $TOP/nbody/spmd2  \
   $TOP/nbody_temp/cpugpu \
   $TOP/blackscholes/seq_c \
   $TOP/blackscholes/cilk  \
   $TOP/blackscholes/cuda  \
   $TOP/blackscholes/fission1  \
   $TOP/blackscholes/spmd1  \
   $TOP/blackscholes/spmd2  \
   $TOP/blackscholes_temp/cpugpu \
   $TOP/scale_flops/seq_c \
   $TOP/scale_flops/cilk \
   $TOP/scale_flops/cuda \
   $TOP/scale_flops2/seq_c \
   $TOP/scale_flops2/cilk \
   $TOP/scale_flops2/cuda \
   $TOP/reduce/cuda \
  "

   # $TOP/smvm/seq_c \
   # $TOP/smvm/cilk  \
   # $TOP/smvm/cuda  \
   # $TOP/smvm/fission1  \
   # $TOP/smvm/spmd1  \
   # $TOP/smvm/spmd2  \

#   $TOP/nbody_temp/2gpu \

# $TOP/scale_flops/cilk $TOP/scale_flops/cuda

# ------------------------------------------------------------
# When benchmarking we always use a sandbox
$CABAL sandbox init
# And we build each individual benchmark in the same sandbox:
for dir in $DIRS; do 
  cd $dir
  cabal sandbox init --sandbox=$HSBENCHER_SANDBOX
  cd $TOP
done

# (0) Install all package dependencies
# ------------------------------------------------------------

$CABAL install $CBLOPTS $PKGS -j

# (1) Build the benchmark harness
# ------------------------------------------------------------

$CABAL install $CBLOPTS --bindir=. --program-suffix=.exe

# (2) Then we run the actual benchmarks
# ----------------------------------------

# Defines CLIENTID, SECRET, TABLENAME:
source ../.hsbencher_fusion_config.sh

TRIALS=3

# Enable upload of benchmarking data to a Google Fusion Table:
./run-accelerate-benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=$TABLENAME --clientid=$CLIENTID --clientsecret=$SECRET $*
