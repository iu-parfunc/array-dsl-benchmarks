#!/bin/bash

# A script used to launch a benchmark suite, used by our benchmarking servers.
# Assumes all submodules are updated.

set -x 
set -e

# module add intel seems to break on fake_bigred2 
# moving it here, so it takes place after the pbs thing
module add intel
which icc || echo ok

# Adding this for Delta on Futuregrid, but it should be harmless elsewhere.
module add cuda/5.5 || echo "Ok that that didnt work."

which -a nvcc  || echo ok
nvcc --version || echo ok

# The first argument to this script is usually the root directory for the repo.
if [ -d "$1" ]; then 
  HERE=$1/accelerate
  shift
else 
  HERE=`pwd`
fi
pwd
if [ "$CABAL" == "" ]; 
then CABAL=cabal
fi


HSBENCHER_SANDBOX=$HERE/.cabal-sandbox/
ACC=../accelerate_src
PKGS=" ../HSBencher/hsbencher/ ../HSBencher/hsbencher-fusion/ ../HSBencher/hgdata/ "
PKGS="$PKGS $ACC/ $ACC/accelerate-backend-kit/backend-kit/ \
                  $ACC/accelerate-backend-kit/icc-opencl/  \
                  $ACC/accelerate-multidev/ \
                  $ACC/accelerate-cuda/  "
                  
# if [ $2 == "nbody/cuda" ]; 
# then PKGS="$PKGS 
# fi 
# #                  $ACC/accelerate-backend-kit/simple-cuda

which $CABAL
$CABAL --version
CBLOPTS="--disable-documentation"

# Individual benchmark directories:
DIRS="$HERE/nbody/seq_c \
   $HERE/nbody/cilk  \
   $HERE/nbody/cuda  \
   $HERE/nbody/fission1  \
   $HERE/nbody/spmd1  \
   $HERE/nbody/spmd2  \
   $HERE/blackscholes/seq_c \
   $HERE/blackscholes/cilk  \
   $HERE/blackscholes/cuda  \
   $HERE/blackscholes/fission1  \
   $HERE/blackscholes/spmd1  \
   $HERE/blackscholes/spmd2  \
   $HERE/blackscholes/cpugpu \
   $HERE/blackscholes/2gpu   \
   $HERE/scale_flops/seq_c \
   $HERE/scale_flops/cilk \
   $HERE/scale_flops/cuda \
   $HERE/scale_flops2/seq_c \
   $HERE/scale_flops2/cilk \
   $HERE/scale_flops2/cuda \
   $HERE/reduce/cuda \
  "

# Retired:
#   $HERE/nbody_temp/cpugpu \
#   $HERE/blackscholes_temp/cpugpu \

   # $HERE/smvm/seq_c \
   # $HERE/smvm/cilk  \
   # $HERE/smvm/cuda  \
   # $HERE/smvm/fission1  \
   # $HERE/smvm/spmd1  \
   # $HERE/smvm/spmd2  \

#   $HERE/nbody_temp/2gpu \

# $HERE/scale_flops/cilk $HERE/scale_flops/cuda

# ------------------------------------------------------------
# When benchmarking we always use a sandbox
cd $HERE
$CABAL sandbox init
# And we build each individual benchmark in the same sandbox:
for dir in $DIRS; do 
  cd $dir
  echo Creating sandbox in $dir 
  echo Pointing to $HSBENCHER_SANDBOX 
  cabal sandbox init --sandbox=$HSBENCHER_SANDBOX
  cd $HERE
done

$CABAL sandbox hc-pkg list

find -name ".genC_*" | xargs rm -f 

# (0) Install all package dependencies
# ------------------------------------------------------------

# The machines we're measuring on currently have CUDA 5.5 and we're
# seeing problems with the cuda-0.6 haskell library [2014.07.06]: 
$CABAL install $CBLOPTS $PKGS -j --force-reinstalls --constraint='cuda<0.6'

# (1) Build the benchmark harness itself
# ------------------------------------------------------------

$CABAL install $CBLOPTS --bindir=. --program-suffix=.exe

# (2) Then we run the actual benchmarks
# ----------------------------------------

# Defines CLIENTID, SECRET, TABLENAME:
source ../.hsbencher_fusion_config.sh

TRIALS=3

# Enable upload of benchmarking data to a Google Fusion Table:
./run-accelerate-benchmark.exe --fusion-upload --name=$TABLENAME --clientid=$CLIENTID --clientsecret=$SECRET $*

# DISABLING --keepgoing for now 
# DISABLING --trials=$TRIALS
