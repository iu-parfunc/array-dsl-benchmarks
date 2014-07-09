#!/bin/bash

set -x

if [ -d "$1" ]; then 
  cd $1/OpenAcc
  shift
fi

set -e
pwd
if [ "$CABAL" == "" ]; 
then CABAL=cabal
fi

CBLOPTS="--disable-documentation --force-reinstalls"

# This benchmark needs the pgi compiler
# ============================================================
module add pgi

# (0) Dependencies, input data
# ============================================================

(cd ../DATA; make)

# (1) Build the benchmark runner
# ============================================================

$CABAL sandbox init
$CABAL install $CBLOPTS ../HSBencher/hsbencher/ ../HSBencher/hsbencher-fusion/ ../HSBencher/hgdata -j
$CABAL install $CBLOPTS --bindir=. --program-suffix=.exe

# (2) Run benchmarks
# ============================================================
# Defines CLIENTID, SECRET, TABLENAME:
source ../.hsbencher_fusion_config.sh

TRIALS=3

# List what is available:
./run-openacc-benchmark.exe -l

# Now run it:
./run-openacc-benchmark.exe --fusion-upload --name=$TABLENAME --clientid=$CLIENTID --clientsecret=$SECRET $WHICHBENCH
#$*

# TEMP DISABLING --keepgoing --trials=$TRIALS
