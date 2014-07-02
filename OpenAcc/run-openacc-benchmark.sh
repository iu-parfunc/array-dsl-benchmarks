#!/bin/bash

set -x
set -e

DIR=`dirname $0`

if [ "$CABAL" == "" ]; 
then CABAL=cabal
fi

cd $DIR
$CABAL install --disable-documentation --bindir=. --program-suffix=.exe

# Fusion table upload
# ============================================================
# Parfunc account, registered app in api console:
CID=905767673358.apps.googleusercontent.com
SEC=2a2H57dBggubW1_rqglC7jtK
# ArrayDSL/Accelerate-multidev table docID:
# TABID=1E17ssTkVafPYjzPjO9m1uOqlq8Cz2T9D48PQo7s
# https://www.google.com/fusiontables/DataSource?docid=1E17ssTkVafPYjzPjO9m1uOqlq8Cz2T9D48PQo7s
TABLENAME=Array-DSL-bench-results
# ============================================================

TRIALS=3

# Now run it:
./run-openacc-benchmark.exe --keepgoing --trials=$TRIALS --fusion-upload --name=$TABLENAME --clientid=$CID --clientsecret=$SEC $*
