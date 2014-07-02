#!/bin/bash

set -x
set -e

DIR=`dirname $0`

if [ "$CABAL" == "" ]; 
then CABAL=cabal
fi

cd $DIR
$CABAL install --disable-documentation --bindir=. --program-suffix=.exe

./run-openacc-benchmark.exe
