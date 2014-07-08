#!/bin/bash

benchname=$1

if [ "$benchname" == "" ]; then
  echo "ERROR: must supply benchmark root name as first argument."
  exit 1
fi

function doit() {
  variant=$1
  cleaned=`echo $variant | sed 's/_//'`
  module=$2
  shift; shift
  rest=$*
  fullname="$benchname-$cleaned"
  dest=./$variant/"$fullname".cabal
  rm -f $dest
  mkdir -p ./$variant/
  cat >> $dest <<EOF
Name:                $fullname
Version:             0.0.0.1
Build-type:          Simple
Cabal-version:       >=1.10

Executable bench-$fullname
    Default-Language:    Haskell2010
    main-is:    Main.hs
    hs-source-dirs: ../common ../../common
    ghc-options: -O2 -threaded -rtsopts 
    cpp-options: -DACCBACKEND=Data.Array.Accelerate.$module $rest
    build-depends: base >= 4.7
                 , time, array >= 0.4, random
                 , mwc-random, vector >= 0.10
                 , bytestring >= 0.10, bytestring-lexing >= 0.4
                 , accelerate-backend-kit >= 0.15.0.4
                 , accelerate-icc-opencl  >= 0.15.0.0
                 , accelerate-cuda        >= 0.15.0.0
                 , accelerate-multidev    >= 0.15.0.0
                 , accelerate             >= 0.15.0.0
                 , cuda >= 0.5.1.1
EOF
}

doit seq_c C 
doit cilk CILK
doit cuda CUDA              -DNOSIMPLE
doit fission1 FissionExampleBackend
doit fission2 FissionCUDA
doit spmd1    SPMD_Example1 -DNOSIMPLE
doit spmd2    SPMD_Example2 
doit cpugpu   Multi_CPUGPU 
doit 2gpu     Multi_2GPU    -DEXTRAINITCUDA

echo "All done generating .cabal files and directories."
