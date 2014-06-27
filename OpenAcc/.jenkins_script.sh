#!/bin/bash

set -x 
set -e

cd nbody
make 

PGI_ACC_TIME=1; ACC_NOTIFY=1; ./nbody.exe
