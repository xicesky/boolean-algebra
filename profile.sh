#!/bin/sh

# A small reminder on how to run the profiler

# Profile run time
PROF_OPTS="-p"

# Profile heap
#PROF_OPTS="-h"

stack run --profile -- +RTS $PROF_OPTS
echo "--------------------------------------------------------------------------------"
echo "This is (probably) the output profile:"
ls -la *.prof
echo "--------------------------------------------------------------------------------"

