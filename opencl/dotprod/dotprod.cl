/* -*- C -*- */

// This preprocessor define should actually be provided by the calling
// program.
#define LOCAL_SIZE 1024

__kernel void dotprod(__global float *x, __global float *y,
					  __global float *z, int N)
{
    __local float temp[LOCAL_SIZE];

    int i = get_global_id(0);
    temp[i] = 0;

    // Phase 1: reduce down to a size that fits in local memory.
    for(int j = i; j < N; j += LOCAL_SIZE) {
        temp[i] += x[j] * y[j];
    }

    // Phase 2: sum up the temporary array
    for(int j = LOCAL_SIZE / 2; j > 0; j >>= 1) {
        barrier(CLK_LOCAL_MEM_FENCE);
        if(i < j) {
            temp[i] += temp[i + j];
        }
    }

    if(i == 0) {
        *z = temp[0];
    }
}
