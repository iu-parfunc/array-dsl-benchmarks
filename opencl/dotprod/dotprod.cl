/* -*- C -*- */

__kernel void dotprod(__global float *x, __global float *y,
					  __global float *z, int N)
{
    size_t num_groups = get_num_groups(0);
    int block_size = (N + num_groups - 1) / num_groups;
    int block_id = get_global_id(0) / get_local_size(0);
   
    int block_start = block_id * block_size;
    int block_end = min(block_start + block_size, N);

    __local float temp[LOCAL_SIZE];

    int i = get_local_id(0);
    temp[i] = 0;

    // Phase 1: reduce down to a size that fits in local memory.
    for(int j = block_start + i; j < block_end; j += LOCAL_SIZE) {
        temp[i] += x[j] * y[j];
    }

    // Phase 2: sum up the temporary array
    for(int j = LOCAL_SIZE / 2; j >= CUTOFF; j >>= 1) {
        barrier(CLK_LOCAL_MEM_FENCE);
        if(i < j) {
            temp[i] += temp[i + j];
        }
    }

    if(i < CUTOFF) {
        z[block_id * CUTOFF + i] = temp[0];
    }
}
