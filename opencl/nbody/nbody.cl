/* -*- C -*- */

#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel void replicate_rows(__global double3 *points,
                             __global double3 *output)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < N && j < N)
        output[i * N + j] = points[i];
}

__kernel void replicate_cols(__global double3 *points,
                             __global double3 *output)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < N && j < N)
        output[i * N + j] = points[j];
}

__kernel void zip_force(__global double3 *left,
                        __global double3 *right,
                        __global double3 *force)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < N && j < N) {
        double3 a = left[i * N + j];
        double3 b = right[i * N + j];
        
        double d = length(a - b);
        if(d > 0)
            force[i * N + j] = (a - b) / (d * d);
    }
}

__kernel void fold_force(__global double3 *force,
                         __global double3 *out)
{
    int i = get_global_id(0);

    if(i >= N) return;

    double3 total = (double3)(0, 0, 0);
    for(int j = 0; j < N; j++) {
        total += force[i * N + j];
    }

    out[i] = total;
}
