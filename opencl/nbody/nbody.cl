/* -*- C -*- */

#pragma OPENCL EXTENSION cl_khr_fp64 : enable

__kernel void replicate_rows(__global double3 *points,
                             __global double3 *output)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < NUM_BODIES && j < NUM_BODIES)
        output[i * NUM_BODIES + j] = points[i];
}

__kernel void replicate_cols(__global double3 *points,
                             __global double3 *output)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < NUM_BODIES && j < NUM_BODIES)
        output[i * NUM_BODIES + j] = points[j];
}

double mag(double3 x) {
#ifdef APPLE
  return sqrt((float)(x.x * x.x + x.y * x.y + x.z * x.z));
#else
  return length(x);
#endif
}

__kernel void zip_force(__global double3 *left,
                        __global double3 *right,
                        __global double3 *force)
{
    int i = get_global_id(1);
    int j = get_global_id(0);

    if(i < NUM_BODIES && j < NUM_BODIES) {
        double3 a = left[i * NUM_BODIES + j];
        double3 b = right[i * NUM_BODIES + j];
        
        double d = mag(a - b);
        if(d > 0)
            force[i * NUM_BODIES + j] = (a - b) / (d * d);
    }
}

__kernel void fold_force(__global double3 *force,
                         __global double3 *out)
{
    int i = get_global_id(0);

    if(i >= NUM_BODIES) return;

    double3 total = (double3)(0, 0, 0);
    for(int j = 0; j < NUM_BODIES; j++) {
        total += force[i * NUM_BODIES + j];
    }

    out[i] = total;
}
