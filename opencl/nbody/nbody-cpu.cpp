#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

// in nbody.cpp
int nbody(cl_device_type type, int LOCAL_SIZE);

int main() {
    return nbody(CL_DEVICE_TYPE_CPU, 1);
}
