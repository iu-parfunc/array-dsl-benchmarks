#include <CL/cl.h>

// in nbody.cpp
int nbody(cl_device_type type, int LOCAL_SIZE);

int main() {
    return nbody(CL_DEVICE_TYPE_GPU, 1);
}
