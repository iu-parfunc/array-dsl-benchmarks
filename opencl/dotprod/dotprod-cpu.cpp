#include <CL/opencl.h>

// in dotprod.cpp
int dotprod(cl_device_type type, int LOCAL_SIZE);

int main() {
    return dotprod(CL_DEVICE_TYPE_CPU, 1);
}
