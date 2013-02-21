#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <stdlib.h>

#include "cl++.h"

using namespace std;
using namespace cl;

void print_device_info(device d);

// Generate a random vector
float *generate_vector(int N) {
    float *v;
    posix_memalign((void **)&v, 32, sizeof(float) * N);

    for(int i = 0; i < N; ++i) {
        v[i] = drand48();
    }

    return v;
}

int main() {
    device_list devs(CL_DEVICE_TYPE_CPU
                     | CL_DEVICE_TYPE_GPU
                     | CL_DEVICE_TYPE_ACCELERATOR
                     );

    cout << "Found " << devs.size() << " devices:" << endl;
    for(int i = 0; i < devs.size(); ++i)
        print_device_info(devs[i]);

    auto dev = devs[0];

    context ctx(devs);

    auto prog = ctx.createProgramFromSourceFile("dotprod.cl");

    prog.build(dev);


    return 0;
}

void print_device_info(device d)
{
    cout << "Device id " << (cl_device_id)d << ": " << d.name() << endl;
    cout << "  Compute units:       " << d.compute_units() << endl;
    cout << "  Max Sub-devices:     " << d.max_subdevices() << endl;
    cout << "  Address bits:        " << d.address_bits() << endl;
    cout << "  Global Cache Size:   " << d.global_cache_size() << endl;
    cout << "  Cacheline Size:      " << d.global_cacheline_size() << endl;
    cout << "  Global Memory Size:  " << d.global_mem_size() << endl;
    cout << "  Host-unified Memory: "
         << (d.host_unified_memory() ? "yes" : " no") << endl;
    cout << "  Max Work Group Size: " << d.max_work_group_size() << endl;
    auto s = d.max_work_item_dimensions();
    cout << "  Max Work Item Sizes: ("
         << s[0] << ", " << s[1] << ", " << s[2] << ")" << endl;
    cout << "  Float Vector Width: " << d.native_float_vector_width() << endl;
}
