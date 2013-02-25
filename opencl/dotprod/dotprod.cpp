#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "cl++.h"

using namespace std;
using namespace cl;

const int N = 33554432;

void print_device_info(device d);

// Generate a random vector
void fill_vector(float *v, int N) {
    for(int i = 0; i < N; ++i) {
        //v[i] = drand48();
        v[i] = 1;
    }
}

int dotprod(cl_device_type type, int LOCAL_SIZE) {
    device_list devs(type);

    cout << "Found " << devs.size() << " devices:" << endl;
    for(int i = 0; i < devs.size(); ++i)
        print_device_info(devs[i]);

    auto dev = devs[0];

    context ctx(devs);
	auto q = ctx.createCommandQueue(dev, true);

    auto prog = ctx.createProgramFromSourceFile("dotprod.cl");

    stringstream options;
    options << "-DLOCAL_SIZE=" << LOCAL_SIZE;
    prog.build(dev, options.str());

    const int NUM_BLOCKS = 64;

	auto x = ctx.createBuffer<float>(N, CL_MEM_READ_ONLY);
	auto y = ctx.createBuffer<float>(N, CL_MEM_READ_ONLY);
	auto z = ctx.createBuffer<float>(NUM_BLOCKS, CL_MEM_WRITE_ONLY);

	{
	  auto xp = q.mapBuffer(x);
	  auto yp = q.mapBuffer(y);
	  fill_vector(xp, N);
	  fill_vector(yp, N);
	}

	auto k = prog.createKernel("dotprod");
	k.setArg(0, x);
	k.setArg(1, y);
	k.setArg(2, z);
	k.setArg(3, N);

	// LOCAL_SIZE needs to match LOCAL_SIZE in the kernel file.
	auto e = q.execute(k, LOCAL_SIZE * NUM_BLOCKS, LOCAL_SIZE);
    e.wait();

	auto zp = q.mapBuffer(z);
    float total = 0;
    for(int i = 0; i < NUM_BLOCKS; ++i) {
        total += zp[i];
    }
	cout << endl << "Result: " << total << endl;

    auto start = e.get_start();
    auto stop  = e.get_stop();

    cout << "SELFTIMED " << double(stop - start) / 1e9 << endl;

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
