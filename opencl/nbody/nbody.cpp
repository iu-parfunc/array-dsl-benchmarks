#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <sstream>
#include <stdlib.h>

#include "cl++.h"

using namespace std;
using namespace cl;

//const int N = 2949;
const int N = 1500;

void print_device_info(device d);

// Generate a random vector
void fill_vector(cl_double3 *v, int N) {
    for(int i = 0; i < N; ++i) {
        //v[i] = drand48();
        v[i].s[0] = drand48();
        v[i].s[1] = drand48();
        v[i].s[2] = drand48();
    }
}

int nbody(cl_device_type type, int LOCAL_SIZE) {
    device_list devs(type);

    cout << "Found " << devs.size() << " devices:" << endl;
    for(int i = 0; i < devs.size(); ++i)
        print_device_info(devs[i]);

    auto dev = devs[0];

    context ctx(devs);
	auto q = ctx.createCommandQueue(dev, true);

    auto prog = ctx.createProgramFromSourceFile("nbody.cl");

    const int CUTOFF = 1;

    stringstream options;
    options << "-DLOCAL_SIZE=" << LOCAL_SIZE;
    options << " -DCUTOFF=" << CUTOFF;
    options << " -DNUM_BODIES=" << N;
#ifdef __APPLE__
	options << " -DAPPLE";
#endif
    prog.build(dev, options.str());

    const int NUM_BLOCKS = (N + LOCAL_SIZE - 1) / LOCAL_SIZE;

	auto points = ctx.createBuffer<cl_double3>(N);
    auto point_cols = ctx.createBuffer<cl_double3>(N * N);
    auto point_rows = ctx.createBuffer<cl_double3>(N * N);
    auto force_mat = ctx.createBuffer<cl_double3>(N * N);
    auto forces = ctx.createBuffer<cl_double3>(N);

	{
	  auto xp = q.mapBuffer(points);
	  fill_vector(xp, N);
	}

#define LOAD_KERNEL(x) auto x = prog.createKernel(#x)
    LOAD_KERNEL(replicate_rows);
    LOAD_KERNEL(replicate_cols);
    LOAD_KERNEL(zip_force);
    LOAD_KERNEL(fold_force);
#undef LOAD_KERNEL

    replicate_rows.setArg(0, points);
    replicate_rows.setArg(1, point_rows);

    replicate_cols.setArg(0, points);
    replicate_cols.setArg(1, point_cols);
    
    zip_force.setArg(0, point_rows);
    zip_force.setArg(1, point_cols);
    zip_force.setArg(2, force_mat);

    fold_force.setArg(0, force_mat);
    fold_force.setArg(1, forces);

    auto global_size = LOCAL_SIZE * NUM_BLOCKS;

    auto rep_row_e = q.execute2d(replicate_rows,
                                 global_size, global_size,
                                 LOCAL_SIZE);

    auto rep_col_e = q.execute2d(replicate_cols,
                                 global_size, global_size,
                                 LOCAL_SIZE);

    auto zip_e = q.execute2dAfter(zip_force,
                                  global_size, global_size,
                                  LOCAL_SIZE,
                                  rep_row_e, rep_col_e);

    auto force_e = q.executeAfter(fold_force,
                                  global_size, LOCAL_SIZE,
                                  zip_e);

    force_e.wait();

    auto start = min(rep_row_e.get_start(), rep_col_e.get_start());
    auto stop  = force_e.get_stop();

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
