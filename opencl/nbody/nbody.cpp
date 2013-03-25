#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <iomanip>
#include <sstream>
#include <stdlib.h>
#include <cassert>

#include "cl++.h"

using namespace std;
using namespace cl;

//const int N = 2949;
//const int N = 10000;

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

template<typename T>
vector<cl_double3> read_data(T &cin) {
	string header;
	cin >> header;
	assert(header == "pbbs_sequencePoint3d");

	vector<cl_double3> result;

	double x, y, z;
	cin >> x >> y >> z;
	while(!cin.eof()) {
		cl_double3 pt;
		pt.s[0] = x;
		pt.s[1] = y;
		pt.s[2] = z;

		result.push_back(pt);

		cin >> x >> y >> z;
	}

	return result;
}

int nbody(cl_device_type type, int LOCAL_SIZE) {
    device_list devs(type);

    cout << "Found " << devs.size() << " devices:" << endl;
    for(int i = 0; i < devs.size(); ++i)
        print_device_info(devs[i]);

    device dev = devs[0];

    context ctx(devs);
	command_queue q = ctx.createCommandQueue(dev, true);

    program prog = ctx.createProgramFromSourceFile("nbody.cl");

    const int CUTOFF = 1;

	vector<cl_double3> data = read_data(cin);
	size_t N = data.size();

    stringstream options;
    options << "-DLOCAL_SIZE=" << LOCAL_SIZE;
    options << " -DCUTOFF=" << CUTOFF;
    options << " -DNUM_BODIES=" << N;
#ifdef __APPLE__
	options << " -DAPPLE";
#endif
    prog.build(dev, options.str());

    const int NUM_BLOCKS = (N + LOCAL_SIZE - 1) / LOCAL_SIZE;

	cout << "Read " << data.size() << " points" << endl;

	buffer<cl_double3> points = ctx.createBuffer<cl_double3>(N);
    buffer<cl_double3> forces = ctx.createBuffer<cl_double3>(N);

    cl_ulong global_size = LOCAL_SIZE * NUM_BLOCKS;

	buffer_map<cl_double3> xp = q.mapBuffer(points);
	for(int i = 0; i < N; ++i) {
		xp[i] = data[i];
	}
	event transfer_e = xp.unmap();
	
#define LOAD_KERNEL(x) kernel x = prog.createKernel(#x)
#ifndef OPTIMIZED
    buffer<cl_double3> point_cols = ctx.createBuffer<cl_double3>(N * N);
    buffer<cl_double3> point_rows = ctx.createBuffer<cl_double3>(N * N);
    buffer<cl_double3> force_mat = ctx.createBuffer<cl_double3>(N * N);

    LOAD_KERNEL(replicate_rows);
    LOAD_KERNEL(replicate_cols);
    LOAD_KERNEL(zip_force);
    LOAD_KERNEL(fold_force);

    replicate_rows.setArg(0, points);
    replicate_rows.setArg(1, point_rows);

    replicate_cols.setArg(0, points);
    replicate_cols.setArg(1, point_cols);
    
    zip_force.setArg(0, point_rows);
    zip_force.setArg(1, point_cols);
    zip_force.setArg(2, force_mat);

    fold_force.setArg(0, force_mat);
    fold_force.setArg(1, forces);

    auto rep_row_e = q.execute2dAfter(replicate_rows,
									  global_size, global_size,
									  LOCAL_SIZE,
									  transfer_e);

    auto rep_col_e = q.execute2dAfter(replicate_cols,
									  global_size, global_size,
									  LOCAL_SIZE,
									  transfer_e);

    auto zip_e = q.execute2dAfter(zip_force,
                                  global_size, global_size,
                                  LOCAL_SIZE,
                                  rep_row_e, rep_col_e);

    auto force_e = q.executeAfter(fold_force,
                                  global_size, LOCAL_SIZE,
                                  zip_e);
    force_e.wait();

    auto compute_start = min(rep_row_e.get_start(), rep_col_e.get_start());

#else
	LOAD_KERNEL(nbody_opt);

	nbody_opt.setArg(0, points);
	nbody_opt.setArg(1, forces);

	event force_e = q.execute(nbody_opt, global_size, LOCAL_SIZE);
	force_e.wait();

	cl_ulong compute_start = force_e.get_start();
#endif
#undef LOAD_KERNEL

	// write out the results
    cl_ulong stop;
	{
		buffer_map<cl_double3> f = q.mapBuffer(forces);
		stop = f.start_event().get_stop();

		ofstream out("nbody.3dpts");
		out << "pbbs_sequencePoint3d" << endl;
		out << setprecision(20);
		for(int i = 0; i < N; ++i) {
			out << f[i].s[0] << " ";
			out << f[i].s[1] << " ";
			out << f[i].s[2] << endl;
		}
	}

	cl_ulong start = transfer_e.get_start();
	cl_ulong compute_stop = force_e.get_stop();

	cout << "SELFTIMED (compute)          "
		 << double(compute_stop - compute_start) / 1e9 << endl;
    cout << "SELFTIMED (compute+transfer) "
		 << double(stop - start) / 1e9 << endl;


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
    vector<size_t> s = d.max_work_item_dimensions();
    cout << "  Max Work Item Sizes: ("
         << s[0] << ", " << s[1] << ", " << s[2] << ")" << endl;
    cout << "  Float Vector Width: " << d.native_float_vector_width() << endl;
}
