#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <stdlib.h>

#include "cl++.h"

using namespace std;
using namespace cl;

// in cl_error.cpp
void check_status(cl_int e);

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
    device_list devs(CL_DEVICE_TYPE_CPU);

    cout << "Found " << devs.size() << " devices:" << endl;
    for(auto i = 0; i < devs.size(); ++i)
        cout << "    " << i << ": " << devs[i].name() << endl;

    return 0;
}
