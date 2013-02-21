#define __CL_ENABLE_EXCEPTIONS

#include <iostream>
#include <malloc.h>
#include <CL/cl.hpp>

using namespace std;
using namespace cl;

// in cl_error.cpp
void check_status(cl_int e);

// Generate a random vector
float *generate_vector(int N) {
    float *v = (float *)memalign(32,
                                 sizeof(float) * N);

    for(int i = 0; i < N; ++i) {
        v[i] = drand48();
    }

    return v;
}

int main() {
    std::vector<Platform> platforms;
    Platform::get(&platforms);

    cout << "Found " << platforms.size() << " platforms:" << endl;
    for(auto i = 0; i < platforms.size(); ++i) {
        std::string name;
        platforms[i].getInfo(CL_PLATFORM_NAME, &name);
        cout << "    " << i << ": " << name << endl;
    }

    return 0;
}
