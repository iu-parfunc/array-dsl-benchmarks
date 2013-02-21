#include <stdio.h>

#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

void handle_error(cl_int e) {
#define HANDLE(x)                                           \
    if(e == x) {                                            \
        fprintf(stderr, "OpenCL Error: " #x " (%d)\n", e);  \
        abort();                                            \
    }
    
    HANDLE(CL_BUILD_PROGRAM_FAILURE);
    HANDLE(CL_COMPILER_NOT_AVAILABLE);
    HANDLE(CL_DEVICE_NOT_FOUND);
    HANDLE(CL_INVALID_ARG_INDEX);
    HANDLE(CL_INVALID_ARG_SIZE);
    HANDLE(CL_INVALID_ARG_VALUE);
    HANDLE(CL_INVALID_BINARY);
    HANDLE(CL_INVALID_BUILD_OPTIONS);
    HANDLE(CL_INVALID_COMMAND_QUEUE);
    HANDLE(CL_INVALID_CONTEXT);
    HANDLE(CL_INVALID_DEVICE);
    HANDLE(CL_INVALID_DEVICE_TYPE);
    HANDLE(CL_INVALID_EVENT_WAIT_LIST);
    HANDLE(CL_INVALID_GLOBAL_OFFSET);
    HANDLE(CL_INVALID_GLOBAL_WORK_SIZE);
    HANDLE(CL_INVALID_IMAGE_SIZE);
    HANDLE(CL_INVALID_KERNEL);
    HANDLE(CL_INVALID_KERNEL_ARGS);
    HANDLE(CL_INVALID_MEM_OBJECT);
    HANDLE(CL_INVALID_OPERATION);
    HANDLE(CL_INVALID_PLATFORM);
    HANDLE(CL_INVALID_PROGRAM);
    HANDLE(CL_INVALID_PROGRAM_EXECUTABLE);
    HANDLE(CL_INVALID_QUEUE_PROPERTIES);
    HANDLE(CL_INVALID_SAMPLER);
    HANDLE(CL_INVALID_VALUE);
    HANDLE(CL_INVALID_WORK_DIMENSION);
    HANDLE(CL_INVALID_WORK_GROUP_SIZE);
    HANDLE(CL_INVALID_WORK_ITEM_SIZE);
    HANDLE(CL_MEM_OBJECT_ALLOCATION_FAILURE);
    HANDLE(CL_MISALIGNED_SUB_BUFFER_OFFSET);
    HANDLE(CL_OUT_OF_RESOURCES);
    HANDLE(CL_OUT_OF_HOST_MEMORY);

    fprintf(stderr, "Unknown OpenCL Error: %d\n", e);
    abort();
}

void check_status(cl_int e) {
    if(CL_SUCCESS != e) {
        handle_error(e);
    }
}

