// -*- c++ -*-
/*
 *  cl++.h
 *  CLmandelbrot
 *
 *  Created by eholk on 12/3/10.
 *
 *  This is a C++ wrapper around OpenCL to make programming it easier.
 */

#pragma once

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif
#include <vector>
#include <string>
#include <fstream>
#include <iostream>

#define CL_CHECK(e) {                                         \
        cl_int __st__ = (e);                                  \
        if(CL_SUCCESS != __st__) {                            \
            cl::handle_error(#e, __st__);                     \
        }                                                     \
    }

namespace cl {
    void handle_error(const char *code, cl_int e);
    std::string format_status(cl_int status);

    // represents an OpenCL device.
    class device {
        cl_device_id id;

        template<typename T>
        T get_device_info(cl_device_info param) {
            T t;
            clGetDeviceInfo(id, param, sizeof(t), &t, NULL);
            return t;
        }

        template<typename T>
        std::vector<T> get_device_infos(cl_device_info param) {
            size_t s;
            clGetDeviceInfo(id, param, 0, NULL, &s);
            
            T *t = new T[s / sizeof(T)];
            clGetDeviceInfo(id, param, s, t, NULL);
            
            std::vector<T> r;
            for(int i = 0; i < s / sizeof(T); ++i)
                r.push_back(t[i]);
            
            delete [] t;
            return r;
        }
        
    public:
        device(cl_device_id id) : id(id) {}
        
        std::string name();
        cl_device_type type();

        int compute_units();
        int max_subdevices();

        int address_bits();
        int global_cache_size();
        //cl_device_mem_cache_type global_cache_type();
        int global_cacheline_size();
        int global_mem_size();
        bool host_unified_memory();

        int native_float_vector_width();

        int max_work_group_size();
        std::vector<size_t> max_work_item_dimensions();

        operator cl_device_id() const;
    };

    // Maintains a list of devices of a certain type.
    class device_list {
        cl_device_type type;
        cl_uint num_ids;
        cl_device_id *devices;
        
    public:
        device_list(cl_device_type type);
        ~device_list();
        
        int size() const;
        const cl_device_id *ids() const;
        
        device operator[](int index);
    };
    
    class context;

    // Tracks an OpenCL buffer
    template<typename T>
    class buffer {
        friend class context;
        friend class command_queue;
        friend class kernel;
        
        cl_mem mem;
        cl_mem_flags flags;
        size_t _count;
        context &ctx;
        
        buffer(cl_mem mem, size_t count, context &ctx, cl_mem_flags flags)
            : mem(mem), _count(count), ctx(ctx), flags(flags) {};
    public:
        ~buffer() { clReleaseMemObject(mem); };

        buffer<T> &operator=(const buffer<T> &rhs);

        size_t count() const {
            return _count;
        }
    };
    
    // An OpenCL kernel.
    class kernel {
        friend class program;
        friend class command_queue;
        
        cl_kernel k;
        
        kernel(cl_kernel k);
    public:
        ~kernel();
        
        size_t maxWorkGroupSize(cl_device_id device);
        
        // This is for simple values
        template<typename T>
        void setArg(int index, T value) {
          clSetKernelArg(k, index, sizeof(T), &value);
        }

        // This is for memory buffers
        template<typename T>
        void setArg(int index, buffer<T> &value) {
          clSetKernelArg(k, index, sizeof(cl_mem), &value.mem);
        }
    };
    
    // An OpenCL program.
    class program {
        friend class context;
        cl_program prog;
        
        program(cl_program prog);
    public:
        ~program();
        
        void build(device dev);
        
        kernel createKernel(std::string name);
    };
    
    // Allows easier access to a buffer
    template<typename T>
    class buffer_map {
        friend class command_queue;
        
        cl_command_queue queue;
        cl_mem mem;
        T *ptr;
        
        buffer_map(cl_command_queue queue, cl_mem mem, T *ptr) 
            : queue(queue), mem(mem), ptr(ptr) 
        {
        };
    public:
        ~buffer_map() {
            cl_event e;
            CL_CHECK(clEnqueueUnmapMemObject(queue, mem, ptr, 0, NULL, &e));
            CL_CHECK(clWaitForEvents(1, &e));
            CL_CHECK(clReleaseEvent(e));
        }
        
        T &operator[](size_t i) {
            return ptr[i];
        }
        
        operator T*() { return ptr; };
    };
    
    // OpenCL Command Queue
    class command_queue {
        friend class context;
        
        cl_command_queue queue;

        command_queue(cl_command_queue queue);
    public:
        command_queue(const command_queue &other);
        ~command_queue();
        
        template<typename T>
        buffer_map<T> mapBuffer(buffer<T> &b) {
            cl_int status;
            cl_event e;
            T *ptr = (T *)clEnqueueMapBuffer(queue,
                                             b.mem,
                                             CL_FALSE, 
                                             CL_MAP_READ | CL_MAP_WRITE,
                                             0, 
                                             sizeof(T) * b.count(),
                                             0, 
                                             NULL, 
                                             &e, 
                                             &status);
            CL_CHECK(status);
            CL_CHECK(clWaitForEvents(1, &e));
            CL_CHECK(clReleaseEvent(e));
            return buffer_map<T>(queue, b.mem, ptr);
        }
        
        // Enqueues the kernel and waits for it to complete.
        void execute(kernel &k, size_t global_size);
        void execute(kernel &k, size_t global_size, size_t local_size);
        void execute2d(kernel &k, size_t dim1, size_t dim2, size_t local_size);
        void executeND(kernel &k, size_t dimensions, size_t global_size[], 
                       size_t local_size[]);

        template<typename T>
        void write_buffer(buffer<T> &b, T *data) {
            clEnqueueWriteBuffer(queue,
                                 b.mem,
                                 CL_TRUE,
                                 0,
                                 sizeof(T) * b.count(),
                                 data,
                                 0,
                                 NULL,
                                 NULL);
        }

        template<typename T>
        void read_buffer(buffer<T> &b, T *data) {
            clEnqueueReadBuffer(queue,
                                b.mem,
                                CL_TRUE,
                                0,
                                sizeof(T) * b.count(),
                                data,
                                0,
                                NULL,
                                NULL);
        }

        operator cl_command_queue() const {
            return queue;
        }

    };
    
    
    // Currently just a context for all devices. We may refine this later.
    class context {
        cl_context ctx;

        static void sLogError(const char *errinfo,
                              const void *private_info,
                              size_t private_info_sz,
                              void *pThis);
        void logError(const char *errinfo,
                      const void *private_info,
                      size_t private_info_sz);        

    public:
        context(device_list &devices);
        ~context();
        
        program createProgramFromSourceFile(std::ifstream &input);
        program createProgramFromSourceFile(std::string filename);
        program createProgramFromSource(std::string source);
        
        command_queue createCommandQueue(cl_device_id dev);
        
        operator cl_context() const { return ctx; }

        template<typename T>
        buffer<T> createBuffer(size_t count, cl_mem_flags flags) {
            cl_mem mem = clCreateBuffer(ctx,
                                        flags,
                                        sizeof(T) * count, 
                                        NULL,
                                        NULL);
            return buffer<T>(mem, count, *this, flags);
        }

        template<typename T>
        buffer<T> createBuffer(size_t count, T* ptr, cl_mem_flags flags) {
            cl_mem mem = clCreateBuffer(ctx,
                                        flags | CL_MEM_USE_HOST_PTR,
                                        sizeof(T) * count, 
                                        ptr,
                                        NULL);
            return buffer<T>(mem, count, *this, flags);
        }        
    };

    template<typename T>
    buffer<T> &buffer<T>::operator=(const buffer<T> &rhs) {
        assert(ctx == rhs.ctx);

        clReleaseMemObject(mem);

        _count = rhs._count;
        flags = rhs.flags;
        mem = clCreateBuffer(ctx,
                             rhs.flags,
                             sizeof(T) * _count,
                             NULL,
                             NULL);
        return *this;
    }
}
