#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>

#include <vector>

#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#include <CL/cl.h>

struct floatexp { double val; int64_t exp; };

const char *error_string(int err) {
  switch (err) {
  case CL_SUCCESS:                            return 0;
  case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
  case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
  case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
  case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
  case CL_OUT_OF_RESOURCES:                   return "Out of resources";
  case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
  case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
  case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
  case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
  case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
  case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
  case CL_MAP_FAILURE:                        return "Map failure";
  case CL_INVALID_VALUE:                      return "Invalid value";
  case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
  case CL_INVALID_PLATFORM:                   return "Invalid platform";
  case CL_INVALID_DEVICE:                     return "Invalid device";
  case CL_INVALID_CONTEXT:                    return "Invalid context";
  case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
  case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
  case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
  case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
  case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
  case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
  case CL_INVALID_SAMPLER:                    return "Invalid sampler";
  case CL_INVALID_BINARY:                     return "Invalid binary";
  case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
  case CL_INVALID_PROGRAM:                    return "Invalid program";
  case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
  case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
  case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
  case CL_INVALID_KERNEL:                     return "Invalid kernel";
  case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
  case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
  case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
  case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
  case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
  case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
  case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
  case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
  case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
  case CL_INVALID_EVENT:                      return "Invalid event";
  case CL_INVALID_OPERATION:                  return "Invalid operation";
  case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
  case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
  case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
  default: return "Unknown";
  }
}

void error_print(int err, int loc) {
  if (err == CL_SUCCESS) { return; }
  fprintf(stderr, "CL ERROR: %d %s (%d)\n", err, error_string(err), loc);
  exit(1);
}

#define E(err) error_print(err, __LINE__)



typedef struct
{
  int32_t count;
  int32_t width;
  int32_t height;
  int32_t m_nX;
  int32_t m_nY;
  int32_t antal;
  int32_t nMaxiter;
  int32_t m_bNoGlitchDetection;
  int32_t m_nSmoothMethod;
  int32_t m_nPower;
  int32_t m_nMaxApproximation;
  int32_t m_nTerms;
  double m_nBailout;
  double m_nBailout2;
  double g_real;
  double g_imag;
  double g_FactorAR;
  double g_FactorAI;
  double m_C;
  double m_S;
} p_config;

struct clformula
{
  int type;
  int power;
  cl_kernel doublek;
  cl_kernel floatexpk;
};

std::vector<clformula> compile_kernels(cl_program program);

struct kfcl
{
  cl_context context;
  cl_device_id device_id;
  cl_command_queue commands;
  cl_program program;

  size_t ref_bytes;
  cl_mem refx;
  cl_mem refy;

  size_t ref_bytesz;
  cl_mem refz;

  size_t config_bytes;
  cl_mem config;

  size_t cx_bytes;
  cl_mem cx;

  size_t pixels_bytes;
  cl_mem pixels;

  size_t trans_bytes;
  cl_mem trans;

  std::vector<clformula> kernels;
};

kfcl initialize_opencl(cl_uint PLATFORM)
{
  cl_platform_id platform_id[64];
  cl_uint platform_ids;
  E(clGetPlatformIDs(64, &platform_id[0], &platform_ids));
  if (! (PLATFORM < platform_ids)) {
    fprintf(stderr, "invalid platform: %d (range is 0 to %d)\n", PLATFORM, platform_ids - 1);
    assert(! "valid platform");
  }
  for (cl_uint i = 0; i < platform_ids; ++i) {
    fprintf(stderr, "platform %d%s\n", i, i == PLATFORM ? " *** SELECTED ***" : "");
    char buf[1024];
    cl_platform_info info[5] =
      { CL_PLATFORM_PROFILE
      , CL_PLATFORM_VERSION
      , CL_PLATFORM_NAME
      , CL_PLATFORM_VENDOR
      , CL_PLATFORM_EXTENSIONS
      };
    for (int j = 0; j < 5; ++j) {
      buf[0] = 0;
      E(clGetPlatformInfo(platform_id[i], info[j], 1024, &buf[0], NULL));
      fprintf(stderr, "\t%s\n", buf);
    }
  }

  // create context
  cl_device_id device_id;
  E(clGetDeviceIDs(platform_id[PLATFORM], CL_DEVICE_TYPE_ALL, 1, &device_id, NULL));
  cl_context_properties properties[] =
    {
      CL_CONTEXT_PLATFORM, (cl_context_properties) platform_id[PLATFORM]
    , 0
    };
  cl_int err;
  cl_context context = clCreateContext(properties, 1, &device_id, NULL, NULL, &err);
  if (! context) { E(err); }
  cl_command_queue commands = clCreateCommandQueue(context, device_id, 0, &err);
  if (! commands) { E(err); }
  // build program
  const char *src = "#include \"kf.cl\"\n";
  cl_program program = clCreateProgramWithSource(context, 1, &src, 0, &err);
  if (! program) { E(err); }
  err = clBuildProgram(program, 1, &device_id, "-I. -cl-finite-math-only" /* -cl-no-signed-zeros" */, 0, 0);
  if (err != CL_SUCCESS) {
    char *buf = (char *) malloc(1000000);
    buf[0] = 0;
    E(clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 1000000, &buf[0], 0));
    fprintf(stderr, "build failed:\n%s\n", buf);
    free(buf);
    E(err);
  }

  cl_mem config = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(p_config), 0, &err);

  kfcl cl = { context, device_id, commands, program, 0, 0, 0, 0, 0, sizeof(p_config), config, 0, 0, 0, 0, 0, 0, compile_kernels(program) };
  return cl;
}

void upload_reference(kfcl &cl, const double *rx, const double *ry, const double *rz, size_t count)
{
  cl_int err;
  size_t bytes = sizeof(double) * count;
  if (bytes != cl.ref_bytes || bytes != cl.ref_bytesz)
  {
    if (cl.ref_bytes > 0)
    {
      clReleaseMemObject(cl.refx);
      clReleaseMemObject(cl.refy);
    }
    if (cl.ref_bytesz > 0)
    {
      clReleaseMemObject(cl.refz);
    }
    cl.refx = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytes, 0, &err);
    if (! cl.refx) { E(err); }
    cl.refy = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytes, 0, &err);
    if (! cl.refy) { E(err); }
    cl.refz = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytes, 0, &err);
    if (! cl.refz) { E(err); }
    cl.ref_bytes = bytes;
    cl.ref_bytesz = bytes;
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(cl.commands, cl.refx, CL_TRUE, 0, bytes, rx, 0, 0, 0));
  E(clEnqueueWriteBuffer(cl.commands, cl.refy, CL_TRUE, 0, bytes, ry, 0, 0, 0));
  E(clEnqueueWriteBuffer(cl.commands, cl.refz, CL_TRUE, 0, bytes, rz, 0, 0, 0));
}

void upload_reference(kfcl &cl, const floatexp *rx, const floatexp *ry, const double *rz, size_t count)
{
  cl_int err;
  size_t bytes = sizeof(floatexp) * count;
  size_t bytesz = sizeof(double) * count;
  if (bytes != cl.ref_bytes || bytesz != cl.ref_bytesz)
  {
    if (cl.ref_bytes > 0)
    {
      clReleaseMemObject(cl.refx);
      clReleaseMemObject(cl.refy);
    }
    if (cl.ref_bytesz > 0)
    {
      clReleaseMemObject(cl.refz);
    }
    cl.refx = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytes, 0, &err);
    if (! cl.refx) { E(err); }
    cl.refy = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytes, 0, &err);
    if (! cl.refy) { E(err); }
    cl.refz = clCreateBuffer(cl.context, CL_MEM_READ_ONLY, bytesz, 0, &err);
    if (! cl.refz) { E(err); }
    cl.ref_bytes = bytes;
    cl.ref_bytesz = bytesz;
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(cl.commands, cl.refx, CL_TRUE, 0, bytes, rx, 0, 0, 0));
  E(clEnqueueWriteBuffer(cl.commands, cl.refy, CL_TRUE, 0, bytes, ry, 0, 0, 0));
  E(clEnqueueWriteBuffer(cl.commands, cl.refz, CL_TRUE, 0, bytes, rz, 0, 0, 0));
}

void upload_config(kfcl &cl,
  int32_t count,
  int32_t width,
  int32_t height,
  int32_t m_nX,
  int32_t m_nY,
  int32_t antal,
  int32_t nMaxiter,
  int32_t m_bNoGlitchDetection,
  int32_t m_nSmoothMethod,
  int32_t m_nPower,
  int32_t m_nMaxApproximation,
  int32_t m_nTerms,
  double m_nBailout,
  double m_nBailout2,
  double g_real,
  double g_imag,
  double g_FactorAR,
  double g_FactorAI,
  double m_C,
  double m_S
)
{
  p_config config =
    {
      count,
      width,
      height,
      m_nX,
      m_nY,
      antal,
      nMaxiter,
      m_bNoGlitchDetection,
      m_nSmoothMethod,
      m_nPower,
      m_nMaxApproximation,
      m_nTerms,
      m_nBailout,
      m_nBailout2,
      g_real,
      g_imag,
      g_FactorAR,
      g_FactorAI,
      m_C,
      m_S
    };
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(cl.commands, cl.config, CL_TRUE, 0, sizeof(config), &config, 0, 0, 0));
}

#include "interface.inc"


int main(int argc, char **argv)
{
  if (argc > 1)
  {
    initialize_opencl(atoi(argv[1]));
  }
  return 0;
}
