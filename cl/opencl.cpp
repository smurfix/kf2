#ifdef KF_OPENCL

#include <assert.h>
#include <math.h>
#include <string.h>
#include <cstring>
#include <iostream>

#include "../fraktal_sft/fraktal_sft.h"
#include "../fraktal_sft/main.h"

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

std::string g_OpenCL_Error_Source = "";
std::string g_OpenCL_Error_Log = "";
std::string g_OpenCL_Error_Message = "";
std::string g_OpenCL_Error_Line = "";

void error_print(int err, int loc) {
  if (err == CL_SUCCESS) { return; }
  g_OpenCL_Error_Message = error_string(err);
  std::ostringstream s;
  s << loc;
  g_OpenCL_Error_Line = s.str();
  throw OpenCLException();
}

#define E(err) error_print(err, __LINE__)

std::vector<cldevice> initialize_opencl(HWND hWnd)
{
  std::vector<cldevice> devices;
  try
  {

  int ok = clewInit();
  if (ok != CLEW_SUCCESS)
  {
    return devices;
  }

  cl_platform_id platform_id[64];
  cl_uint platform_ids;
  int err = clGetPlatformIDs(64, &platform_id[0], &platform_ids);
  if (err != CL_SUCCESS)
  {
    return devices;
  }
  char buf[1024];
  for (cl_uint i = 0; i < platform_ids; ++i) {
    buf[0] = 0;
    E(clGetPlatformInfo(platform_id[i], CL_PLATFORM_VERSION, 1024, &buf[0], 0));
    std::string version(buf);
    buf[0] = 0;
    E(clGetPlatformInfo(platform_id[i], CL_PLATFORM_VENDOR, 1024, &buf[0], 0));
    std::string vendor(buf);

    cl_device_id device_id[64];
    cl_uint device_ids;
    E(clGetDeviceIDs(platform_id[i], CL_DEVICE_TYPE_ALL, 64, &device_id[0], &device_ids));
    for (cl_uint j = 0; j < device_ids; ++j)
    {
      buf[0] = 0;
      E(clGetDeviceInfo(device_id[j], CL_DEVICE_NAME, 1024, &buf[0], 0));
      std::string dname(buf);
      buf[0] = 0;
      cl_uint dvecsize = 0;
      cl_int status = clGetDeviceInfo(device_id[j], CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE, sizeof(dvecsize), &dvecsize, 0);
      if (status != CL_SUCCESS)
      {
        dvecsize = 0;
      }
      cldevice d = { platform_id[i], device_id[j], dname, vendor, version, dvecsize > 0 };
      devices.push_back(d);
    }
  }

  }
  catch (OpenCLException &e)
  {
    OpenCLErrorDialog(hWnd, false);
  }
  return devices;
}


OpenCL::OpenCL(cl_platform_id platform_id0, cl_device_id device_id0, bool supports_double)
: platform_id(platform_id0)
, device_id(device_id0)
, supports_double(supports_double)
, context(0)
, commands(0)
, program(0)
, config(0)
, refx_bytes(0), refx(0)
, refy_bytes(0), refy(0)
, refz_bytes(0), refz(0)
, refN_bytes(0), refN(0)
, refX_bytes(0), refX(0)
, refY_bytes(0), refY(0)
, refZ_bytes(0), refZ(0)
, n1_bytes(0), n1(0)
, n0_bytes(0), n0(0)
, nf_bytes(0), nf(0)
, phase_bytes(0), phase(0)
, dex_bytes(0), dex(0)
, dey_bytes(0), dey(0)
, glitch_f_bytes(0), glitch_f(0)
, glitch_x_bytes(0), glitch_x(0)
, counts_bytes(0), counts(0)
, glitch_out_bytes(0), glitch_out(0)

{
  mutex = CreateMutex(0,0,0);
  platform_id = platform_id0;
  device_id = device_id0;
  // create context
  cl_context_properties properties[] =
    {
      CL_CONTEXT_PLATFORM, (cl_context_properties) platform_id
    , 0
    };
  cl_int err;
  context = clCreateContext(properties, 1, &device_id, NULL, NULL, &err);
  if (! context) { E(err); }
  commands = clCreateCommandQueue(context, device_id, 0, &err);
  if (! commands) { E(err); }
  size_t max_config_bytes = supports_double ? sizeof(p_config<double, int64_t>) : sizeof(p_config<float, int32_t>);
  config = clCreateBuffer(context, CL_MEM_READ_ONLY, max_config_bytes, 0, &err); if (! config) { E(err); }
}

OpenCL::~OpenCL()
{
  lock();
  if (config) { clReleaseMemObject(config); config = 0; }
  if (refx) { clReleaseMemObject(refx); refx = 0; }
  if (refy) { clReleaseMemObject(refy); refy = 0; }
  if (refz) { clReleaseMemObject(refz); refz = 0; }
  if (refN) { clReleaseMemObject(refN); refN = 0; }
  if (refX) { clReleaseMemObject(refX); refX = 0; }
  if (refY) { clReleaseMemObject(refY); refY = 0; }
  if (refZ) { clReleaseMemObject(refZ); refZ = 0; }
  if (n1) { clReleaseMemObject(n1); n1 = 0; }
  if (n0) { clReleaseMemObject(n0); n0 = 0; }
  if (nf) { clReleaseMemObject(nf); nf = 0; }
  if (phase) { clReleaseMemObject(phase); phase = 0; }
  if (dex) { clReleaseMemObject(dex); dex = 0; }
  if (dey) { clReleaseMemObject(dey); dey = 0; }
  if (glitch_f) { clReleaseMemObject(glitch_f); glitch_f = 0; }
  if (glitch_x) { clReleaseMemObject(glitch_x); glitch_x = 0; }
  if (counts) { clReleaseMemObject(counts); counts = 0; }
  if (glitch_out) { clReleaseMemObject(glitch_out); glitch_out = 0; }
  if (commands) { clReleaseCommandQueue(commands); commands = 0; }
  if (context) { clReleaseContext(context); context = 0; }
  unlock();
}

void OpenCL::lock()
{
  WaitForSingleObject(mutex, INFINITE);
}

void OpenCL::unlock()
{
  ReleaseMutex(mutex);
}

struct softfloat
{
  uint32_t se;
  uint32_t m;
};
#define SF_EXPONENT_BIAS ((1U << 30U) - 1U)
#define SF_MANTISSA_BITS 32

bool sf_sign_bit(const softfloat f)
{
  return !!(f.se & 0x80000000U);
}

uint32_t sf_biased_exponent(const softfloat f)
{
  return f.se & 0x7FFFFFFFU;
}

uint32_t sf_mantissa(const softfloat f)
{
  return f.m;
}

bool sf_is_zero(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0 &&
    sf_mantissa(f) == 0;
}

bool sf_is_denormal(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0 &&
    sf_mantissa(f) != 0;
}

bool sf_is_inf(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0x7FFFFFFFU &&
    sf_mantissa(f) == 0;
}

bool sf_is_nan(const softfloat f)
{
  return
    sf_biased_exponent(f) == 0x7FFFFFFFU &&
    sf_mantissa(f) != 0;
}

softfloat sf_from_double(const double x)
{
  if (isnan(x))
  {
    softfloat f = { ((uint32_t)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0xFFFFFFFFU };
    return f;
  }
  else if (isinf(x))
  {
    softfloat f = { ((uint32_t)(!!signbit(x)) << 31) | 0x7FFFFFFFU, 0U };
    return f;
  }
  else if (x == 0.0)
  {
    softfloat f = { ((uint32_t)(!!signbit(x)) << 31) | 0U, 0U };
    return f;
  }
  else
  {
    int e;
    double y = frexp(fabs(x), &e);
    double z = ldexp(y, SF_MANTISSA_BITS);
    uint32_t mantissa = (uint32_t) trunc(z); // rounding might overflow rarely
    uint32_t biased_e = e + SF_EXPONENT_BIAS; // always in range?
    assert(0 < biased_e);
    assert(biased_e < 0x7FFFFFFFU);
    assert((mantissa >> (SF_MANTISSA_BITS - 1)) == 1U);
    softfloat f = { ((uint32_t)(!!signbit(x)) << 31) | biased_e, mantissa };
    return f;
  }
}

softfloat sf_ldexp(const softfloat a, int e)
{
  if (sf_is_zero(a) || sf_is_inf(a) || sf_is_nan(a))
  {
    return a;
  }
  else if (e >= (int32_t)(0x7FFFFFFFU - sf_biased_exponent(a)))
  {
    // overflow to +/-infinity
    softfloat o = { (a.se & 0x80000000U) | 0x7FFFFFFFU, 0U };
    return o;
  }
  else if ((int)(sf_biased_exponent(a)) + e <= 0)
  {
    // underfloat to 0
    softfloat o = { (a.se & 0x80000000U) | 0U, 0U };
    return o;
  }
  else
  {
    softfloat o = { (a.se & 0x80000000U) | (sf_biased_exponent(a) + e), sf_mantissa(a) };
    return o;
  }
}

softfloat sf_from_floatexp(const floatexp x)
{
  return sf_ldexp(sf_from_double(x.val), x.exp < INT_MIN ? INT_MIN : x.exp > INT_MAX ? INT_MAX : x.exp);
}

softfloat sf_softfloat(const floatexp f)
{
  return sf_from_floatexp(f);
}

softfloat sf_softfloat(const double f)
{
  return sf_from_double(f);
}

extern const char *perturbation_decl_float;
extern const char *perturbation_decl_double;
extern const char *perturbation_scaled_loop_empty;
extern const char *perturbation_opencl_common;
extern const char *perturbation_opencl_double_pre;
extern const char *perturbation_opencl_double_pre_m;
extern const char *perturbation_opencl_double_post;
extern const char *perturbation_opencl_double_post_m;
extern const char *perturbation_opencl_floatexp_pre;
extern const char *perturbation_opencl_floatexp_pre_m;
extern const char *perturbation_opencl_floatexp_post;
extern const char *perturbation_opencl_floatexp_post_m;
#if 0
extern const char *perturbation_opencl_softfloat_pre;
extern const char *perturbation_opencl_softfloat_post;
extern const char *perturbation_opencl_softfloat_post_m;
#endif

extern std::string perturbation_opencl(const hybrid_formula &h, int derivatives, bool single, bool scaled)
{
  std::ostringstream o;
  if (single)
  {
    o << perturbation_decl_float;
  }
  else
  {
    o << perturbation_decl_double;
  }
  o << perturbation_opencl_common;
  if (scaled)
  {
    o << hybrid_perturbation_scaled_opencl(h, derivatives);
  }
  else
  {
  // double
  if (derivatives)
  {
    o << perturbation_opencl_double_pre_m;
  }
  else
  {
    o << perturbation_opencl_double_pre;
  }
  o << hybrid_perturbation_double_opencl(h, derivatives);
  if (derivatives)
  {
    o << perturbation_opencl_double_post_m;
  }
  else
  {
    o << perturbation_opencl_double_post;
  }
  // scaled
  o << perturbation_scaled_loop_empty;
  // floatexp
  if (derivatives)
  {
    o << perturbation_opencl_floatexp_pre_m;
  }
  else
  {
    o << perturbation_opencl_floatexp_pre;
  }
  o << hybrid_perturbation_floatexp_opencl(h, derivatives);
  if (derivatives)
  {
    o << perturbation_opencl_floatexp_post_m;
  }
  else
  {
    o << perturbation_opencl_floatexp_post;
  }
#if 0
  // softfloat
  o << perturbation_opencl_softfloat_pre;
//  o << hybrid_perturbation_softfloat_opencl(h, derivatives); // FIXME TODO
  if (derivatives)
  {
    o << perturbation_opencl_softfloat_post_m;
  }
  else
  {
    o << perturbation_opencl_softfloat_post;
  }
#endif
  }
  return o.str();
}

#endif
