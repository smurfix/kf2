#ifdef KF_OPENCL

#include <assert.h>
#include <math.h>
#include <string.h>

#include "../fraktal_sft/fraktal_sft.h"

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

std::vector<cldevice> initialize_opencl()
{
  std::vector<cldevice> devices;
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
      cldevice d = { platform_id[i], device_id[j], dname, vendor, version };
      devices.push_back(d);
    }
  }
  return devices;
}


OpenCL::OpenCL(cl_platform_id platform_id0, cl_device_id device_id0)
: platform_id(platform_id0)
, device_id(device_id0)
, context(0)
, commands(0)
, program(0)
, config_bytes(0)
, config(0)
, n1_bytes(0), n1(0)
, n0_bytes(0), n0(0)
, nf_bytes(0), nf(0)
, phase_bytes(0), phase(0)
, dex_bytes(0), dex(0)
, dey_bytes(0), dey(0)
{
  //memset(this, 0, sizeof(*this));
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
  config_bytes = sizeof(p_config);
  config = clCreateBuffer(context, CL_MEM_READ_ONLY, config_bytes, 0, &err); if (! config) { E(err); }
}

OpenCL::~OpenCL()
{
  WaitForSingleObject(mutex, INFINITE);
  // FIXME TODO
  ReleaseMutex(mutex);
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

template <typename T>
void OpenCL::run
(
  // for pixel -> parameter mapping
  int32_t m_nX,
  int32_t m_nY,
  uint32_t JitterSeed,
  int32_t JitterShape,
  double JitterScale,
  floatexp m_pixel_center_x,
  floatexp m_pixel_center_y,
  floatexp m_pixel_scale,
  double transform00,
  double transform01,
  double transform10,
  double transform11,
  // for result -> output mapping
  int64_t stride_y,
  int64_t stride_x,
  int64_t stride_offset,
  // for iteration control
  double m_nBailout,
  double m_nBailout2,
  double log_m_nPower,
  int64_t m_nGlitchIter,
  int64_t m_nMaxIter,
  int64_t nMaxIter,
  int64_t nMinIter,
  int16_t m_bNoGlitchDetection,
  int16_t m_bAddReference,
  int16_t m_nSmoothMethod,
  double g_real,
  double g_imag,
  double g_FactorAR,
  double g_FactorAI,
  double m_epsilon,
  // for series approximation
  double m_dPixelSpacing,
  floatexp m_fPixelSpacing,
  int64_t m_nMaxApproximation,
  int32_t m_nApproxTerms,
  int32_t approximation_type,
  floatexp *APr,
  floatexp *APi,
  SeriesR2 *APs,
 
  // reference orbit
  const T *rx,
  const T *ry,
  const double *rz,
  size_t roffset,
  size_t rcount,
 
  // formula selection
  int type,
  int m_nFractalType,
  int m_nPower,
  int16_t derivatives,

  // output arrays
  uint32_t *n1_p,
  uint32_t *n0_p,
  float *nf_p,
  float *phase_p,
  float *dex_p,
  float *dey_p
)
{
  assert(roffset < rcount);
  lock();

  // upload config
  p_config configdata =
    {
      sizeof(p_config),
      m_nX,
      m_nY,
      JitterSeed,
      JitterShape,
      JitterScale,
      m_pixel_center_x,
      m_pixel_center_y,
      m_pixel_scale,
      transform00,
      transform01,
      transform10,
      transform11,
      stride_y,
      stride_x,
      stride_offset,
      m_nBailout,
      m_nBailout2,
      log_m_nPower,
      m_nGlitchIter,
      m_nMaxIter,
      nMaxIter,
      nMinIter,
      m_bNoGlitchDetection,
      derivatives,
      m_bAddReference,
      m_nSmoothMethod,
      g_real,
      g_imag,
      g_FactorAR,
      g_FactorAI,
      m_epsilon,
      m_dPixelSpacing,
      m_fPixelSpacing,
      m_nMaxApproximation,
      m_nApproxTerms,
      approximation_type
      // arrays go here
    };
  const floatexp zero(0);
  for (int i = 0; i < MAX_APPROX_TERMS + 1; ++i)
  {
    configdata.m_APr[i] = APr ? APr[i] : zero;
    configdata.m_APi[i] = APi ? APi[i] : zero;
    for (int j = 0; j < MAX_APPROX_TERMS + 1; ++j)
    {
      configdata.m_APs_s[i][j] = APs ? APs->s[i][j] : zero;
      configdata.m_APs_t[i][j] = APs ? APs->t[i][j] : zero;
    }
  }
  cl_event config_uploaded;
  E(clEnqueueWriteBuffer(commands, config, CL_FALSE, 0, sizeof(configdata), &configdata, 0, 0, &config_uploaded));

  // upload reference
  cl_int err;
  int64_t ref_bytes  = sizeof(rx[0]) * (rcount - roffset);
  int64_t ref_bytesz = sizeof(rz[0]) * (rcount - roffset);
  cl_mem refx = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes,  nullptr, &err); if (! refx) { E(err); }
  cl_mem refy = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes,  nullptr, &err); if (! refy) { E(err); }
  cl_mem refz = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytesz, nullptr, &err); if (! refz) { E(err); }
  cl_event refx_uploaded, refy_uploaded, refz_uploaded;
  E(clEnqueueWriteBuffer(commands, refx, CL_FALSE, 0, ref_bytes,  &rx[roffset], 0, 0, &refx_uploaded));
  E(clEnqueueWriteBuffer(commands, refy, CL_FALSE, 0, ref_bytes,  &ry[roffset], 0, 0, &refy_uploaded));
  E(clEnqueueWriteBuffer(commands, refz, CL_FALSE, 0, ref_bytesz, &rz[roffset], 0, 0, &refz_uploaded));

  // reallocate output buffers if necessary
  {
    if (n1_p)
    {
      size_t bytes = sizeof(uint32_t) * m_nX * m_nY;
      if (n1_bytes != bytes)
      {
        if (n1_bytes)
        {
          clReleaseMemObject(n1);
        }
        n1_bytes = bytes;
        if (n1_bytes)
        {
          n1 = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! n1) { E(err); }
        }
      }
    }
    else
    {
      if (n1_bytes)
      {
        clReleaseMemObject(n1);
        n1_bytes = 0;
      }
    }
  
    if (n0_p)
    {
      size_t bytes = sizeof(uint32_t) * m_nX * m_nY;
      if (n0_bytes != bytes)
      {
        if (n0_bytes)
        {
          clReleaseMemObject(n0);
        }
        n0_bytes = bytes;
        if (n0_bytes)
        {
          n0 = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! n0) { E(err); }
        }
      }
    }
    else
    {
      if (n0_bytes)
      {
        clReleaseMemObject(n0);
        n0_bytes = 0;
      }
    }
  
    if (nf_p)
    {
      size_t bytes = sizeof(float) * m_nX * m_nY;
      if (nf_bytes != bytes)
      {
        if (nf_bytes)
        {
          clReleaseMemObject(nf);
        }
        nf_bytes = bytes;
        if (nf_bytes)
        {
          nf = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! nf) { E(err); }
        }
      }
    }
    else
    {
      if (nf_bytes)
      {
        clReleaseMemObject(nf);
        nf_bytes = 0;
      }
    }
  
    if (phase_p)
    {
      size_t bytes = sizeof(float) * m_nX * m_nY;
      if (phase_bytes != bytes)
      {
        if (phase_bytes)
        {
          clReleaseMemObject(phase);
        }
        phase_bytes = bytes;
        if (phase_bytes)
        {
          phase = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! phase) { E(err); }
        }
      }
    }
    else
    {
      if (phase_bytes)
      {
        clReleaseMemObject(phase);
        phase_bytes = 0;
      }
    }

    if (dex_p)
    {
      size_t bytes = sizeof(float) * m_nX * m_nY;
      if (dex_bytes != bytes)
      {
        if (dex_bytes)
        {
          clReleaseMemObject(dex);
        }
        dex_bytes = bytes;
        if (dex_bytes)
        {
          dex = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! dex) { E(err); }
        }
      }
    }
    else
    {
      if (dex_bytes)
      {
        clReleaseMemObject(dex);
        dex_bytes = 0;
      }
    }
  
    if (dey_p)
    {
      size_t bytes = sizeof(float) * m_nX * m_nY;
      if (dey_bytes != bytes)
      {
        if (dey_bytes)
        {
          clReleaseMemObject(dey);
        }
        dey_bytes = bytes;
        if (dey_bytes)
        {
          dey = clCreateBuffer(context, CL_MEM_READ_WRITE, bytes, nullptr, &err); if (! dey) { E(err); }
        }
      }
    }
    else
    {
      if (dey_bytes)
      {
        clReleaseMemObject(dey);
        dey_bytes = 0;
      }
    }
  }

  // upload buffers
  // FIXME synchronous, async would be better
  if (n1_p)  E(clEnqueueWriteBuffer(commands, n1,  CL_TRUE, 0, n1_bytes,  n1_p,  0, 0, 0));
  if (n0_p)  E(clEnqueueWriteBuffer(commands, n0,  CL_TRUE, 0, n0_bytes,  n0_p,  0, 0, 0));
  if (nf_p)  E(clEnqueueWriteBuffer(commands, nf,  CL_TRUE, 0, nf_bytes,  nf_p,  0, 0, 0));
  if (phase_p)E(clEnqueueWriteBuffer(commands,phase,CL_TRUE,0, phase_bytes,phase_p,0,0,0));
  if (dex_p) E(clEnqueueWriteBuffer(commands, dex, CL_TRUE, 0, dex_bytes, dex_p, 0, 0, 0));
  if (dey_p) E(clEnqueueWriteBuffer(commands, dey, CL_TRUE, 0, dey_bytes, dey_p, 0, 0, 0));
  
  // compile formula or retrieve kernel from cache
  cl_event formula_executed;
  clformula *formula = nullptr;
  for (int i = 0; i < (int) formulas.size(); ++i)
  {
    if (   formulas[i].type == type
        && formulas[i].fractalType == m_nFractalType
        && formulas[i].power == m_nPower
        && formulas[i].derivatives == derivatives
       )
    {
      formula = &formulas[i];
    }
  }
  if (formula == nullptr)
  {
    // build program
    std::string source = perturbation_opencl(m_nFractalType, m_nPower, derivatives);
    std::string name =
      type == 0 ? "perturbation_double"   :
      type == 2 ? "perturbation_floatexp" :
      "#error unknown type\n";
    const char *src = source.c_str();
    program = clCreateProgramWithSource(context, 1, &src, 0, &err);
    if (! program) { E(err); }
    // FIXME synchronous program building, async would be better
    std::ostringstream options;
    options << "-Werror -D DERIVATIVES=" << (derivatives ? 1 : 0)
            << " -D MAX_APPROX_TERMS=" << MAX_APPROX_TERMS;
    err = clBuildProgram(program, 1, &device_id, options.str().c_str(), 0, 0);
    if (err != CL_SUCCESS) {
      char *buf = (char *) malloc(1000000);
      buf[0] = 0;
      E(clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 1000000, &buf[0], 0));
      fprintf(stderr, "source:\n%s\nbuild failed:\n%s\n", source.c_str(), buf);
      free(buf);
      E(err);
    }
    cl_kernel kernel = clCreateKernel(program, name.c_str(), &err);
    if (! kernel) { E(err); }
    clformula newformula = { type, m_nFractalType, m_nPower, derivatives, kernel };
    formulas.push_back(newformula);
    formula = &formulas[ssize_t(formulas.size()) - 1];
  }
  assert(formula);

  // execute formula
  E(clSetKernelArg(formula->kernel, 0, sizeof(cl_mem), &config));
  E(clSetKernelArg(formula->kernel, 1, sizeof(cl_mem), &refx));
  E(clSetKernelArg(formula->kernel, 2, sizeof(cl_mem), &refy));
  E(clSetKernelArg(formula->kernel, 3, sizeof(cl_mem), &refz));
  E(clSetKernelArg(formula->kernel, 4, sizeof(cl_mem), n1_p  ? &n1  : nullptr));
  E(clSetKernelArg(formula->kernel, 5, sizeof(cl_mem), n0_p  ? &n0  : nullptr));
  E(clSetKernelArg(formula->kernel, 6, sizeof(cl_mem), nf_p  ? &nf  : nullptr));
  E(clSetKernelArg(formula->kernel, 7, sizeof(cl_mem), phase_p?&phase:nullptr));
  E(clSetKernelArg(formula->kernel, 8, sizeof(cl_mem), dex_p ? &dex : nullptr));
  E(clSetKernelArg(formula->kernel, 9, sizeof(cl_mem), dey_p ? &dey : nullptr));
  size_t global[2] = { (size_t) m_nY, (size_t) m_nX };
  cl_event uploaded[4] =
    { config_uploaded
    , refx_uploaded
    , refy_uploaded
    , refz_uploaded
    };
  E(clEnqueueNDRangeKernel(commands, formula->kernel, 2, nullptr, global, nullptr, 4, uploaded, &formula_executed));

  // download results
  // FIXME synchronous, async would be better
  if (n1_p)  E(clEnqueueReadBuffer(commands, n1,  CL_TRUE, 0, n1_bytes,  n1_p,  1, &formula_executed, 0));
  if (n0_p)  E(clEnqueueReadBuffer(commands, n0,  CL_TRUE, 0, n0_bytes,  n0_p,  1, &formula_executed, 0));
  if (nf_p)  E(clEnqueueReadBuffer(commands, nf,  CL_TRUE, 0, nf_bytes,  nf_p,  1, &formula_executed, 0));
  if (phase_p)E(clEnqueueReadBuffer(commands,phase,CL_TRUE,0, phase_bytes,phase_p,1,&formula_executed,0));
  if (dex_p) E(clEnqueueReadBuffer(commands, dex, CL_TRUE, 0, dex_bytes, dex_p, 1, &formula_executed, 0));
  if (dey_p) E(clEnqueueReadBuffer(commands, dey, CL_TRUE, 0, dey_bytes, dey_p, 1, &formula_executed, 0));

  // clean up reference
  clReleaseMemObject(refz);
  clReleaseMemObject(refy);
  clReleaseMemObject(refx);

  unlock();  
}

template void OpenCL::run<double>(
  // for pixel -> parameter mapping
  int32_t m_nX,
  int32_t m_nY,
  uint32_t JitterSeed,
  int32_t JitterShape,
  double JitterScale,
  floatexp m_pixel_center_x,
  floatexp m_pixel_center_y,
  floatexp m_pixel_scale,
  double transform00,
  double transform01,
  double transform10,
  double transform11,
  // for result -> output mapping
  int64_t stride_y,
  int64_t stride_x,
  int64_t stride_offset,
  // for iteration control
  double m_nBailout,
  double m_nBailout2,
  double log_m_nPower,
  int64_t m_nGlitchIter,
  int64_t m_nMaxIter,
  int64_t nMaxIter,
  int64_t nMinIter,
  int16_t m_bNoGlitchDetection,
  int16_t m_bAddReference,
  int16_t m_nSmoothMethod,
  double g_real,
  double g_imag,
  double g_FactorAR,
  double g_FactorAI,
  double m_epsilon,
  // for series approximation
  double m_dPixelSpacing,
  floatexp m_fPixelSpacing,
  int64_t m_nMaxApproximation,
  int32_t m_nApproxTerms,
  int32_t approximation_type,
  floatexp *APr,
  floatexp *APi,
  SeriesR2 *APs,
 
  // reference orbit
  const double *rx,
  const double *ry,
  const double *rz,
  size_t roffset,
  size_t rcount,
 
  // formula selection
  int type,
  int m_nFractalType,
  int m_nPower,
  int16_t derivatives,

  // output arrays
  uint32_t *n1_p,
  uint32_t *n0_p,
  float *nf_p,
  float *phase_p,
  float *dex_p,
  float *dey_p
);

template void OpenCL::run<floatexp>(
  // for pixel -> parameter mapping
  int32_t m_nX,
  int32_t m_nY,
  uint32_t JitterSeed,
  int32_t JitterShape,
  double JitterScale,
  floatexp m_pixel_center_x,
  floatexp m_pixel_center_y,
  floatexp m_pixel_scale,
  double transform00,
  double transform01,
  double transform10,
  double transform11,
  // for result -> output mapping
  int64_t stride_y,
  int64_t stride_x,
  int64_t stride_offset,
  // for iteration control
  double m_nBailout,
  double m_nBailout2,
  double log_m_nPower,
  int64_t m_nGlitchIter,
  int64_t m_nMaxIter,
  int64_t nMaxIter,
  int64_t nMinIter,
  int16_t m_bNoGlitchDetection,
  int16_t m_bAddReference,
  int16_t m_nSmoothMethod,
  double g_real,
  double g_imag,
  double g_FactorAR,
  double g_FactorAI,
  double m_epsilon,
  // for series approximation
  double m_dPixelSpacing,
  floatexp m_fPixelSpacing,
  int64_t m_nMaxApproximation,
  int32_t m_nApproxTerms,
  int32_t approximation_type,
  floatexp *APr,
  floatexp *APi,
  SeriesR2 *APs,
 
  // reference orbit
  const floatexp *rx,
  const floatexp *ry,
  const double *rz,
  size_t roffset,
  size_t rcount,
 
  // formula selection
  int type,
  int m_nFractalType,
  int m_nPower,
  int16_t derivatives,

  // output arrays
  uint32_t *n1_p,
  uint32_t *n0_p,
  float *nf_p,
  float *phase_p,
  float *dex_p,
  float *dey_p
);

#endif
