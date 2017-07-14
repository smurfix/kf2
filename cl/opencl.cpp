#ifdef KF_OPENCL
#include <assert.h>
#include <math.h>
#include <string.h>

#include "opencl.h"

extern const char *kf_opencl_source;

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
  E(clGetPlatformIDs(64, &platform_id[0], &platform_ids));
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
{
  memset(this, 0, sizeof(*this));
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
  // build program
  program = clCreateProgramWithSource(context, 1, &kf_opencl_source, 0, &err);
  if (! program) { E(err); }
  // FIXME synchronous program building, async would be better
  err = clBuildProgram(program, 1, &device_id, "-Werror", 0, 0);
  if (err != CL_SUCCESS) {
    char *buf = (char *) malloc(1000000);
    buf[0] = 0;
    E(clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 1000000, &buf[0], 0));
    fprintf(stderr, "build failed:\n%s\n", buf);
    free(buf);
    E(err);
  }

  config_bytes = sizeof(p_config);
  config = clCreateBuffer(context, CL_MEM_READ_ONLY, config_bytes, 0, &err);

  formulas = compile_kernels(program);
  approxdoublek = clCreateKernel(program, "series_approximation_double", &err);
  if (! approxdoublek) { E(err); }
  approxfloatexpk = clCreateKernel(program, "series_approximation_floatexp", &err);
  if (! approxfloatexpk) { E(err); }
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

void OpenCL::upload_reference(const double *rx, const double *ry, const double *rz, size_t count)
{
  cl_int err;
  size_t bytes = sizeof(double) * count;
  if (bytes != ref_bytes || bytes != ref_bytesz)
  {
    if (ref_bytes > 0)
    {
      clReleaseMemObject(refx);
      clReleaseMemObject(refy);
    }
    if (ref_bytesz > 0)
    {
      clReleaseMemObject(refz);
    }
    ref_bytes = bytes;
    ref_bytesz = bytes;
    refx = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes, 0, &err);
    if (! refx) { E(err); }
    refy = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes, 0, &err);
    if (! refy) { E(err); }
    refz = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes, 0, &err);
    if (! refz) { E(err); }
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(commands, refx, CL_TRUE, 0, bytes, rx, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, refy, CL_TRUE, 0, bytes, ry, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, refz, CL_TRUE, 0, bytes, rz, 0, 0, 0));
}

void OpenCL::upload_reference(const floatexp *rx, const floatexp *ry, const double *rz, size_t count)
{
  WaitForSingleObject(mutex, INFINITE);
  cl_int err;
  size_t bytes = sizeof(floatexp) * count;
  size_t bytesz = sizeof(double) * count;
  if (bytes != ref_bytes || bytesz != ref_bytesz)
  {
    if (ref_bytes > 0)
    {
      clReleaseMemObject(refx);
      clReleaseMemObject(refy);
    }
    if (ref_bytesz > 0)
    {
      clReleaseMemObject(refz);
    }
    ref_bytes = bytes;
    ref_bytesz = bytesz;
    refx = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes, 0, &err);
    if (! refx) { E(err); }
    refy = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytes, 0, &err);
    if (! refy) { E(err); }
    refz = clCreateBuffer(context, CL_MEM_READ_ONLY, ref_bytesz, 0, &err);
    if (! refz) { E(err); }
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(commands, refx, CL_TRUE, 0, bytes, rx, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, refy, CL_TRUE, 0, bytes, ry, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, refz, CL_TRUE, 0, bytes, rz, 0, 0, 0));
}

void OpenCL::upload_config(
  int32_t count,
  int32_t width,
  int32_t height,
  int32_t m_nX,
  int32_t m_nY,
  int32_t antal,
  int32_t m_nMaxIter,
  int32_t m_nGlitchIter,
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
  double m_S,
  int32_t m_bAddReference
)
{
  p_config configdata =
    {
      count,
      width,
      height,
      m_nX,
      m_nY,
      antal,
      m_nMaxIter,
      m_nGlitchIter,
      m_bNoGlitchDetection,
      m_nSmoothMethod,
      m_nPower,
      m_nMaxApproximation,
      m_nTerms,
      m_bAddReference,
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
  E(clEnqueueWriteBuffer(commands, config, CL_TRUE, 0, sizeof(configdata), &configdata, 0, 0, 0));
}

void OpenCL::upload_approximation(const double *m_pDX, size_t m_nX, const double *m_pDY, size_t m_nY, const floatexp *m_APr, const floatexp *m_APi, size_t m_nTerms)
{
  cl_int err;
  size_t bytesx = sizeof(double) * m_nX;
  size_t bytesy = sizeof(double) * m_nY;
  size_t bytesa = sizeof(floatexp) * m_nTerms;
  size_t bytesc = sizeof(double) * 4 * m_nX * m_nY;
  size_t bytesp = sizeof(int) * m_nX * m_nY;
  size_t bytest = sizeof(float) * m_nX * m_nY;
  if (bytesx != dx_bytes || bytesy != dy_bytes || bytesa != ap_bytes || bytesc != cx_bytes || bytesp != pixels_bytes || bytest != trans_bytes)
  {
    if (dx_bytes > 0)
    {
      clReleaseMemObject(dx);
    }
    if (dy_bytes > 0)
    {
      clReleaseMemObject(dy);
    }
    if (ap_bytes > 0)
    {
      clReleaseMemObject(apr);
      clReleaseMemObject(api);
    }
    if (cx_bytes > 0)
    {
      clReleaseMemObject(cx);
    }
    if (pixels_bytes > 0)
    {
      clReleaseMemObject(pixels);
    }
    if (trans_bytes > 0)
    {
      clReleaseMemObject(trans);
    }
    dx_bytes = bytesx;
    dy_bytes = bytesy;
    ap_bytes = bytesa;
    cx_bytes = bytesc;
    pixels_bytes = bytesp;
    trans_bytes = bytest;
    dx = clCreateBuffer(context, CL_MEM_READ_ONLY, dx_bytes, 0, &err);
    if (! dx) { E(err); }
    dy = clCreateBuffer(context, CL_MEM_READ_ONLY, dy_bytes, 0, &err);
    if (! dy) { E(err); }
    apr = clCreateBuffer(context, CL_MEM_READ_ONLY, ap_bytes, 0, &err);
    if (! apr) { E(err); }
    api = clCreateBuffer(context, CL_MEM_READ_ONLY, ap_bytes, 0, &err);
    if (! api) { E(err); }
    cx = clCreateBuffer(context, CL_MEM_READ_WRITE, cx_bytes, 0, &err);
    if (! cx) { E(err); }
    pixels = clCreateBuffer(context, CL_MEM_READ_WRITE, pixels_bytes, 0, &err);
    if (! pixels) { E(err); }
    trans = clCreateBuffer(context, CL_MEM_READ_WRITE, trans_bytes, 0, &err);
    if (! trans) { E(err); }
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(commands, dx, CL_TRUE, 0, dx_bytes, m_pDX, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, dy, CL_TRUE, 0, dy_bytes, m_pDY, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, apr, CL_TRUE, 0, ap_bytes, m_APr, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, api, CL_TRUE, 0, ap_bytes, m_APi, 0, 0, 0));
}

void OpenCL::upload_approximation(const floatexp *m_DX, size_t m_nX, const floatexp *m_DY, size_t m_nY, const floatexp *m_APr, const floatexp *m_APi, size_t m_nTerms)
{
  cl_int err;
  size_t bytesx = sizeof(floatexp) * m_nX;
  size_t bytesy = sizeof(floatexp) * m_nY;
  size_t bytesa = sizeof(floatexp) * m_nTerms;
  size_t bytesc = sizeof(floatexp) * 4 * m_nX * m_nY;
  size_t bytesp = sizeof(int) * m_nX * m_nY;
  size_t bytest = sizeof(float) * m_nX * m_nY;
  if (bytesx != dx_bytes || bytesy != dy_bytes || bytesa != ap_bytes || bytesc != cx_bytes || bytesp != pixels_bytes || bytest != trans_bytes)
  {
    if (dx_bytes > 0)
    {
      clReleaseMemObject(dx);
    }
    if (dy_bytes > 0)
    {
      clReleaseMemObject(dy);
    }
    if (ap_bytes > 0)
    {
      clReleaseMemObject(apr);
      clReleaseMemObject(api);
    }
    if (cx_bytes > 0)
    {
      clReleaseMemObject(cx);
    }
    if (pixels_bytes > 0)
    {
      clReleaseMemObject(pixels);
    }
    if (trans_bytes > 0)
    {
      clReleaseMemObject(trans);
    }
    dx_bytes = bytesx;
    dy_bytes = bytesy;
    ap_bytes = bytesa;
    cx_bytes = bytesc;
    pixels_bytes = bytesp;
    trans_bytes = bytest;
    dx = clCreateBuffer(context, CL_MEM_READ_ONLY, dx_bytes, 0, &err);
    if (! dx) { E(err); }
    dy = clCreateBuffer(context, CL_MEM_READ_ONLY, dy_bytes, 0, &err);
    if (! dy) { E(err); }
    apr = clCreateBuffer(context, CL_MEM_READ_ONLY, ap_bytes, 0, &err);
    if (! apr) { E(err); }
    api = clCreateBuffer(context, CL_MEM_READ_ONLY, ap_bytes, 0, &err);
    if (! api) { E(err); }
    cx = clCreateBuffer(context, CL_MEM_READ_WRITE, cx_bytes, 0, &err);
    if (! cx) { E(err); }
    pixels = clCreateBuffer(context, CL_MEM_READ_WRITE, pixels_bytes, 0, &err);
    if (! pixels) { E(err); }
    trans = clCreateBuffer(context, CL_MEM_READ_WRITE, trans_bytes, 0, &err);
    if (! trans) { E(err); }
  }
  // FIXME synchronous upload to device, async would be better
  E(clEnqueueWriteBuffer(commands, dx, CL_TRUE, 0, dx_bytes, m_DX, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, dy, CL_TRUE, 0, dy_bytes, m_DY, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, apr, CL_TRUE, 0, ap_bytes, m_APr, 0, 0, 0));
  E(clEnqueueWriteBuffer(commands, api, CL_TRUE, 0, ap_bytes, m_APi, 0, 0, 0));
}

void OpenCL::execute_approximation(int type, cl_int count)
{
  if (type == 0)
  {
    size_t local, global;
    E(clSetKernelArg(approxdoublek, 0, sizeof(cl_mem), &config));
    E(clSetKernelArg(approxdoublek, 1, sizeof(cl_mem), &dx));
    E(clSetKernelArg(approxdoublek, 2, sizeof(cl_mem), &dy));
    E(clSetKernelArg(approxdoublek, 3, sizeof(cl_mem), &apr));
    E(clSetKernelArg(approxdoublek, 4, sizeof(cl_mem), &api));
    E(clSetKernelArg(approxdoublek, 5, sizeof(cl_mem), &cx));
    E(clSetKernelArg(approxdoublek, 6, sizeof(cl_mem), &pixels));
    E(clSetKernelArg(approxdoublek, 7, sizeof(cl_mem), &trans));
    E(clGetKernelWorkGroupInfo(approxdoublek, device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, 0));
    global = ((count + local - 1)/local) * local;
    cl_event ev;
    E(clEnqueueNDRangeKernel(commands, approxdoublek, 1, 0, &global, &local, 0, 0, &ev));
    clWaitForEvents(1, &ev); // FIXME should be async
  }
  else
  {
    size_t local, global;
    E(clSetKernelArg(approxfloatexpk, 0, sizeof(cl_mem), &config));
    E(clSetKernelArg(approxfloatexpk, 1, sizeof(cl_mem), &dx));
    E(clSetKernelArg(approxfloatexpk, 2, sizeof(cl_mem), &dy));
    E(clSetKernelArg(approxfloatexpk, 3, sizeof(cl_mem), &apr));
    E(clSetKernelArg(approxfloatexpk, 4, sizeof(cl_mem), &api));
    E(clSetKernelArg(approxfloatexpk, 5, sizeof(cl_mem), &cx));
    E(clSetKernelArg(approxfloatexpk, 6, sizeof(cl_mem), &pixels));
    E(clSetKernelArg(approxfloatexpk, 7, sizeof(cl_mem), &trans));
    E(clGetKernelWorkGroupInfo(approxfloatexpk, device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, 0));
    global = ((count + local - 1)/local) * local;
    cl_event ev;
    E(clEnqueueNDRangeKernel(commands, approxfloatexpk, 1, 0, &global, &local, 0, 0, &ev));
    clWaitForEvents(1, &ev); // FIXME should be async
  }
}

void OpenCL::execute_formula(int type, int m_nFractalType, int m_nPower, cl_int count)
{
  for (int i = 0; i < formulas.size(); ++i)
  {
    if (formulas[i].type == m_nFractalType && formulas[i].power == m_nPower)
    {
      if (type == 0)
      {
        size_t local, global;
        E(clSetKernelArg(formulas[i].doublek, 0, sizeof(cl_mem), &refx));
        E(clSetKernelArg(formulas[i].doublek, 1, sizeof(cl_mem), &refy));
        E(clSetKernelArg(formulas[i].doublek, 2, sizeof(cl_mem), &refz));
        E(clSetKernelArg(formulas[i].doublek, 3, sizeof(cl_mem), &config));
        E(clSetKernelArg(formulas[i].doublek, 4, sizeof(cl_mem), &cx));
        E(clSetKernelArg(formulas[i].doublek, 5, sizeof(cl_mem), &pixels));
        E(clSetKernelArg(formulas[i].doublek, 6, sizeof(cl_mem), &trans));
        E(clGetKernelWorkGroupInfo(formulas[i].doublek, device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, 0));
        global = ((count + local - 1)/local) * local;
        cl_event ev;
        E(clEnqueueNDRangeKernel(commands, formulas[i].doublek, 1, 0, &global, &local, 0, 0, &ev));
        clWaitForEvents(1, &ev); // FIXME should be async
      }
      else
      {
        size_t local, global;
        E(clSetKernelArg(formulas[i].floatexpk, 0, sizeof(cl_mem), &refx));
        E(clSetKernelArg(formulas[i].floatexpk, 1, sizeof(cl_mem), &refy));
        E(clSetKernelArg(formulas[i].floatexpk, 2, sizeof(cl_mem), &refz));
        E(clSetKernelArg(formulas[i].floatexpk, 3, sizeof(cl_mem), &config));
        E(clSetKernelArg(formulas[i].floatexpk, 4, sizeof(cl_mem), &cx));
        E(clSetKernelArg(formulas[i].floatexpk, 5, sizeof(cl_mem), &pixels));
        E(clSetKernelArg(formulas[i].floatexpk, 6, sizeof(cl_mem), &trans));
        E(clGetKernelWorkGroupInfo(formulas[i].floatexpk, device_id, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, 0));
        global = ((count + local - 1)/local) * local;
        cl_event ev;
        E(clEnqueueNDRangeKernel(commands, formulas[i].floatexpk, 1, 0, &global, &local, 0, 0, &ev));
        clWaitForEvents(1, &ev); // FIXME should be async
      }
      return;
    }
  }
  assert(! "found kernel in execute_formula");
}

void OpenCL::download_iterations(int **m_nPixels, float **m_nTrans, size_t m_nX, size_t m_nY)
{
  for (int x = 0; x < m_nX; ++x)
  {
    // FIXME synchronous download from device, async would be better
    E(clEnqueueReadBuffer(commands, pixels, CL_TRUE, x * m_nY * sizeof(int),   m_nY * sizeof(int),   m_nPixels[x], 0, 0, 0));
    E(clEnqueueReadBuffer(commands, trans,  CL_TRUE, x * m_nY * sizeof(float), m_nY * sizeof(float), m_nTrans[x],  0, 0, 0));
  }
}

#include "opencl.inc"
#endif
