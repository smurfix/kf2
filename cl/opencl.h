#ifdef KF_OPENCL
#ifndef KF_OPENCL_H
#define KF_OPENCL_H 1

#include <windows.h>
#include <stdint.h>
#include <string>
#include <vector>
#ifndef WINVER
#include <mutex>
#endif

#include "../fraktal_sft/defs.h"
#include "../fraktal_sft/floatexp.h"
#include "../fraktal_sft/hybrid.h"

#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#include "clew.h"

extern std::string perturbation_opencl
( int m_nFractalType
, int m_nPower
, int derivatives
, int scaled
, int single
);

extern std::string perturbation_opencl
( const hybrid_formula &hybrid
, int derivatives
, bool single
, bool scaled
);

template <typename mantissa, typename exponent>
struct p_config
{
  cl_long BYTES;
  // for pixel -> parameter mapping
  cl_int m_nX;
  cl_int m_nY;
  cl_uint JitterSeed;
  cl_int JitterShape;
  mantissa JitterScale;
  tfloatexp<mantissa, exponent> m_pixel_center_x;
  tfloatexp<mantissa, exponent> m_pixel_center_y;
  tfloatexp<mantissa, exponent> m_pixel_scale;
  mantissa transform00;
  mantissa transform01;
  mantissa transform10;
  mantissa transform11;
  cl_long ExponentialMap;
  // for result -> output mapping
  cl_long stride_y;
  cl_long stride_x;
  cl_long stride_offset;
  // for iteration control
  mantissa m_nBailout;
  mantissa m_nBailout2;
  mantissa log_m_nBailout;
  mantissa log_m_nPower;
  cl_long m_nMaxIter;
  cl_long m_nRSize;
  cl_long nMaxIter;
  cl_long reference_size_x;
  cl_short m_bNoGlitchDetection;
  cl_short derivatives;
  cl_short m_bAddReference;
  cl_short m_nSmoothMethod;
  mantissa g_real;
  mantissa g_imag;
  mantissa norm_p;
  mantissa g_FactorAR;
  mantissa g_FactorAI;
  mantissa m_epsilon;
  // for series approximation
  cl_long m_nMaxApproximation;
  cl_int m_nApproxTerms;
  cl_int approximation_type;
  // for guessing
  cl_int UseGuessing;
  cl_int GuessingPass;
  cl_int g_nAddRefX;
  cl_int g_nAddRefY;
  // for glitch selection
  cl_short singleref;
  cl_short glitch_select_argminz;
  // for ignore isolated glitches
  cl_int ignore_isolated_neighbourhood; // 4 or 8
  // for hybrid
  cl_short hybrid_loop_start;
  cl_short hybrid_nstanzas;
  cl_int hybrid_repeats[MAX_HYBRID_STANZAS];
  mantissa hybrid_log_powers[MAX_HYBRID_STANZAS];
  // 130kB data follows
  tfloatexp<mantissa, exponent> m_APr[MAX_APPROX_TERMS + 1];
  tfloatexp<mantissa, exponent> m_APi[MAX_APPROX_TERMS + 1];
  tfloatexp<mantissa, exponent> m_APs_s[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
  tfloatexp<mantissa, exponent> m_APs_t[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
} __attribute__((packed));

struct p_glitch
{
  cl_long count;
  cl_int x;
  cl_int y;
  cl_float f;
} __attribute__((packed));

struct cldevice
{
  cl_platform_id pid;
  cl_device_id did;
  std::string name;
  std::string vendor;
  std::string version;
  bool supports_double;
};

struct clformula
{
  Reference_Type reftype;
  int fractalType;
  int power;
  bool useHybrid;
  hybrid_formula hybrid;
  int derivatives;
  bool tia;
  cl_kernel kernel;
  cl_kernel guessing_kernel;
  cl_kernel glitch_select_2d;
  cl_kernel glitch_select_1d;
  cl_kernel ignore_isolated_glitches;
};

class OpenCLException : public std::exception
{
  virtual const char* what() const noexcept
  {
    return "OpenCL Error";
  }
};

struct OpenCL_ErrorInfo {
  // error handling
  std::string source;
  std::string log;
  std::string message;
  std::string line;
};

struct OpenCL
{
public:

  cl_platform_id platform_id;
  cl_device_id device_id;
  bool supports_double;
  cl_context context;
  cl_command_queue commands;
  cl_program program;

  // config (r)
  cl_mem config;

  // reference (r)
  size_t refx_bytes; cl_mem refx;
  size_t refy_bytes; cl_mem refy;
  size_t refz_bytes; cl_mem refz;
  size_t refN_bytes; cl_mem refN;
  size_t refX_bytes; cl_mem refX;
  size_t refY_bytes; cl_mem refY;
  size_t refZ_bytes; cl_mem refZ;

  // pixel data (rw)
  size_t n1_bytes;  cl_mem n1;
  size_t n0_bytes;  cl_mem n0;
  size_t nf_bytes;  cl_mem nf;
  size_t phase_bytes;cl_mem phase;
  size_t dex_bytes; cl_mem dex;
  size_t dey_bytes; cl_mem dey;

  // glitch selection
  size_t glitch_f_bytes; cl_mem glitch_f;
  size_t glitch_x_bytes; cl_mem glitch_x;
  size_t counts_bytes; cl_mem counts;
  size_t glitch_out_bytes; cl_mem glitch_out;

#ifdef WINVER
  HANDLE mutex;
#else
  std::mutex mutex;
#endif

  // formula kernels
  std::vector<clformula> formulas;

  OpenCL(OpenCL_ErrorInfo*, cl_platform_id platform, cl_device_id device_id, bool supports_double);
  ~OpenCL();

  void lock();
  void unlock();

  OpenCL_ErrorInfo *error;
  void error_print(int err, int loc);
  #define E(err) error_print(err, __LINE__)

  template<typename mantissa, typename exponent, typename R>
  void run
  (
    Reference_Type reftype,
    // for pixel -> parameter mapping
    int32_t m_nX,
    int32_t m_nY,
    uint32_t JitterSeed,
    int32_t JitterShape,
    mantissa JitterScale,
    tfloatexp<mantissa, exponent> m_pixel_center_x,
    tfloatexp<mantissa, exponent> m_pixel_center_y,
    tfloatexp<mantissa, exponent> m_pixel_scale,
    mantissa transform00,
    mantissa transform01,
    mantissa transform10,
    mantissa transform11,
    bool ExponentialMap,
    // for result -> output mapping
    int64_t stride_y,
    int64_t stride_x,
    int64_t stride_offset,
    // for iteration control
    mantissa m_nBailout,
    mantissa m_nBailout2,
    mantissa log_m_nBailout,
    mantissa log_m_nPower,
    int64_t m_nMaxIter,
    int64_t nMaxIter,
    int64_t reference_size_x,
    int16_t m_bNoGlitchDetection,
    int16_t m_bAddReference,
    int16_t m_nSmoothMethod,
    mantissa g_real,
    mantissa g_imag,
    mantissa norm_p,
    mantissa g_FactorAR,
    mantissa g_FactorAI,
    mantissa m_epsilon,
    // for series approximation
    int64_t m_nMaxApproximation,
    int32_t m_nApproxTerms,
    int32_t approximation_type,
    tfloatexp<mantissa, exponent> *APr,
    tfloatexp<mantissa, exponent> *APi,
    SeriesR2<mantissa, exponent> *APs,

    // reference orbit
    const R *rx,
    const R *ry,
    const R *rz,
    size_t roffset,
    size_t rcount,
    size_t rN_size,
    const int64_t *rN,
    const tfloatexp<mantissa, exponent> *rX,
    const tfloatexp<mantissa, exponent> *rY,
    const tfloatexp<mantissa, exponent> *rZ,

    // formula selection
    int m_nFractalType,
    int m_nPower,
    int16_t derivatives,
    bool tia,

    bool UseHybrid,
    const hybrid_formula &hybrid,

    bool UseGuessing,
    int g_nAddRefX,
    int g_nAddRefY,

    int glitch_selection_method,
    int ignore_isolated_neighbourhood,

    // output arrays
    uint32_t *n1_p,
    uint32_t *n0_p,
    float *nf_p,
    float *phase_p,
    float *dex_p,
    float *dey_p,

    bool interactive,
    int reference_number,
    int max_references,

    bool &glitched,
    int &glitched_x,
    int &glitched_y,
    int64_t &glitched_count
  )
  {
    assert(roffset < rcount);
    const bool scaled = reftype == Reference_ScaledFloat || reftype == Reference_ScaledDouble;
    const bool single = reftype == Reference_Float || reftype == Reference_ScaledFloat || reftype == Reference_FloatExpFloat;
    const bool extended = reftype == Reference_FloatExpFloat || reftype == Reference_FloatExpDouble;
    lock();
    try
    {

    // upload reference
    cl_int err;
    refz_bytes = refy_bytes = refx_bytes  = sizeof(rx[0]) * (rcount - roffset);
    refx = clCreateBuffer(context, CL_MEM_READ_ONLY, refx_bytes, nullptr, &err); if (! refx) { E(err); }
    refy = clCreateBuffer(context, CL_MEM_READ_ONLY, refy_bytes, nullptr, &err); if (! refy) { E(err); }
    refz = clCreateBuffer(context, CL_MEM_READ_ONLY, refz_bytes, nullptr, &err); if (! refz) { E(err); }
    refN_bytes = sizeof(rN[0]) * rN_size;
    refZ_bytes = refY_bytes = refX_bytes = sizeof(rX[0]) * rN_size;
    if (rN_size > 0)
    {
      refN = clCreateBuffer(context, CL_MEM_READ_ONLY, refN_bytes, nullptr, &err); if (! refN) { E(err); }
      refX = clCreateBuffer(context, CL_MEM_READ_ONLY, refX_bytes, nullptr, &err); if (! refX) { E(err); }
      refY = clCreateBuffer(context, CL_MEM_READ_ONLY, refY_bytes, nullptr, &err); if (! refY) { E(err); }
      refZ = clCreateBuffer(context, CL_MEM_READ_ONLY, refZ_bytes, nullptr, &err); if (! refZ) { E(err); }
    }
    else
    {
      refN = 0;
      refX = 0;
      refY = 0;
      refZ = 0;
    }
    cl_event uploaded[8];
    int uploaded_n = 0;
    if (rx) E(clEnqueueWriteBuffer(commands, refx, CL_FALSE, 0, refx_bytes, &rx[roffset], 0, 0, &uploaded[uploaded_n++]));
    if (ry) E(clEnqueueWriteBuffer(commands, refy, CL_FALSE, 0, refy_bytes, &ry[roffset], 0, 0, &uploaded[uploaded_n++]));
    if (rz) E(clEnqueueWriteBuffer(commands, refz, CL_FALSE, 0, refz_bytes, &rz[roffset], 0, 0, &uploaded[uploaded_n++]));
    if (rN_size > 0)
    {
      if (rN) E(clEnqueueWriteBuffer(commands, refN, CL_FALSE, 0, refN_bytes, &rN[0], 0, 0, &uploaded[uploaded_n++]));
      if (rX) E(clEnqueueWriteBuffer(commands, refX, CL_FALSE, 0, refX_bytes, &rX[0], 0, 0, &uploaded[uploaded_n++]));
      if (rY) E(clEnqueueWriteBuffer(commands, refY, CL_FALSE, 0, refY_bytes, &rY[0], 0, 0, &uploaded[uploaded_n++]));
      if (rZ) E(clEnqueueWriteBuffer(commands, refZ, CL_FALSE, 0, refZ_bytes, &rZ[0], 0, 0, &uploaded[uploaded_n++]));
    }

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
            n1 = 0;
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
          n1 = 0;
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
            n0 = 0;
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
          n0 = 0;
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
            nf = 0;
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
          nf = 0;
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
            phase = 0;
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
          phase = 0;
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
            dex = 0;
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
          dex = 0;
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
            dey = 0;
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
          dey = 0;
          dey_bytes = 0;
        }
      }
    }

    // upload buffers
    bool upload = m_bAddReference == 0 || glitch_selection_method == 0;
    if (upload)
    {
      // FIXME synchronous, async would be better
      if (n1_p)  E(clEnqueueWriteBuffer(commands, n1,  CL_TRUE, 0, n1_bytes,  n1_p,  0, 0, 0));
      if (n0_p)  E(clEnqueueWriteBuffer(commands, n0,  CL_TRUE, 0, n0_bytes,  n0_p,  0, 0, 0));
      if (nf_p)  E(clEnqueueWriteBuffer(commands, nf,  CL_TRUE, 0, nf_bytes,  nf_p,  0, 0, 0));
      if (phase_p)E(clEnqueueWriteBuffer(commands,phase,CL_TRUE,0, phase_bytes,phase_p,0,0,0));
      if (dex_p) E(clEnqueueWriteBuffer(commands, dex, CL_TRUE, 0, dex_bytes, dex_p, 0, 0, 0));
      if (dey_p) E(clEnqueueWriteBuffer(commands, dey, CL_TRUE, 0, dey_bytes, dey_p, 0, 0, 0));
    }

    // compile formula or retrieve kernel from cache
    clformula *formula = nullptr;
    for (int i = 0; i < (int) formulas.size(); ++i)
    {
      if ( ( ! UseHybrid
          && ! formulas[i].useHybrid
          && formulas[i].reftype == reftype
          && formulas[i].fractalType == m_nFractalType
          && formulas[i].power == m_nPower
          && formulas[i].derivatives == derivatives
          && formulas[i].tia == tia
         ) || ( UseHybrid
          && formulas[i].useHybrid
          && formulas[i].reftype == reftype
          && formulas[i].hybrid == hybrid
          && formulas[i].derivatives == derivatives
          && formulas[i].tia == tia
         ) )
      {
        formula = &formulas[i];
        break;
      }
    }
    if (formula == nullptr)
    {
      std::string source;
      // build program
      if (UseHybrid)
      {
        source = perturbation_opencl(hybrid, derivatives, single, scaled);
      }
      else
      {
        source = perturbation_opencl(m_nFractalType, m_nPower, derivatives, scaled, single);
      }
      std::string name =
        extended ? "perturbation_floatexp" :
        scaled   ? "perturbation_scaled" :
                   "perturbation_double";
      const char *src = source.c_str();
      program = clCreateProgramWithSource(context, 1, &src, 0, &err);
      if (! program) { E(err); }
      // FIXME synchronous program building, async would be better
      std::ostringstream options;
      options << (single ? "-cl-single-precision-constant" : "")
              << " -cl-fast-relaxed-math"
              << " -D MAX_APPROX_TERMS=" << MAX_APPROX_TERMS
              << " -D MAX_HYBRID_STANZAS=" << MAX_HYBRID_STANZAS
              << (tia ? " -D TRIANGLE_INEQUALITY_AVERAGE=1" : "");
      err = clBuildProgram(program, 1, &device_id, options.str().c_str(), 0, 0);
      error->source = source;
      error->log = "";
      if (err != CL_SUCCESS) {
        char *buf = (char *) malloc(1000000);
        buf[0] = 0;
        E(clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, 1000000, &buf[0], 0));
        error->log = buf;
        free(buf);
        E(err);
      }
      cl_kernel kernel = clCreateKernel(program, name.c_str(), &err);
      if (! kernel) { E(err); }
      cl_kernel guessing_kernel = clCreateKernel(program, "guessing", &err);
      if (! guessing_kernel) { E(err); }
      cl_kernel k_glitch_select_2d = clCreateKernel(program, "glitch_select_2d", &err);
      if (! k_glitch_select_2d) { E(err); }
      cl_kernel k_glitch_select_1d = clCreateKernel(program, "glitch_select_1d", &err);
      if (! k_glitch_select_1d) { E(err); }
      cl_kernel k_ignore_isolated_glitches = clCreateKernel(program, "ignore_isolated_glitches", &err);
      if (! k_ignore_isolated_glitches) { E(err); }
      clformula newformula = { reftype, m_nFractalType, m_nPower, UseHybrid, hybrid, derivatives, tia, kernel, guessing_kernel, k_glitch_select_2d, k_glitch_select_1d, k_ignore_isolated_glitches };
      formulas.push_back(newformula);
      formula = &formulas[ssize_t(formulas.size()) - 1];
    }
    assert(formula);

    // upload config
    p_config<mantissa, exponent> configdata =
      {
        sizeof(p_config<mantissa, exponent>),
        UseGuessing ? m_nX / 2 : m_nX,
        UseGuessing ? m_nY / 2 : m_nY,
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
        ExponentialMap,
        stride_y,
        stride_x,
        stride_offset,
        m_nBailout,
        m_nBailout2,
        log_m_nBailout,
        log_m_nPower,
        m_nMaxIter,
        int64_t(rN_size),
        nMaxIter,
        reference_size_x,
        m_bNoGlitchDetection,
        derivatives,
        m_bAddReference,
        m_nSmoothMethod,
        g_real,
        g_imag,
        norm_p,
        g_FactorAR,
        g_FactorAI,
        m_epsilon,
        m_nMaxApproximation,
        m_nApproxTerms,
        approximation_type,
        UseGuessing,
        0,
        g_nAddRefX,
        g_nAddRefY,
        glitch_selection_method == 3,
        glitch_selection_method == 1,
        ignore_isolated_neighbourhood,
        (int16_t) hybrid.loop_start,
        (int16_t) hybrid.stanzas.size(),
        // arrays go here
      };
    for (int i = 0; i < MAX_HYBRID_STANZAS; ++i)
    {
      if (i < (int) hybrid.stanzas.size())
      {
        configdata.hybrid_repeats[i] = hybrid.stanzas[i].repeats;
        configdata.hybrid_log_powers[i] = log(hybrid_power_inf(hybrid.stanzas[i]));
      }
      else
      {
        configdata.hybrid_repeats[i] = 0;
        configdata.hybrid_log_powers[i] = 0;
      }
    }
    const tfloatexp<mantissa, exponent> zero(0);
    for (int i = 0; i < MAX_APPROX_TERMS + 1; ++i)
    {
      configdata.m_APr[i] = APr && i < m_nApproxTerms ? tfloatexp<mantissa, exponent>(APr[i]) : zero;
      configdata.m_APi[i] = APi && i < m_nApproxTerms ? tfloatexp<mantissa, exponent>(APi[i]) : zero;
      for (int j = 0; j < MAX_APPROX_TERMS + 1; ++j)
      {
        configdata.m_APs_s[i][j] = APs ? tfloatexp<mantissa, exponent>(APs->s[i][j]) : zero;
        configdata.m_APs_t[i][j] = APs ? tfloatexp<mantissa, exponent>(APs->t[i][j]) : zero;
      }
    }

    E(clEnqueueWriteBuffer(commands, config, CL_TRUE, 0, sizeof(configdata), &configdata, 0, 0, &uploaded[uploaded_n++]));

    // execute formula
    cl_event formula_executed0;
    E(clSetKernelArg(formula->kernel, 0, sizeof(cl_mem), &config));
    E(clSetKernelArg(formula->kernel, 1, sizeof(cl_mem), refx ? &refx : nullptr));
    E(clSetKernelArg(formula->kernel, 2, sizeof(cl_mem), refy ? &refy : nullptr));
    E(clSetKernelArg(formula->kernel, 3, sizeof(cl_mem), refz ? &refz : nullptr));
    E(clSetKernelArg(formula->kernel, 4, sizeof(cl_mem), n1_p  ? &n1  : nullptr));
    E(clSetKernelArg(formula->kernel, 5, sizeof(cl_mem), n0_p  ? &n0  : nullptr));
    E(clSetKernelArg(formula->kernel, 6, sizeof(cl_mem), nf_p  ? &nf  : nullptr));
    E(clSetKernelArg(formula->kernel, 7, sizeof(cl_mem), phase_p?&phase:nullptr));
    E(clSetKernelArg(formula->kernel, 8, sizeof(cl_mem), dex_p ? &dex : nullptr));
    E(clSetKernelArg(formula->kernel, 9, sizeof(cl_mem), dey_p ? &dey : nullptr));
    E(clSetKernelArg(formula->kernel, 10, sizeof(cl_mem), refN ? &refN : nullptr));
    E(clSetKernelArg(formula->kernel, 11, sizeof(cl_mem), refX ? &refX : nullptr));
    E(clSetKernelArg(formula->kernel, 12, sizeof(cl_mem), refY ? &refY : nullptr));
    E(clSetKernelArg(formula->kernel, 13, sizeof(cl_mem), refZ ? &refZ : nullptr));
    size_t global[2] = { (size_t) configdata.m_nY, (size_t) configdata.m_nX };
    E(clEnqueueNDRangeKernel(commands, formula->kernel, 2, nullptr, global, nullptr, uploaded_n, uploaded, &formula_executed0));

    // make copies for async uploads
    p_config<mantissa, exponent> configdata0 = configdata;
    p_config<mantissa, exponent> configdata1 = configdata;
    p_config<mantissa, exponent> configdata2 = configdata;
    p_config<mantissa, exponent> configdata3 = configdata;
    cl_event formula_executed3;
    if (UseGuessing)
    {
      cl_event guessed;
      // guess at full resolution with 3 more passes at 1/2x2 resolution
      configdata0.m_nX = m_nX;
      configdata0.m_nY = m_nY;
      configdata1.GuessingPass = 1;
      configdata2.GuessingPass = 2;
      configdata3.GuessingPass = 3;
      // do guessing
      cl_event config_uploaded0, config_uploaded1, config_uploaded2, config_uploaded3, formula_executed1, formula_executed2;
      E(clEnqueueWriteBuffer(commands, config, CL_FALSE, 0, sizeof(configdata0), &configdata0, 1, &formula_executed0, &config_uploaded0));
      size_t global_guess[2] = { (size_t) configdata0.m_nY, (size_t) configdata0.m_nX };
      E(clSetKernelArg(formula->guessing_kernel, 0, sizeof(cl_mem), &config));
      E(clSetKernelArg(formula->guessing_kernel, 1, sizeof(cl_mem), n1_p  ? &n1  : nullptr));
      E(clSetKernelArg(formula->guessing_kernel, 2, sizeof(cl_mem), n0_p  ? &n0  : nullptr));
      E(clSetKernelArg(formula->guessing_kernel, 3, sizeof(cl_mem), nf_p  ? &nf  : nullptr));
      E(clSetKernelArg(formula->guessing_kernel, 4, sizeof(cl_mem), phase_p?&phase:nullptr));
      E(clSetKernelArg(formula->guessing_kernel, 5, sizeof(cl_mem), dex_p ? &dex : nullptr));
      E(clSetKernelArg(formula->guessing_kernel, 6, sizeof(cl_mem), dey_p ? &dey : nullptr));
      E(clEnqueueNDRangeKernel(commands, formula->guessing_kernel, 2, nullptr, global_guess, nullptr, 1, &config_uploaded0, &guessed));
      // do 3 more passes at 1/2x2 resolution
      E(clEnqueueWriteBuffer(commands, config, CL_FALSE, 0, sizeof(configdata1), &configdata1, 1, &guessed, &config_uploaded1));
      E(clEnqueueNDRangeKernel(commands, formula->kernel, 2, nullptr, global, nullptr, 1, &config_uploaded1, &formula_executed1));
      E(clEnqueueWriteBuffer(commands, config, CL_FALSE, 0, sizeof(configdata2), &configdata2, 1, &formula_executed1, &config_uploaded2));
      E(clEnqueueNDRangeKernel(commands, formula->kernel, 2, nullptr, global, nullptr, 1, &config_uploaded2, &formula_executed2));
      E(clEnqueueWriteBuffer(commands, config, CL_FALSE, 0, sizeof(configdata3), &configdata3, 1, &formula_executed2, &config_uploaded3));
      E(clEnqueueNDRangeKernel(commands, formula->kernel, 2, nullptr, global, nullptr, 1, &config_uploaded3, &formula_executed3));
    }
    cl_event formula_executed = UseGuessing ? formula_executed3 : formula_executed0;

    cl_event ignored;
    if (ignore_isolated_neighbourhood > 0)
    {
      size_t global[2] = { (size_t) configdata0.m_nY, (size_t) configdata0.m_nX };
      E(clSetKernelArg(formula->ignore_isolated_glitches, 0, sizeof(cl_mem), &config));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 1, sizeof(cl_mem), n1_p  ? &n1  : nullptr));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 2, sizeof(cl_mem), n0_p  ? &n0  : nullptr));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 3, sizeof(cl_mem), nf_p  ? &nf  : nullptr));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 4, sizeof(cl_mem), phase_p?&phase:nullptr));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 5, sizeof(cl_mem), dex_p ? &dex : nullptr));
      E(clSetKernelArg(formula->ignore_isolated_glitches, 6, sizeof(cl_mem), dey_p ? &dey : nullptr));
      E(clEnqueueNDRangeKernel(commands, formula->ignore_isolated_glitches, 2, nullptr, global, nullptr, 1, &formula_executed, &ignored));
    }
    else
    {
      ignored = formula_executed;
    }

    glitched = glitch_selection_method != 3;
    if (glitch_selection_method != 0)
    {
      if (glitch_f_bytes != sizeof(cl_float) * m_nX)
      {
        if (glitch_f)
        {
          clReleaseMemObject(glitch_f);
        }
        glitch_f_bytes = sizeof(cl_float) * m_nX;
        glitch_f = clCreateBuffer(context, CL_MEM_READ_WRITE, glitch_f_bytes, nullptr, &err); if (! glitch_f) { E(err); }
      }
      if (glitch_x_bytes != sizeof(cl_int) * m_nX)
      {
        if (glitch_x)
        {
          clReleaseMemObject(glitch_x);
        }
        glitch_x_bytes = sizeof(cl_int) * m_nX;
        glitch_x = clCreateBuffer(context, CL_MEM_READ_WRITE, glitch_x_bytes, nullptr, &err); if (! glitch_f) { E(err); }
      }
      if (counts_bytes != sizeof(cl_int) * m_nX)
      {
        if (counts)
        {
          clReleaseMemObject(counts);
        }
        counts_bytes = sizeof(cl_int) * m_nX;
        counts = clCreateBuffer(context, CL_MEM_READ_WRITE, counts_bytes, nullptr, &err); if (! counts) { E(err); }
      }
      if (glitch_out_bytes != sizeof(p_glitch))
      {
        if (glitch_out)
        {
          clReleaseMemObject(glitch_out);
        }
        glitch_out_bytes = sizeof(p_glitch);
        glitch_out = clCreateBuffer(context, CL_MEM_WRITE_ONLY, glitch_out_bytes, nullptr, &err); if (! glitch_out) { E(err); }
      }
      cl_event glitch_selected_2d;
      size_t rows[1] = { (size_t) configdata0.m_nY };
      E(clSetKernelArg(formula->glitch_select_2d, 0, sizeof(cl_mem), &config));
      E(clSetKernelArg(formula->glitch_select_2d, 1, sizeof(cl_mem), nf_p ? &nf : nullptr));
      E(clSetKernelArg(formula->glitch_select_2d, 2, sizeof(cl_mem), &glitch_f));
      E(clSetKernelArg(formula->glitch_select_2d, 3, sizeof(cl_mem), &glitch_x));
      E(clSetKernelArg(formula->glitch_select_2d, 4, sizeof(cl_mem), &counts));
      E(clEnqueueNDRangeKernel(commands, formula->glitch_select_2d, 1, nullptr, rows, nullptr, 1, &ignored, &glitch_selected_2d));
      cl_event glitch_selected_1d;
      size_t singleton[1] = { (size_t) 1 };
      E(clSetKernelArg(formula->glitch_select_1d, 0, sizeof(cl_mem), &config));
      E(clSetKernelArg(formula->glitch_select_1d, 1, sizeof(cl_mem), &glitch_f));
      E(clSetKernelArg(formula->glitch_select_1d, 2, sizeof(cl_mem), &glitch_x));
      E(clSetKernelArg(formula->glitch_select_1d, 3, sizeof(cl_mem), &counts));
      E(clSetKernelArg(formula->glitch_select_1d, 4, sizeof(cl_mem), &glitch_out));
      E(clEnqueueNDRangeKernel(commands, formula->glitch_select_1d, 1, nullptr, singleton, nullptr, 1, &glitch_selected_2d, &glitch_selected_1d));
      p_glitch glitch_out_p = { 0, -1, -1, 1.0f / 0.0f };
      E(clEnqueueReadBuffer(commands, glitch_out,  CL_TRUE, 0, glitch_out_bytes, &glitch_out_p, 1, &glitch_selected_1d, 0));
      glitched = glitch_out_p.f < 1.0f / 0.0f;
      glitched_x = glitch_out_p.x;
      glitched_y = glitch_out_p.y;
      glitched_count = glitch_out_p.count;
    }

    // download results
    bool download
      = interactive
      || glitch_selection_method == 0
      || ! glitched
      || reference_number == max_references;
    if (download)
    {
      // FIXME synchronous, async would be better
      if (n1_p)  E(clEnqueueReadBuffer(commands, n1,  CL_TRUE, 0, n1_bytes,  n1_p,  1, &ignored, 0));
      if (n0_p)  E(clEnqueueReadBuffer(commands, n0,  CL_TRUE, 0, n0_bytes,  n0_p,  1, &ignored, 0));
      if (nf_p)  E(clEnqueueReadBuffer(commands, nf,  CL_TRUE, 0, nf_bytes,  nf_p,  1, &ignored, 0));
      if (phase_p)E(clEnqueueReadBuffer(commands,phase,CL_TRUE,0, phase_bytes,phase_p,1,&ignored,0));
      if (dex_p) E(clEnqueueReadBuffer(commands, dex, CL_TRUE, 0, dex_bytes, dex_p, 1, &ignored, 0));
      if (dey_p) E(clEnqueueReadBuffer(commands, dey, CL_TRUE, 0, dey_bytes, dey_p, 1, &ignored, 0));
    }

    // clean up reference
    if (refN_bytes > 0)
    {
      clReleaseMemObject(refZ);
      refZ = 0;
      clReleaseMemObject(refY);
      refY = 0;
      clReleaseMemObject(refX);
      refX = 0;
      clReleaseMemObject(refN);
      refN = 0;
    }
    clReleaseMemObject(refz);
    refz = 0;
    clReleaseMemObject(refy);
    refy = 0;
    clReleaseMemObject(refx);
    refx = 0;

    }
    catch (OpenCLException &e)
    {
      unlock();
      throw;
    }
    unlock();
  }

#undef E

};

std::vector<cldevice> initialize_opencl(OpenCL_ErrorInfo *cle
#ifdef WINVER
                                        , HWND hWnd
#endif
                                        );

#endif
#endif
