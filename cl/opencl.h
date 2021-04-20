#ifdef KF_OPENCL
#ifndef KF_OPENCL_H
#define KF_OPENCL_H 1

#include <windows.h>
#include <stdint.h>
#include <string>
#include <vector>

#include "../fraktal_sft/floatexp.h"
struct SeriesR2;
#include "../fraktal_sft/hybrid.h"

#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#include "clew.h"

extern std::string perturbation_opencl
( int m_nFractalType
, int m_nPower
, int derivatives
, int scaled
);

extern std::string perturbation_opencl
( const hybrid_formula &hybrid
, int derivatives
);

typedef struct __attribute__((packed))
{
  cl_long BYTES;
  // for pixel -> parameter mapping
  cl_int m_nX;
  cl_int m_nY;
  cl_uint JitterSeed;
  cl_int JitterShape;
  cl_double JitterScale;
  floatexp m_pixel_center_x;
  floatexp m_pixel_center_y;
  floatexp m_pixel_scale;
  cl_double transform00;
  cl_double transform01;
  cl_double transform10;
  cl_double transform11;
  cl_long ExponentialMap;
  // for result -> output mapping
  cl_long stride_y;
  cl_long stride_x;
  cl_long stride_offset;
  // for iteration control
  cl_double m_nBailout;
  cl_double m_nBailout2;
  cl_double log_m_nBailout;
  cl_double log_m_nPower;
  cl_long m_nGlitchIter;
  cl_long m_nMaxIter;
  cl_long m_nRSize;
  cl_long nMaxIter;
  cl_long nMinIter;
  cl_short m_bNoGlitchDetection;
  cl_short derivatives;
  cl_short m_bAddReference;
  cl_short m_nSmoothMethod;
  cl_double g_real;
  cl_double g_imag;
  cl_double norm_p;
  cl_double g_FactorAR;
  cl_double g_FactorAI;
  cl_double m_epsilon;
  // for series approximation
  cl_long m_nMaxApproximation;
  cl_int m_nApproxTerms;
  cl_int approximation_type;
  // for guessing
  cl_int UseGuessing;
  cl_int GuessingPass;
  cl_int g_nAddRefX;
  cl_int g_nAddRefY;
  // for hybrid
  cl_short hybrid_loop_start;
  cl_short hybrid_nstanzas;
  cl_int hybrid_repeats[MAX_HYBRID_STANZAS];
  cl_double hybrid_log_powers[MAX_HYBRID_STANZAS];
  // 130kB data follows
  floatexp m_APr[MAX_APPROX_TERMS + 1];
  floatexp m_APi[MAX_APPROX_TERMS + 1];
  floatexp m_APs_s[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
  floatexp m_APs_t[MAX_APPROX_TERMS + 1][MAX_APPROX_TERMS + 1];
} p_config;

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
  int type;
  int fractalType;
  int power;
  bool useHybrid;
  hybrid_formula hybrid;
  int derivatives;
  bool scaled;
  cl_kernel kernel;
  cl_kernel guessing_kernel;
};

struct OpenCL
{
private:

  cl_platform_id platform_id;
  cl_device_id device_id;
  cl_context context;
  cl_command_queue commands;
  cl_program program;

  // config (r)
  size_t config_bytes;
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

  HANDLE mutex;

  // formula kernels
  std::vector<clformula> formulas;

public:

  OpenCL(cl_platform_id platform, cl_device_id device_id);
  ~OpenCL();

  void lock();
  void unlock();

  void run(
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
    bool ExponentialMap,
    // for result -> output mapping
    int64_t stride_y,
    int64_t stride_x,
    int64_t stride_offset,
    // for iteration control
    double m_nBailout,
    double m_nBailout2,
    double log_m_nBailout,
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
    double norm_p,
    double g_FactorAR,
    double g_FactorAI,
    double m_epsilon,
    // for series approximation
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
    size_t rN_size,
    const int64_t *rN,
    const floatexp *rX,
    const floatexp *rY,
    const floatexp *rZ,

    // formula selection
    int type,
    int m_nFractalType,
    int m_nPower,
    int16_t derivatives,
    bool scaled,

    bool UseHybrid,
    const hybrid_formula &hybrid,

    bool UseGuessing,
    int g_nAddRefX,
    int g_nAddRefY,

    // output arrays
    uint32_t *n1_p,
    uint32_t *n0_p,
    float *nf_p,
    float *phase_p,
    float *dex_p,
    float *dey_p
  );
};

std::vector<cldevice> initialize_opencl(HWND hWnd);

class OpenCLException : public std::exception
{
  virtual const char* what() const noexcept
  {
    return "OpenCL Error";
  }
};

extern std::string g_OpenCL_Error_Source;
extern std::string g_OpenCL_Error_Log;
extern std::string g_OpenCL_Error_Message;
extern std::string g_OpenCL_Error_Line;

#endif
#endif
