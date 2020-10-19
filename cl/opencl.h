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
);

extern std::string perturbation_opencl
( const hybrid_formula &hybrid
, int derivatives
);

typedef struct __attribute__((packed))
{
  size_t BYTES;
  // for pixel -> parameter mapping
  int32_t m_nX;
  int32_t m_nY;
  uint32_t JitterSeed;
  int32_t JitterShape;
  double JitterScale;
  floatexp m_pixel_center_x;
  floatexp m_pixel_center_y;
  floatexp m_pixel_scale;
  double transform00;
  double transform01;
  double transform10;
  double transform11;
  // for result -> output mapping
  int64_t stride_y;
  int64_t stride_x;
  int64_t stride_offset;
  // for iteration control
  double m_nBailout;
  double m_nBailout2;
  double log_m_nBailout;
  double log_m_nPower;
  int64_t m_nGlitchIter;
  int64_t m_nMaxIter;
  int64_t nMaxIter;
  int64_t nMinIter;
  int16_t m_bNoGlitchDetection;
  int16_t derivatives;
  int16_t m_bAddReference;
  int16_t m_nSmoothMethod;
  double g_real;
  double g_imag;
  double norm_p;
  double g_FactorAR;
  double g_FactorAI;
  double m_epsilon;
  // for series approximation
  double m_dPixelSpacing;
  floatexp m_fPixelSpacing;
  int64_t m_nMaxApproximation;
  int32_t m_nApproxTerms;
  int32_t approximation_type;
  // for hybrid
  int16_t hybrid_loop_start;
  int16_t hybrid_nstanzas;
  int32_t hybrid_repeats[MAX_HYBRID_STANZAS];
  int32_t hybrid_powers[MAX_HYBRID_STANZAS];
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
  cl_kernel kernel;
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

  template <typename T>
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

    bool UseHybrid,
    const hybrid_formula &hybrid,

    // output arrays
    uint32_t *n1_p,
    uint32_t *n0_p,
    float *nf_p,
    float *phase_p,
    float *dex_p,
    float *dey_p
  );
};

std::vector<cldevice> initialize_opencl();

#endif
#endif
