#ifndef OPENCL_H
#define OPENCL_H 1

#include <stdint.h>
#include <string>
#include <vector>

#include "../fraktal_sft/floatexp.h"

#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#include "clew.h"

struct p_config
{
  int32_t count;
  int32_t width;
  int32_t height;
  int32_t m_nX;
  int32_t m_nY;
  int32_t antal;
  int32_t m_nMaxIter;
  int32_t m_nGlitchIter;
  int32_t m_bNoGlitchDetection;
  int32_t m_nSmoothMethod;
  int32_t m_nPower;
  int32_t m_nMaxApproximation;
  int32_t m_nTerms;
  int32_t m_bAddReference;
  double m_nBailout;
  double m_nBailout2;
  double g_real;
  double g_imag;
  double g_FactorAR;
  double g_FactorAI;
  double m_C;
  double m_S;
};

struct cldevice
{
  cl_platform_id pid;
  cl_device_id did;
  std::string name;
  std::string vendor;
  std::string version;
};

struct clformula
{
  int type;
  int power;
  cl_kernel doublek;
  cl_kernel floatexpk;
};

std::vector<clformula> compile_kernels(cl_program program);

struct OpenCL
{
private:

  HANDLE mutex;

  cl_platform_id platform_id;
  cl_device_id device_id;
  cl_context context;
  cl_command_queue commands;
  cl_program program;

  // reference (r)
  size_t ref_bytes;
  cl_mem refx;
  cl_mem refy;
  size_t ref_bytesz;
  cl_mem refz;

  // approximation (r)
  size_t dx_bytes;
  cl_mem dx;
  size_t dy_bytes;
  cl_mem dy;
  size_t ap_bytes;
  cl_mem apr;
  cl_mem api;

  // config (r)
  size_t config_bytes;
  cl_mem config;

  // initial iterates (rw)
  size_t cx_bytes;
  cl_mem cx;

  // iteration output (w)
  size_t pixels_bytes;
  cl_mem pixels;
  size_t trans_bytes;
  cl_mem trans;

  // series approximation kernels
  cl_kernel approxdoublek;
  cl_kernel approxfloatexpk;

  // formula kernels
  std::vector<clformula> formulas;

public:

  OpenCL(cl_platform_id platform, cl_device_id device_id);
  ~OpenCL();

  void lock();
  void unlock();

  void upload_reference(const double *rx, const double *ry, const double *rz, size_t m_nMaxIter);
  void upload_reference(const floatexp *rx, const floatexp *ry, const double *rz, size_t m_nMaxIter);
  void upload_approximation(const double *m_pDX, size_t m_nX, const double *m_pDY, size_t m_nY, const floatexp *m_APr, const floatexp *m_APi, size_t m_nTerms);
  void upload_approximation(const floatexp *m_DX, size_t m_nX, const floatexp *m_DY, size_t m_nY, const floatexp *m_APr, const floatexp *m_APi, size_t m_nTerms);
  void upload_config(
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
  );
  void execute_approximation(int type, cl_int count);
  void execute_formula(int type, int m_nFractalType, int m_nPower, cl_int count);
  void download_iterations(int **m_nPixels, float **m_nTrans, size_t m_nX, size_t m_nY);
};

std::vector<cldevice> initialize_opencl();

#endif
