#ifndef OPENCL_H
#define OPENCL_H 1

#include <stdint.h>
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
  double m_nBailout;
  double m_nBailout2;
  double g_real;
  double g_imag;
  double g_FactorAR;
  double g_FactorAI;
  double m_C;
  double m_S;
};

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

kfcl initialize_opencl(cl_uint PLATFORM);
void upload_reference(kfcl &cl, const double *rx, const double *ry, const double *rz, size_t count);
void upload_reference(kfcl &cl, const floatexp *rx, const floatexp *ry, const double *rz, size_t count);
void upload_config(kfcl &cl,
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
  double m_S
);

#endif
