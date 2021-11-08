/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef KF_FORMULA_H
#define KF_FORMULA_H 1

#include <algorithm>
#include <cstdint>
#include <cmath>

#include "../fraktal_sft/floatexp.h"

class CFixedFloat;
class Reference;

// https://fractalforums.org/fractal-mathematics-and-new-theories/28/perturbation-theory/487/msg3170#msg3170
// |2w'(w+z)+1|/|delta0|+|w|(|w+2z|+|w|+2|z|)<epsilon/h
template <typename R>
static inline R mag(const R &x, const R &y)
{
  using std::abs;
  return abs(x) + abs(y);
}

template <typename R>
static inline bool type_0_power_2_pixel_has_glitched(R cr, R ci, R zr, R zi, R Zr, R Zi, R dr, R di, R e, R h)
{
  R Zzr = Zr + zr;
  R Zzi = Zi + zi;
  R a = mag(2 * (dr * Zzr - di * Zzi) + 1, 2 * (dr * Zzi + di * Zzr));
  R b = mag(cr, ci) + mag(zr, zi) * (mag(zr + 2 * Zr, zi + 2 * Zi) + mag(zr, zi) + 2 * mag(Zr, Zi));
  return a * h < b * e;
}

static inline float diffabs(const float &c, const float &d)
{
  const float cd = c + d;
  const float c2d = 2 * c + d;
  return c >= 0 ? cd >= 0 ? d : -c2d : cd > 0 ? c2d : -d;
}

static inline double diffabs(const double &c, const double &d)
{
  const double cd = c + d;
  const double c2d = 2.0 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

static inline long double diffabs(const long double &c, const long double &d)
{
  const long double cd = c + d;
  const long double c2d = 2.0 * c + d;
  return c >= 0.0 ? cd >= 0.0 ? d : -c2d : cd > 0.0 ? c2d : -d;
}

template<typename R>
static inline R pnorm(const double g_real, const double g_imag, const double p, const R x, const R y)
{
  using std::abs;
  using std::max;
  using std::pow;
  if (g_real == 1.0 && g_imag == 1.0 && p == 2.0)
  {
    return x * x + y * y;
  }
  if (p == 1.0)
  {
    return abs(g_real * abs(x) + g_imag * abs(y));
  }
  if (p == 1.0/0.0)
  {
    return abs(max(g_real * abs(x), g_imag * abs(y)));
  }
  return abs(g_real * pow(abs(x), p) + g_imag * pow(abs(y), p));
}

template <>
inline floatexp pnorm(const double g_real, const double g_imag, const double p, const floatexp xx, const floatexp yy)
{
  using std::abs;
  using std::max;
  using std::pow;
  double x(xx);
  double y(yy);
  if (g_real == 1.0 && g_imag == 1.0 && p == 2.0)
  {
    return x * x + y * y;
  }
  if (p == 1.0)
  {
    return abs(g_real * abs(x) + g_imag * abs(y));
  }
  if (p == 1.0/0.0)
  {
    return abs(max(g_real * abs(x), g_imag * abs(y)));
  }
  return abs(g_real * pow(abs(x), p) + g_imag * pow(abs(y), p));
}


using std::isinf;
using std::copysign;
using std::isnan;

#define I(T) inline T infnan_to_zero(const T &a) { return isinf(a) ? copysign((T)(1e30), a) : isnan(a) ? (T)(0) : a; }
I(float)
I(double)
I(long double)
#undef I

static inline int sgn(const int &a)
{
  return (a > 0) - (a < 0);
}
static inline float sgn(const float &a)
{
  return (a > 0) - (a < 0);
}
static inline double sgn(const double &a)
{
  return (a > 0) - (a < 0);
}
static inline long double sgn(const long double &a)
{
  return (a > 0) - (a < 0);
}
static inline tfloatexp<float,int32_t> sgn(const tfloatexp<float,int32_t> &a)
{
  return (a.val > 0) - (a.val < 0);
}
static inline tfloatexp<double,int64_t> sgn(const tfloatexp<double,int64_t> &a)
{
  return (a.val > 0) - (a.val < 0);
}

template<typename T>
static inline T special_93_2(const T &XC, const T &Xr, const T &Xxr, const T &xr) noexcept
{
  if (Xr > T(-7.0/4.0))
  {
    return Xxr > T(-7.0/4.0) ? T(2.0) * Xr + xr : T(-7.0/2.0) - xr;
  }
  else
  {
    return Xxr > T(-7.0/4.0) ? xr - T(7.0/2.0) : -XC - xr;
  }
}

template<typename T, typename V>
static inline V special_93_2(const T &XC, const T &Xr, const V &Xxr, const V &xr) noexcept
{
  if (Xr > T(-7.0/4.0))
  {
    return Xxr.v > T(-7.0/4.0) ? (T(2.0 * Xr) + xr).v : (T(-7.0/2.0) - xr).v;
  }
  else
  {
    return Xxr.v > T(-7.0/4.0) ? (xr - T(7.0/2.0)).v : (-XC - xr).v;
  }
}

// reference

bool reference
  ( const int m_nFractalType, const int m_nPower
  , Reference *Reference
  , bool &m_bStop, int64_t &m_nRDone, int64_t &m_nMaxIter
  , const CFixedFloat &Cr, const CFixedFloat &Ci
  , const double g_SeedR, const double g_SeedI
  , const double g_FactorAR, const double g_FactorAI
  , const double terminate
  , const double m_bGlitchLowTolerance
  );

int reference_glitches(const int m_nFractalType, const int m_nPower);

// perturbation

template <typename T>
bool perturbation_simple
  ( const int m_nFractalType, const int m_nPower
  , const Reference *Reference
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double &m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p
  , const double &g_FactorAR, const double &g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , const bool singleref
  );

// perturbation with derivatives

template <typename T>
bool perturbation_simple_derivatives
  ( const int m_nFractalType, const int m_nPower
  , const Reference *Reference
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double &m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p
  , const double &g_FactorAR, const double &g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , T &Jxa, T &Jxb, T &Jya, T &Jyb
  , const T &e, const T &h
  , const T &daa, const T &dab, const T &dba, const T &dbb
  , const bool noDerivativeGlitch
  , const bool singleref
  );

// perturbation with SIMD

template <typename double1, typename intN, typename doubleN>
bool perturbation_SIMD
  ( const int m_nFractalType, const int m_nPower
  , const Reference *Reference
  , intN &antal, doubleN &test1, doubleN &test2, doubleN &phase, intN &bGlitch
  , const double1 &m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double1 &g_real, const double1 &g_imag, const double1 &p
  , const double1 &g_FactorAR, const double1 &g_FactorAI
  , doubleN &xr, doubleN &xi
  , const doubleN &cr, const doubleN &ci
  , const int64_t chunksize
  );

// perturbation with SIMD and derivatives

template <typename double1, typename intN, typename doubleN>
bool perturbation_SIMD_derivatives
  ( const int m_nFractalType, const int m_nPower
  , const Reference *Reference
  , intN &antal, doubleN &test1, doubleN &test2, doubleN &phase, intN &bGlitch
  , const double1 &m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double1 &g_real, const double1 &g_imag, const double1 &p
  , const double1 &g_FactorAR, const double1 &g_FactorAI
  , doubleN &xr, doubleN &xi
  , const doubleN &cr, const doubleN &ci
  , doubleN &Jxa, doubleN &Jxb, doubleN &Jya, doubleN &Jyb
  , const double1 &e, const double1 &h
  , const doubleN &daa, const doubleN &dab, const doubleN &dba, const doubleN &dbb
  , const int64_t chunksize
  , const bool noDerivativeGlitch
  );

// perturbation with scaling

template <typename mantissa, typename exponent>
bool perturbation_scaled
  ( const int m_nFractalType, const int m_nPower
  , const Reference *m_Reference
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double &m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p
  , const double &g_FactorAR, const double &g_FactorAI
  , tfloatexp<mantissa, exponent> &xr00, tfloatexp<mantissa, exponent> &xi00
  , const tfloatexp<mantissa, exponent> &cr0, const tfloatexp<mantissa, exponent> &ci0
  , const bool singleref
  );

#if 0
// perturbation with SIMD and scaling

template <typename intN, typename doubleN>
bool perturbation
  ( int m_nFractalType, int m_nPower
  , const Reference *Reference
  , intN &antal0, doubleN &test10, doubleN &test20, doubleN &phase0, intN &bGlitch0
  , double m_nBailout2, const int64_t nMaxIter
  , const intN &m_bNoGlitchDetection, const double g_real, const double g_imag, const double p
  , const double g_FactorAR, const double g_FactorAI
  , doubleN &xr00, doubleN &xi00
  , const doubleN &cr0, const doubleN &ci0
  , const int64_t chunksize
  , const double s, const double S
  );

#endif

// perturbation with derivatives and scaling

template <typename mantissa, typename exponent>
bool perturbation_scaled_derivatives
  ( int m_nFractalType, int m_nPower
  , const Reference *m_Reference
  , int64_t &antal, double &test1, double &test2, double &phase, bool &bGlitch
  , const double &m_nBailout2, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p
  , const double &g_FactorAR, const double &g_FactorAI
  , tfloatexp<mantissa, exponent> &xr0, tfloatexp<mantissa, exponent> &xi0
  , const tfloatexp<mantissa, exponent> &cr, const tfloatexp<mantissa, exponent> &ci
  , tfloatexp<mantissa, exponent> &Jxa0F, tfloatexp<mantissa, exponent> &Jxb0F, tfloatexp<mantissa, exponent> &Jya0F, tfloatexp<mantissa, exponent> &Jyb0F
  , const tfloatexp<mantissa, exponent> &daaF, const tfloatexp<mantissa, exponent> &dabF, const tfloatexp<mantissa, exponent> &dbaF, const tfloatexp<mantissa, exponent> &dbbF
  , const bool singleref
  );

// convergent perturbation

template <typename T>
bool perturbation_convergent_simple
  ( const int m_nFractalType, const int m_nPower
  , const Reference *Reference
  , int64_t &antal, double &test1, double &smooth, double &phase, bool &bGlitch
  , const T &m_nBailoutSmallP, const int64_t nMaxIter
  , const bool m_bNoGlitchDetection, const double &g_real, const double &g_imag, const double &p
  , const double &g_FactorAR, const double &g_FactorAI
  , T &xr, T &xi
  , const T &cr, const T &ci
  , const bool singleref
  );

// miscellaneous

typedef struct HWND__* HWND;
bool is_convergent(const int m_nFractalType, const int m_nPower);
bool scaling_supported(const int m_nFractalType, const int m_nPower, const bool derivatives);
void combo5_addstrings(HWND hWnd, const int combo, bool ignore_hybrids);
int combo5_lookup_fractal_type(HWND hWnd, int index, bool ignore_hybrids);
int combo5_lookup_dropdown_index(HWND hWnd, int type, bool ignore_hybrids);
int validate_power_for_fractal_type(const int m_nFractalType, const int m_nPower);
void update_power_dropdown_for_fractal_type(HWND hWnd, const int combo, const int m_nFractalType, const int m_nPower);
bool builtin_get_hybrid(const int type, const int power, std::string &hybrid);
bool hybrid_get_builtin(const std::string &hybrid, int &type, int &power);

#endif
